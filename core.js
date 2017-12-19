"use strict";

const flatten = (...list) => {
	const begin = Symbol();
	const end = Symbol();
	const f = list => [].concat(...list.map(a => Array.isArray(a) ? [begin, ...f(a), end] : a));
	return [begin, end, f(list)];
};

const unflatten = (begin, end, list) => {
	const pos = list.lastIndexOf(begin);
	if(pos < 0){
		if(list.includes(end)) throw SyntaxError("Invalid token");
		return list;
	}
	list.splice(pos, 0, list.splice(pos, list.indexOf(end, pos) - pos + 1).slice(1, -1));
	return unflatten(begin, end, list);
};

const code2ast = source => {
	source = source.replace(/\r\n?/gu, "\n");
	const words = source.match(/`(?:\\`|[^`])*`|[[\]]|(?:(?![[;`\]])\S)+|;.*/gu);
	return unflatten("[", "]", words.filter(a => !/^;/u.test(a)).map(a => {
		if(/^[[\]]$/u.test(a)) return a;
		if(/^`.*`$/u.test(a)) return {flag: "`", content:
			a.slice(1, -1)
			.replace(/(^|[^\\])\\[nrt]/gu, ($, $1) => $1 + {n: "\n", r: "\r", t: "\t"}[$1])
			.replace(/(^|[^\\])\\([0-9a-f]+);/giu, ($, $1, $2) => $1 + String.fromCodePoint(Number.parseInt($2, 16)))
			.replace(/\\(\\+(?:[0-9A-Fa-f]+;|[nrt])|\\*`)/gu, "$1")};
		return {flag: "", content: a};
	}));
};

const buffer_fn = f => (...args) => f(...args.map(a => new Uint8Array(a))).buffer;

const num2uint = number => {
	if(!Number.isSafeInteger(number) || number < 0) throw TypeError("Only safe natural number can be converted to uint.");
	const result = [];
	while(number >= 1){
		result.push(number % 0xff);
		number /= 0xff;
	}
	return new Uint8Array(result).buffer;
};

const str2num = (a, radix = 10) => {
	if(a.toLowerCase() !== (a = Number.parseInt(a, radix)).toString(radix) || Number.isNaN(a)) throw SyntaxError("Invalid token");
	return a;
};

const ast2signals = source => {
	const [begin, end, list] = flatten(...source);
	for(let i in list){
		if(typeof list[i] !== "object") continue;
		let {content} = list[i];
		if(!list[i].flag){
			if(/^\\/u.test(content)){
				list[i] = num2uint(str2num(content.slice(1)));
				continue;
			}
			if(/^%/u.test(content)){
				if(!(content.length % 2)) throw SyntaxError("Invalid token");
				content = content.slice(1);
				list[i] = new Uint8Array(content.length / 2).map((a, i) => str2num(content.substr(i * 2, 2).replace(/^0*/u, ""), 16)).buffer;
				continue;
			}
			if(/^\$\d/u.test(content)) content = "$".repeat(str2num(content.slice(1)));
		}
		list[i] = content;
	}
	return unflatten(begin, end, list);
};

const code2signals = source => ast2signals(code2ast(source));

const reflex = free => {
	const off = () => children.size || listeners.size || free && free();
	const children = new Map;
	const listeners = new Set;
	const f = path => ({
		emit: (...list) => {
			list = path.concat(list);
			const listeners1 = Array.from(listeners).reverse();
			if(list.length) (children.get(list[0]) || {emit(){}}).emit(...list.slice(1));
			listeners1.forEach(f => listeners.has(f) && f(...list));
		},
		on: (...rest) => {
			let first = (rest = path.concat(rest)).shift();
			if(!rest.length){
				first = first.bind();
				listeners.add(first);
				return () => {
					listeners.delete(first);
					off();
				};
			}
			if(!children.has(first)) children.set(first, reflex(() => {
				children.delete(first);
				off();
			}));
			return children.get(first).on(...rest);
		},
		path: (...list) => f(path.concat(list)),
	});
	return f([]);
};

const multi_key_map = () => {
	const dot = new Map;
	const f = (node, keys) => node && keys.length ? f(node.get(keys.shift()), keys) : node;
	return {
		set: (...keys) => {
			const [value] = keys.splice(-1, 1, dot);
			const f = parent => {
				const key = keys.shift();
				if(value === undefined){
					if(key !== dot){
						const child = parent.get(key);
						if(!child || !f(child)) return;
					}
					parent.delete(key);
					return !parent.size;
				}
				if(key !== dot) return f(parent.get(key) || (() => {
					const child = new Map;
					parent.set(key, child);
					return child;
				})());
				parent.set(key, value);
			};
			f(dot);
		},
		get: (...keys) => f(dot, keys.concat(dot)),
	};
};

const is_buffer = a => {
	try{
		Reflect.getOwnPropertyDescriptor(ArrayBuffer.prototype, "byteLength").get.call(a);
		return new Uint8Array(a).buffer;
	}catch(error){}
};

const buffer2bin = buffer => Array.from(new Uint8Array(buffer)).map(a => String.fromCodePoint(a)).join("");

const str2utf8 = string => new Uint8Array([].concat(...Array.from(string).map(a => {
	const f = (a, i) => i ? f(a >> 6, i - 1).concat(0x80 | a & 0x3f) : [];
	a = a.codePointAt();
	if(a < 0x80) return [a];
	const list = f(a, a = Math.floor(Math.log2(a) / 6) + 1);
	list[0] |= ~(1 << 8 - a) + 1;
	return list;
}))).buffer;

const str2bin = string => buffer2bin(str2utf8(string));

const bin2buffer = binary => new Uint8Array(Array.from(binary).map(a => a.codePointAt())).buffer;

const is_str = a => typeof a === "string";

const is_token = a => is_str(a) && /^\$*$/u.test(a);

const vm = () => {
	const flatten1 = (...list) => {
		const [begin1, end1] = [, , list] = flatten(...list);
		return list.map(a => a === begin1 ? begin : a === end1 ? end : a);
	};
	const unflatten1 = list => unflatten(begin, end, list);
	const encode = (...list) => flatten1(...list).map(a => {
		const buffer = is_buffer(a);
		return buffer ? buffer2bin(buffer) : is_str(a) ? str2bin(a) : a;
	});
	const decode = list => unflatten1(list.map(a => is_str(a) ? bin2buffer(a) : a));
	const on = (path, listener) => {
		if(!listener) return reflex0.on((...list) => path(...decode(list)));
		path = encode(path);
		const m = path.slice().reverse().concat(0).findIndex(a => a !== end);
		return reflex0.on(...path.slice(0, path.length - m), (...list) => listener(...decode(Array((m || 1) - 1).fill(begin).concat(list.slice(0, -1)))));
	};
	const emit0 = (...signals) => signals.forEach(signal => reflex0.emit(...encode(signal)));
	const emit1 = list => unflatten1(list).forEach(signal => reflex0.emit(...flatten1(signal)));
	const escape = (list, times = 1) => list.map(a => is_token(a) ? "$".repeat(times) + a : a);
	const unescape = list => list.map(a => {
		if(a === "") throw SyntaxError("Invalid encoding");
		return is_token(a) ? a.slice(1) : a;
	});
	const begin = Symbol();
	const end = Symbol();
	const reflex0 = reflex();
	const handles = multi_key_map();
	reflex0.on(begin, "reflex", (...list) => {
		const match = list => {
			const f = (pattern, ...list) => {
				if(pattern === "") return args.push(list);
				if(list.length > 1) return;
				if(!Array.isArray(pattern) || !Array.isArray(list[0])) return list.includes(pattern);
				return pattern.length <= list[0].length + 1 && (pattern.length ? list[0].splice(0, pattern.length - 1).map(a => [a]).concat(list).every((a, i) => f(pattern[i], ...a)) : !list[0].length);
			};
			const args = [unflatten1(escape(flatten1(pattern, ...effects)))];
			return f(pattern, ...unflatten1(escape(path.concat(list)))) && args;
		};
		if(handles.get(...list)) return;
		const [pattern, ...effects] = unflatten1(list.slice(0, -1));
		const list1 = flatten1(pattern);
		const path = unescape(list1.slice(0, list1.concat("").indexOf("")));
		handles.set(...list, reflex0.on(...path, (...args) => (args = match(args)) && emit1([].concat(...flatten1(...effects).map(a => is_token(a) ? a.length < args.length ? args[a.length] : a.slice(args.length) : a)))));
	});
	reflex0.on(begin, "unreflex", (...list) => {
		const handle = handles.get(...list);
		if(!handle) return;
		handle();
		handles.set(...list, undefined);
	});
	reflex0.on(begin, "unesc", (...list) => {
		try{
			list = unescape(list.slice(0, -1));
		}catch(error){
			return;
		}
		emit1(list);
	});
	on(["fn", ["esc"]], (args, ...results) => {
		const list = encode(args[0]);
		const n = list.filter(a => a === "").length;
		let i = 0;
		if(!results.length && args.length) emit0(["fn", ["esc", ...args],
			...decode(list.map(a => is_token(a) ? a ? a + "$".repeat(n) : "$".repeat((i += 1) - 1) : a)),
			...[].concat(...args.slice(1).map((a, i) => decode(escape(encode(a), i + n)))),
		]);
	});
	return {
		on,
		emit: emit0,
		exec: code => emit0(...code2signals(code)),
	};
};

const buffer_and = buffer_fn((a, b) => new Uint8Array(Math.max(a.length, b.length)).map((_, i) => (a[i] || 0xff) & (b[i] || 0xff)));

const buffer_or = buffer_fn((a, b) => new Uint8Array(Math.max(a.length, b.length)).map((_, i) => (a[i] || 0x00) | (b[i] || 0x00)));

const buffer_xor = buffer_fn((a, b) => new Uint8Array(Math.max(a.length, b.length)).map((_, i) => (a[i] || 0xff) ^ (b[i] || 0x00)));

const uint_trim = buffer_fn(a => {
	let i = a.length;
	while(!a[i - 1] && i) i -= 1;
	return a.slice(0, i);
});

const uint_fn = f => (...args) => uint_trim(f(...args.map(a => new Uint8Array(uint_trim(a)))).buffer);

const uint_add = uint_fn((a, b) => {
	const result = new Uint8Array(Math.max(a.length, b.length) + 1);
	for(let i = 0; i < result.length - 1; i += 1){
		const n = result[i] + (a[i] || 0) + (b[i] || 0);
		result[i + 1] = n / 0xff;
		result[i] = n & 0xff;
	}
	return result;
});

const uint_cmp = (a, b) => {
	[a, b] = [a, b].map(a => new Uint8Array(uint_trim(a)));
	if(a.length !== b.length) return Math.sign(a.length - b.length);
	for(let i = a.length - 1; i >= 0; i -= 1){
		const cmp = Math.sign(a[i] - b[i]);
		if(cmp) return cmp;
	}
	return 0;
};

const uint_sub = uint_fn((a, b) => {
	if(uint_cmp(a.buffer, b.buffer) < 0) throw RangeError("The subtrahend is greater than the minuend.");
	a = new Uint8Array(a);
	a.forEach((_, i) => (a[i] -= b[i] || 0) < 0 && a.subarray(i + 1).every((_, j) => (a[i + j + 1] -= 1) < 0));
	return a;
});

const uint_mul = uint_fn((a, b) => {
	if(a.length > b.length) [a, b] = [b, a];
	let result = new ArrayBuffer;
	a.forEach((a, i) => {
		const line = new Uint8Array(i + b.length + 1);
		b.forEach((b, j) => {
			const n = line[i + j] + a * b;
			line[i + j + 1] = n / 0xff;
			line[i + j] = n & 0xff;
		});
		result = uint_add(result, line.buffer);
	});
	return new Uint8Array(result);
});

const uint_div = uint_fn((a, b) => {
	if(!b.length) throw ReferenceError("The divisor cannot be zero.");
	if(a.length < b.length) return new Uint8Array;
	let result = new ArrayBuffer;
	for(let i = a.length - b.length + 1; i; i -= 1){
		const step = new Uint8Array(i);
		step.subarray(-1)[0] = 1;
		for(let j = 1; j < 0xff && uint_cmp(uint_mul(uint_add(result, step.buffer), b.buffer), a.buffer) <= 0; j += 1) result = uint_add(result, step.buffer);
	}
	return new Uint8Array(result);
});

const uint_mod = (a, b) => uint_sub(a, uint_mul(uint_div(a, b), b));

const same_lists = (...lists) => {
	if(Array.isArray(lists[0])) return lists.slice(1).every(a => Array.isArray(a) && a.length === lists[0].length) && lists[0].every((_, i) => same_lists(...lists.map(list => list[i])));
	return lists.splice(1).every(a => {
		const [buffer0, buffer1] = lists.concat([a]).map(is_buffer);
		return lists.includes(a) || buffer0 && buffer1 && buffer2bin(buffer0) === buffer2bin(buffer1);
	});
};

const uint2num = uint => {
	uint = new Uint8Array(uint_trim(uint));
	const length = Number.MAX_SAFE_INTEGER.toString(2).length;
	if(uint.length > Math.ceil(length / 8) || uint.subarray(-1)[0] & ~((1 << length % 8) - 1)) throw RangeError("Not safe");
	let result = 0;
	uint.reverse().forEach(a => result = result * 0x100 + a);
	return result;
};

const stdvm = () => {
	const vm0 = vm();

	vm0.on(["defer"], (...signals) => Promise.resolve().then(() => vm0.emit(...signals)).catch(error => setTimeout(() => {
		throw error;
	})));

	const defn = (...path) => {
		const f = path.pop();
		return vm0.on(["fn", path], (args, ...results) => {
			if(results.length) return;
			try{
				args = ["fn", path.concat(args), ...f(...args)];
			}catch(error){
				return;
			}
			if(args.length > 1) vm0.emit(args);
		});
	};
	defn("=", (...args) => [same_lists(...arg).toString()]);
	defn("&", (...args) => args.every(is_buffer) ? new Uint8Array([].concat(...args.map(a => new Uint8Array(a)))).buffer : [].concat(...args.map(a => {
		const list = Array.from(a);
		return list.length ? list : a;
	})));

	const defn_bin = (...path) => {
		const f = path.pop();
		return defn(...path, (...bins) => bins.every(is_buffer) && f(...bins));
	};

	defn_bin("bin", "&", (...bins) => [bins.reduce(buffer_and)]);
	defn_bin("bin", "|", (...bins) => [bins.reduce(buffer_or)]);
	defn_bin("bin", "^", (...bins) => [bins.reduce(buffer_xor)]);

	let uint_iota = new ArrayBuffer;
	const uint_atom = num2uint(1);
	defn_bin("uint", "trim", (...uints) => uints.map(uint_trim));
	defn_bin("uint", "=", (...uints) => [same_lists(...uints.map(uint_trim)).toString()]);
	defn_bin("uint", "+", (...uints) => [uints.reduce(uint_add)]);
	defn_bin("uint", "-", (...uints) => [uints.reduce(uint_sub)]);
	defn_bin("uint", "*", (...uints) => [uints.reduce(uint_mul)]);
	defn_bin("uint", "/", (...uints) => uints.length === 2 && [uint_div(...uints), uint_mod(...uints)]);
	defn_bin("uint", "<", (...uints) => [uints.slice(1).every((a, i) => uint_cmp(uints[i - 1], a) < 0).toString()]);
	defn_bin("uint", ">", (...uints) => [uints.slice(1).every((a, i) => uint_cmp(uints[i - 1], a) > 0).toString()]);
	defn_bin("uint", "<=", (...uints) => [uints.slice(1).every((a, i) => uint_cmp(uints[i - 1], a) <= 0).toString()]);
	defn_bin("uint", ">=", (...uints) => [uints.slice(1).every((a, i) => uint_cmp(uints[i - 1], a) >= 0).toString()]);
	defn_bin("uint", "iota", (...args) => args.length || [uint_iota = uint_add(uint_iota, uint_atom)]);

	vm0.exec(`
		;[reflex [def $0 $0] [reflex [fn $2] [fn $2 $3]]]

		;[reflex [undef $0 $0] [unreflex [fn $2] [fn $2 $3]]]

		;[reflex [let $0 $0 $0]
		;	[reflex [fn [esc $2 [$1 $3]] $4 [$4 $4]] [unesc
		;		[unreflex $4]
		;		[reflex [fn [esc $6 _] $5 _] [unesc
		;			[unreflex $9]
		;			$7
		;		]]
		;		[unesc [fn [esc $6 _]]]
		;	]]
		;	[unesc [fn [esc $2 [$1 $3]]]]
		;]

		[reflex [_ reflex $0 $0] [unesc [reflex $1
			[unesc [unreflex $4]]
			$2
		]]]

		[reflex [let $0 $0 $0]
			[_ reflex [fn [esc _ $2 [$1 $3]] _ $4 [$4 $4]] [unesc
				[_ reflex [fn [esc _ $6 _] _ $5 _] [unesc $7]]
				[unesc [fn [esc _ $6 _]]]
			]]
			[unesc [fn [esc _ $2 [$1 $3]]]]
		]

		[reflex [let $0 $0 $0]
			[_ reflex [fn [esc _ $2 $1 $3] _ $4 $4 $4] [unesc
				[_ reflex [fn [esc $6 $9 $9] $9 _ _] $7]
				[unesc [fn [esc $6 _ _]]]
			]]
			[unesc [fn [esc _ $2 $1 $3]]]
		]

		;[reflex [fn [listener-to-reflex $0 $0]]
		;	[reflex [fn [esc $5 _ _ [$2 $3]] _ _ [$4 $4]]
		;		[unesc [unreflex $4]]
		;		[reflex [fn [esc $6]]
		;			[unesc [unreflex $8]]
		;			[fn [listener-to-reflex $6 $7] $6
		;			]
		;		]
		;		[unesc [fn [esc $6 _
		;			[reflex [_ listener 
		;		]]]
		;	]
		;	[unesc [fn [esc $5 _ _ [$2 $3]]]]
		;]

		;[reflex [fn [listener-to-reflex $0 $0]]
		;	[reflex [fn [esc $2 _ _ $2] $4 _ _ $4]
		;		[unesc [unreflex $4]]
		;		[reflex [fn [esc $6]]
		;			[unesc [unreflex $
		;		]
		;		[unesc [fn [esc $6]]]
		;	]
		;	[unesc [fn [esc $2 _ _ $2]]]
		;]

		;[reflex [on $0 $0]
		;	[reflex [fn [esc _ [$1 $2]] _ [$3]] [unesc
		;		[unreflex $3]
		;		[unesc [reflex $1 [unesc $2]]]
		;	]]
		;	[unesc [fn [esc _ [$1 $2]]]]
		;]

		;[reflex [off $0 $0]
		;	[reflex [fn [esc _ [$1 $2]] _ [$3]] [unesc
		;		[unreflex $3]
		;		[unesc [unreflex $1 [unesc $2]]]
		;	]]
		;	[unesc [fn [esc _ [$1 $2]]]]
		;]
	`);
	return vm0;
};

const utf8_to_str = utf8 => {
	utf8 = new Uint8Array(utf8);
	let result = "";
	for(let i = 0; i < utf8.length; i += 1){
		const byte = utf8[i];
		if(!(byte & 0x80)){
			result += String.fromCodePoint(byte);
			continue;
		}
		const length = byte.toString(2).indexOf("0");
		if(length < 2 || length > 6) throw SyntaxError("Invalid encoding");
		let point = byte & (1 << 8 - length) - 1;
		for(let j = length - 1; j; j -= 1){
			i += 1;
			if(i >= utf8.length || (utf8[i] & 0xc0) !== 0x80) throw SyntaxError("Invalid encoding");
			point = (point << 6) | utf8[i] & 0x3f;
		}
		if(point < (1 << 4 * length + 1)) throw SyntaxError("Invalid encoding");
		result += String.fromCodePoint(point);
	};
	return result;
};

const signals2code = (options = {}) => (...signals) => {
	const [begin, end, list] = flatten(...signals);
	return list
	.map(a => {
		if(a === begin) return "[ \t";
		if(a === end) return "\t ]";
		const buffer = is_buffer(a);
		if(buffer){
			if(options.utf8_to_str) try{
				a = utf8_to_str(buffer);
			}catch(error){}
			if(!is_str(a)) return "%" + Array.from(new Uint8Array(buffer)).map(a => a.toString(16).padStart(2, "0")).join("");
		}
		if(!is_str(a)) throw TypeError("Unsupported");
		if(is_token(a)) try{
			if(str2num(a.length.toString()) === a.length) return "$" + a.length;
		}catch(error){}
		return /^\\|^%|^\$\d|[[;`\s\0-\u{1f}\u{7f}\]]/u.test(a) ? "`" + a
		.replace(/\\(?:[0-9A-Fa-f]+;|[nrt])|`/gu, "\\$&")
		.replace(/[\n\r\t]/gu, $ => ({"\n": "\\n", "\r": "\\r", "\t": "\\t"})[$])
		.replace(/[\0-\u{1f}\u{7f}]/gu, $ => "\\" + $.codePointAt().toString(16) + ";") + "`" : a;
	})
	.join(" ")
	.replace(/ (?:\t )+/gu, "");
};

const cvm = log => {
	const vm = stdvm();
	return vm;
};

({vm, stdvm, cvm});

/*test*
var signals2code0 = signals2code({utf8_to_str: true});
var vm0 = stdvm();
vm0.on(["test", "echo"], (...args) => console.log(signals2code0(...args)));
//vm0.on((...signals) => console.log("log", signals2code0(...signals)));
//*/
