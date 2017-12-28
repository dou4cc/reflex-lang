"use strict";

const serialize = (...list) => {
	const begin = Symbol();
	const end = Symbol();
	const f = list => [].concat(...list.map(a => Array.isArray(a) ? [begin, ...f(a), end] : a));
	return [begin, end, f(list)];
};

const deserialize = (begin, end, [...list]) => {
	const pos = list.lastIndexOf(begin);
	if(pos < 0){
		if(list.includes(end)) throw SyntaxError("Invalid token");
		return list;
	}
	list.splice(pos, 0, list.splice(pos, list.indexOf(end, pos) - pos + 1).slice(1, -1));
	return deserialize(begin, end, list);
};

const code2ast = code => {
	code = code.replace(/\r\n?/gu, "\n").trim();
	const tokens = code.match(/`(?:\\`|[^`])*`|[[\]]|(?:(?![[;`\]])\S)+|;.*/gu) || [];
	return deserialize("[", "]", tokens.filter(token => !/^;/u.test(token)).map(token => {
		if(/^[[\]]$/u.test(token)) return token;
		if(/`/u.test(token)) return {flag: "`", value:
			token.slice(1, -1)
			.replace(/(^|[^\\])\\[nrt]/gu, ($, $1) => $1 + {n: "\n", r: "\r", t: "\t"}[$1])
			.replace(/(^|[^\\])\\([0-9a-f]+);/giu, ($, $1, $2) => $1 + String.fromCodePoint(Number.parseInt($2, 16)))
			.replace(/\\(\\+(?:[0-9A-Fa-f]+;|[nrt])|\\*`)/gu, "$1")
		};
		return {flag: "", value: token};
	}));
};

const buffer_fn = f => (...buffers) => f(...buffers.map(buffer => new Uint8Array(buffer))).buffer;

const num2uint = number => {
	if(!Number.isSafeInteger(number) || number < 0) throw TypeError("Only safe natural number can be converted to uint.");
	const array = [];
	while(number >= 1){
		array.push(number % 0xff);
		number /= 0xff;
	}
	return new Uint8Array(array).buffer;
};

const str2num = (a, radix = 10) => {
	if(a.toLowerCase() !== (a = Number.parseInt(a, radix)).toString(radix) || Number.isNaN(a)) throw SyntaxError("Invalid token");
	return a;
};

const hex2buffer = hex => new Uint8Array(Array(hex.length / 2)).map((a, i) => str2num(hex.substr(i * 2, 2).replace(/^(?:0(?!$))*/u, ""), 16)).buffer;

const ast2signals = ast => {
	const [begin, end, tokens] = serialize(...ast);
	return deserialize(begin, end, tokens.map(token => {
		if(typeof token === "symbol") return token;
		let {value} = token;
		if(!token.flag){
			if(/^\\/u.test(value)) return num2uint(str2num(value.slice(1)));
			if(/^%/u.test(value)) return hex2buffer(value.slice(1));
			if(/^\$\d/u.test(value)) value = "$".repeat(str2num(value.slice(1)));
		}
		return value;
	}));
};

const code2signals = code => ast2signals(code2ast(code));

const run = f => {
	try{
		f();
	}catch(error){
		setTimeout(() => {
			throw error;
		});
	}
};

const reflexion = free => {
	const off = () => children.size || reflexes.size || free && run(free);
	const children = new Map;
	const reflexes = new Set;
	return {
		emit: (...list) => {
			const reflexes1 = Array.from(reflexes).reverse();
			if(list.length) (children.get(list[0]) || {emit(){}}).emit(...list.slice(1));
			reflexes1.forEach(f => reflexes.has(f) && f(...list));
		},
		on: (...rest) => {
			const first = rest.shift();
			if(!rest.length){
				const reflex = (...args) => run(() => first(...args));
				reflexes.add(reflex);
				return () => {
					reflexes.delete(reflex);
					off();
				};
			}
			if(!children.has(first)) children.set(first, reflexion(() => {
				children.delete(first);
				off();
			}));
			return children.get(first).on(...rest);
		},
	};
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

const is_param = a => is_str(a) && /^\$*$/u.test(a);

const minvm = () => {
	const serialize1 = (...list) => {
		const [begin1, end1] = [, , list] = serialize(...list);
		return list.map(a => a === begin1 ? begin : a === end1 ? end : a);
	};
	const deserialize1 = list => deserialize(begin, end, list);
	const encode = (...list) => serialize1(...list).map(a => {
		const buffer = is_buffer(a);
		return buffer ? buffer2bin(buffer) : is_str(a) ? str2bin(a) : a;
	});
	const decode = list => deserialize1(list.map(a => is_str(a) ? bin2buffer(a) : a));
	const escape = list => list.map(a => is_param(a) ? "$" + a : a);
	const unescape = list => list.map(a => is_param(a) ? a.slice(1) : a);
	const match = (target, pattern) => {
		const f = (pattern, target, ...tail) => {
			if(pattern === "") return args.push([target, ...tail]);
			if(tail.length) return;
			if(![pattern, target].every(Array.isArray)) return escape([target]).includes(pattern);
			if(pattern.length > target.length + 1) return;
			if(!pattern.length) return !target.length;
			target = target.slice();
			return target.splice(0, pattern.length - 1).map(a => [a]).concat([target]).every((a, i) => f(pattern[i], ...a));
		};
		const args = [];
		if(f(pattern, target)) return args;
	};
	const apply = (effect, ...args) => reflexion0.emit(...[].concat(...serialize1(...effect).map(a =>
		is_param(a) ? a.length < args.length ? args[a.length] : a.slice(args.length) : a
	)));
	const emit = (...signal) => reflexion0.emit(...encode(...signal));
	const begin = Symbol();
	const end = Symbol();
	const reflexion0 = reflexion();
	const handles = multi_key_map();
	reflexion0.on("reflex", (...list) => {
		if(handles.get(...list)) return;
		const [pattern, ...effect] = deserialize1(list);
		if(!Array.isArray(pattern)) return;
		const path0 = serialize1(...pattern);
		const path1 = unescape(path0.slice(0, path0.concat("").indexOf("")));
		handles.set(...list, reflexion0.on(...path1, (...target) =>
			match(target = deserialize1(path1.concat(target)), pattern) && apply(effect, [pattern, ...effect], target)
		));
	});
	reflexion0.on("unreflex", (...list) => {
		const handle = handles.get(...list);
		if(!handle) return;
		handle();
		handles.set(...list, undefined);
	});
	reflexion0.on("match", (...args) => {
		const [target, pattern, effect0, ...effect1] = deserialize1(args);
		args = match(target, pattern);
		if(!args) return apply(effect1);
		if(Array.isArray(effect0)) apply(effect0, ...args);
	});
	reflexion0.on("escape", (...list) => {
		const [target, ...effect] = deserialize1(list);
		apply(effect, deserialize1(escape(serialize1(target))));
	});
	reflexion0.on("start", (...list) => deserialize1(list).filter(Array.isArray).forEach(signal => reflexion0.emit(...serialize1(...signal))));
	return {
		emit,
		on: (...path) => {
			const reflex = path.pop();
			path = encode(...path);
			const m = path.slice().reverse().concat(0).findIndex(a => a !== end);
			return reflexion0.on(...path.slice(0, path.length - m), (...list) => reflex(...decode(Array(m).fill(begin).concat(list))));
		},
		exec: code => emit("start", ...code2signals(code)),
	};
};

const buffer_and = buffer_fn((a, b) => new Uint8Array(Math.max(a.length, b.length)).map((_, i) => (a[i] || 0xff) & (b[i] || 0xff)));

const buffer_or = buffer_fn((a, b) => new Uint8Array(Math.max(a.length, b.length)).map((_, i) => (a[i] || 0x00) | (b[i] || 0x00)));

const buffer_xor = buffer_fn((a, b) => new Uint8Array(Math.max(a.length, b.length)).map((_, i) => (a[i] || 0xff) ^ (b[i] || 0x00)));

const uint_trim = buffer_fn(uint => {
	let i = uint.length;
	while(!uint[i - 1] && i) i -= 1;
	return uint.slice(0, i);
});

const uint_fn = f => (...uints) => uint_trim(f(...uints.map(uint => new Uint8Array(uint_trim(uint)))).buffer);

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
	[a, b] = [a, b].map(uint => new Uint8Array(uint_trim(uint)));
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
		for(let j = 1; j < 0xff && uint_cmp(uint_mul(uint_add(result, step.buffer), b.buffer), a.buffer) <= 0; j += 1){
			result = uint_add(result, step.buffer);
		}
	}
	return new Uint8Array(result);
});

const uint_mod = (a, b) => uint_sub(a, uint_mul(uint_div(a, b), b));

const same_lists = (...lists) => {
	if(!Array.isArray(lists[0])) return lists.splice(1).every(a => {
		const [buffer0, buffer1] = lists.concat([a]).map(is_buffer);
		return lists.includes(a) || buffer0 && buffer1 && buffer2bin(buffer0) === buffer2bin(buffer1);
	});
	if(!lists.slice(1).every(a => Array.isArray(a) && a.length === lists[0].length)) return false;
	return lists[0].every((_, i) => same_lists(...lists.map(list => list[i])));
};

const uint2num = uint => {
	uint = new Uint8Array(uint_trim(uint));
	const top = Number.MAX_SAFE_INTEGER.toString(2).length;
	if(uint.length > Math.ceil(top / 8) || uint.subarray(-1)[0] & ~((1 << top % 8) - 1)) throw RangeError("Not safe");
	let number = 0;
	uint.reverse().forEach(a => number = number * 0x100 + a);
	return number;
};

const buffer_concat = buffer_fn((...buffers) => new Uint8Array([].concat(...buffers.map(a => Array.from(a)))));

const stdvm = () => {
	const defn = (...path) => {
		const f = path.pop();
		return vm.on(...path, (...effect) => {
			let results;
			try{
				results = [...f(effect)];
			}catch(error){}
			if(results) vm.emit("bind", results, ...effect);
		});
	};
	const defop = length => (...path) => {
		const f = path.pop();
		return defn(...path, args => f(...args.splice(0, length)));
	};
	const defop_2 = defop(2);
	const bin_fn = f => (...buffers) => buffers.every(is_buffer) && f(...buffers);
	const symbols = new Map([
		[true, "T"],
		[false, "F"],
	]);
	const vm = minvm();

	vm.exec(`
		[reflex [reflex [$0] $0] match [$1] [reflex [emit $2] $2] _ match [[$1] $1] [[reflex $2] reflex [$2] $2]
			[escape [[reflex $2] $3] match $5 [$6 $6]
				[escape $6 reflex [emit $7] start $8]
			]
		]

		[reflex [unreflex [$0] $0] match [$1] [unreflex [emit $2] $2] _ match [[$1] $1] [[unreflex $2] unreflex [$2] $2]
			[escape [[reflex $2] $3] match $5 [$6 $6]
				[escape $6 unreflex [emit $7] start $8]
			]
		]

		[reflex [bind [$0] $0] match [$1] [bind $2 $2]
			[match $2 [$4] [$3]]
		]

		[reflex [fn $0] escape [$1] match $2 [fn $3 [$3 $3] $3 $3]
			[escape [match [$3 $8] [$4 $5] $6] bind [[$5] start $8] $7]
		]

		[fn _ [_ defn $0 $0 $0] [fn $0 $1 [$2] reflex $3] reflex $0]

		[fn _ [_ undefn $0 $0 $0] [fn $0 $1 [$2] unreflex $3] reflex $0]

		[defn _ [_ call $0] escape [$0] match $1 [$2 [$2] $2]
			[bind [match [$2 $5] [$6 $6] [$4]] $3 $5]
		]

		[defn _ [_ match? $0] call [$0] [escape [$0]] match [$1 $2] [[$3] [$3 $3 $3]]
			[call [$4 $3] [escape [$6]] match [$7 $8] [[$9 $9 $9 $9] [$9]]
				[match [[[_ match? $9 $11] start [unreflex $14] [bind [T] $13] ] $10 $11 $12] [[$14] $14 $14 $14]
					[start
						[reflex $14]
						[_ match? $15 $15]
						[match $15 $16 _ start
							[unreflex $14]
							[bind [F] $17]
						]
					]
				]
			]
		]

		[defn _ [_ list? $0 $0] call [$1] [match? $0 [$2]] bind [$3] start $2]
	`);

	vm.on("defer", (...signal) => Promise.resolve().then(() => run(() => vm.emit(...signal))));
	defop_2("=", (a, b) => [symbols.get(same_lists(a, b))]);
	defop_2("+", (...args) =>
		args.every(is_buffer)
		? [buffer_concat(...args)]
		: [].concat(...args.map(a => is_buffer(a) ? Array.from(new Uint8Array(a)).map(a => new Uint8Array([a]).buffer) : [...a]))
	);
	defop(3)("slice", (a, b, list) => {
		if(![a, b].every(is_buffer)) return;
		[a, b] = [a, b].map(uint2num);
		return Array.isArray(list) ? list.slice(a, b) : [is_buffer(list).slice(a, b)];
	});
	defop(1)("length", list => [num2uint(Array.isArray(list) ? list.length : is_buffer(list).byteLength)]);

	defop_2("bin", "&", bin_fn((a, b) => [buffer_and(a, b)]));
	defop_2("bin", "|", bin_fn((a, b) => [buffer_or(a, b)]));
	defop_2("bin", "^", bin_fn((a, b) => [buffer_xor(a, b)]));

	let uint_iota = new ArrayBuffer;
	const uint_1 = num2uint(1);
	defop(1)("uint", "trim", bin_fn(uint => [uint_trim(uint)]));
	defop_2("uint", "=", bin_fn((...uints) => [symbols.get(same_lists(...uints.map(uint_trim)))]));
	defop_2("uint", "+", bin_fn((a, b) => [uint_add(a, b)]));
	defop_2("uint", "-", bin_fn((a, b) => [uint_sub(a, b)]));
	defop_2("uint", "*", bin_fn((a, b) => [uint_mul(a, b)]));
	defop_2("uint", "/", bin_fn((a, b) => [uint_div(a, b), uint_mod(a, b)]));
	defop_2("uint", "<", bin_fn((a, b) => [symbols.get(uint_cmp(a, b) < 0)]));
	defop_2("uint", ">", bin_fn((a, b) => [symbols.get(uint_cmp(a, b) > 0)]));
	defop_2("uint", "<=", bin_fn((a, b) => [symbols.get(uint_cmp(a, b) <= 0)]));
	defop_2("uint", ">=", bin_fn((a, b) => [symbols.get(uint_cmp(a, b) >= 0)]));
	defn("uint", "iota", () => [uint_iota = uint_add(uint_iota, uint_1)]);

	vm.exec(`
		
	`);
	return vm;
};

const utf8_to_str = utf8 => {
	utf8 = new Uint8Array(utf8);
	let string = "";
	for(let i = 0; i < utf8.length; i += 1){
		const byte = utf8[i];
		if(!(byte & 0x80)){
			string += String.fromCodePoint(byte);
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
		string += String.fromCodePoint(point);
	};
	return string;
};

const buffer2hex = buffer => Array.from(new Uint8Array(buffer)).map(a => a.toString(16).padStart(2, "0")).join("");

const signals2code = (options = {}) => (...signals) => {
	const [begin, end, list] = serialize(...signals);
	return list
	.map(a => {
		if(a === begin) return "[ \t";
		if(a === end) return "\t ]";
		const buffer = is_buffer(a);
		if(buffer){
			if(options.utf8_to_str) try{
				a = utf8_to_str(buffer);
			}catch(error){}
			if(!is_str(a)) return "%" + buffer2hex(buffer);
		}
		if(!is_str(a)) throw TypeError("Unsupported");
		if(is_param(a)) try{
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
	if(log) vm.on("log", (...message) => {
		try{
			message = signals2code({utf8_to_str: true})(...message);
		}catch(error){}
		if(is_str(message)) log(message);
	});
	return vm;
};

({minvm, stdvm, cvm});

/*test*
var vm = cvm(message => console.log("log: " + message));
vm.on((...signal) => console.log("signal: " + signals2code({utf8_to_str: true})(...signal)));
//*/
