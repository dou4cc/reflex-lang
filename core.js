"use strict";

const flatten = (...list) => {
	const begin = Symbol();
	const end = Symbol();
	const f = list => [].concat(...list.map(a => Array.isArray(a) ? [begin, ...f(a), end] : a));
	return [begin, end, ...f(list)];
};

const unflatten = (begin, end, ...list) => {
	const pos = list.lastIndexOf(begin);
	if(pos < 0) return list;
	list.splice(pos, 0, list.splice(pos, list.indexOf(end, pos) - pos + 1).slice(1, -1));
	return unflatten(begin, end, ...list);
};

const code2ast = source => {
	source = source.replace(/\r\n?/gu, "\n");
	const words = source.match(/`(?:\\`|[^`])*`|[[\]]|(?:(?!\]|;)\S)+|;.*/gu);
	return unflatten("[", "]", ...words.filter(a => !/^;/u.test(a)).map(a => {
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

const uint_shorten = buffer_fn(a => {
	let i = a.length;
	while(!a[i - 1] && i >= 0) i -= 1;
	return a.slice(0, i);
});

const num2uint = number => {
	if(!Number.isSafeInteger(number) || number < 0) throw new TypeError("Only safe natural number can be converted to uint.");
	const result = new Uint8Array(8);
	for(let i = 0; number >= 1; i += 1){
		result[i] = number % 0xff;
		number /= 0xff;
	}
	return uint_shorten(result.buffer);
};

const str2num = (a, radix = 10) => {
	if(a.toLowerCase() !== (a = Number.parseInt(a, radix)).toString(radix) || Number.isNaN(a)) throw new SyntaxError("Invalid token");
	return a;
};

const ast2signals = source => {
	const [begin, end, ...list] = flatten(...source);
	for(let i in list){
		if(typeof list[i] !== "object") continue;
		let {content} = list[i];
		if(!list[i].flag){
			if(/^\\/u.test(content)){
				list[i] = num2uint(str2num(content.slice(1)));
				continue;
			}
			if(/^%/u.test(content)){
				if(!(content.length % 2)) throw new SyntaxError("Invalid token");
				content = content.slice(1);
				list[i] = new Uint8Array(content.length / 2).map((a, i) => str2num(content.substr(i * 2, 2).slice(content[i * 2] === "0"), 16)).buffer;
				continue;
			}
			if(/^\$\d/u.test(content)) content = "$".repeat(str2num(content.slice(1)));
		}
		list[i] = content;
	}
	return unflatten(begin, end, ...list);
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
		return new Int8Array(a).buffer;
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

const vm = () => {
	const flatten1 = (...list) => {
		const [begin1, end1] = [, , ...list] = flatten(...list);
		return list.map(a => a === begin1 ? begin : a === end1 ? end : a);
	};
	const unflatten1 = (...list) => unflatten(begin, end, ...list);
	const encode = (...list) => flatten1(...list).map(a => {
		const buffer = is_buffer(a);
		return buffer ? buffer2bin(buffer) : typeof a === "string" ? str2bin(a) : a;
	});
	const decode = (...list) => unflatten1(...list.map(a => typeof a === "string" ? bin2buffer(a) : a));
	const on = (path, listener) => {
		if(!listener) return reflex0.on((...list) => path(...decode(...list)));
		path = encode(path);
		const m = path.slice().reverse().concat(0).findIndex(a => a !== end);
		return reflex0.on(...path.slice(0, path.length - m), (...list) => listener(...decode(...Array((m || 1) - 1).fill(begin).concat(list.slice(0, -1)))));
	};
	const emit0 = (...signals) => signals.forEach(signal => reflex0.emit(...encode(signal)));
	const emit1 = (reflex, ...list) => unflatten1(...list).forEach(signal => reflex.emit(...flatten1(signal)));
	const is_token = a => typeof a === "string" && /^\$*$/u.test(a);
	const escape = list => list.map(a => is_token(a) ? "$" + a : a);
	const unescape = list => list.map(a => {
		if(a === "") throw new ReferenceError("Invalid encoding");
		return is_token(a) ? a.slice(1) : a;
	});
	const begin = Symbol();
	const end = Symbol();
	const reflex0 = reflex();
	const reflex1 = reflex();
	const handles = multi_key_map();
	reflex0.on(begin, "on", (...list) => {
		const match = list => {
			const f = (pattern, ...list) => {
				if(pattern === "") return args.push(list);
				if(list.length > 1) return;
				if(!Array.isArray(pattern) || !Array.isArray(list[0])) return list.includes(pattern);
				return pattern.length <= list[0].length + 1 && (pattern.length ? list[0].splice(0, pattern.length - 1).map(a => [a]).concat(list).every((a, i) => f(pattern[i], ...a)) : !list[0].length);
			};
			const args = [unflatten1(...escape(flatten1(pattern, ...effects)))];
			return f(pattern, ...unflatten1(...escape(path.concat(list)))) && args;
		};
		if(handles.get(...list)) return;
		const [pattern, ...effects] = unflatten1(...list.slice(0, -1));
		const list1 = flatten1(pattern);
		const path = unescape(list1.slice(0, list1.concat("").indexOf("")));
		handles.set(...list, [
			reflex0.on(...path, (...args) => (args = match(args)) && emit1(reflex0, ...[].concat(...flatten1(...effects).map(a => is_token(a) ? a.length < args.length ? args[a.length] : a.slice(args.length) : a)))),
			reflex1.on(...path, (...list1) => match(list1) && reflex0.emit(begin, "on", ...list)),
		]);
	});
	reflex0.on(begin, "off", (...list) => {
		const handle = handles.get(...list);
		if(!handle) return;
		handle.forEach(f => f());
		handles.set(...list, undefined);
	});
	reflex0.on(begin, "match", (...list) => emit1(reflex1, ...list.slice(0, -1)));
	reflex0.on(begin, "emit", (...list) => {
		try{
			list = unescape(list.slice(0, -1));
		}catch(error){
			return;
		}
		emit1(reflex0, ...list);
	});
	on([["escape"]], (args, ...rest) => rest.length || args.length && emit0([["escape", ...args], ...decode(...escape(encode(...args)))]));
	return {
		on,
		emit: emit0,
		exec: code => emit0(...code2signals(code)),
	};
};

const buffer_and = buffer_fn((a, b) => new Uint8Array(Math.max(a.length, b.length)).map((_, i) => (a[i] || 0xff) & (b[i] || 0xff)));

const buffer_or = buffer_fn((a, b) => new Uint8Array(Math.max(a.length, b.length)).map((_, i) => (a[i] || 0x00) | (b[i] || 0x00)));

const buffer_xor = buffer_fn((a, b) => new Uint8Array(Math.max(a.length, b.length)).map((_, i) => (a[i] || 0xff) ^ (b[i] || 0x00)));

const uint_fn = f => (...args) => uint_shorten(f(...args.map(a => new Uint8Array(uint_shorten(a)))).buffer);

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
	[a, b] = [a, b].map(a => new Uint8Array(uint_shorten(a)));
	return a.length + b.length && Math.sign(a.length - b.length || a.subarray(-1)[0] - b.subarray(-1)[0]) || uint_cmp(...[a, b].map(a => a.slice(0, -1).buffer));
};

const uint_sub = uint_fn((a, b) => {
	if(uint_cmp(a.buffer, b.buffer) < 0) throw new RangeError("The subtrahend is greater than the minuend.");
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

const uint_div = (a, b) => {
	[a, b] = [a, b].map(a => new Uint8Array(uint_shorten(a)));
	if(!b.length) throw new ReferenceError("The divisor cannot be 0.");
	if(a.length < b.length) return [new ArrayBuffer, a.buffer];
	let result = new ArrayBuffer;
	for(let i = a.length - b.length + 1; i; i -= 1){
		const step = new Uint8Array(i);
		step.subarray(-1)[0] = 1;
		for(let j = 1; j < 0xff && uint_cmp(uint_mul(uint_add(result, step.buffer), b.buffer), a.buffer) <= 0; j += 1) result = uint_add(result, step.buffer);
	}
	return [uint_shorten(result.buffer), uint_sub(a.buffer, uint_mul(result.buffer, b.buffer))];
};

const same_lists = (...lists) => {
	if(Array.isArray(lists[0])) return lists.slice(1).every(a => Array.isArray(a) && a.length === lists[0].length) && lists[0].every((_, i) => same_lists(...lists.map(list => list[i])));
	return lists.splice(1).every(a => {
		const [buffer0, buffer1] = lists.concat([a]).map(is_buffer);
		return lists.includes(a) || buffer0 && buffer1 && buffer2bin(buffer0) === buffer2bin(buffer1);
	});
	
};

const stdvm = () => {
	const vm0 = vm();

	vm0.on(["defer"], (...signals) => Promise.resolve().then(() => vm0.emit(...signals)).catch(error => setTimeout(() => {
		throw error;
	})));

	const defn = (...path) => {
		const f = path.pop();
		return vm0.on([path], (args, ...rest) => {
			if(rest.length) return;
			try{
				args = [path.concat(args), ...f(...args)];
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
	defn_bin("uint", "shorten", (...uints) => uints.map(uint_shorten));
	defn_bin("uint", "=", (...uints) => [same_lists(...uints.map(uint_shorten)).toString()]);
	defn_bin("uint", "+", (...uints) => [uints.reduce(uint_add)]);
	defn_bin("uint", "-", (...uints) => [uints.reduce(uint_sub)]);
	defn_bin("uint", "*", (...uints) => [uints.reduce(uint_mul)]);
	defn_bin("uint", "/", (...uints) => uints.length === 2 && uint_div(...uints));
	defn_bin("uint", "cmp", (...uints) => uints.length === 2 && [num2uint(uint_cmp(...uints))]);
	defn_bin("uint", "iota", (...args) => args.length || (uint_iota = uint_add(uint_iota, uint_atom)));

	vm0.exec(`
		[on [def $0 $0] [on [$1] [$1 $2]]]
		[on [undef $0 $0] [off [$1] [$1 $2]]]
		[on [[true [$0] $0]] [emit [[true [$1] $2] $1]]]
		[on [[false [$0] $0]] [emit [[false [$1] $2] $2]]]
		[on [once $0 $0] [emit [on $1
			[emit [off $3]]
			$2
		]]]
		[on [unonce $0 $0] [emit [off $1
			[emit [off $3]]
			$2
		]]]
		[on [[list $0]] [emit [[list $1] $1]]]
		[on [let [$0] $0]
			[once [[list $1] $3] $2]
			[emit [[list $1]]]
		]
		[on [call $0 $0]
			[once [$1 $3 $3] [emit [let [$4 $5] $2]]]
			[emit [$1]]
		]
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
		if(length < 2 || length > 6) throw new ReferenceError("Invalid encoding");
		let point = byte & (1 << 8 - length) - 1;
		for(let j = length - 1; j; j -= 1){
			i += 1;
			if(i >= utf8.length || (utf8[i] & 0xc0) !== 0x80) throw new ReferenceError("Invalid encoding");
			point = (point << 6) | utf8[i] & 0x3f;
		}
		if(point < (1 << 4 * length + 1)) throw new ReferenceError("Invalid encoding");
		result += String.fromCodePoint(point);
	};
	return result;
};

const signals2code = (options = {}) => (...signals) => {
	const [begin, end, ...list] = flatten(...signals);
	return list
	.map(a => {
		if(a === begin) return "[ \t";
		if(a === end) return "\t ]";
		const buffer = is_buffer(a);
		if(buffer){
			if(options.utf8_to_str) try{
				a = utf8_to_str(buffer);
			}catch(error){}
			if(typeof a !== "string") return "%" + Array.from(new Uint8Array(buffer)).map(a => a.toString(16).padStart(2, "0")).join("");
		}
		if(typeof a !== "string") throw new TypeError("Unsupported type");
		if(/^\$*$/u.test(a) && str2num(a.length.toString()) === a.length) return "$" + a.length;
		return /^\\|^%|^\$\d|[[;\s\0-\u{1f}\u{7f}\]]/u.test(a) ? "`" + a
		.replace(/\\(?:[0-9A-Fa-f]+;|[nrt])|`/gu, "\\$&")
		.replace(/[\n\r\t]/gu, $ => ({"\n": "\\n", "\r": "\\r", "\t": "\\t"})[$])
		.replace(/[\0-\u{1f}\u{7f}]/gu, $ => "\\" + $.codePointAt().toString(16) + ";") + "`" : a;
	})
	.join(" ")
	.replace(/ \t /gu, "");
};

stdvm;

/*test*
var signals2code0 = signals2code({utf8_to_str: true});
var vm0 = stdvm();
vm0.on(["test", "echo"], (...args) => console.log(signals2code0(...args)));
//vm0.on((...signals) => console.log("log", signals2code0(...signals)));
//*/
