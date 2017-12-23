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
	const words = source.match(/`(?:\\`|[^`])*`|[[\]]|(?:(?![[;`\]])\S)+|;.*/gu) || [];
	return unflatten("[", "]", words.filter(a => !/^;/u.test(a)).map(a => {
		if(/^[[\]]$/u.test(a)) return a;
		if(/^`.*`$/u.test(a)) return {flag: "`", value:
			a.slice(1, -1)
			.replace(/(^|[^\\])\\[nrt]/gu, ($, $1) => $1 + {n: "\n", r: "\r", t: "\t"}[$1])
			.replace(/(^|[^\\])\\([0-9a-f]+);/giu, ($, $1, $2) => $1 + String.fromCodePoint(Number.parseInt($2, 16)))
			.replace(/\\(\\+(?:[0-9A-Fa-f]+;|[nrt])|\\*`)/gu, "$1")};
		return {flag: "", value: a};
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
		let {value} = list[i];
		if(!list[i].flag){
			if(/^\\/u.test(value)){
				list[i] = num2uint(str2num(value.slice(1)));
				continue;
			}
			if(/^%/u.test(value)){
				value = value.slice(1);
				list[i] = new Uint8Array(Array(value.length / 2)).map((a, i) => str2num(value.substr(i * 2, 2).replace(/^(?:0(?!$))*/u, ""), 16)).buffer;
				continue;
			}
			if(/^\$\d/u.test(value)) value = "$".repeat(str2num(value.slice(1)));
		}
		list[i] = value;
	}
	return unflatten(begin, end, list);
};

const code2signals = source => ast2signals(code2ast(source));

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
	const off = () => children.size || reflexes.size || free && free();
	const children = new Map;
	const reflexes = new Set;
	const f = path => ({
		emit: (...list) => {
			list = path.concat(list);
			const reflexes1 = Array.from(reflexes).reverse();
			if(list.length) (children.get(list[0]) || {emit(){}}).emit(...list.slice(1));
			reflexes1.forEach(f => reflexes.has(f) && f(...list));
		},
		on: (...rest) => {
			const first = (rest = path.concat(rest)).shift();
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
	const emit = (...signals) => signals.forEach(signal => Array.isArray(signal) && reflexion0.emit(...encode(...signal)));
	const escape = list => list.map(a => is_token(a) ? "$" + a : a);
	const unescape = list => list.map(a => {
		if(a === "") throw SyntaxError("Invalid encoding");
		return is_token(a) ? a.slice(1) : a;
	});
	const match = (target, pattern) => {
		const f = (pattern, ...list) => {
			if(pattern === "") return args.push(list);
			if(list.length > 1) return;
			if(!Array.isArray(pattern) || !Array.isArray(list[0])) return escape(list).includes(pattern);
			if(pattern.length > list[0].length + 1) return;
			if(!pattern.length) return !list[0].length;
			return list[0].splice(0, pattern.length - 1).map(a => [a]).concat(list).every((a, i) => f(pattern[i], ...a));
		};
		const args = [];
		if(f(pattern, target)) return args;
	};
	const apply = (effects, ...args) => unflatten1([].concat(...flatten1(...effects).map(a =>
		is_token(a) ? a.length < args.length ? args[a.length] : a.slice(args.length) : a
	))).forEach(signal => Array.isArray(signal) && reflexion0.emit(...flatten1(...signal)));
	const begin = Symbol();
	const end = Symbol();
	const reflexion0 = reflexion();
	const handles = multi_key_map();
	reflexion0.on("reflex", (...list) => {
		if(handles.get(...list)) return;
		const self = unflatten1(list);
		const list1 = Array.isArray(self[0]) ? flatten1(...self[0]) : [];
		const path = unescape(list1.slice(0, list1.concat("").indexOf("")));
		handles.set(...list, reflexion0.on(...path, (...target) =>
			match(target = unflatten1(path.concat(target)), self[0]) && apply(self.slice(1), self, target)
		));
	});
	reflexion0.on("unreflex", (...list) => {
		const handle = handles.get(...list);
		if(!handle) return;
		handle();
		handles.set(...list, undefined);
	});
	reflexion0.on("match?", (...args) => {
		const [target, pattern, effects0, ...effects1] = unflatten1(args);
		args = match(target, pattern);
		if(!args) return apply(effects1);
		if(Array.isArray(effects0)) apply(effects0, ...args);
	});
	reflexion0.on("escape", (...list) => {
		const [target, ...effects] = unflatten1(list);
		apply(effects, ...unflatten1(escape(flatten1(target))));
	});
	return {
		emit,
		on: (...path) => {
			const reflex = path.pop();
			path = encode(...path);
			const m = path.slice().reverse().concat(0).findIndex(a => a !== end);
			return reflexion0.on(...path.slice(0, path.length - m), (...list) => reflex(...decode(Array(m).fill(begin).concat(list))));
		},
		exec: code => emit(...code2signals(code)),
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
	const length = Number.MAX_SAFE_INTEGER.toString(2).length;
	if(uint.length > Math.ceil(length / 8) || uint.subarray(-1)[0] & ~((1 << length % 8) - 1)) throw RangeError("Not safe");
	let result = 0;
	uint.reverse().forEach(a => result = result * 0x100 + a);
	return result;
};

const stdvm = () => {
	const defn = (...path) => {
		const f = path.pop();
		return vm0.on(...path, (...effects) => {
			let results;
			try{
				results = [...f(effects)];
			}catch(error){}
			if(results) vm0.emit(["match?", results, [""], effects]);
		});
	};
	const defop = length => (...path) => {
		const f = path.pop();
		return defn(...path, args => f(...args.splice(0, length)));
	};
	const defop_2 = defop(2);
	const bin_fn = f => (...args) => args.every(is_buffer) && f(...args);
	const literal_map = new Map([
		[true, "T"],
		[false, "F"],
	]);
	const vm0 = vm();

	vm0.on("defer", (...signals) => Promise.resolve().then(() => run(() => vm0.emit(...signals))));
	defop_2("=", (a, b) => [literal_map.get(same_lists(a, b))]);
	defop_2("&", (a, b) => [a, b].every(is_buffer) ? [new Uint8Array([].concat(...[a, b].map(a => new Uint8Array(a)))).buffer] : [].concat(...a, ...b));

	defop_2("bin", "&", bin_fn((a, b) => [buffer_and(a, b)]));
	defop_2("bin", "|", bin_fn((a, b) => [buffer_or(a, b)]));
	defop_2("bin", "^", bin_fn((a, b) => [buffer_xor(a, b)]));

	let uint_s = new ArrayBuffer;
	const uint_1 = num2uint(1);
	defop(1)("uint", "trim", bin_fn(uint => [uint_trim(uint)]));
	defop_2("uint", "=", bin_fn((...uints) => [literal_map.get(same_lists(...uints.map(uint_trim)))]));
	defop_2("uint", "+", bin_fn((a, b) => [uint_add(a, b)]));
	defop_2("uint", "-", bin_fn((a, b) => [uint_sub(a, b)]));
	defop_2("uint", "*", bin_fn((a, b) => [uint_mul(a, b)]));
	defop_2("uint", "/", bin_fn((a, b) => uint_div(a, b)));
	defop_2("uint", "<", bin_fn((a, b) => [literal_map.get(uint_cmp(a, b) < 0)]));
	defop_2("uint", ">", bin_fn((a, b) => [literal_map.get(uint_cmp(a, b) > 0)]));
	defop_2("uint", "<=", bin_fn((a, b) => [literal_map.get(uint_cmp(a, b) <= 0)]));
	defop_2("uint", ">=", bin_fn((a, b) => [literal_map.get(uint_cmp(a, b) >= 0)]));
	defn("uint", "iota", () => [uint_s = uint_add(uint_s, uint_1)]);

	vm0.exec(`
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
vm0.on("test", "echo", (...args) => console.log(signals2code0(...args)));
//vm0.on((...signals) => console.log("signal", signals2code0(...signals)));
//*/
