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
	const check = () => children.size || reflexes.size || free && run(free);
	const children = new Map;
	const reflexes = new Set;
	return {
		emit: (...list) => {
			const reflexes1 = Array.from(reflexes).reverse();
			if(list.length) (children.get(list[0]) || {emit(){}}).emit(...list.slice(1));
			reflexes1.forEach(f => reflexes.has(f) && f(...list));
		},
		on: (first, ...rest) => {
			if(rest.length){
				if(!children.has(first)) children.set(first, reflexion(() => {
					children.delete(first);
					check();
				}));
				return children.get(first).on(...rest);
			}
			const reflex = (...args) => run(() => first(...args));
			reflexes.add(reflex);
			return () => {
				reflexes.delete(reflex);
				check();
			};
		},
	};
};

const path_map = free => {
	const check = () => children.size || value === undefined && free && run(free);
	let value;
	const children = new Map;
	return {
		set: (first, ...rest) => {
			if(rest.length){
				if(!children.has(first)) children.set(first, path_map(() => {
					children.delete(first);
					check();
				}));
				return children.get(first).set(...rest);
			}
			value = first;
			check();
		},
		get: (...path) => path.length ? (children.get(path.shift()) || {get(){}}).get(...path) : value,
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

const vm_min = () => {
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
		const f = (pattern, ...list) => {
			if(pattern === "") return args.push(list);
			if(list.length > 1) return;
			if(![pattern, ...list].every(Array.isArray)) return escape(list).includes(pattern);
			if(pattern.length > list[0].length + 1) return;
			if(!pattern.length) return !list[0].length;
			const target = list[0].slice();
			return target.splice(0, pattern.length - 1).map(a => [a]).concat([target]).every((a, i) => f(pattern[i], ...a));
		};
		const args = [];
		if(f(pattern, target)) return args;
	};
	const apply = (effect, ...args) => {
		args = args.map(a => serialize1(...a));
		return reflexion0.emit(...[].concat(...serialize1(...effect).map(a =>
			is_param(a) ? a.length < args.length ? args[a.length] : a.slice(args.length) : a
		)));
	};
	const emit = (...signal) => reflexion0.emit(...encode(...signal));
	const begin = Symbol();
	const end = Symbol();
	const reflexion0 = reflexion();
	const handles = path_map();
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

const uint2num = uint => {
	uint = new Uint8Array(uint_trim(uint));
	const top = Number.MAX_SAFE_INTEGER.toString(2).length;
	if(uint.length > Math.ceil(top / 8) || uint.subarray(-1)[0] & ~((1 << top % 8) - 1)) throw RangeError("Not safe");
	let number = 0;
	uint.reverse().forEach(a => number = number * 0x100 + a);
	return number;
};

const buffer_concat = buffer_fn((...buffers) => new Uint8Array([].concat(...buffers.map(a => Array.from(a)))));

const buffer_uuid = free => {
	const check = () => used || children.size || free && run(free);
	let used;
	const children = new Map;
	return {
		alloc: () => {
			if(!used){
				used = true;
				return new ArrayBuffer;
			}
			const byte = new Uint8Array([Math.random() * 0xff]).buffer;
			const key = buffer2bin(byte);
			if(!children.has(key)) children.set(key, buffer_uuid(() => {
				children.delete(key);
				check();
			}));
			return buffer_concat(byte, children.get(key).alloc());
		},
		free: uuid => {
			if(uuid.byteLength){
				const key = buffer2bin(uuid.slice(0, 1));
				if(children.has(key)) children.get(key).free(uuid.slice(1));
				return;
			}
			used = false;
			check();
		},
	};
};

const vm_defn = vm => (...path) => {
	const f = path.pop();
	return vm.on(...path, (...effect) => {
		let results;
		try{
			results = [...f(effect)];
		}catch(error){}
		if(results) vm.emit("quote", results, ...effect);
	});
};

const vm_defop = defn => (length, ...path) => {
	const f = path.pop();
	return defn(...path, args => f(...args.splice(0, length)));
};
	
const vm_symbol = a => new Map([
	[true, "T"],
	[false, "F"],
]).get(a);

const uint_counter = () => {
	let i = num2uint(0);
	return () => i = uint_add(i, num2uint(1));
};

const vm_std = () => {
	const vm = vm_min();
	const defn = vm_defn(vm);
	const defop = vm_defop(defn);
	const bin_fn = f => (...buffers) => buffers.every(is_buffer) && f(...buffers);
	const length = list => Array.isArray(list) ? list.length : is_buffer(list).byteLength;
	
	vm.exec(`
		[match
			[reflex [reflex [$0] $0] match [$1] [reflex [emit $2] $2] _ escape [$1] match $2 [reflex [$3] $3]
				[escape [reflex [$3] $4] reflex [emit $3] start $5]
			]
			$0
			[start $0 $0]
		]
		
		[reflex [unreflex [$0] $0] match [$1] [unreflex [emit $2] $2] _ escape [$1] match $2 [unreflex [$3] $3]
			[escape [reflex [$3] $4] unreflex [emit $3] start $5]
		]
		
		[reflex [quote [$0] $0] match [$1] [quote $2 $2]
			[match $2 [$4] [$3]]
		]
		
		[reflex [fn $0] escape [$1] match $2 [fn $3 [$3 $3] $3 $3]
			[escape [match [$3 $8] [$4 $5] $6] quote [[$5] start $8] $7]
		]
		
		[fn _ [_ defn $0 $0 $0] [fn $0 $1 [$2] reflex $3] reflex $0]
		
		[fn _ [_ undefn $0 $0 $0] [fn $0 $1 [$2] unreflex $3] reflex $0]
		
		[defn _ [_ call $0] escape [$0] match $1 [$2 [$2] $2]
			[quote [match [$2 $5] [$6] [$4]] $3 $5]
		]
		
		[defn _ [_ match? $0] call [$0] [escape [$0]] match [$1] [[$2] [$2 $2 $2]]
			[call [$3 $2] [escape [$5]] match [$6] [[$7 $7 $7 $7] [$7]]
				[match [[[_ match? $7 $9] start [unreflex $12] [quote [T] $11] ] $8 $9 $10] [[$12] $12 $12 $12]
					[start
						[reflex $12]
						[_ match? $13 $13]
						[match $13 $14 _ start
							[unreflex $12]
							[quote [F] $15]
						]
					]
				]
			]
		]
		
		[defn _ [_ list ? $0 $0] call [$1] [match? $0 [$2]] match [$2] [[$3] $3]
			[quote [$4] $3]
		]
		
		[defn _ [_ cond $0 $0 $0 $0]
			match $0 T [quote [$1] $3]
			match $0 F [quote [$2] $3]
		]
		
		[defn _ [_ and $0 $0 $0]
			match [$0 $1] [T T] [quote [T] $2]
			match [$0 $1] [T F] [quote [F] $2]
			match [$0 $1] [F T] [quote [F] $2]
			match [$0 $1] [F F] [quote [F] $2]
		]
		
		[defn _ [_ or $0 $0 $0]
			match [$0 $1] [T T] [quote [T] $2]
			match [$0 $1] [T F] [quote [T] $2]
			match [$0 $1] [F T] [quote [T] $2]
			match [$0 $1] [F F] [quote [F] $2]
		]
		
		[defn _ [_ not $0 $0]
			match $0 T [quote [F] $1]
			match $0 F [quote [T] $1]
		]
		
		[defn _ [_ = $0 $0 $0] call [$0 $2] [escape $1] match [$3] [[$4 $4] $4]
			[match? $4 $6 $5]
		]
		
		[defn _ [_ _ eval [$0] [] $0] $0 $1]
		
		[defn _ [_ _ eval [$0] [$0 $0] $0] match [[$0] [$1 $2] $3] [$4 [[$4] $4] $4]
			[call [$4 [$6] $7] [$5] match [$8] [[[$9] $9 $9] $9]
				[_ eval [$9 $12] $10 $11]
			]
			_ eval [$0 $1] [$2] $3
		]
		
		[defn _ [_ eval $0 $0] _ eval [] $0 $1]
		
		[defn _ [_ head [$0 $0] $0] quote [$0] $2]
		
		[defn _ [_ tail [$0 $0] $0] quote [$1] $2]
		
		[defn _ [_ for-each [$0 $0] $0] start
			[quote [$0] $2]
			[for-each [$1] $2]
		]
		
		[for-each [[alias defn] [unalias undefn]] match $0 [$1 $1]
			[defn _ [_ $1 $3] escape [$3] match $4 [[$5] $5]
				[$2 _ [_ $5 $7] $6 $7]
			]
		]
	`);

	vm.on("defer", (...signal) => Promise.resolve().then(() => run(() => vm.emit(...signal))));
	
	defop(1, "list", "length", list => Array.isArray(list) && [num2uint(list.length)]);
	defop(2, "list", "concat", (a, b) => [a, b].every(Array.isArray) && a.concat(b));
	defop(3, "list", "slice", (begin, end, list) => {
		if(!Array.isArray(list)) return;
		[begin, end] = [begin, end].map(uint2num);
		if(end <= list.length) return list.slice(begin, end);
	});
	
	defop(1, "binary", "?", a => vm_symbol(Boolean(is_buffer(binary))));
	defop(1, "binary", "length", bin_fn(binary => [num2uint(binary.length)]));
	defop(2, "binary", "&", bin_fn((a, b) => [buffer_and(a, b)]));
	defop(2, "binary", "|", bin_fn((a, b) => [buffer_or(a, b)]));
	defop(2, "binary", "^", bin_fn((a, b) => [buffer_xor(a, b)]));
	defop(2, "binary", "concat", bin_fn((a, b) => [buffer_concat(a, b)]));
	defop(3, "binary", "slice", (begin, end, binary) => {
		if(!is_buffer(binary)) return;
		[begin, end] = [begin, end].map(uint2num);
		if(end <= binary.byteLength) return [binary.slice(begin, end)];
	});
	
	const uuid = buffer_uuid();
	defn("uuid", "alloc", () => [uuid.alloc()]);
	vm.on("uuid", "free", (a, ...rest) => rest.length || is_buffer(a) && uuid.free(a));
	
	const uint_iota = uint_counter();
	defop(1, "uint", "trim", bin_fn(uint => [uint_trim(uint)]));
	defop(2, "uint", "+", bin_fn((a, b) => [uint_add(a, b)]));
	defop(2, "uint", "-", bin_fn((a, b) => [uint_sub(a, b)]));
	defop(2, "uint", "*", bin_fn((a, b) => [uint_mul(a, b)]));
	defop(2, "uint", "/", bin_fn((a, b) => [uint_div(a, b), uint_mod(a, b)]));
	defop(2, "uint", "=", bin_fn((a, b) => [vm_symbol(!uint_cmp(a, b))]));
	defop(2, "uint", "<", bin_fn((a, b) => [vm_symbol(uint_cmp(a, b) < 0)]));
	defop(2, "uint", ">", bin_fn((a, b) => [vm_symbol(uint_cmp(a, b) > 0)]));
	defop(2, "uint", "<=", bin_fn((a, b) => [vm_symbol(uint_cmp(a, b) <= 0)]));
	defop(2, "uint", ">=", bin_fn((a, b) => [vm_symbol(uint_cmp(a, b) >= 0)]));
	defn("uint", "iota", () => [uint_iota()]);
	
	vm.exec(`
		[for-each [list binary] start
			[defn _ [_ $0 at $1 $1 $1] eval [$0 slice [quote [$1]] [uint + $1 \\1] [quote [$2]]] $3]
			[defn _ [_ $0 last-slice $1 $1 $1 $1] eval [$0 slice
				[eval [uint - [$0 length $3] [quote [$2]]]]
				[eval [uint - [$0 length $3] [quote [$1]]]]
				[quote [$3]]]
				$4
			]
			[defn _ [_ $0 cut $1 $1 $1] eval [$0 slice [quote [$1]] [$0 length $2] [quote [$2]]] $3]
			[defn _ [_ $0 last-cut $1 $1 $1] eval [$0 last-slice [quote [$1]] [$0 length $2] [quote [$2]]] $3]
		]
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

const guid = () => {
	let d = Date.now();
	return "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx".replace(/x|y/gu, c => {
		const r = Math.floor(d + Math.random() * 16) % 16;
		d /= 16;
		return (c === "x" ? r : r & 0x3 | 0x8).toString(16);
	});
};

const guid2buffer = guid => hex2buffer(guid.replace(/-/gu, ""));

const vm_common = log => {
	const vm = vm_std();
	if(log) vm.on("log", (...message) => {
		try{
			message = signals2code({utf8_to_str: true})(...message);
		}catch(error){}
		if(is_str(message)) log(message);
	});
	return vm;
};

({vm_min, vm_std, vm_common});

/*test*
var vm = vm_common(message => console.log("log: " + message));
//vm.on((...signal) => console.log("signal: " + signals2code({utf8_to_str: true})(...signal)));
//*/
