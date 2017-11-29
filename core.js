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
	return a.subarray(0, i);
});

const num2uint = number => {
	if(!Number.isSafeInteger(number) || number < 0) throw new Error;
	const result = new Uint8Array(8);
	for(let i = 0; number >= 1; i += 1){
		result[i] = number % 0xff;
		number /= 0xff;
	}
	return uint_shorten(result.buffer);
};

const ast2signals = source => {
	const [begin, end, ...list] = flatten(...source);
	for(let i in list){
		if(typeof list[i] !== "object") continue;
		let {content} = list[i];
		if(!list[i].flag){
			if(/^\\/u.test(content)){
				list[i] = num2uint(+content.slice(1));
				continue;
			}
			if(/^%/u.test(content)){
				if(!(content.length % 2)) throw new Error;
				content = content.slice(1);
				list[i] = new Uint8Array(content.length / 2).map((a, i) => {
					a = Number.parseInt(content.substr(i * 2, 2), 16);
					if(Number.isNaN(a)) throw new Error;
					return a;
				}).buffer;
				continue;
			}
			if(/^\$\d/u.test(content)){
				content = +content.slice(1);
				if(Number.isNaN(content)) throw new Error;
				content = "$".repeat(content);
			}
		}
		list[i] = content;
	}
	return unflatten(begin, end, ...list);
};

const code2signals = source => ast2signals(code2ast(source));

const reflex = free => {
	const off = () => !children.size && !listeners.size && free && free();
	const children = new Map;
	const listeners = new Set;
	const f = path => ({
		emit: (...list) => {
			list = path.concat(list);
			if(list.length) (children.get(list[0]) || {emit(){}}).emit(...list.slice(1));
			listeners.forEach(f => f(...list));
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
		return Reflect.getOwnPropertyDescriptor(Reflect.getPrototypeOf(Int8Array.prototype), "buffer").get.call(a);
	}catch(error){}
	try{
		Reflect.getOwnPropertyDescriptor(ArrayBuffer.prototype, "byteLength").get.call(a);
		return new Int8Array(a).buffer;
	}catch(error){}
	return false;
};

const buffer2bin = buffer => Array.from(new Uint8Array(buffer)).map(a => String.fromCodePoint(a)).join("");

const str2utf8 = string => typeof Buffer === "undefined" ? new Uint8Array([].concat(...Array.from(string).map(a => {
	const f = (a, i) => i ? f(a >> 6, i - 1).concat(0x80 | a & 0x3f) : [];
	a = a.codePointAt();
	if(a < 0x80) return [a];
	const list = f(a, a = Math.floor(Math.log2(a) / 6) + 1);
	list[0] |= ~(1 << 8 - a) + 1;
	return list;
}))).buffer : new Uint8Array(Buffer(string)).buffer;

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
	const emit = (...signals) => signals.forEach(signal => reflex0.emit(...encode(signal)));
	const begin = Symbol();
	const end = Symbol();
	const reflex0 = reflex();
	const reflex1 = reflex();
	const handles = multi_key_map();
	reflex0.on(begin, "on", (...list) => {
		const is_token = a => typeof a === "string" && /^\$*$/u.test(a);
		const escape = a => is_token(a) ? a.slice(1) : a;
		const match = list => {
			const f = (pattern, ...list) => {
				if(pattern === "") return args.push(list);
				if(list.length > 1) return;
				if(!Array.isArray(pattern) || !Array.isArray(list[0])) return list.includes(escape(pattern));
				return pattern.length <= list[0].length + 1 && (pattern.length ? list[0].splice(0, pattern.length - 1).map(a => [a]).concat(list).every((a, i) => f(pattern[i], ...a)) : !list[0].length);
			};
			const args = [[pattern, ...effects]];
			return f(pattern, ...unflatten1(...path, ...list)) && args;
		};
		if(handles.get(...list)) return;
		const [pattern, ...effects] = unflatten1(...list.slice(0, -1));
		const list1 = flatten1(pattern);
		const path = list1.slice(0, list1.concat("").indexOf("")).map(escape);
		handles.set(...list, [
			reflex0.on(...path, (...list) => {
				const args = match(list);
				if(args) unflatten1(...[].concat(...flatten1(...effects).map(a => is_token(a) ? a.length < args.length ? args[a.length] : a.slice(args.length) : a))).forEach(signal => reflex0.emit(...flatten1(signal)));
			}),
			reflex1.on(...path, (...list1) => match(list1) && reflex0.emit(begin, "on", ...list)),
		]);
	});
	reflex0.on(begin, "off", (...list) => {
		const handle = handles.get(...list);
		if(!handle) return;
		handle.forEach(f => f());
		handles.set(...list, undefined);
	});
	reflex0.on(begin, "match", (...list) => unflatten1(...list.slice(0, -1)).forEach(signal => reflex1.emit(...flatten1(signal))));
	return {
		emit,
		on: (path, listener) => {
			if(!listener) return reflex0.on((...list) => path(...decode(...list)));
			path = encode(path);
			const m = path.slice().reverse().concat(0).findIndex(a => a !== end);
			return reflex0.on(...path.slice(0, path.length - m), (...list) => listener(...decode(...Array((m || 1) - 1).fill(begin).concat(list.slice(0, -1)))));
		},
		exec: code => emit(...code2signals(code)),
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
		result[i + 1] = n >= 0xff;
		result[i] = n & 0xff;
	}
	return result;
});

const uint_cmp = (a, b) => {
	[a, b] = [a, b].map(a => new Uint8Array(uint_shorten(a)));
	return a.length + b.length && Math.sign(a.length - b.length || a.subarray(-1)[0] - b.subarray(-1)[0]) || uint_cmp(...[a, b].map(a => a.subarray(0, -1)));
};

const uint_sub = uint_fn((a, b) => {
	if(uint_cmp(a.buffer, b.buffer) < 0) throw new Error;
	a = new Uint8Array(a);
	a.forEach((_, i) => (a[i] -= b[i] || 0) < 0 && a.subarray(i + 1).every((_, j) => (a[i + j + 1] -= 1) < 0));
	return a;
});

const same_lists = (...lists) => {
	if(!Array.isArray(lists[0])) return lists.splice(1).every(a => lists.includes(a) || indexedDB.cmp(is_buffer(...lists), is_buffer(a) || 0));
	return lists.slice(1).every(a => Array.isArray(a) && a.length === lists[0].length) && lists[0].every((_, i) => same_lists(...lists.map(list => list[i])));
};

const stdvm = () => {
	const vm0 = vm();
	vm0.on([["same"]], (args, ...rest) => rest.length || Array.isArray(args) && vm0.emit([["same", ...args], same_lists(...args).toString()]));
	vm0.on([["concat"]], (args, ...rest) => rest.length || Array.isArray(args) && vm0.emit([["concat", ...args], args.every(is_buffer) ? new Uint8Array([].concat(...args.map(a => new Uint8Array(a)))).buffer : [].concat(...args.map(a => {
		const list = Array.from(a);
		return list.length ? list : a;
	}))]));
	vm0.emit(
		["on", ["true", [""], ""], "$"],
		["on", ["false", [""], ""], "$$"],
	);
	return vm0;
};

const utf82str = utf8 => {
	utf8 = new Uint8Array(utf8);
	let result = "";
	for(let i = 0; i < utf8.length; i += 1){
		const byte = utf8[i];
		if(!(byte & 0x80)){
			result += String.fromCodePoint(byte);
			continue;
		}
		const length = byte.toString(2).indexOf("0");
		if(length < 2 || length > 6) throw new Error;
		let point = byte & (1 << 8 - length) - 1;
		for(let j = length - 1; j; j -= 1){
			i += 1;
			if(i >= utf8.length || (utf8[i] & 0xc0) !== 0x80) throw new Error;
			point = (point << 6) | utf8[i] & 0x3f;
		}
		if(point < (1 << 4 * length + 1)) throw new Error;
		result += String.fromCodePoint(point);
	};
	return result;
};

const signals2code = (...signals) => {
	const [begin, end, ...list] = flatten(...signals);
	return list
	.map(a => {
		if(a === begin) return "[ \t";
		if(a === end) return "\t ]";
		const buffer = is_buffer(a);
		if(buffer) return "%" + Array.from(new Uint8Array(buffer)).map(a => a.toString(16).padStart(2, "0")).join("");
		if(typeof a !== "string") throw new Error;
		if(/^\$*$/u.test(a)) return "$" + a.length;
		return /^\\|^%|^\$\d|[[\s\]]/u.test(a) ? "`" + a
		.replace(/\\(?:[0-9A-Fa-f]+;|[nrt])|`/gu, "\\$&")
		.replace(/[\n\r\t]/gu, $ => ({"\n": "\\n", "\r": "\\r", "\t": "\\t"})[$]) + "`" : a;
	})
	.join(" ")
	.replace(/ \t /gu, "");
};

stdvm;

/*test*
var vm0 = stdvm();
vm0.on(["+"], (a, b, ...rest) => rest.length || vm0.emit(["+", a, b, (+utf82str(a) + +utf82str(b)).toString()]));
vm0.on(["log"], (...args) => console.log(...args.map(utf82str)));
vm0.emit(["on", ["+", "", "", "", ""], ["log", "$", "+", "$$", "=", "$$$"]]);
vm0.emit(["+", "1", "1"]);
//*/
