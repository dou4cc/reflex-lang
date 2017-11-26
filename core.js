﻿"use strict";

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
	return unflatten("[", "]", ...words.filter(a => !a.startsWith(";")).map(a => {
		if(/^[[\]]$/u.test(a)) return a;
		if(/^`.*`$/u.test(a)) return {flag: "`", content:
			a.slice(1, -1)
			.replace(/(^|[^\\])\\[nrt]/gu, ($, $1) => $1 + {n: "\n", r: "\r", t: "\t"}[$1])
			.replace(/(^|[^\\])\\([0-9a-f]+);/giu, ($, $1, $2) => $1 + String.fromCodePoint(Number.parseInt($2, 16)))
			.replace(/\\(\\+(?:[0-9A-Fa-f]+;|[nrt])|\\*`)/gu, "$1")};
		return {flag: "", content: a};
	}));
};

const uint_shorten = a => {
	a = new Uint8Array(a);
	let i = a.length - 1;
	while(!a[i] && i > -2) i -= 1;
	return a.subarray(0, i + 1).buffer;
};

const num2uint = number => {
	if(!Number.isSafeInteger(number) || number < 0) throw new Error;
	const result = new Uint8Array(8);
	result.every((a, i) => {
		if(number < 1) return;
		result[i] = number % 256;
		number /= 256;
	});
	return uint_shorten(result.buffer);
};

const buffer2str = async buffer => {
	try{
		return Buffer(buffer).toString();
	}catch(error){}
	const reader = new FileReader;
	const result = new Promise((resolve, reject) => {
		reader.addEventListener("load", () => resolve(reader.result));
		reader.addEventListener("error", ({error}) => reject(error));
	});
	reader.readAsBinaryString(new Blob([buffer]));
	return await result;
};

const ast2signals = async source => {
	const [begin, end, ...list] = flatten(...source);
	for(let i in list){
		if(typeof list[i] !== "object") continue;
		let {content} = list[i];
		if(!list[i].flag){
			if(content.startsWith("\\")){
				list[i] = await buffer2str(num2uint(+content.slice(1)));
				continue;
			}
			if(content.startsWith("%")){
				content = (content.length % 2 ? "" : "0") + content.slice(1);
				list[i] = await buffer2str(new Uint8Array(content.length / 2).map((a, i) => {
					if(Number.isNaN(a = Number.parseInt(content.substr(i * 2, 2), 16))) throw new Error;
					return a;
				}).buffer);
				continue;
			}
			if(/^\$\d/u.test(content)){
				content = +content.slice(1);
				if(Number.isNaN(content)) throw new Error;
				list[i] = "$".repeat(content);
				continue;
			}
		}
		list[i] = content;
	}
	return unflatten(begin, end, ...list);
};

const code2signals = async source => await ast2signals(code2ast(source));

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

const vm = () => {
	const flatten1 = (...list) => {
		const [begin1, end1] = [, , ...list] = flatten(...list);
		return list.map(a => a === begin1 ? begin : a === end1 ? end : a);
	};
	const unflatten1 = (...list) => unflatten(begin, end, ...list);
	const emit = (...signals) => signals.forEach(signal => reflex0.emit(...flatten1(signal)));
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
				if(args) emit(...unflatten1(...[].concat(...flatten1(...effects).map(a => is_token(a) ? a.length < args.length ? args[a.length] : a.slice(args.length) : a))));
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
			if(!listener) return reflex0.on((...list) => path(...unflatten1(...list)));
			path = flatten1(path);
			const m = path.slice().reverse().concat(0).findIndex(a => a !== end);
			return reflex0.on(...path.slice(0, path.length - m), (...list) => listener(...unflatten1(...Array((m || 1) - 1).fill(begin).concat(list.slice(0, -1)))));
		},
		exec: async code => emit(...await code2signals(code)),
	};
};

const uint_add = (a, b) => {
	[a, b] = [a, b].map(a => new Uint8Array(uint_shorten(a)));
	const result = new Uint8Array(Math.max(a.length, b.length) + 1);
	result.reduce((_0, _1, i) => {
		const n = result[i - 1] + (a[i - 1] || 0) + (b[i - 1] || 0);
		result[i] = n > 255;
		result[i - 1] = n % 256;
	});
	return uint_shorten(result.buffer);
};

const uint_cmp = (a, b) => {
	[a, b] = [a, b].map(a => new Uint8Array(uint_shorten(a)));
	return a.length + b.length && Math.sign(a.length - b.length || a.subarray(-1)[0] - b.subarray(-1)[0]) || uint_cmp(...[a, b].map(a => a.subarray(0, -1)));
};

const uint_sub = (a, b) => {
	if(uint_cmp(a, b) < 0) throw new Error;
	[a, b] = [a, b].map(a => new Uint8Array(uint_shorten(a)));
	a = new Uint8Array(a);
	a.forEach((_, i) => (a[i] -= b[i] || 0) < 0 && a.subarray(i + 1).every((_, j) => (a[i + j + 1] -= 1) < 0));
	return uint_shorten(a.buffer);
};

const str2buffer = async string => {
	try{
		return new Uint8Array(Buffer(string)).buffer;
	}catch(error){}
	const reader = new FileReader;
	const result = new Promise((resolve, reject) => {
		reader.addEventListener("load", () => resolve(reader.result));
		reader.addEventListener("error", ({error}) => reject(error));
	});
	reader.readAsArrayBuffer(new Blob([string]));
	return await result;
};

const same_lists = (...lists) => {
	if(!Array.isArray(lists[0])) return lists.splice(1).every(a => lists.includes(a));
	return lists.slice(1).every(a => Array.isArray(a) && a.length === lists[0].length) && lists[0].every((_, i) => same_lists(...lists.map(list => list[i])));
};

const stdvm = () => {
	const vm0 = vm();
	vm0.on([["same"]], (args, ...rest) => rest.length || Array.isArray(args) && vm0.emit([["same", ...args], same_lists(...args).toString()]));
	vm0.on([["concat"]], (args, ...rest) => rest.length || Array.isArray(args) && vm0.emit([["concat", ...args], args.every(a => typeof a === "string") ? args.join("") : [].concat(...args)]));
	vm0.on([["split"]], ([arg, ...rest0], ...rest1) => rest0.length || rest1.length || Array.from(arg).length && vm0.emit([["split", arg], ...arg]));
	vm0.emit(
		["on", ["true", [""], ""], "$"],
		["on", ["false", [""], ""], "$$"],
	);
	return vm0;
};

const signals2code = (...signals) => {
	const [begin, end, ...list] = flatten(...signals);
	return list
	.map(a =>
		a === begin ? "[ \t" : a === end ? "\t ]": "`" + a
		.replace(/\\(?:[0-9A-Fa-f]+;|[nrt])|`/gu, "\\$&")
		.replace(/[\n\r\t]/gu, $ => ({"\n": "\\n", "\r": "\\r", "\t": "\\t"})[$]) + "`")
	.join(" ")
	.replace(/ \t /gu, "");
};

stdvm;

/*test*
var vm0 = stdvm();
vm0.on(["+"], (a, b, ...rest) => rest.length || vm0.emit(["+", a, b, (+a + +b).toString()]));
vm0.on(["log"], console.log);
vm0.emit(["on", ["+", "", "", "", ""], ["log", "$", "+", "$$", "=", "$$$"]]);
vm0.emit(["+", "1", "1"]);
//*/
