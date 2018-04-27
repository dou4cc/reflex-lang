//ES2018 with BigInt

"use strict";

const equal = (a, b) => [a].includes(b);

const capture = async => {
	(async () => {
		try{
			await async;
		}catch(_){}
	})();
	return async;
};

const dispatch = async async => {
	(async () => async)();
	try{
		await async;
	}catch(_){}
};

const async = () => {
	const async = () => {
		let returns;
		return [new Promise((...returns1) => returns = returns1), ...returns];
	};
	const queue = (function*(){
		for(; ; ){
			const [index, ...args] = yield;
			returns[index](...args);
			[, ...returns] = async();
			returns[0] = () => {};
		}
	})();
	queue.next();
	let returns;
	return [
		capture(([, ...returns] = async())[0]),
		...returns.map((_, i) => (...args) => {
			queue.next([i, ...args]);
		}),
	];
};

const gen = function*(fn){
	for(; ; ) yield fn();
};

const [hang] = async();

const call = async (fn, ...args) => fn(...args);

const defer = async (fn, ...args) => (await fn)(...args);

const fn_bind = (fn, ...args0) => (...args1) => fn(...args0, ...args1);

const value = value => value;

const thunk = fn_bind(fn_bind, value);

const lazy = thunk => {
	const [async0, resolve] = async();
	return async () => {
		resolve({then: resolve => resolve(thunk())});
		return async0;
	};
};

const catch_all = async (...args) => {
	try{
		return await call(...args);
	}catch(error){
		(await throw_call_stack_error)(error);
	}
};

const scheduler = (free, mode = (fn, ...args) => fn(...args)) => {
	let count = 0n;
	return (...args) => {
		count += 1n;
		const async0 = mode(...args);
		return (async () => {
			const [async1, ...returns] = async();
			await (async () => async0)().then(...returns);
			count -= 1n;
			defer(free, () => !count);
			return async1;
		})();
	};
};

const thread = () => {
	const queue = (async function*(){
		for(; ; ) await (yield)();
	})();
	queue.next();
	return async (...args) => {
		const [async0, ...returns] = async();
		await queue.next(() => call(...args).then(...returns));
		return async0;
	};
};

const lock = () => {
	const [thread0, thread1] = gen(thread);
	return (mode, ...args) => {
		switch(mode){
			case "readonly":
			return (async () => {
				const [async0, ...returns] = async();
				await thread0(() => {
					thread1(thunk(capture(call(...args).then(...returns))));
				});
				return async0;
			})();
			case "readwrite":
			return thread0(thread1, ...args);
		}
		throw_unsupported_error();
	};
};

const mutex = () => {
	const [thread0, thread1] = gen(thread);
	let mode;
	return (index, ...args) => [
		async (...args) => {
			const [async0, resolve] = async();
			await thread0(() => resolve(call(...args)));
			return async0;
		},
		...[false, true].map(macro => async (...args) => {
			const [async0, ...returns] = async();
			await thread0(async () => {
				if(mode === macro){
					thread1(thunk(capture(call(...args).then(...returns))));
					return;
				}
				const [async0, resolve] = async();
				thread1(() => {
					resolve();
					return call(...args);
				}).then(...returns);
				await async0;
				mode = macro;
			});
			return async0;
		}),
	][index](...args);
};

const atom = ([...modes], index0, fn, ...args) => {
	index0 = Math.min(modes.length - 1, Math.floor(index0));
	const [async0, ...returns] = async();
	const async1 = modes[index0](async () => {
		const [async0, resolve] = async();
		(async () => resolve(await call(fn, index => {
			if(index >= index0) return;
			const async = atom(modes, index, fn, ...args);
			return (async () => {
				resolve();
				await (async () => async)().then(...returns);
				await hang;
			})();
		}, ...args).then(...returns)))();
		return async0;
	});
	return (async () => {
		await async1;
		return async0;
	})();
};

const atom_lock = (lock, ...args) => atom([
	fn_bind(lock, "readwrite"),
	fn_bind(lock, "readonly"),
	call,
], ...args);

const schedulers = mode => {
	const lock0 = lock();
	const map = new Map;
	return fn_bind(lock0, "readwrite", (key, ...args) => {
		if(!map.has(key)){
			const mode0 = scheduler(fn_bind(atom_lock, lock0, 2, async (mode, idle) => {
				if(!idle()) return;
				await mode(1);
				if(map.get(key) !== mode0) return;
				await mode(0);
				map.delete(key);
			}), mode());
			map.set(key, mode0);
		}
		return map.get(key)(...args);
	});
};

const is_object = a => Object(a) === a;

const [
	is_string,
	is_big_int,
] = [
	"string",
	"bigint",
].map(macro => a => typeof a === macro);

const is_param = a => is_string(a) && /^\$*$/u.test(a);

const is_buffer = a => {
	try{
		Reflect.getOwnPropertyDescriptor(ArrayBuffer.prototype, "byteLength").get.call(a);
		return new Uint8Array(a).buffer;
	}catch(error){
		if(!(error instanceof TypeError)) throw error;
	}
};

const [is_list, list, list_clone] = (() => {
	const locks = schedulers(lock);
	const cache = new WeakMap;
	const lists = new WeakSet;
	return [
		a => {
			if(!is_object(a)) return false;
			if(lists.has(a)) return true;
			return atom_lock(fn_bind(locks, a), 1, async mode => {
				if(!cache.has(a)){
					await mode(0);
					cache.set(a, Boolean(await catch_all(() => {
						for(let iter of [
							"asyncIterator",
							"iterator",
						]){
							iter = a[Symbol[iter]];
							if(iter != null) return is_object(Function.prototype.call.call(iter, a));
						}
					})));
				}
				return cache.get(a);
			});
		},
		() => {
			const key = Symbol.asyncIterator;
			const [returns, resolve] = async();
			const entry = lazy(() => {
				const thread0 = thread();
				let resolve0;
				const [entry] = [, resolve0] = async();
				resolve([
					async value => {
						const resolve1 = resolve0;
						const [next] = [, resolve0] = async();
						const [async0, resolve] = async();
						resolve1([lazy(() => {
							resolve();
							return next;
						}), capture((async () => list_clone(await value))())]);
						await async0;
					},
					() => resolve0(),
					reason => resolve0((async () => {
						throw reason;
					})()),
				].map(fn => fn_bind(thread0, fn)));
				return entry;
			});
			const list = {};
			if(!Reflect.defineProperty(list, key, {value: () => {
				let cursor = entry;
				const iter = {
					[key]: () => iter,
					next: fn_bind(thread(), async () => {
						if(cursor){
							const cursor0 = cursor;
							cursor = null;
							cursor = await cursor0();
						}
						return cursor ? {done: false, value: ([cursor] = cursor)[1]} : {done: true};
					}),
				};
				return iter;
			}})) throw_unsupported_error();
			lists.add(list);
			return [list, returns];
		},
		async list => await is_list(list) && !lists.has(list) ? list_map(list_clone, list) : list,
	];
})();

const fn_to_list = (fn, ...args) => {
	const [list0, returns0] = list();
	(async () => {
		const [append, ...returns] = await returns0;
		call(fn, append, ...args).then(...returns);
	})();
	return list0;
};

const list_concat = fn_bind(fn_to_list, async (append, lists) => {
	for await(let list of lists) for await(let a of await list) await append(a);
});

const list_to_array = async list => {
	if(!await is_list(list)) return list;
	const array = [];
	for await(let a of list) array.push(capture((async () => list_to_array(await a))()));
	return Promise.all(array);
};

const list_map = fn_bind(fn_to_list, async (append, fn, list) => {
	for await(let a of list) await append((async () => fn(await a))());
});

const list_flat_map = async (fn, list) => await is_list(list) ? list_map(fn_bind(list_flat_map, fn), list) : fn(list);

const throw_unsupported_error = () => {
	throw new TypeError("Unsupported");
};

const throw_call_stack_error = capture(defer(async () => {
	const samples = await list_to_array(list_map(fn => {
		try{
			fn();
		}catch(error){
			return String(error);
		}
		throw_unsupported_error();
	}, [
		() => {
			const fn = () => !fn();
			fn();
		},
	]));
	return error => {
		error = String(error);
		if(samples.some(sample => sample === error)) throw error;
	};
}));

const throw_syntax_error = () => {
	throw new SyntaxError("Unexpected token");
};

const list_next = async list0 => {
	const list = (async function*(){
		for await(let a of list0) yield thunk(a);
	})();
	const {done, value} = await list.next();
	return done ? [] : [value(), list_map(call, list)];
};

const list_trim = (list, rest) => [list, fn_to_list(async append => {
	for await(let _ of list);
	for await(let a of rest()) await append(a);
})];

const list_flatten = fn_bind(fn_to_list, async (append, begin, end, list) => {
	if(!await is_list(list)) return append(list);
	for await(let a of list_concat([[begin], list_concat(list_map(fn_bind(list_flatten, begin, end), list)), [end]])) await append(a);
});

const list_unflatten = (begin, end, list) => list_trim(fn_to_list(async append => {
	let a;
	if(equal(await ([, list] = await list_next(list))[0], begin)) while(([a, list] = await list_next(list)).length){
		a = await a;
		if(equal(a, end)) return;
		await append(equal(a, begin) ? ([, list] = list_unflatten(begin, end, list_concat([[begin], list])))[0] : a);
	}
	throw_syntax_error();
}), () => list);

const strings_match = async (regex, strings) => {
	regex = new RegExp(regex);
	let string = "";
	let $;
	while(!($ = regex.exec(string))){
		const [value] = [, strings] = await list_next(strings);
		if(!strings) return null;
		string += await value;
	}
	return [list_concat([[string.slice(regex.lastIndex)], strings]), ...$];
};

const strings_match_all = fn_bind(fn_to_list, async (append, regex, cursor) => {
	while(cursor = await strings_match(regex, cursor)) await append(([cursor] = cursor).slice(1));
});

const strings_normalize = fn_bind(fn_to_list, async (append, strings) => {
	let end, rest;
	for await(let string of await
		(async () => [strings, ["\0"]])()
		.then(list_concat)
		.then(fn_bind(strings_match_all, /[^\r]+|\r(?=[^])\n?/gu))
		.then(fn_bind(list_map, async $ => (await list_to_array($))[0].replace(/\r\n?/u, "\n")))
	){
		if(rest){
			await append(rest);
			rest = null;
		}
		string = await string;
		if(/\0$/u.test(string)){
			rest = "\0";
			string = string.slice(0, -1);
		}
		await append(string);
		end = string.slice(-1) || end;
	}
	if(end !== "\n") await append("\n");
});

const string_concat = async strings => (await list_to_array(strings)).join("");

const numbers_to_buffer = async numbers => new Uint8Array(await list_to_array(numbers)).buffer;

const hex_to_buffer = hex => numbers_to_buffer(fn_to_list(append => {
	let i = hex.length;
	if(i % 2) throw_syntax_error();
	while(i) append((async () => {
		const byte = await defer(hex.substring.bind(hex), i, i -= 2);
		if(/^[\da-f]*$/iu.test(byte)) return Number.parseInt(byte, 16);
		throw_syntax_error();
	})());
}));

const code_to_list = code => {
	const [begin, end] = gen(Symbol);
	const [list, rest] = list_unflatten(begin, end, list_concat([[begin], fn_to_list(async append => {
		code = strings_normalize(code);
		let cursor;
		while(cursor = await strings_match(/`(?:\\`|[^`])*`|#.*(?=\n)|[[\]]|[^[#`\s\]]+(?=[[#`\s\]])/gu, code)){
			const [, token] = [code] = cursor;
			if(!/^#/u.test(token)) await append(defer(() => {
				if(token === "[") return begin;
				if(token === "]") return end;
				if(/^`/u.test(token)) return token
				.slice(1, -1)
				.replace(/(^|[^\\])\\([nrt])/gu, (...$) => $[1] + new Map([
					["n", "\n"],
					["r", "\r"],
					["t", "\t"],
				]).get($[2]))
				.replace(/(^|[^\\])\\(\[.*?(?:\]|$))/gu, (...$) => {
					if(/^\[[\da-f]*\]$/iu.test($[2])) return $[1] + String.fromCodePoint(Number.parseInt("0" + $[2], 16));
					throw_syntax_error();
				})
				.replace(/\\(\\+[nrt[]|\\*`)/gu, "$1");
				if(/^%/u.test(token)) return hex_to_buffer(token.slice(1).replace(/-/gu, ""));
				if(/^\d/u.test(token)){
					if(/^(?!0.)\d+$/u.test(token)) return BigInt(token);
					throw_syntax_error();
				}
				if(/^\$\d/u.test(token)){
					if(/^\$(?!0.)\d+$/u.test(token)) return "$".repeat(Number(token.slice(1)));
					throw_syntax_error();
				}
				return token;
			}));
		}
		if(!/^\s*$/u.test(await string_concat(code))) throw_syntax_error();
	}), [end]]));
	return fn_to_list(async append => {
		for await(let a of list) await append(a);
		for await(let _ of rest) throw_syntax_error();
	});
};

const string_to_utf8 = async string => numbers_to_buffer(list_concat(list_map(a => {
	a = a.codePointAt();
	if(a < 0x80) return [a];
	const bytes = [];
	for(let i = a, j = a = Math.floor(Math.log2(a) / 6) + 1; j; i >>= 6, j -= 1) bytes.push(0x80 | i & 0x3f);
	bytes.reverse();
	bytes[0] |= ~(1 << 8 - a) + 1;
	return bytes;
}, Object(string))));

const big_int_to_uint = fn_bind(fn_to_list, async (append, big_int) => {
	const write = byte => append(numbers_to_buffer([Number(byte)]));
	big_int = BigInt(big_int);
	if(big_int < 0n) throw new RangeError("Signed");
	let last;
	for(; ; ){
		last = big_int & 0x7fn;
		big_int >>= 7n;
		if(!big_int) break;
		await write(last | 0x80n);
	}
	await write(last);
});

const buffer_to_binary = buffer => string_concat(list_map(String.fromCodePoint, new Uint8Array(buffer)));

const list_normalize = async list => {
	if(await is_list(list)) return list_map(list_normalize, list);
	if(is_buffer(list)) return buffer_to_binary(list);
	if(is_string(list)) return buffer_to_binary(await string_to_utf8(list));
	if(is_big_int(list)) return list_map(buffer_to_binary, big_int_to_uint(list));
	return list;
};

const reflexion = (extension = value) => {
	const list_enum = (list0, exit0) => {
		list0 = capture(async () => list_clone(await list0))();
		return async method => {
			switch(method){
				case "done":
				return [exit0, false];
				case "next":
				return [exit0, list0];
				case "enter":
				if(await is_list(await list0)) return [async method => {
					if(method === "exit") return [exit0];
					const [value, list] = await list_next(await list0);
					const exit = fn_bind(defer, capture(defer(async () => (await list_enum(list || [], exit0)("enter"))[0])));
					if(!list) switch(method){
						case "done":
						return [exit, !list];
						case "next":
						return [exit, Symbol()];
					}
					return list_enum(value, exit)(method);
				}];
			}
			return [];
		};
	};
	const node = (node0, free = () => {}) => {
		const init = () => {
			node1.values_count = values_count;
			node1.children_count = children_count;
			sweep();
		};
		const count = node => node.values_count + node.children_count;
		const check = async () => {
			if(!count(node1)) await free();
		};
		const create_child = (children, key, child = {methods: node1.methods}) => {
			child = node(child, async () => {
				await children_lock("readonly", () => children_count && children.black.add(key));
				children.white.delete(key);
				node1.children_count -= 1n;
				await check();
			});
			children.white.set(key, child);
			return child;
		};
		const sweep = fn_bind(defer, async () => {
			if(!(await list_to_array([
				values_lock("readonly", () => values_count || values.black.clear()),
				children_lock("readonly", () => children_count || branches.forEach(({black}) => black.clear())),
			])).some(value)) node0 = null;
		});
		const has = value => {
			if(values.black.has(value)) return false;
			if(values.white.has(value)) return true;
			return atom_lock(values_lock, 1, async mode => {
				if(!values_count) return false;
				await mode(0);
				if(!await node0.has(value)){
					values.black.add(value);
					return false;
				}
				values.white.add(value);
				values_count -= 1n;
				sweep();
				return true;
			});
		};
		const child = (method, key) => {
			const children = branches.get(method);
			if(children.black.has(key)) return null;
			if(children.white.has(key)) return children.white.get(key);
			return atom_lock(children_lock, 1, async mode => {
				if(!children_count) return null;
				await mode(0);
				const child0 = await node0.child(method, key);
				if(child0){
					const child = create_child(children, key, child0);
					children_count -= 1n;
					sweep();
					return child;
				}
				children.black.add(key);
				return child;
			});
		};
		const node1 = {};
		node1.methods = [...node0.methods];
		node1.mutex = mutex();
		node1.add = fn_bind(node1.mutex, 1, async (cursor, value) => {
			cursor = capture(defer(list_next, cursor));
			const [async0, resolve] = async();
			await lock0("readwrite", async () => {
				try{
					const [branch] = [, cursor] = await cursor;
					if(!cursor) return resolve(values_mutex(1, values.threads, value, async () => {
						if(await has(value)) return;
						node1.values_count += 1n;
						values.white.add(value);
					}));
					const [method, key] = await list_to_array(await branch);
					const children = branches.get(method);
					resolve(children_mutex(1, children.threads, key, async () => {
						let child0 = await child(method, key);
						if(!child0){
							node1.children_count += 1n;
							child0 = create_child(children, key);
						}
						const count0 = count(child0);
						const async = child0.add(cursor, value);
						if(!count0) await async;
					}));
				}catch(error){
					resolve();
					await check();
					throw error;
				}
			});
			await async0;
		});
		node1.delete = fn_bind(node1.mutex, 1, async (cursor, value) => {
			cursor = capture(defer(list_next, cursor));
			const [async0, resolve] = async();
			await lock0("readwrite", async () => {
				const [branch] = [, cursor] = await cursor;
				if(!cursor) resolve(values_mutex(1, values.threads, value, async () => {
					if(!await has(value)) return;
					await values_lock("readonly", () => values_count && values.black.add(value));
					values.white.delete(value);
					node1.values_count -= 1n;
					await check();
				}));
				const [method, key] = await list_to_array(await branch);
				resolve(children_mutex(1, branches.get(method).threads, key, async () => {
					const child0 = await child(method, key);
					if(!child0) return;
					const count0 = count(child0);
					const async = child0.delete(cursor, value);
					if(count0 <= 1n) await async;
				}));
			});
			await async0;
		});
		node1.has = fn_bind(node1.mutex, 0, async value => {
			const [async0, resolve] = async();
			await lock0("readonly", () => resolve(values_mutex(0, values.threads, value, has, value)));
			return async0;
		}),
		node1.child = fn_bind(node1.mutex, 0, async (method, key) => {
			const [async0, resolve] = async();
			await lock0("readonly", () => resolve(children_mutex(0, branches.get(method).threads, key, child, method, key)));
			return async0;
		});
		node1.for_each = fn_bind(node1.mutex, 0, (cursor0 = placeholder, fn) => {
			if(cursor0 === placeholder) return (async () => {
				const [async0, ...returns] = async();
				await lock0("readonly", () => {
					values_mutex(2, async () => {
						await atom_lock(values_lock, 1, async mode => {
							if(!values_count) return;
							await mode(0);
							const [async0, ...returns] = async();
							node0.for_each(entry, value => {
								try{
									if(!values.black.has(value)) values.white.add(value);
								}catch(error){
									returns[0]((async () => {
										throw error;
									})());
								}
							}).then(...returns);
							await async0;
							values_count = 0n;
							sweep();
						});
						returns[0](defer(list_to_array, list_map(value => dispatch(call(fn, value)), [...values.white])));
					}).then(...returns);
				});
				await async0;
			})();
			lock0("readonly", () => {
				children_mutex(2, list_to_array, list_map(method => dispatch((async () => {
					const [cursor, key] = await cursor0(method);
					const child0 = await child(method, await key);
					if(child0) child0.for_each(cursor, fn);
				})()), node1.methods));
			});
		});
		const placeholder = Symbol();
		let values_count = 0n;
		let children_count = 0n;
		const [lock0, values_lock, children_lock] = gen(lock);
		const [values_mutex, children_mutex] = gen(mutex);
		const values = {};
		values.threads = schedulers(thread);
		[values.black, values.white] = gen(() => new Set);
		const branches = new Map(node1.methods.map(method => [method, {
			threads: schedulers(thread),
			black: new Set,
			white: new Map,
		}]));
		if(node0.mutex){
			const thunk0 = thunk((async () => {
				const [async0, resolve] = async();
				node0.mutex(2, async () => {
					resolve();
					await hang;
				});
				await async0;
				({values_count, children_count} = node0);
				init();
			})());
			lock0("readwrite", thunk0);
			node1.mutex(1, thunk0);
		}else{
			init();
		}
		return node1;
	};
	const reflexion = (ref0 = node({methods: [
		"done",
		"enter",
		"exit",
		"next",
	]}), shared) => {
		const assert_not_closed = () => {
			if(closed) throw new TypeError("Closed");
		};
		const create_reflexion = (commit, emit, emit_all, close) => {
			const reflexion = {};
			[
				["on", "add"],
				["off", "delete"],
			].forEach(macros => reflexion[macros[0]] = fn_bind(commit, async (ref, wildcard, pattern, reflex) => ref[macros[1]](fn_to_list(async append => {
				const [begin, end] = gen(Symbol);
				let matching;
				for await(let a of list_flatten(begin, end, pattern)){
					a = await a;
					if(matching){
						matching = false;
						if(a === end){
							await append(["exit"]);
							continue;
						}
						await append(["done", false]);
					}
					if(equal(a, wildcard)){
						matching = true;
						continue;
					}
					for(let branch of
						a === begin ? [["enter"]]
						: a === end ? [
							["done", true],
							["exit"],
						]
						: [["next", a]]
					) await append(branch);
				}
				if(matching) await append(["done", false]);
			}), reflex)));
			reflexion.emit = fn_bind(emit, emit = message => {
				message = defer(list_clone, message);
				ref0.for_each(list_enum(message), async reflex => reflex(reflexion, await message));
			});
			reflexion.emit_all = fn_bind(emit_all, async messages => {
				for await(let message of messages) (async () => emit(await message))();
			});
			reflexion.close = close;
			return extension(reflexion);
		};
		let closed;
		const lock0 = lock();
		return create_reflexion(
			async (commit, ...args) => {
				await lock0("readonly", () => {
					assert_not_closed();
					shared = true;
				});
				const ref = node(ref0);
				commit(ref, ...args);
				return reflexion(ref);
			},
			fn_bind(lock0, "readonly", (...args) => {
				assert_not_closed();
				(shared ? defer : call)(...args);
			}),
			fn_bind(lock0, "readonly", (...args) => {
				assert_not_closed();
				shared = true;
				defer(...args);
			}),
			fn_bind(lock0, "readwrite", async () => {
				const assert_not_used = () => {
					if(used) assert_not_closed();
				};
				if(closed) return;
				if(shared) return reflexion(ref0, shared);
				let used;
				const lock0 = lock();
				closed = true;
				return create_reflexion(
					async (commit, ...args) => {
						await lock0("readwrite", async () => {
							assert_not_used();
							used = true;
						});
						commit(ref0, ...args);
						return reflexion(ref0, shared);
					},
					fn_bind(lock0, "readonly", (...args) => {
						assert_not_used();
						(shared ? defer : call)(...args);
					}),
					fn_bind(lock0, "readonly", (...args) => {
						assert_not_used();
						shared = true;
						defer(...args);
					}),
					() => {},
				);
			}),
		);
	};
	return reflexion();
};

const buffer_random = () => numbers_to_buffer([Math.random() * 0x100]);

const bin2buffer = binary => new Uint8Array(Array.from(binary).map(a => a.codePointAt())).buffer;

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
		catch_all(() => results = [...f(effect)]);
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
		
		[defn _ [_ call* $0 $0] call [$1] $0 match [$2] [[$3] $3]
			[quote [[$4]] $3]
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
		
		[defn _ [_ lambda $0 $0 $0] call [$0 $2] [escape $1] match [$3] [[$4 $4 $4] [$4]]
			[match [[$6] $5] [[$8] $4]
				[$7 $8]
			]
		]
		
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
		
		[defn _ [_ list ? $0 $0] call [$1] [match? $0 [$2]] match [$2] [[$3] $3]
			[quote [$4] $3]
		]
		
		[defn _ [_ list concat [$0] [$0] $0] quote [$0 $1] $2]
		
		[defn _ [_ list map [$0] [] $0] quote [] $1]
		
		[defn _ [_ list map [$0] [$0 $0] $0] eval [list concat [call* [$0 $1]] [call* [list map [$0] [$2]]]] $3]
	`);
	
	vm.on("defer", (...signal) => Promise.resolve().then(() => run(() => vm.emit(...signal))));
	
	defop(1, "list", "length", list => Array.isArray(list) && [num2uint(list.length)]);
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
				[quote [$3]]
			] $4]
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
			if(options.utf8_to_str) catch_all(() => a = utf8_to_str(buffer));
			if(!is_str(a)) return "%" + buffer2hex(buffer);
		}
		if(!is_str(a)) throw TypeError("Unsupported");
		if(is_param(a)){
			let literal;
			catch_all(() => literal = str2num(a.length.toString()) === a.length && "$" + a.length);
			if(literal) return literal;
		}
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
		catch_all(() => message = signals2code({utf8_to_str: true})(...message));
		if(is_str(message)) log(message);
	});
	return vm;
};

({vm_min, vm_std, vm_common});

/*test*
var vm = vm_common(message => console.log("log: " + message));
if(1) vm.exec(`
	[defn _ [_ echo $0 $0] start
		[log $0]
		[quote [$0] $1]
	]
`);
if(0) vm.on((...signal) => console.log("signal: " + signals2code({utf8_to_str: true})(...signal)));
//*/
