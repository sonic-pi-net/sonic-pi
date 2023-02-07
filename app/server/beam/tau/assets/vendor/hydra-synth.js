// https://cdn.jsdelivr.net/npm/hydra-synth@1.3.24/dist/hydra-synth.js

(function (f) {
  if (typeof exports === "object" && typeof module !== "undefined") {
    module.exports = f();
  } else if (typeof define === "function" && define.amd) {
    define([], f);
  } else {
    var g;
    if (typeof window !== "undefined") {
      g = window;
    } else if (typeof global !== "undefined") {
      g = global;
    } else if (typeof self !== "undefined") {
      g = self;
    } else {
      g = this;
    }
    g.Hydra = f();
  }
})(function () {
  var define, module, exports;
  return (function () {
    function r(e, n, t) {
      function o(i, f) {
        if (!n[i]) {
          if (!e[i]) {
            var c = "function" == typeof require && require;
            if (!f && c) return c(i, !0);
            if (u) return u(i, !0);
            var a = new Error("Cannot find module '" + i + "'");
            throw ((a.code = "MODULE_NOT_FOUND"), a);
          }
          var p = (n[i] = { exports: {} });
          e[i][0].call(
            p.exports,
            function (r) {
              var n = e[i][1][r];
              return o(n || r);
            },
            p,
            p.exports,
            r,
            e,
            n,
            t
          );
        }
        return n[i].exports;
      }
      for (
        var u = "function" == typeof require && require, i = 0;
        i < t.length;
        i++
      )
        o(t[i]);
      return o;
    }
    return r;
  })()(
    {
      1: [
        function (require, module, exports) {
          // Copyright Joyent, Inc. and other Node contributors.
          //
          // Permission is hereby granted, free of charge, to any person obtaining a
          // copy of this software and associated documentation files (the
          // "Software"), to deal in the Software without restriction, including
          // without limitation the rights to use, copy, modify, merge, publish,
          // distribute, sublicense, and/or sell copies of the Software, and to permit
          // persons to whom the Software is furnished to do so, subject to the
          // following conditions:
          //
          // The above copyright notice and this permission notice shall be included
          // in all copies or substantial portions of the Software.
          //
          // THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
          // OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
          // MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
          // NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
          // DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
          // OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
          // USE OR OTHER DEALINGS IN THE SOFTWARE.

          "use strict";

          var R = typeof Reflect === "object" ? Reflect : null;
          var ReflectApply =
            R && typeof R.apply === "function"
              ? R.apply
              : function ReflectApply(target, receiver, args) {
                  return Function.prototype.apply.call(target, receiver, args);
                };

          var ReflectOwnKeys;
          if (R && typeof R.ownKeys === "function") {
            ReflectOwnKeys = R.ownKeys;
          } else if (Object.getOwnPropertySymbols) {
            ReflectOwnKeys = function ReflectOwnKeys(target) {
              return Object.getOwnPropertyNames(target).concat(
                Object.getOwnPropertySymbols(target)
              );
            };
          } else {
            ReflectOwnKeys = function ReflectOwnKeys(target) {
              return Object.getOwnPropertyNames(target);
            };
          }

          function ProcessEmitWarning(warning) {
            if (console && console.warn) console.warn(warning);
          }

          var NumberIsNaN =
            Number.isNaN ||
            function NumberIsNaN(value) {
              return value !== value;
            };

          function EventEmitter() {
            EventEmitter.init.call(this);
          }
          module.exports = EventEmitter;
          module.exports.once = once;

          // Backwards-compat with node 0.10.x
          EventEmitter.EventEmitter = EventEmitter;

          EventEmitter.prototype._events = undefined;
          EventEmitter.prototype._eventsCount = 0;
          EventEmitter.prototype._maxListeners = undefined;

          // By default EventEmitters will print a warning if more than 10 listeners are
          // added to it. This is a useful default which helps finding memory leaks.
          var defaultMaxListeners = 10;

          function checkListener(listener) {
            if (typeof listener !== "function") {
              throw new TypeError(
                'The "listener" argument must be of type Function. Received type ' +
                  typeof listener
              );
            }
          }

          Object.defineProperty(EventEmitter, "defaultMaxListeners", {
            enumerable: true,
            get: function () {
              return defaultMaxListeners;
            },
            set: function (arg) {
              if (typeof arg !== "number" || arg < 0 || NumberIsNaN(arg)) {
                throw new RangeError(
                  'The value of "defaultMaxListeners" is out of range. It must be a non-negative number. Received ' +
                    arg +
                    "."
                );
              }
              defaultMaxListeners = arg;
            },
          });

          EventEmitter.init = function () {
            if (
              this._events === undefined ||
              this._events === Object.getPrototypeOf(this)._events
            ) {
              this._events = Object.create(null);
              this._eventsCount = 0;
            }

            this._maxListeners = this._maxListeners || undefined;
          };

          // Obviously not all Emitters should be limited to 10. This function allows
          // that to be increased. Set to zero for unlimited.
          EventEmitter.prototype.setMaxListeners = function setMaxListeners(n) {
            if (typeof n !== "number" || n < 0 || NumberIsNaN(n)) {
              throw new RangeError(
                'The value of "n" is out of range. It must be a non-negative number. Received ' +
                  n +
                  "."
              );
            }
            this._maxListeners = n;
            return this;
          };

          function _getMaxListeners(that) {
            if (that._maxListeners === undefined)
              return EventEmitter.defaultMaxListeners;
            return that._maxListeners;
          }

          EventEmitter.prototype.getMaxListeners = function getMaxListeners() {
            return _getMaxListeners(this);
          };

          EventEmitter.prototype.emit = function emit(type) {
            var args = [];
            for (var i = 1; i < arguments.length; i++) args.push(arguments[i]);
            var doError = type === "error";

            var events = this._events;
            if (events !== undefined)
              doError = doError && events.error === undefined;
            else if (!doError) return false;

            // If there is no 'error' event listener then throw.
            if (doError) {
              var er;
              if (args.length > 0) er = args[0];
              if (er instanceof Error) {
                // Note: The comments on the `throw` lines are intentional, they show
                // up in Node's output if this results in an unhandled exception.
                throw er; // Unhandled 'error' event
              }
              // At least give some kind of context to the user
              var err = new Error(
                "Unhandled error." + (er ? " (" + er.message + ")" : "")
              );
              err.context = er;
              throw err; // Unhandled 'error' event
            }

            var handler = events[type];

            if (handler === undefined) return false;

            if (typeof handler === "function") {
              ReflectApply(handler, this, args);
            } else {
              var len = handler.length;
              var listeners = arrayClone(handler, len);
              for (var i = 0; i < len; ++i)
                ReflectApply(listeners[i], this, args);
            }

            return true;
          };

          function _addListener(target, type, listener, prepend) {
            var m;
            var events;
            var existing;

            checkListener(listener);

            events = target._events;
            if (events === undefined) {
              events = target._events = Object.create(null);
              target._eventsCount = 0;
            } else {
              // To avoid recursion in the case that type === "newListener"! Before
              // adding it to the listeners, first emit "newListener".
              if (events.newListener !== undefined) {
                target.emit(
                  "newListener",
                  type,
                  listener.listener ? listener.listener : listener
                );

                // Re-assign `events` because a newListener handler could have caused the
                // this._events to be assigned to a new object
                events = target._events;
              }
              existing = events[type];
            }

            if (existing === undefined) {
              // Optimize the case of one listener. Don't need the extra array object.
              existing = events[type] = listener;
              ++target._eventsCount;
            } else {
              if (typeof existing === "function") {
                // Adding the second element, need to change to array.
                existing = events[type] = prepend
                  ? [listener, existing]
                  : [existing, listener];
                // If we've already got an array, just append.
              } else if (prepend) {
                existing.unshift(listener);
              } else {
                existing.push(listener);
              }

              // Check for listener leak
              m = _getMaxListeners(target);
              if (m > 0 && existing.length > m && !existing.warned) {
                existing.warned = true;
                // No error code for this since it is a Warning
                // eslint-disable-next-line no-restricted-syntax
                var w = new Error(
                  "Possible EventEmitter memory leak detected. " +
                    existing.length +
                    " " +
                    String(type) +
                    " listeners " +
                    "added. Use emitter.setMaxListeners() to " +
                    "increase limit"
                );
                w.name = "MaxListenersExceededWarning";
                w.emitter = target;
                w.type = type;
                w.count = existing.length;
                ProcessEmitWarning(w);
              }
            }

            return target;
          }

          EventEmitter.prototype.addListener = function addListener(
            type,
            listener
          ) {
            return _addListener(this, type, listener, false);
          };

          EventEmitter.prototype.on = EventEmitter.prototype.addListener;

          EventEmitter.prototype.prependListener = function prependListener(
            type,
            listener
          ) {
            return _addListener(this, type, listener, true);
          };

          function onceWrapper() {
            if (!this.fired) {
              this.target.removeListener(this.type, this.wrapFn);
              this.fired = true;
              if (arguments.length === 0)
                return this.listener.call(this.target);
              return this.listener.apply(this.target, arguments);
            }
          }

          function _onceWrap(target, type, listener) {
            var state = {
              fired: false,
              wrapFn: undefined,
              target: target,
              type: type,
              listener: listener,
            };
            var wrapped = onceWrapper.bind(state);
            wrapped.listener = listener;
            state.wrapFn = wrapped;
            return wrapped;
          }

          EventEmitter.prototype.once = function once(type, listener) {
            checkListener(listener);
            this.on(type, _onceWrap(this, type, listener));
            return this;
          };

          EventEmitter.prototype.prependOnceListener =
            function prependOnceListener(type, listener) {
              checkListener(listener);
              this.prependListener(type, _onceWrap(this, type, listener));
              return this;
            };

          // Emits a 'removeListener' event if and only if the listener was removed.
          EventEmitter.prototype.removeListener = function removeListener(
            type,
            listener
          ) {
            var list, events, position, i, originalListener;

            checkListener(listener);

            events = this._events;
            if (events === undefined) return this;

            list = events[type];
            if (list === undefined) return this;

            if (list === listener || list.listener === listener) {
              if (--this._eventsCount === 0) this._events = Object.create(null);
              else {
                delete events[type];
                if (events.removeListener)
                  this.emit("removeListener", type, list.listener || listener);
              }
            } else if (typeof list !== "function") {
              position = -1;

              for (i = list.length - 1; i >= 0; i--) {
                if (list[i] === listener || list[i].listener === listener) {
                  originalListener = list[i].listener;
                  position = i;
                  break;
                }
              }

              if (position < 0) return this;

              if (position === 0) list.shift();
              else {
                spliceOne(list, position);
              }

              if (list.length === 1) events[type] = list[0];

              if (events.removeListener !== undefined)
                this.emit("removeListener", type, originalListener || listener);
            }

            return this;
          };

          EventEmitter.prototype.off = EventEmitter.prototype.removeListener;

          EventEmitter.prototype.removeAllListeners =
            function removeAllListeners(type) {
              var listeners, events, i;

              events = this._events;
              if (events === undefined) return this;

              // not listening for removeListener, no need to emit
              if (events.removeListener === undefined) {
                if (arguments.length === 0) {
                  this._events = Object.create(null);
                  this._eventsCount = 0;
                } else if (events[type] !== undefined) {
                  if (--this._eventsCount === 0)
                    this._events = Object.create(null);
                  else delete events[type];
                }
                return this;
              }

              // emit removeListener for all listeners on all events
              if (arguments.length === 0) {
                var keys = Object.keys(events);
                var key;
                for (i = 0; i < keys.length; ++i) {
                  key = keys[i];
                  if (key === "removeListener") continue;
                  this.removeAllListeners(key);
                }
                this.removeAllListeners("removeListener");
                this._events = Object.create(null);
                this._eventsCount = 0;
                return this;
              }

              listeners = events[type];

              if (typeof listeners === "function") {
                this.removeListener(type, listeners);
              } else if (listeners !== undefined) {
                // LIFO order
                for (i = listeners.length - 1; i >= 0; i--) {
                  this.removeListener(type, listeners[i]);
                }
              }

              return this;
            };

          function _listeners(target, type, unwrap) {
            var events = target._events;

            if (events === undefined) return [];

            var evlistener = events[type];
            if (evlistener === undefined) return [];

            if (typeof evlistener === "function")
              return unwrap
                ? [evlistener.listener || evlistener]
                : [evlistener];

            return unwrap
              ? unwrapListeners(evlistener)
              : arrayClone(evlistener, evlistener.length);
          }

          EventEmitter.prototype.listeners = function listeners(type) {
            return _listeners(this, type, true);
          };

          EventEmitter.prototype.rawListeners = function rawListeners(type) {
            return _listeners(this, type, false);
          };

          EventEmitter.listenerCount = function (emitter, type) {
            if (typeof emitter.listenerCount === "function") {
              return emitter.listenerCount(type);
            } else {
              return listenerCount.call(emitter, type);
            }
          };

          EventEmitter.prototype.listenerCount = listenerCount;
          function listenerCount(type) {
            var events = this._events;

            if (events !== undefined) {
              var evlistener = events[type];

              if (typeof evlistener === "function") {
                return 1;
              } else if (evlistener !== undefined) {
                return evlistener.length;
              }
            }

            return 0;
          }

          EventEmitter.prototype.eventNames = function eventNames() {
            return this._eventsCount > 0 ? ReflectOwnKeys(this._events) : [];
          };

          function arrayClone(arr, n) {
            var copy = new Array(n);
            for (var i = 0; i < n; ++i) copy[i] = arr[i];
            return copy;
          }

          function spliceOne(list, index) {
            for (; index + 1 < list.length; index++)
              list[index] = list[index + 1];
            list.pop();
          }

          function unwrapListeners(arr) {
            var ret = new Array(arr.length);
            for (var i = 0; i < ret.length; ++i) {
              ret[i] = arr[i].listener || arr[i];
            }
            return ret;
          }

          function once(emitter, name) {
            return new Promise(function (resolve, reject) {
              function errorListener(err) {
                emitter.removeListener(name, resolver);
                reject(err);
              }

              function resolver() {
                if (typeof emitter.removeListener === "function") {
                  emitter.removeListener("error", errorListener);
                }
                resolve([].slice.call(arguments));
              }

              eventTargetAgnosticAddListener(emitter, name, resolver, {
                once: true,
              });
              if (name !== "error") {
                addErrorHandlerIfEventEmitter(emitter, errorListener, {
                  once: true,
                });
              }
            });
          }

          function addErrorHandlerIfEventEmitter(emitter, handler, flags) {
            if (typeof emitter.on === "function") {
              eventTargetAgnosticAddListener(emitter, "error", handler, flags);
            }
          }

          function eventTargetAgnosticAddListener(
            emitter,
            name,
            listener,
            flags
          ) {
            if (typeof emitter.on === "function") {
              if (flags.once) {
                emitter.once(name, listener);
              } else {
                emitter.on(name, listener);
              }
            } else if (typeof emitter.addEventListener === "function") {
              // EventTarget does not have `error` event semantics like Node
              // EventEmitters, we do not listen for `error` events here.
              emitter.addEventListener(name, function wrapListener(arg) {
                // IE does not have builtin `{ once: true }` support so we
                // have to do it manually.
                if (flags.once) {
                  emitter.removeEventListener(name, wrapListener);
                }
                listener(arg);
              });
            } else {
              throw new TypeError(
                'The "emitter" argument must be of type EventEmitter. Received type ' +
                  typeof emitter
              );
            }
          }
        },
        {},
      ],
      2: [
        function (require, module, exports) {
          if (typeof Object.create === "function") {
            // implementation from standard node.js 'util' module
            module.exports = function inherits(ctor, superCtor) {
              if (superCtor) {
                ctor.super_ = superCtor;
                ctor.prototype = Object.create(superCtor.prototype, {
                  constructor: {
                    value: ctor,
                    enumerable: false,
                    writable: true,
                    configurable: true,
                  },
                });
              }
            };
          } else {
            // old school shim for old browsers
            module.exports = function inherits(ctor, superCtor) {
              if (superCtor) {
                ctor.super_ = superCtor;
                var TempCtor = function () {};
                TempCtor.prototype = superCtor.prototype;
                ctor.prototype = new TempCtor();
                ctor.prototype.constructor = ctor;
              }
            };
          }
        },
        {},
      ],
      3: [
        function (require, module, exports) {
          !(function (r, t) {
            "object" == typeof exports && "undefined" != typeof module
              ? (module.exports = t())
              : "function" == typeof define && define.amd
              ? define(t)
              : ((r =
                  "undefined" != typeof globalThis
                    ? globalThis
                    : r || self).Meyda = t());
          })(this, function () {
            "use strict";
            function r(r, t, e) {
              if (e || 2 === arguments.length)
                for (var a, n = 0, o = t.length; n < o; n++)
                  (!a && n in t) ||
                    (a || (a = Array.prototype.slice.call(t, 0, n)),
                    (a[n] = t[n]));
              return r.concat(a || Array.prototype.slice.call(t));
            }
            var t = Object.freeze({
                __proto__: null,
                blackman: function (r) {
                  for (
                    var t = new Float32Array(r),
                      e = (2 * Math.PI) / (r - 1),
                      a = 2 * e,
                      n = 0;
                    n < r / 2;
                    n++
                  )
                    t[n] =
                      0.42 - 0.5 * Math.cos(n * e) + 0.08 * Math.cos(n * a);
                  for (n = Math.ceil(r / 2); n > 0; n--) t[r - n] = t[n - 1];
                  return t;
                },
                sine: function (r) {
                  for (
                    var t = Math.PI / (r - 1), e = new Float32Array(r), a = 0;
                    a < r;
                    a++
                  )
                    e[a] = Math.sin(t * a);
                  return e;
                },
                hanning: function (r) {
                  for (var t = new Float32Array(r), e = 0; e < r; e++)
                    t[e] = 0.5 - 0.5 * Math.cos((2 * Math.PI * e) / (r - 1));
                  return t;
                },
                hamming: function (r) {
                  for (var t = new Float32Array(r), e = 0; e < r; e++)
                    t[e] = 0.54 - 0.46 * Math.cos(2 * Math.PI * (e / r - 1));
                  return t;
                },
              }),
              e = {};
            function a(r) {
              for (; r % 2 == 0 && r > 1; ) r /= 2;
              return 1 === r;
            }
            function n(r, a) {
              if ("rect" !== a) {
                if (
                  (("" !== a && a) || (a = "hanning"),
                  e[a] || (e[a] = {}),
                  !e[a][r.length])
                )
                  try {
                    e[a][r.length] = t[a](r.length);
                  } catch (r) {
                    throw new Error("Invalid windowing function");
                  }
                r = (function (r, t) {
                  for (var e = [], a = 0; a < Math.min(r.length, t.length); a++)
                    e[a] = r[a] * t[a];
                  return e;
                })(r, e[a][r.length]);
              }
              return r;
            }
            function o(r, t, e) {
              for (var a = new Float32Array(r), n = 0; n < a.length; n++)
                (a[n] = (n * t) / e),
                  (a[n] =
                    13 * Math.atan(a[n] / 1315.8) +
                    3.5 * Math.atan(Math.pow(a[n] / 7518, 2)));
              return a;
            }
            function i(r) {
              return Float32Array.from(r);
            }
            function u(r) {
              return 1125 * Math.log(1 + r / 700);
            }
            function f(r, t, e) {
              for (
                var a,
                  n = new Float32Array(r + 2),
                  o = new Float32Array(r + 2),
                  i = t / 2,
                  f = u(0),
                  c = (u(i) - f) / (r + 1),
                  l = new Array(r + 2),
                  s = 0;
                s < n.length;
                s++
              )
                (n[s] = s * c),
                  (o[s] = ((a = n[s]), 700 * (Math.exp(a / 1125) - 1))),
                  (l[s] = Math.floor(((e + 1) * o[s]) / t));
              for (var m = new Array(r), p = 0; p < m.length; p++) {
                m[p] = new Array(e / 2 + 1).fill(0);
                for (s = l[p]; s < l[p + 1]; s++)
                  m[p][s] = (s - l[p]) / (l[p + 1] - l[p]);
                for (s = l[p + 1]; s < l[p + 2]; s++)
                  m[p][s] = (l[p + 2] - s) / (l[p + 2] - l[p + 1]);
              }
              return m;
            }
            function c(t, e, a, n, o, i, u) {
              void 0 === n && (n = 5),
                void 0 === o && (o = 2),
                void 0 === i && (i = !0),
                void 0 === u && (u = 440);
              var f = Math.floor(a / 2) + 1,
                c = new Array(a).fill(0).map(function (r, n) {
                  return (
                    t *
                    (function (r, t) {
                      return Math.log2((16 * r) / t);
                    })((e * n) / a, u)
                  );
                });
              c[0] = c[1] - 1.5 * t;
              var l,
                s,
                m,
                p = c
                  .slice(1)
                  .map(function (r, t) {
                    return Math.max(r - c[t]);
                  }, 1)
                  .concat([1]),
                h = Math.round(t / 2),
                g = new Array(t).fill(0).map(function (r, e) {
                  return c.map(function (r) {
                    return ((10 * t + h + r - e) % t) - h;
                  });
                }),
                w = g.map(function (r, t) {
                  return r.map(function (r, e) {
                    return Math.exp(-0.5 * Math.pow((2 * g[t][e]) / p[e], 2));
                  });
                });
              if (
                ((s = (l = w)[0].map(function () {
                  return 0;
                })),
                (m = l
                  .reduce(function (r, t) {
                    return (
                      t.forEach(function (t, e) {
                        r[e] += Math.pow(t, 2);
                      }),
                      r
                    );
                  }, s)
                  .map(Math.sqrt)),
                (w = l.map(function (r, t) {
                  return r.map(function (r, t) {
                    return r / (m[t] || 1);
                  });
                })),
                o)
              ) {
                var v = c.map(function (r) {
                  return Math.exp(-0.5 * Math.pow((r / t - n) / o, 2));
                });
                w = w.map(function (r) {
                  return r.map(function (r, t) {
                    return r * v[t];
                  });
                });
              }
              return (
                i && (w = r(r([], w.slice(3), !0), w.slice(0, 3), !0)),
                w.map(function (r) {
                  return r.slice(0, f);
                })
              );
            }
            function l(r, t) {
              for (var e = 0, a = 0, n = 0; n < t.length; n++)
                (e += Math.pow(n, r) * Math.abs(t[n])), (a += t[n]);
              return e / a;
            }
            function s(r) {
              var t = r.ampSpectrum,
                e = r.barkScale,
                a = r.numberOfBarkBands,
                n = void 0 === a ? 24 : a;
              if ("object" != typeof t || "object" != typeof e)
                throw new TypeError();
              var o = n,
                i = new Float32Array(o),
                u = 0,
                f = t,
                c = new Int32Array(o + 1);
              c[0] = 0;
              for (var l = e[f.length - 1] / o, s = 1, m = 0; m < f.length; m++)
                for (; e[m] > l; )
                  (c[s++] = m), (l = (s * e[f.length - 1]) / o);
              c[o] = f.length - 1;
              for (m = 0; m < o; m++) {
                for (var p = 0, h = c[m]; h < c[m + 1]; h++) p += f[h];
                i[m] = Math.pow(p, 0.23);
              }
              for (m = 0; m < i.length; m++) u += i[m];
              return { specific: i, total: u };
            }
            function m(r) {
              var t = r.ampSpectrum;
              if ("object" != typeof t) throw new TypeError();
              for (var e = new Float32Array(t.length), a = 0; a < e.length; a++)
                e[a] = Math.pow(t[a], 2);
              return e;
            }
            function p(r) {
              var t = r.ampSpectrum,
                e = r.melFilterBank,
                a = r.bufferSize;
              if ("object" != typeof t)
                throw new TypeError(
                  "Valid ampSpectrum is required to generate melBands"
                );
              if ("object" != typeof e)
                throw new TypeError(
                  "Valid melFilterBank is required to generate melBands"
                );
              for (
                var n = m({ ampSpectrum: t }),
                  o = e.length,
                  i = Array(o),
                  u = new Float32Array(o),
                  f = 0;
                f < u.length;
                f++
              ) {
                (i[f] = new Float32Array(a / 2)), (u[f] = 0);
                for (var c = 0; c < a / 2; c++)
                  (i[f][c] = e[f][c] * n[c]), (u[f] += i[f][c]);
                u[f] = Math.log(u[f] + 1);
              }
              return Array.prototype.slice.call(u);
            }
            function h(r) {
              return r &&
                r.__esModule &&
                Object.prototype.hasOwnProperty.call(r, "default")
                ? r.default
                : r;
            }
            var g = { exports: {} },
              w = null;
            var v = function (r, t) {
              var e = r.length;
              return (
                (t = t || 2),
                (w && w[e]) ||
                  (function (r) {
                    (w = w || {})[r] = new Array(r * r);
                    for (var t = Math.PI / r, e = 0; e < r; e++)
                      for (var a = 0; a < r; a++)
                        w[r][a + e * r] = Math.cos(t * (a + 0.5) * e);
                  })(e),
                r
                  .map(function () {
                    return 0;
                  })
                  .map(function (a, n) {
                    return (
                      t *
                      r.reduce(function (r, t, a, o) {
                        return r + t * w[e][a + n * e];
                      }, 0)
                    );
                  })
              );
            };
            !(function (r) {
              r.exports = v;
            })(g);
            var d = h(g.exports);
            var y = Object.freeze({
              __proto__: null,
              buffer: function (r) {
                return r.signal;
              },
              rms: function (r) {
                var t = r.signal;
                if ("object" != typeof t) throw new TypeError();
                for (var e = 0, a = 0; a < t.length; a++)
                  e += Math.pow(t[a], 2);
                return (e /= t.length), (e = Math.sqrt(e));
              },
              energy: function (r) {
                var t = r.signal;
                if ("object" != typeof t) throw new TypeError();
                for (var e = 0, a = 0; a < t.length; a++)
                  e += Math.pow(Math.abs(t[a]), 2);
                return e;
              },
              complexSpectrum: function (r) {
                return r.complexSpectrum;
              },
              spectralSlope: function (r) {
                var t = r.ampSpectrum,
                  e = r.sampleRate,
                  a = r.bufferSize;
                if ("object" != typeof t) throw new TypeError();
                for (
                  var n = 0,
                    o = 0,
                    i = new Float32Array(t.length),
                    u = 0,
                    f = 0,
                    c = 0;
                  c < t.length;
                  c++
                ) {
                  n += t[c];
                  var l = (c * e) / a;
                  (i[c] = l), (u += l * l), (o += l), (f += l * t[c]);
                }
                return (t.length * f - o * n) / (n * (u - Math.pow(o, 2)));
              },
              spectralCentroid: function (r) {
                var t = r.ampSpectrum;
                if ("object" != typeof t) throw new TypeError();
                return l(1, t);
              },
              spectralRolloff: function (r) {
                var t = r.ampSpectrum,
                  e = r.sampleRate;
                if ("object" != typeof t) throw new TypeError();
                for (
                  var a = t, n = e / (2 * (a.length - 1)), o = 0, i = 0;
                  i < a.length;
                  i++
                )
                  o += a[i];
                for (var u = 0.99 * o, f = a.length - 1; o > u && f >= 0; )
                  (o -= a[f]), --f;
                return (f + 1) * n;
              },
              spectralFlatness: function (r) {
                var t = r.ampSpectrum;
                if ("object" != typeof t) throw new TypeError();
                for (var e = 0, a = 0, n = 0; n < t.length; n++)
                  (e += Math.log(t[n])), (a += t[n]);
                return (Math.exp(e / t.length) * t.length) / a;
              },
              spectralSpread: function (r) {
                var t = r.ampSpectrum;
                if ("object" != typeof t) throw new TypeError();
                return Math.sqrt(l(2, t) - Math.pow(l(1, t), 2));
              },
              spectralSkewness: function (r) {
                var t = r.ampSpectrum;
                if ("object" != typeof t) throw new TypeError();
                var e = l(1, t),
                  a = l(2, t),
                  n = l(3, t);
                return (
                  (2 * Math.pow(e, 3) - 3 * e * a + n) /
                  Math.pow(Math.sqrt(a - Math.pow(e, 2)), 3)
                );
              },
              spectralKurtosis: function (r) {
                var t = r.ampSpectrum;
                if ("object" != typeof t) throw new TypeError();
                var e = t,
                  a = l(1, e),
                  n = l(2, e),
                  o = l(3, e),
                  i = l(4, e);
                return (
                  (-3 * Math.pow(a, 4) + 6 * a * n - 4 * a * o + i) /
                  Math.pow(Math.sqrt(n - Math.pow(a, 2)), 4)
                );
              },
              amplitudeSpectrum: function (r) {
                return r.ampSpectrum;
              },
              zcr: function (r) {
                var t = r.signal;
                if ("object" != typeof t) throw new TypeError();
                for (var e = 0, a = 1; a < t.length; a++)
                  ((t[a - 1] >= 0 && t[a] < 0) ||
                    (t[a - 1] < 0 && t[a] >= 0)) &&
                    e++;
                return e;
              },
              loudness: s,
              perceptualSpread: function (r) {
                for (
                  var t = s({
                      ampSpectrum: r.ampSpectrum,
                      barkScale: r.barkScale,
                    }),
                    e = 0,
                    a = 0;
                  a < t.specific.length;
                  a++
                )
                  t.specific[a] > e && (e = t.specific[a]);
                return Math.pow((t.total - e) / t.total, 2);
              },
              perceptualSharpness: function (r) {
                for (
                  var t = s({
                      ampSpectrum: r.ampSpectrum,
                      barkScale: r.barkScale,
                    }),
                    e = t.specific,
                    a = 0,
                    n = 0;
                  n < e.length;
                  n++
                )
                  a +=
                    n < 15
                      ? (n + 1) * e[n + 1]
                      : 0.066 * Math.exp(0.171 * (n + 1));
                return (a *= 0.11 / t.total);
              },
              powerSpectrum: m,
              mfcc: function (r) {
                var t = r.ampSpectrum,
                  e = r.melFilterBank,
                  a = r.numberOfMFCCCoefficients,
                  n = r.bufferSize,
                  o = Math.min(40, Math.max(1, a || 13));
                if (e.length < o)
                  throw new Error(
                    "Insufficient filter bank for requested number of coefficients"
                  );
                var i = p({ ampSpectrum: t, melFilterBank: e, bufferSize: n });
                return d(i).slice(0, o);
              },
              chroma: function (r) {
                var t = r.ampSpectrum,
                  e = r.chromaFilterBank;
                if ("object" != typeof t)
                  throw new TypeError(
                    "Valid ampSpectrum is required to generate chroma"
                  );
                if ("object" != typeof e)
                  throw new TypeError(
                    "Valid chromaFilterBank is required to generate chroma"
                  );
                var a = e.map(function (r, e) {
                    return t.reduce(function (t, e, a) {
                      return t + e * r[a];
                    }, 0);
                  }),
                  n = Math.max.apply(Math, a);
                return n
                  ? a.map(function (r) {
                      return r / n;
                    })
                  : a;
              },
              spectralFlux: function (r) {
                var t = r.signal,
                  e = r.previousSignal,
                  a = r.bufferSize;
                if ("object" != typeof t || "object" != typeof e)
                  throw new TypeError();
                for (var n = 0, o = -a / 2; o < t.length / 2 - 1; o++)
                  (x = Math.abs(t[o]) - Math.abs(e[o])),
                    (n += (x + Math.abs(x)) / 2);
                return n;
              },
              spectralCrest: function (r) {
                var t = r.ampSpectrum;
                if ("object" != typeof t) throw new TypeError();
                var e = 0,
                  a = -1 / 0;
                return (
                  t.forEach(function (r) {
                    (e += Math.pow(r, 2)), (a = r > a ? r : a);
                  }),
                  (e /= t.length),
                  (e = Math.sqrt(e)),
                  a / e
                );
              },
              melBands: p,
            });
            function S(r) {
              if (Array.isArray(r)) {
                for (var t = 0, e = Array(r.length); t < r.length; t++)
                  e[t] = r[t];
                return e;
              }
              return Array.from(r);
            }
            var _ = {},
              b = {},
              M = {
                bitReverseArray: function (r) {
                  if (void 0 === _[r]) {
                    for (
                      var t = (r - 1).toString(2).length,
                        e = "0".repeat(t),
                        a = {},
                        n = 0;
                      n < r;
                      n++
                    ) {
                      var o = n.toString(2);
                      (o = e.substr(o.length) + o),
                        (o = [].concat(S(o)).reverse().join("")),
                        (a[n] = parseInt(o, 2));
                    }
                    _[r] = a;
                  }
                  return _[r];
                },
                multiply: function (r, t) {
                  return {
                    real: r.real * t.real - r.imag * t.imag,
                    imag: r.real * t.imag + r.imag * t.real,
                  };
                },
                add: function (r, t) {
                  return { real: r.real + t.real, imag: r.imag + t.imag };
                },
                subtract: function (r, t) {
                  return { real: r.real - t.real, imag: r.imag - t.imag };
                },
                euler: function (r, t) {
                  var e = (-2 * Math.PI * r) / t;
                  return { real: Math.cos(e), imag: Math.sin(e) };
                },
                conj: function (r) {
                  return (r.imag *= -1), r;
                },
                constructComplexArray: function (r) {
                  var t = {};
                  t.real = void 0 === r.real ? r.slice() : r.real.slice();
                  var e = t.real.length;
                  return (
                    void 0 === b[e] &&
                      (b[e] = Array.apply(null, Array(e)).map(
                        Number.prototype.valueOf,
                        0
                      )),
                    (t.imag = b[e].slice()),
                    t
                  );
                },
              },
              F = function (r) {
                var t = {};
                void 0 === r.real || void 0 === r.imag
                  ? (t = M.constructComplexArray(r))
                  : ((t.real = r.real.slice()), (t.imag = r.imag.slice()));
                var e = t.real.length,
                  a = Math.log2(e);
                if (Math.round(a) != a)
                  throw new Error("Input size must be a power of 2.");
                if (t.real.length != t.imag.length)
                  throw new Error(
                    "Real and imaginary components must have the same length."
                  );
                for (
                  var n = M.bitReverseArray(e),
                    o = { real: [], imag: [] },
                    i = 0;
                  i < e;
                  i++
                )
                  (o.real[n[i]] = t.real[i]), (o.imag[n[i]] = t.imag[i]);
                for (var u = 0; u < e; u++)
                  (t.real[u] = o.real[u]), (t.imag[u] = o.imag[u]);
                for (var f = 1; f <= a; f++)
                  for (var c = Math.pow(2, f), l = 0; l < c / 2; l++)
                    for (var s = M.euler(l, c), m = 0; m < e / c; m++) {
                      var p = c * m + l,
                        h = c * m + l + c / 2,
                        g = { real: t.real[p], imag: t.imag[p] },
                        w = { real: t.real[h], imag: t.imag[h] },
                        v = M.multiply(s, w),
                        d = M.subtract(g, v);
                      (t.real[h] = d.real), (t.imag[h] = d.imag);
                      var y = M.add(v, g);
                      (t.real[p] = y.real), (t.imag[p] = y.imag);
                    }
                return t;
              },
              A = F,
              E = (function () {
                function r(r, t) {
                  var e = this;
                  if (((this._m = t), !r.audioContext))
                    throw this._m.errors.noAC;
                  if (r.bufferSize && !a(r.bufferSize))
                    throw this._m._errors.notPow2;
                  if (!r.source) throw this._m._errors.noSource;
                  (this._m.audioContext = r.audioContext),
                    (this._m.bufferSize =
                      r.bufferSize || this._m.bufferSize || 256),
                    (this._m.hopSize =
                      r.hopSize || this._m.hopSize || this._m.bufferSize),
                    (this._m.sampleRate =
                      r.sampleRate || this._m.audioContext.sampleRate || 44100),
                    (this._m.callback = r.callback),
                    (this._m.windowingFunction =
                      r.windowingFunction || "hanning"),
                    (this._m.featureExtractors = y),
                    (this._m.EXTRACTION_STARTED = r.startImmediately || !1),
                    (this._m.channel =
                      "number" == typeof r.channel ? r.channel : 0),
                    (this._m.inputs = r.inputs || 1),
                    (this._m.outputs = r.outputs || 1),
                    (this._m.numberOfMFCCCoefficients =
                      r.numberOfMFCCCoefficients ||
                      this._m.numberOfMFCCCoefficients ||
                      13),
                    (this._m.numberOfBarkBands =
                      r.numberOfBarkBands || this._m.numberOfBarkBands || 24),
                    (this._m.spn = this._m.audioContext.createScriptProcessor(
                      this._m.bufferSize,
                      this._m.inputs,
                      this._m.outputs
                    )),
                    this._m.spn.connect(this._m.audioContext.destination),
                    (this._m._featuresToExtract = r.featureExtractors || []),
                    (this._m.barkScale = o(
                      this._m.bufferSize,
                      this._m.sampleRate,
                      this._m.bufferSize
                    )),
                    (this._m.melFilterBank = f(
                      Math.max(
                        this._m.melBands,
                        this._m.numberOfMFCCCoefficients
                      ),
                      this._m.sampleRate,
                      this._m.bufferSize
                    )),
                    (this._m.inputData = null),
                    (this._m.previousInputData = null),
                    (this._m.frame = null),
                    (this._m.previousFrame = null),
                    this.setSource(r.source),
                    (this._m.spn.onaudioprocess = function (r) {
                      var t;
                      null !== e._m.inputData &&
                        (e._m.previousInputData = e._m.inputData),
                        (e._m.inputData = r.inputBuffer.getChannelData(
                          e._m.channel
                        )),
                        e._m.previousInputData
                          ? ((t = new Float32Array(
                              e._m.previousInputData.length +
                                e._m.inputData.length -
                                e._m.hopSize
                            )).set(e._m.previousInputData.slice(e._m.hopSize)),
                            t.set(
                              e._m.inputData,
                              e._m.previousInputData.length - e._m.hopSize
                            ))
                          : (t = e._m.inputData),
                        (function (r, t, e) {
                          if (r.length < t)
                            throw new Error(
                              "Buffer is too short for frame length"
                            );
                          if (e < 1)
                            throw new Error("Hop length cannot be less that 1");
                          if (t < 1)
                            throw new Error(
                              "Frame length cannot be less that 1"
                            );
                          var a = 1 + Math.floor((r.length - t) / e);
                          return new Array(a).fill(0).map(function (a, n) {
                            return r.slice(n * e, n * e + t);
                          });
                        })(t, e._m.bufferSize, e._m.hopSize).forEach(function (
                          r
                        ) {
                          e._m.frame = r;
                          var t = e._m.extract(
                            e._m._featuresToExtract,
                            e._m.frame,
                            e._m.previousFrame
                          );
                          "function" == typeof e._m.callback &&
                            e._m.EXTRACTION_STARTED &&
                            e._m.callback(t),
                            (e._m.previousFrame = e._m.frame);
                        });
                    });
                }
                return (
                  (r.prototype.start = function (r) {
                    (this._m._featuresToExtract =
                      r || this._m._featuresToExtract),
                      (this._m.EXTRACTION_STARTED = !0);
                  }),
                  (r.prototype.stop = function () {
                    this._m.EXTRACTION_STARTED = !1;
                  }),
                  (r.prototype.setSource = function (r) {
                    this._m.source && this._m.source.disconnect(this._m.spn),
                      (this._m.source = r),
                      this._m.source.connect(this._m.spn);
                  }),
                  (r.prototype.setChannel = function (r) {
                    r <= this._m.inputs
                      ? (this._m.channel = r)
                      : console.error(
                          "Channel "
                            .concat(
                              r,
                              " does not exist. Make sure you've provided a value for 'inputs' that is greater than "
                            )
                            .concat(r, " when instantiating the MeydaAnalyzer")
                        );
                  }),
                  (r.prototype.get = function (r) {
                    return this._m.inputData
                      ? this._m.extract(
                          r || this._m._featuresToExtract,
                          this._m.inputData,
                          this._m.previousInputData
                        )
                      : null;
                  }),
                  r
                );
              })(),
              C = {
                audioContext: null,
                spn: null,
                bufferSize: 512,
                sampleRate: 44100,
                melBands: 26,
                chromaBands: 12,
                callback: null,
                windowingFunction: "hanning",
                featureExtractors: y,
                EXTRACTION_STARTED: !1,
                numberOfMFCCCoefficients: 13,
                numberOfBarkBands: 24,
                _featuresToExtract: [],
                windowing: n,
                _errors: {
                  notPow2: new Error(
                    "Meyda: Buffer size must be a power of 2, e.g. 64 or 512"
                  ),
                  featureUndef: new Error("Meyda: No features defined."),
                  invalidFeatureFmt: new Error("Meyda: Invalid feature format"),
                  invalidInput: new Error("Meyda: Invalid input."),
                  noAC: new Error("Meyda: No AudioContext specified."),
                  noSource: new Error("Meyda: No source node specified."),
                },
                createMeydaAnalyzer: function (r) {
                  return new E(r, Object.assign({}, C));
                },
                listAvailableFeatureExtractors: function () {
                  return Object.keys(this.featureExtractors);
                },
                extract: function (r, t, e) {
                  var n = this;
                  if (!t) throw this._errors.invalidInput;
                  if ("object" != typeof t) throw this._errors.invalidInput;
                  if (!r) throw this._errors.featureUndef;
                  if (!a(t.length)) throw this._errors.notPow2;
                  (void 0 !== this.barkScale &&
                    this.barkScale.length == this.bufferSize) ||
                    (this.barkScale = o(
                      this.bufferSize,
                      this.sampleRate,
                      this.bufferSize
                    )),
                    (void 0 !== this.melFilterBank &&
                      this.barkScale.length == this.bufferSize &&
                      this.melFilterBank.length == this.melBands) ||
                      (this.melFilterBank = f(
                        Math.max(this.melBands, this.numberOfMFCCCoefficients),
                        this.sampleRate,
                        this.bufferSize
                      )),
                    (void 0 !== this.chromaFilterBank &&
                      this.chromaFilterBank.length == this.chromaBands) ||
                      (this.chromaFilterBank = c(
                        this.chromaBands,
                        this.sampleRate,
                        this.bufferSize
                      )),
                    "buffer" in t && void 0 === t.buffer
                      ? (this.signal = i(t))
                      : (this.signal = t);
                  var u = k(t, this.windowingFunction, this.bufferSize);
                  if (
                    ((this.signal = u.windowedSignal),
                    (this.complexSpectrum = u.complexSpectrum),
                    (this.ampSpectrum = u.ampSpectrum),
                    e)
                  ) {
                    var l = k(e, this.windowingFunction, this.bufferSize);
                    (this.previousSignal = l.windowedSignal),
                      (this.previousComplexSpectrum = l.complexSpectrum),
                      (this.previousAmpSpectrum = l.ampSpectrum);
                  }
                  var s = function (r) {
                    return n.featureExtractors[r]({
                      ampSpectrum: n.ampSpectrum,
                      chromaFilterBank: n.chromaFilterBank,
                      complexSpectrum: n.complexSpectrum,
                      signal: n.signal,
                      bufferSize: n.bufferSize,
                      sampleRate: n.sampleRate,
                      barkScale: n.barkScale,
                      melFilterBank: n.melFilterBank,
                      previousSignal: n.previousSignal,
                      previousAmpSpectrum: n.previousAmpSpectrum,
                      previousComplexSpectrum: n.previousComplexSpectrum,
                      numberOfMFCCCoefficients: n.numberOfMFCCCoefficients,
                      numberOfBarkBands: n.numberOfBarkBands,
                    });
                  };
                  if ("object" == typeof r)
                    return r.reduce(function (r, t) {
                      var e;
                      return Object.assign({}, r, (((e = {})[t] = s(t)), e));
                    }, {});
                  if ("string" == typeof r) return s(r);
                  throw this._errors.invalidFeatureFmt;
                },
              },
              k = function (r, t, e) {
                var a = {};
                void 0 === r.buffer ? (a.signal = i(r)) : (a.signal = r),
                  (a.windowedSignal = n(a.signal, t)),
                  (a.complexSpectrum = A(a.windowedSignal)),
                  (a.ampSpectrum = new Float32Array(e / 2));
                for (var o = 0; o < e / 2; o++)
                  a.ampSpectrum[o] = Math.sqrt(
                    Math.pow(a.complexSpectrum.real[o], 2) +
                      Math.pow(a.complexSpectrum.imag[o], 2)
                  );
                return a;
              };
            return "undefined" != typeof window && (window.Meyda = C), C;
          });
        },
        {},
      ],
      4: [
        function (require, module, exports) {
          (function (process) {
            (function () {
              // Generated by CoffeeScript 1.12.2
              (function () {
                var getNanoSeconds,
                  hrtime,
                  loadTime,
                  moduleLoadTime,
                  nodeLoadTime,
                  upTime;

                if (
                  typeof performance !== "undefined" &&
                  performance !== null &&
                  performance.now
                ) {
                  module.exports = function () {
                    return performance.now();
                  };
                } else if (
                  typeof process !== "undefined" &&
                  process !== null &&
                  process.hrtime
                ) {
                  module.exports = function () {
                    return (getNanoSeconds() - nodeLoadTime) / 1e6;
                  };
                  hrtime = process.hrtime;
                  getNanoSeconds = function () {
                    var hr;
                    hr = hrtime();
                    return hr[0] * 1e9 + hr[1];
                  };
                  moduleLoadTime = getNanoSeconds();
                  upTime = process.uptime() * 1e9;
                  nodeLoadTime = moduleLoadTime - upTime;
                } else if (Date.now) {
                  module.exports = function () {
                    return Date.now() - loadTime;
                  };
                  loadTime = Date.now();
                } else {
                  module.exports = function () {
                    return new Date().getTime() - loadTime;
                  };
                  loadTime = new Date().getTime();
                }
              }.call(this));
            }.call(this));
          }.call(this, require("_process")));
        },
        { _process: 5 },
      ],
      5: [
        function (require, module, exports) {
          // shim for using process in browser
          var process = (module.exports = {});

          // cached from whatever global is present so that test runners that stub it
          // don't break things.  But we need to wrap it in a try catch in case it is
          // wrapped in strict mode code which doesn't define any globals.  It's inside a
          // function because try/catches deoptimize in certain engines.

          var cachedSetTimeout;
          var cachedClearTimeout;

          function defaultSetTimout() {
            throw new Error("setTimeout has not been defined");
          }
          function defaultClearTimeout() {
            throw new Error("clearTimeout has not been defined");
          }
          (function () {
            try {
              if (typeof setTimeout === "function") {
                cachedSetTimeout = setTimeout;
              } else {
                cachedSetTimeout = defaultSetTimout;
              }
            } catch (e) {
              cachedSetTimeout = defaultSetTimout;
            }
            try {
              if (typeof clearTimeout === "function") {
                cachedClearTimeout = clearTimeout;
              } else {
                cachedClearTimeout = defaultClearTimeout;
              }
            } catch (e) {
              cachedClearTimeout = defaultClearTimeout;
            }
          })();
          function runTimeout(fun) {
            if (cachedSetTimeout === setTimeout) {
              //normal enviroments in sane situations
              return setTimeout(fun, 0);
            }
            // if setTimeout wasn't available but was latter defined
            if (
              (cachedSetTimeout === defaultSetTimout || !cachedSetTimeout) &&
              setTimeout
            ) {
              cachedSetTimeout = setTimeout;
              return setTimeout(fun, 0);
            }
            try {
              // when when somebody has screwed with setTimeout but no I.E. maddness
              return cachedSetTimeout(fun, 0);
            } catch (e) {
              try {
                // When we are in I.E. but the script has been evaled so I.E. doesn't trust the global object when called normally
                return cachedSetTimeout.call(null, fun, 0);
              } catch (e) {
                // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error
                return cachedSetTimeout.call(this, fun, 0);
              }
            }
          }
          function runClearTimeout(marker) {
            if (cachedClearTimeout === clearTimeout) {
              //normal enviroments in sane situations
              return clearTimeout(marker);
            }
            // if clearTimeout wasn't available but was latter defined
            if (
              (cachedClearTimeout === defaultClearTimeout ||
                !cachedClearTimeout) &&
              clearTimeout
            ) {
              cachedClearTimeout = clearTimeout;
              return clearTimeout(marker);
            }
            try {
              // when when somebody has screwed with setTimeout but no I.E. maddness
              return cachedClearTimeout(marker);
            } catch (e) {
              try {
                // When we are in I.E. but the script has been evaled so I.E. doesn't  trust the global object when called normally
                return cachedClearTimeout.call(null, marker);
              } catch (e) {
                // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error.
                // Some versions of I.E. have different rules for clearTimeout vs setTimeout
                return cachedClearTimeout.call(this, marker);
              }
            }
          }
          var queue = [];
          var draining = false;
          var currentQueue;
          var queueIndex = -1;

          function cleanUpNextTick() {
            if (!draining || !currentQueue) {
              return;
            }
            draining = false;
            if (currentQueue.length) {
              queue = currentQueue.concat(queue);
            } else {
              queueIndex = -1;
            }
            if (queue.length) {
              drainQueue();
            }
          }

          function drainQueue() {
            if (draining) {
              return;
            }
            var timeout = runTimeout(cleanUpNextTick);
            draining = true;

            var len = queue.length;
            while (len) {
              currentQueue = queue;
              queue = [];
              while (++queueIndex < len) {
                if (currentQueue) {
                  currentQueue[queueIndex].run();
                }
              }
              queueIndex = -1;
              len = queue.length;
            }
            currentQueue = null;
            draining = false;
            runClearTimeout(timeout);
          }

          process.nextTick = function (fun) {
            var args = new Array(arguments.length - 1);
            if (arguments.length > 1) {
              for (var i = 1; i < arguments.length; i++) {
                args[i - 1] = arguments[i];
              }
            }
            queue.push(new Item(fun, args));
            if (queue.length === 1 && !draining) {
              runTimeout(drainQueue);
            }
          };

          // v8 likes predictible objects
          function Item(fun, array) {
            this.fun = fun;
            this.array = array;
          }
          Item.prototype.run = function () {
            this.fun.apply(null, this.array);
          };
          process.title = "browser";
          process.browser = true;
          process.env = {};
          process.argv = [];
          process.version = ""; // empty string to avoid regexp issues
          process.versions = {};

          function noop() {}

          process.on = noop;
          process.addListener = noop;
          process.once = noop;
          process.off = noop;
          process.removeListener = noop;
          process.removeAllListeners = noop;
          process.emit = noop;
          process.prependListener = noop;
          process.prependOnceListener = noop;

          process.listeners = function (name) {
            return [];
          };

          process.binding = function (name) {
            throw new Error("process.binding is not supported");
          };

          process.cwd = function () {
            return "/";
          };
          process.chdir = function (dir) {
            throw new Error("process.chdir is not supported");
          };
          process.umask = function () {
            return 0;
          };
        },
        {},
      ],
      6: [
        function (require, module, exports) {
          var inherits = require("inherits");
          var EventEmitter = require("events").EventEmitter;
          var now = require("right-now");
          var raf = require("raf");

          module.exports = Engine;
          function Engine(fn) {
            if (!(this instanceof Engine)) return new Engine(fn);
            this.running = false;
            this.last = now();
            this._frame = 0;
            this._tick = this.tick.bind(this);

            if (fn) this.on("tick", fn);
          }

          inherits(Engine, EventEmitter);

          Engine.prototype.start = function () {
            if (this.running) return;
            this.running = true;
            this.last = now();
            this._frame = raf(this._tick);
            return this;
          };

          Engine.prototype.stop = function () {
            this.running = false;
            if (this._frame !== 0) raf.cancel(this._frame);
            this._frame = 0;
            return this;
          };

          Engine.prototype.tick = function () {
            this._frame = raf(this._tick);
            var time = now();
            var dt = time - this.last;
            this.emit("tick", dt);
            this.last = time;
          };
        },
        { events: 1, inherits: 2, raf: 7, "right-now": 9 },
      ],
      7: [
        function (require, module, exports) {
          (function (global) {
            (function () {
              var now = require("performance-now"),
                root = typeof window === "undefined" ? global : window,
                vendors = ["moz", "webkit"],
                suffix = "AnimationFrame",
                raf = root["request" + suffix],
                caf = root["cancel" + suffix] || root["cancelRequest" + suffix];

              for (var i = 0; !raf && i < vendors.length; i++) {
                raf = root[vendors[i] + "Request" + suffix];
                caf =
                  root[vendors[i] + "Cancel" + suffix] ||
                  root[vendors[i] + "CancelRequest" + suffix];
              }

              // Some versions of FF have rAF but not cAF
              if (!raf || !caf) {
                var last = 0,
                  id = 0,
                  queue = [],
                  frameDuration = 1000 / 60;

                raf = function (callback) {
                  if (queue.length === 0) {
                    var _now = now(),
                      next = Math.max(0, frameDuration - (_now - last));
                    last = next + _now;
                    setTimeout(function () {
                      var cp = queue.slice(0);
                      // Clear queue here to prevent
                      // callbacks from appending listeners
                      // to the current frame's queue
                      queue.length = 0;
                      for (var i = 0; i < cp.length; i++) {
                        if (!cp[i].cancelled) {
                          try {
                            cp[i].callback(last);
                          } catch (e) {
                            setTimeout(function () {
                              throw e;
                            }, 0);
                          }
                        }
                      }
                    }, Math.round(next));
                  }
                  queue.push({
                    handle: ++id,
                    callback: callback,
                    cancelled: false,
                  });
                  return id;
                };

                caf = function (handle) {
                  for (var i = 0; i < queue.length; i++) {
                    if (queue[i].handle === handle) {
                      queue[i].cancelled = true;
                    }
                  }
                };
              }

              module.exports = function (fn) {
                // Wrap in a new function to prevent
                // `cancel` potentially being assigned
                // to the native rAF function
                return raf.call(root, fn);
              };
              module.exports.cancel = function () {
                caf.apply(root, arguments);
              };
              module.exports.polyfill = function (object) {
                if (!object) {
                  object = root;
                }
                object.requestAnimationFrame = raf;
                object.cancelAnimationFrame = caf;
              };
            }.call(this));
          }.call(
            this,
            typeof global !== "undefined"
              ? global
              : typeof self !== "undefined"
              ? self
              : typeof window !== "undefined"
              ? window
              : {}
          ));
        },
        { "performance-now": 4 },
      ],
      8: [
        function (require, module, exports) {
          (function (aa, ia) {
            "object" === typeof exports && "undefined" !== typeof module
              ? (module.exports = ia())
              : "function" === typeof define && define.amd
              ? define(ia)
              : (aa.createREGL = ia());
          })(this, function () {
            function aa(a, b) {
              this.id = Ab++;
              this.type = a;
              this.data = b;
            }
            function ia(a) {
              if (0 === a.length) return [];
              var b = a.charAt(0),
                c = a.charAt(a.length - 1);
              if (1 < a.length && b === c && ('"' === b || "'" === b))
                return [
                  '"' +
                    a
                      .substr(1, a.length - 2)
                      .replace(/\\/g, "\\\\")
                      .replace(/"/g, '\\"') +
                    '"',
                ];
              if ((b = /\[(false|true|null|\d+|'[^']*'|"[^"]*")\]/.exec(a)))
                return ia(a.substr(0, b.index))
                  .concat(ia(b[1]))
                  .concat(ia(a.substr(b.index + b[0].length)));
              b = a.split(".");
              if (1 === b.length)
                return [
                  '"' + a.replace(/\\/g, "\\\\").replace(/"/g, '\\"') + '"',
                ];
              a = [];
              for (c = 0; c < b.length; ++c) a = a.concat(ia(b[c]));
              return a;
            }
            function Za(a) {
              return "[" + ia(a).join("][") + "]";
            }
            function Bb() {
              var a = { "": 0 },
                b = [""];
              return {
                id: function (c) {
                  var e = a[c];
                  if (e) return e;
                  e = a[c] = b.length;
                  b.push(c);
                  return e;
                },
                str: function (a) {
                  return b[a];
                },
              };
            }
            function Cb(a, b, c) {
              function e() {
                var b = window.innerWidth,
                  e = window.innerHeight;
                a !== document.body &&
                  ((e = a.getBoundingClientRect()),
                  (b = e.right - e.left),
                  (e = e.bottom - e.top));
                g.width = c * b;
                g.height = c * e;
                E(g.style, { width: b + "px", height: e + "px" });
              }
              var g = document.createElement("canvas");
              E(g.style, { border: 0, margin: 0, padding: 0, top: 0, left: 0 });
              a.appendChild(g);
              a === document.body &&
                ((g.style.position = "absolute"),
                E(a.style, { margin: 0, padding: 0 }));
              window.addEventListener("resize", e, !1);
              e();
              return {
                canvas: g,
                onDestroy: function () {
                  window.removeEventListener("resize", e);
                  a.removeChild(g);
                },
              };
            }
            function Db(a, b) {
              function c(c) {
                try {
                  return a.getContext(c, b);
                } catch (g) {
                  return null;
                }
              }
              return (
                c("webgl") || c("experimental-webgl") || c("webgl-experimental")
              );
            }
            function $a(a) {
              return "string" === typeof a ? a.split() : a;
            }
            function ab(a) {
              return "string" === typeof a ? document.querySelector(a) : a;
            }
            function Eb(a) {
              var b = a || {},
                c,
                e,
                g,
                d;
              a = {};
              var n = [],
                f = [],
                r = "undefined" === typeof window ? 1 : window.devicePixelRatio,
                q = !1,
                t = function (a) {},
                m = function () {};
              "string" === typeof b
                ? (c = document.querySelector(b))
                : "object" === typeof b &&
                  ("string" === typeof b.nodeName &&
                  "function" === typeof b.appendChild &&
                  "function" === typeof b.getBoundingClientRect
                    ? (c = b)
                    : "function" === typeof b.drawArrays ||
                      "function" === typeof b.drawElements
                    ? ((d = b), (g = d.canvas))
                    : ("gl" in b
                        ? (d = b.gl)
                        : "canvas" in b
                        ? (g = ab(b.canvas))
                        : "container" in b && (e = ab(b.container)),
                      "attributes" in b && (a = b.attributes),
                      "extensions" in b && (n = $a(b.extensions)),
                      "optionalExtensions" in b &&
                        (f = $a(b.optionalExtensions)),
                      "onDone" in b && (t = b.onDone),
                      "profile" in b && (q = !!b.profile),
                      "pixelRatio" in b && (r = +b.pixelRatio)));
              c && ("canvas" === c.nodeName.toLowerCase() ? (g = c) : (e = c));
              if (!d) {
                if (!g) {
                  c = Cb(e || document.body, t, r);
                  if (!c) return null;
                  g = c.canvas;
                  m = c.onDestroy;
                }
                d = Db(g, a);
              }
              return d
                ? {
                    gl: d,
                    canvas: g,
                    container: e,
                    extensions: n,
                    optionalExtensions: f,
                    pixelRatio: r,
                    profile: q,
                    onDone: t,
                    onDestroy: m,
                  }
                : (m(),
                  t(
                    "webgl not supported, try upgrading your browser or graphics drivers http://get.webgl.org"
                  ),
                  null);
            }
            function Fb(a, b) {
              function c(b) {
                b = b.toLowerCase();
                var c;
                try {
                  c = e[b] = a.getExtension(b);
                } catch (g) {}
                return !!c;
              }
              for (var e = {}, g = 0; g < b.extensions.length; ++g) {
                var d = b.extensions[g];
                if (!c(d))
                  return (
                    b.onDestroy(),
                    b.onDone(
                      '"' +
                        d +
                        '" extension is not supported by the current WebGL context, try upgrading your system or a different browser'
                    ),
                    null
                  );
              }
              b.optionalExtensions.forEach(c);
              return {
                extensions: e,
                restore: function () {
                  Object.keys(e).forEach(function (a) {
                    if (e[a] && !c(a))
                      throw Error("(regl): error restoring extension " + a);
                  });
                },
              };
            }
            function J(a, b) {
              for (var c = Array(a), e = 0; e < a; ++e) c[e] = b(e);
              return c;
            }
            function bb(a) {
              var b, c;
              b = (65535 < a) << 4;
              a >>>= b;
              c = (255 < a) << 3;
              a >>>= c;
              b |= c;
              c = (15 < a) << 2;
              a >>>= c;
              b |= c;
              c = (3 < a) << 1;
              return b | c | ((a >>> c) >> 1);
            }
            function cb() {
              function a(a) {
                a: {
                  for (var b = 16; 268435456 >= b; b *= 16)
                    if (a <= b) {
                      a = b;
                      break a;
                    }
                  a = 0;
                }
                b = c[bb(a) >> 2];
                return 0 < b.length ? b.pop() : new ArrayBuffer(a);
              }
              function b(a) {
                c[bb(a.byteLength) >> 2].push(a);
              }
              var c = J(8, function () {
                return [];
              });
              return {
                alloc: a,
                free: b,
                allocType: function (b, c) {
                  var d = null;
                  switch (b) {
                    case 5120:
                      d = new Int8Array(a(c), 0, c);
                      break;
                    case 5121:
                      d = new Uint8Array(a(c), 0, c);
                      break;
                    case 5122:
                      d = new Int16Array(a(2 * c), 0, c);
                      break;
                    case 5123:
                      d = new Uint16Array(a(2 * c), 0, c);
                      break;
                    case 5124:
                      d = new Int32Array(a(4 * c), 0, c);
                      break;
                    case 5125:
                      d = new Uint32Array(a(4 * c), 0, c);
                      break;
                    case 5126:
                      d = new Float32Array(a(4 * c), 0, c);
                      break;
                    default:
                      return null;
                  }
                  return d.length !== c ? d.subarray(0, c) : d;
                },
                freeType: function (a) {
                  b(a.buffer);
                },
              };
            }
            function ma(a) {
              return (
                !!a &&
                "object" === typeof a &&
                Array.isArray(a.shape) &&
                Array.isArray(a.stride) &&
                "number" === typeof a.offset &&
                a.shape.length === a.stride.length &&
                (Array.isArray(a.data) || M(a.data))
              );
            }
            function db(a, b, c, e, g, d) {
              for (var n = 0; n < b; ++n)
                for (var f = a[n], r = 0; r < c; ++r)
                  for (var q = f[r], t = 0; t < e; ++t) g[d++] = q[t];
            }
            function eb(a, b, c, e, g) {
              for (var d = 1, n = c + 1; n < b.length; ++n) d *= b[n];
              var f = b[c];
              if (4 === b.length - c) {
                var r = b[c + 1],
                  q = b[c + 2];
                b = b[c + 3];
                for (n = 0; n < f; ++n) db(a[n], r, q, b, e, g), (g += d);
              } else for (n = 0; n < f; ++n) eb(a[n], b, c + 1, e, g), (g += d);
            }
            function Ha(a) {
              return Ia[Object.prototype.toString.call(a)] | 0;
            }
            function fb(a, b) {
              for (var c = 0; c < b.length; ++c) a[c] = b[c];
            }
            function gb(a, b, c, e, g, d, n) {
              for (var f = 0, r = 0; r < c; ++r)
                for (var q = 0; q < e; ++q) a[f++] = b[g * r + d * q + n];
            }
            function Gb(a, b, c, e) {
              function g(b) {
                this.id = r++;
                this.buffer = a.createBuffer();
                this.type = b;
                this.usage = 35044;
                this.byteLength = 0;
                this.dimension = 1;
                this.dtype = 5121;
                this.persistentData = null;
                c.profile && (this.stats = { size: 0 });
              }
              function d(b, c, k) {
                b.byteLength = c.byteLength;
                a.bufferData(b.type, c, k);
              }
              function n(a, b, c, h, l, e) {
                a.usage = c;
                if (Array.isArray(b)) {
                  if (((a.dtype = h || 5126), 0 < b.length))
                    if (Array.isArray(b[0])) {
                      l = hb(b);
                      for (var v = (h = 1); v < l.length; ++v) h *= l[v];
                      a.dimension = h;
                      b = Qa(b, l, a.dtype);
                      d(a, b, c);
                      e ? (a.persistentData = b) : x.freeType(b);
                    } else
                      "number" === typeof b[0]
                        ? ((a.dimension = l),
                          (l = x.allocType(a.dtype, b.length)),
                          fb(l, b),
                          d(a, l, c),
                          e ? (a.persistentData = l) : x.freeType(l))
                        : M(b[0]) &&
                          ((a.dimension = b[0].length),
                          (a.dtype = h || Ha(b[0]) || 5126),
                          (b = Qa(b, [b.length, b[0].length], a.dtype)),
                          d(a, b, c),
                          e ? (a.persistentData = b) : x.freeType(b));
                } else if (M(b))
                  (a.dtype = h || Ha(b)),
                    (a.dimension = l),
                    d(a, b, c),
                    e &&
                      (a.persistentData = new Uint8Array(
                        new Uint8Array(b.buffer)
                      ));
                else if (ma(b)) {
                  l = b.shape;
                  var g = b.stride,
                    v = b.offset,
                    f = 0,
                    q = 0,
                    r = 0,
                    t = 0;
                  1 === l.length
                    ? ((f = l[0]), (q = 1), (r = g[0]), (t = 0))
                    : 2 === l.length &&
                      ((f = l[0]), (q = l[1]), (r = g[0]), (t = g[1]));
                  a.dtype = h || Ha(b.data) || 5126;
                  a.dimension = q;
                  l = x.allocType(a.dtype, f * q);
                  gb(l, b.data, f, q, r, t, v);
                  d(a, l, c);
                  e ? (a.persistentData = l) : x.freeType(l);
                }
              }
              function f(c) {
                b.bufferCount--;
                for (var d = 0; d < e.state.length; ++d) {
                  var k = e.state[d];
                  k.buffer === c &&
                    (a.disableVertexAttribArray(d), (k.buffer = null));
                }
                a.deleteBuffer(c.buffer);
                c.buffer = null;
                delete q[c.id];
              }
              var r = 0,
                q = {};
              g.prototype.bind = function () {
                a.bindBuffer(this.type, this.buffer);
              };
              g.prototype.destroy = function () {
                f(this);
              };
              var t = [];
              c.profile &&
                (b.getTotalBufferSize = function () {
                  var a = 0;
                  Object.keys(q).forEach(function (b) {
                    a += q[b].stats.size;
                  });
                  return a;
                });
              return {
                create: function (m, e, d, h) {
                  function l(b) {
                    var m = 35044,
                      e = null,
                      d = 0,
                      k = 0,
                      g = 1;
                    Array.isArray(b) || M(b) || ma(b)
                      ? (e = b)
                      : "number" === typeof b
                      ? (d = b | 0)
                      : b &&
                        ("data" in b && (e = b.data),
                        "usage" in b && (m = jb[b.usage]),
                        "type" in b && (k = Ra[b.type]),
                        "dimension" in b && (g = b.dimension | 0),
                        "length" in b && (d = b.length | 0));
                    u.bind();
                    e
                      ? n(u, e, m, k, g, h)
                      : (d && a.bufferData(u.type, d, m),
                        (u.dtype = k || 5121),
                        (u.usage = m),
                        (u.dimension = g),
                        (u.byteLength = d));
                    c.profile && (u.stats.size = u.byteLength * ja[u.dtype]);
                    return l;
                  }
                  b.bufferCount++;
                  var u = new g(e);
                  q[u.id] = u;
                  d || l(m);
                  l._reglType = "buffer";
                  l._buffer = u;
                  l.subdata = function (b, c) {
                    var m = (c || 0) | 0,
                      e;
                    u.bind();
                    if (M(b)) a.bufferSubData(u.type, m, b);
                    else if (Array.isArray(b)) {
                      if (0 < b.length)
                        if ("number" === typeof b[0]) {
                          var d = x.allocType(u.dtype, b.length);
                          fb(d, b);
                          a.bufferSubData(u.type, m, d);
                          x.freeType(d);
                        } else if (Array.isArray(b[0]) || M(b[0]))
                          (e = hb(b)),
                            (d = Qa(b, e, u.dtype)),
                            a.bufferSubData(u.type, m, d),
                            x.freeType(d);
                    } else if (ma(b)) {
                      e = b.shape;
                      var h = b.stride,
                        k = (d = 0),
                        g = 0,
                        F = 0;
                      1 === e.length
                        ? ((d = e[0]), (k = 1), (g = h[0]), (F = 0))
                        : 2 === e.length &&
                          ((d = e[0]), (k = e[1]), (g = h[0]), (F = h[1]));
                      e = Array.isArray(b.data) ? u.dtype : Ha(b.data);
                      e = x.allocType(e, d * k);
                      gb(e, b.data, d, k, g, F, b.offset);
                      a.bufferSubData(u.type, m, e);
                      x.freeType(e);
                    }
                    return l;
                  };
                  c.profile && (l.stats = u.stats);
                  l.destroy = function () {
                    f(u);
                  };
                  return l;
                },
                createStream: function (a, b) {
                  var c = t.pop();
                  c || (c = new g(a));
                  c.bind();
                  n(c, b, 35040, 0, 1, !1);
                  return c;
                },
                destroyStream: function (a) {
                  t.push(a);
                },
                clear: function () {
                  S(q).forEach(f);
                  t.forEach(f);
                },
                getBuffer: function (a) {
                  return a && a._buffer instanceof g ? a._buffer : null;
                },
                restore: function () {
                  S(q).forEach(function (b) {
                    b.buffer = a.createBuffer();
                    a.bindBuffer(b.type, b.buffer);
                    a.bufferData(
                      b.type,
                      b.persistentData || b.byteLength,
                      b.usage
                    );
                  });
                },
                _initBuffer: n,
              };
            }
            function Hb(a, b, c, e) {
              function g(a) {
                this.id = r++;
                f[this.id] = this;
                this.buffer = a;
                this.primType = 4;
                this.type = this.vertCount = 0;
              }
              function d(e, d, g, h, l, u, v) {
                e.buffer.bind();
                if (d) {
                  var f = v;
                  v ||
                    (M(d) && (!ma(d) || M(d.data))) ||
                    (f = b.oes_element_index_uint ? 5125 : 5123);
                  c._initBuffer(e.buffer, d, g, f, 3);
                } else a.bufferData(34963, u, g), (e.buffer.dtype = f || 5121), (e.buffer.usage = g), (e.buffer.dimension = 3), (e.buffer.byteLength = u);
                f = v;
                if (!v) {
                  switch (e.buffer.dtype) {
                    case 5121:
                    case 5120:
                      f = 5121;
                      break;
                    case 5123:
                    case 5122:
                      f = 5123;
                      break;
                    case 5125:
                    case 5124:
                      f = 5125;
                  }
                  e.buffer.dtype = f;
                }
                e.type = f;
                d = l;
                0 > d &&
                  ((d = e.buffer.byteLength),
                  5123 === f ? (d >>= 1) : 5125 === f && (d >>= 2));
                e.vertCount = d;
                d = h;
                0 > h &&
                  ((d = 4),
                  (h = e.buffer.dimension),
                  1 === h && (d = 0),
                  2 === h && (d = 1),
                  3 === h && (d = 4));
                e.primType = d;
              }
              function n(a) {
                e.elementsCount--;
                delete f[a.id];
                a.buffer.destroy();
                a.buffer = null;
              }
              var f = {},
                r = 0,
                q = { uint8: 5121, uint16: 5123 };
              b.oes_element_index_uint && (q.uint32 = 5125);
              g.prototype.bind = function () {
                this.buffer.bind();
              };
              var t = [];
              return {
                create: function (a, b) {
                  function k(a) {
                    if (a)
                      if ("number" === typeof a)
                        h(a),
                          (l.primType = 4),
                          (l.vertCount = a | 0),
                          (l.type = 5121);
                      else {
                        var b = null,
                          c = 35044,
                          e = -1,
                          g = -1,
                          f = 0,
                          m = 0;
                        if (Array.isArray(a) || M(a) || ma(a)) b = a;
                        else if (
                          ("data" in a && (b = a.data),
                          "usage" in a && (c = jb[a.usage]),
                          "primitive" in a && (e = Sa[a.primitive]),
                          "count" in a && (g = a.count | 0),
                          "type" in a && (m = q[a.type]),
                          "length" in a)
                        )
                          f = a.length | 0;
                        else if (((f = g), 5123 === m || 5122 === m)) f *= 2;
                        else if (5125 === m || 5124 === m) f *= 4;
                        d(l, b, c, e, g, f, m);
                      }
                    else
                      h(), (l.primType = 4), (l.vertCount = 0), (l.type = 5121);
                    return k;
                  }
                  var h = c.create(null, 34963, !0),
                    l = new g(h._buffer);
                  e.elementsCount++;
                  k(a);
                  k._reglType = "elements";
                  k._elements = l;
                  k.subdata = function (a, b) {
                    h.subdata(a, b);
                    return k;
                  };
                  k.destroy = function () {
                    n(l);
                  };
                  return k;
                },
                createStream: function (a) {
                  var b = t.pop();
                  b || (b = new g(c.create(null, 34963, !0, !1)._buffer));
                  d(b, a, 35040, -1, -1, 0, 0);
                  return b;
                },
                destroyStream: function (a) {
                  t.push(a);
                },
                getElements: function (a) {
                  return "function" === typeof a && a._elements instanceof g
                    ? a._elements
                    : null;
                },
                clear: function () {
                  S(f).forEach(n);
                },
              };
            }
            function kb(a) {
              for (
                var b = x.allocType(5123, a.length), c = 0;
                c < a.length;
                ++c
              )
                if (isNaN(a[c])) b[c] = 65535;
                else if (Infinity === a[c]) b[c] = 31744;
                else if (-Infinity === a[c]) b[c] = 64512;
                else {
                  lb[0] = a[c];
                  var e = Ib[0],
                    g = (e >>> 31) << 15,
                    d = ((e << 1) >>> 24) - 127,
                    e = (e >> 13) & 1023;
                  b[c] =
                    -24 > d
                      ? g
                      : -14 > d
                      ? g + ((e + 1024) >> (-14 - d))
                      : 15 < d
                      ? g + 31744
                      : g + ((d + 15) << 10) + e;
                }
              return b;
            }
            function pa(a) {
              return Array.isArray(a) || M(a);
            }
            function Ea(a) {
              return "[object " + a + "]";
            }
            function mb(a) {
              return (
                Array.isArray(a) && (0 === a.length || "number" === typeof a[0])
              );
            }
            function nb(a) {
              return Array.isArray(a) && 0 !== a.length && pa(a[0]) ? !0 : !1;
            }
            function na(a) {
              return Object.prototype.toString.call(a);
            }
            function Ta(a) {
              if (!a) return !1;
              var b = na(a);
              return 0 <= Jb.indexOf(b) ? !0 : mb(a) || nb(a) || ma(a);
            }
            function ob(a, b) {
              36193 === a.type
                ? ((a.data = kb(b)), x.freeType(b))
                : (a.data = b);
            }
            function Ja(a, b, c, e, g, d) {
              a = "undefined" !== typeof y[a] ? y[a] : L[a] * qa[b];
              d && (a *= 6);
              if (g) {
                for (e = 0; 1 <= c; ) (e += a * c * c), (c /= 2);
                return e;
              }
              return a * c * e;
            }
            function Kb(a, b, c, e, g, d, n) {
              function f() {
                this.format = this.internalformat = 6408;
                this.type = 5121;
                this.flipY = this.premultiplyAlpha = this.compressed = !1;
                this.unpackAlignment = 1;
                this.colorSpace = 37444;
                this.channels = this.height = this.width = 0;
              }
              function r(a, b) {
                a.internalformat = b.internalformat;
                a.format = b.format;
                a.type = b.type;
                a.compressed = b.compressed;
                a.premultiplyAlpha = b.premultiplyAlpha;
                a.flipY = b.flipY;
                a.unpackAlignment = b.unpackAlignment;
                a.colorSpace = b.colorSpace;
                a.width = b.width;
                a.height = b.height;
                a.channels = b.channels;
              }
              function q(a, b) {
                if ("object" === typeof b && b) {
                  "premultiplyAlpha" in b &&
                    (a.premultiplyAlpha = b.premultiplyAlpha);
                  "flipY" in b && (a.flipY = b.flipY);
                  "alignment" in b && (a.unpackAlignment = b.alignment);
                  "colorSpace" in b && (a.colorSpace = wa[b.colorSpace]);
                  "type" in b && (a.type = G[b.type]);
                  var c = a.width,
                    e = a.height,
                    d = a.channels,
                    h = !1;
                  "shape" in b
                    ? ((c = b.shape[0]),
                      (e = b.shape[1]),
                      3 === b.shape.length && ((d = b.shape[2]), (h = !0)))
                    : ("radius" in b && (c = e = b.radius),
                      "width" in b && (c = b.width),
                      "height" in b && (e = b.height),
                      "channels" in b && ((d = b.channels), (h = !0)));
                  a.width = c | 0;
                  a.height = e | 0;
                  a.channels = d | 0;
                  c = !1;
                  "format" in b &&
                    ((c = b.format),
                    (e = a.internalformat = U[c]),
                    (a.format = Lb[e]),
                    c in G && !("type" in b) && (a.type = G[c]),
                    c in W && (a.compressed = !0),
                    (c = !0));
                  !h && c
                    ? (a.channels = L[a.format])
                    : h &&
                      !c &&
                      a.channels !== La[a.format] &&
                      (a.format = a.internalformat = La[a.channels]);
                }
              }
              function t(b) {
                a.pixelStorei(37440, b.flipY);
                a.pixelStorei(37441, b.premultiplyAlpha);
                a.pixelStorei(37443, b.colorSpace);
                a.pixelStorei(3317, b.unpackAlignment);
              }
              function m() {
                f.call(this);
                this.yOffset = this.xOffset = 0;
                this.data = null;
                this.needsFree = !1;
                this.element = null;
                this.needsCopy = !1;
              }
              function C(a, b) {
                var c = null;
                Ta(b)
                  ? (c = b)
                  : b &&
                    (q(a, b),
                    "x" in b && (a.xOffset = b.x | 0),
                    "y" in b && (a.yOffset = b.y | 0),
                    Ta(b.data) && (c = b.data));
                if (b.copy) {
                  var e = g.viewportWidth,
                    d = g.viewportHeight;
                  a.width = a.width || e - a.xOffset;
                  a.height = a.height || d - a.yOffset;
                  a.needsCopy = !0;
                } else if (!c)
                  (a.width = a.width || 1),
                    (a.height = a.height || 1),
                    (a.channels = a.channels || 4);
                else if (M(c))
                  (a.channels = a.channels || 4),
                    (a.data = c),
                    "type" in b ||
                      5121 !== a.type ||
                      (a.type = Ia[Object.prototype.toString.call(c)] | 0);
                else if (mb(c)) {
                  a.channels = a.channels || 4;
                  e = c;
                  d = e.length;
                  switch (a.type) {
                    case 5121:
                    case 5123:
                    case 5125:
                    case 5126:
                      d = x.allocType(a.type, d);
                      d.set(e);
                      a.data = d;
                      break;
                    case 36193:
                      a.data = kb(e);
                  }
                  a.alignment = 1;
                  a.needsFree = !0;
                } else if (ma(c)) {
                  e = c.data;
                  Array.isArray(e) ||
                    5121 !== a.type ||
                    (a.type = Ia[Object.prototype.toString.call(e)] | 0);
                  var d = c.shape,
                    h = c.stride,
                    f,
                    l,
                    p,
                    w;
                  3 === d.length ? ((p = d[2]), (w = h[2])) : (w = p = 1);
                  f = d[0];
                  l = d[1];
                  d = h[0];
                  h = h[1];
                  a.alignment = 1;
                  a.width = f;
                  a.height = l;
                  a.channels = p;
                  a.format = a.internalformat = La[p];
                  a.needsFree = !0;
                  f = w;
                  c = c.offset;
                  p = a.width;
                  w = a.height;
                  l = a.channels;
                  for (
                    var z = x.allocType(
                        36193 === a.type ? 5126 : a.type,
                        p * w * l
                      ),
                      I = 0,
                      fa = 0;
                    fa < w;
                    ++fa
                  )
                    for (var ga = 0; ga < p; ++ga)
                      for (var xa = 0; xa < l; ++xa)
                        z[I++] = e[d * ga + h * fa + f * xa + c];
                  ob(a, z);
                } else if (na(c) === Ua || na(c) === pb)
                  na(c) === Ua ? (a.element = c) : (a.element = c.canvas),
                    (a.width = a.element.width),
                    (a.height = a.element.height),
                    (a.channels = 4);
                else if (na(c) === qb)
                  (a.element = c),
                    (a.width = c.width),
                    (a.height = c.height),
                    (a.channels = 4);
                else if (na(c) === rb)
                  (a.element = c),
                    (a.width = c.naturalWidth),
                    (a.height = c.naturalHeight),
                    (a.channels = 4);
                else if (na(c) === sb)
                  (a.element = c),
                    (a.width = c.videoWidth),
                    (a.height = c.videoHeight),
                    (a.channels = 4);
                else if (nb(c)) {
                  e = a.width || c[0].length;
                  d = a.height || c.length;
                  h = a.channels;
                  h = pa(c[0][0]) ? h || c[0][0].length : h || 1;
                  f = Ma.shape(c);
                  p = 1;
                  for (w = 0; w < f.length; ++w) p *= f[w];
                  p = x.allocType(36193 === a.type ? 5126 : a.type, p);
                  Ma.flatten(c, f, "", p);
                  ob(a, p);
                  a.alignment = 1;
                  a.width = e;
                  a.height = d;
                  a.channels = h;
                  a.format = a.internalformat = La[h];
                  a.needsFree = !0;
                }
              }
              function k(b, c, d, h, f) {
                var g = b.element,
                  l = b.data,
                  k = b.internalformat,
                  p = b.format,
                  w = b.type,
                  z = b.width,
                  I = b.height;
                t(b);
                g
                  ? a.texSubImage2D(c, f, d, h, p, w, g)
                  : b.compressed
                  ? a.compressedTexSubImage2D(c, f, d, h, k, z, I, l)
                  : b.needsCopy
                  ? (e(),
                    a.copyTexSubImage2D(c, f, d, h, b.xOffset, b.yOffset, z, I))
                  : a.texSubImage2D(c, f, d, h, z, I, p, w, l);
              }
              function h() {
                return P.pop() || new m();
              }
              function l(a) {
                a.needsFree && x.freeType(a.data);
                m.call(a);
                P.push(a);
              }
              function u() {
                f.call(this);
                this.genMipmaps = !1;
                this.mipmapHint = 4352;
                this.mipmask = 0;
                this.images = Array(16);
              }
              function v(a, b, c) {
                var d = (a.images[0] = h());
                a.mipmask = 1;
                d.width = a.width = b;
                d.height = a.height = c;
                d.channels = a.channels = 4;
              }
              function N(a, b) {
                var c = null;
                if (Ta(b))
                  (c = a.images[0] = h()), r(c, a), C(c, b), (a.mipmask = 1);
                else if ((q(a, b), Array.isArray(b.mipmap)))
                  for (var d = b.mipmap, e = 0; e < d.length; ++e)
                    (c = a.images[e] = h()),
                      r(c, a),
                      (c.width >>= e),
                      (c.height >>= e),
                      C(c, d[e]),
                      (a.mipmask |= 1 << e);
                else (c = a.images[0] = h()), r(c, a), C(c, b), (a.mipmask = 1);
                r(a, a.images[0]);
              }
              function B(b, c) {
                for (var d = b.images, h = 0; h < d.length && d[h]; ++h) {
                  var f = d[h],
                    g = c,
                    l = h,
                    k = f.element,
                    p = f.data,
                    w = f.internalformat,
                    z = f.format,
                    I = f.type,
                    fa = f.width,
                    ga = f.height,
                    xa = f.channels;
                  t(f);
                  k
                    ? a.texImage2D(g, l, z, z, I, k)
                    : f.compressed
                    ? a.compressedTexImage2D(g, l, w, fa, ga, 0, p)
                    : f.needsCopy
                    ? (e(),
                      a.copyTexImage2D(
                        g,
                        l,
                        z,
                        f.xOffset,
                        f.yOffset,
                        fa,
                        ga,
                        0
                      ))
                    : ((f = !p) && (p = x.zero.allocType(I, fa * ga * xa)),
                      a.texImage2D(g, l, z, fa, ga, 0, z, I, p),
                      f && p && x.zero.freeType(p));
                }
              }
              function D() {
                var a = tb.pop() || new u();
                f.call(a);
                for (var b = (a.mipmask = 0); 16 > b; ++b) a.images[b] = null;
                return a;
              }
              function ib(a) {
                for (var b = a.images, c = 0; c < b.length; ++c)
                  b[c] && l(b[c]), (b[c] = null);
                tb.push(a);
              }
              function y() {
                this.magFilter = this.minFilter = 9728;
                this.wrapT = this.wrapS = 33071;
                this.anisotropic = 1;
                this.genMipmaps = !1;
                this.mipmapHint = 4352;
              }
              function O(a, b) {
                "min" in b &&
                  ((a.minFilter = Va[b.min]),
                  0 <= Mb.indexOf(a.minFilter) &&
                    !("faces" in b) &&
                    (a.genMipmaps = !0));
                "mag" in b && (a.magFilter = V[b.mag]);
                var c = a.wrapS,
                  d = a.wrapT;
                if ("wrap" in b) {
                  var e = b.wrap;
                  "string" === typeof e
                    ? (c = d = K[e])
                    : Array.isArray(e) && ((c = K[e[0]]), (d = K[e[1]]));
                } else "wrapS" in b && (c = K[b.wrapS]), "wrapT" in b && (d = K[b.wrapT]);
                a.wrapS = c;
                a.wrapT = d;
                "anisotropic" in b && (a.anisotropic = b.anisotropic);
                if ("mipmap" in b) {
                  c = !1;
                  switch (typeof b.mipmap) {
                    case "string":
                      a.mipmapHint = ua[b.mipmap];
                      c = a.genMipmaps = !0;
                      break;
                    case "boolean":
                      c = a.genMipmaps = b.mipmap;
                      break;
                    case "object":
                      (a.genMipmaps = !1), (c = !0);
                  }
                  !c || "min" in b || (a.minFilter = 9984);
                }
              }
              function R(c, d) {
                a.texParameteri(d, 10241, c.minFilter);
                a.texParameteri(d, 10240, c.magFilter);
                a.texParameteri(d, 10242, c.wrapS);
                a.texParameteri(d, 10243, c.wrapT);
                b.ext_texture_filter_anisotropic &&
                  a.texParameteri(d, 34046, c.anisotropic);
                c.genMipmaps &&
                  (a.hint(33170, c.mipmapHint), a.generateMipmap(d));
              }
              function F(b) {
                f.call(this);
                this.mipmask = 0;
                this.internalformat = 6408;
                this.id = ya++;
                this.refCount = 1;
                this.target = b;
                this.texture = a.createTexture();
                this.unit = -1;
                this.bindCount = 0;
                this.texInfo = new y();
                n.profile && (this.stats = { size: 0 });
              }
              function T(b) {
                a.activeTexture(33984);
                a.bindTexture(b.target, b.texture);
              }
              function Aa() {
                var b = ha[0];
                b
                  ? a.bindTexture(b.target, b.texture)
                  : a.bindTexture(3553, null);
              }
              function A(b) {
                var c = b.texture,
                  e = b.unit,
                  h = b.target;
                0 <= e &&
                  (a.activeTexture(33984 + e),
                  a.bindTexture(h, null),
                  (ha[e] = null));
                a.deleteTexture(c);
                b.texture = null;
                b.params = null;
                b.pixels = null;
                b.refCount = 0;
                delete X[b.id];
                d.textureCount--;
              }
              var ua = {
                  "don't care": 4352,
                  "dont care": 4352,
                  nice: 4354,
                  fast: 4353,
                },
                K = { repeat: 10497, clamp: 33071, mirror: 33648 },
                V = { nearest: 9728, linear: 9729 },
                Va = E(
                  {
                    mipmap: 9987,
                    "nearest mipmap nearest": 9984,
                    "linear mipmap nearest": 9985,
                    "nearest mipmap linear": 9986,
                    "linear mipmap linear": 9987,
                  },
                  V
                ),
                wa = { none: 0, browser: 37444 },
                G = {
                  uint8: 5121,
                  rgba4: 32819,
                  rgb565: 33635,
                  "rgb5 a1": 32820,
                },
                U = {
                  alpha: 6406,
                  luminance: 6409,
                  "luminance alpha": 6410,
                  rgb: 6407,
                  rgba: 6408,
                  rgba4: 32854,
                  "rgb5 a1": 32855,
                  rgb565: 36194,
                },
                W = {};
              b.ext_srgb && ((U.srgb = 35904), (U.srgba = 35906));
              b.oes_texture_float && (G.float32 = G["float"] = 5126);
              b.oes_texture_half_float && (G.float16 = G["half float"] = 36193);
              b.webgl_depth_texture &&
                (E(U, { depth: 6402, "depth stencil": 34041 }),
                E(G, { uint16: 5123, uint32: 5125, "depth stencil": 34042 }));
              b.webgl_compressed_texture_s3tc &&
                E(W, {
                  "rgb s3tc dxt1": 33776,
                  "rgba s3tc dxt1": 33777,
                  "rgba s3tc dxt3": 33778,
                  "rgba s3tc dxt5": 33779,
                });
              b.webgl_compressed_texture_atc &&
                E(W, {
                  "rgb atc": 35986,
                  "rgba atc explicit alpha": 35987,
                  "rgba atc interpolated alpha": 34798,
                });
              b.webgl_compressed_texture_pvrtc &&
                E(W, {
                  "rgb pvrtc 4bppv1": 35840,
                  "rgb pvrtc 2bppv1": 35841,
                  "rgba pvrtc 4bppv1": 35842,
                  "rgba pvrtc 2bppv1": 35843,
                });
              b.webgl_compressed_texture_etc1 && (W["rgb etc1"] = 36196);
              var Nb = Array.prototype.slice.call(a.getParameter(34467));
              Object.keys(W).forEach(function (a) {
                var b = W[a];
                0 <= Nb.indexOf(b) && (U[a] = b);
              });
              var ca = Object.keys(U);
              c.textureFormats = ca;
              var J = [];
              Object.keys(U).forEach(function (a) {
                J[U[a]] = a;
              });
              var da = [];
              Object.keys(G).forEach(function (a) {
                da[G[a]] = a;
              });
              var oa = [];
              Object.keys(V).forEach(function (a) {
                oa[V[a]] = a;
              });
              var za = [];
              Object.keys(Va).forEach(function (a) {
                za[Va[a]] = a;
              });
              var ka = [];
              Object.keys(K).forEach(function (a) {
                ka[K[a]] = a;
              });
              var Lb = ca.reduce(function (a, b) {
                  var c = U[b];
                  6409 === c ||
                  6406 === c ||
                  6409 === c ||
                  6410 === c ||
                  6402 === c ||
                  34041 === c
                    ? (a[c] = c)
                    : 32855 === c || 0 <= b.indexOf("rgba")
                    ? (a[c] = 6408)
                    : (a[c] = 6407);
                  return a;
                }, {}),
                P = [],
                tb = [],
                ya = 0,
                X = {},
                ea = c.maxTextureUnits,
                ha = Array(ea).map(function () {
                  return null;
                });
              E(F.prototype, {
                bind: function () {
                  this.bindCount += 1;
                  var b = this.unit;
                  if (0 > b) {
                    for (var c = 0; c < ea; ++c) {
                      var e = ha[c];
                      if (e) {
                        if (0 < e.bindCount) continue;
                        e.unit = -1;
                      }
                      ha[c] = this;
                      b = c;
                      break;
                    }
                    n.profile &&
                      d.maxTextureUnits < b + 1 &&
                      (d.maxTextureUnits = b + 1);
                    this.unit = b;
                    a.activeTexture(33984 + b);
                    a.bindTexture(this.target, this.texture);
                  }
                  return b;
                },
                unbind: function () {
                  --this.bindCount;
                },
                decRef: function () {
                  0 >= --this.refCount && A(this);
                },
              });
              n.profile &&
                (d.getTotalTextureSize = function () {
                  var a = 0;
                  Object.keys(X).forEach(function (b) {
                    a += X[b].stats.size;
                  });
                  return a;
                });
              return {
                create2D: function (b, c) {
                  function e(a, b) {
                    var c = f.texInfo;
                    y.call(c);
                    var d = D();
                    "number" === typeof a
                      ? "number" === typeof b
                        ? v(d, a | 0, b | 0)
                        : v(d, a | 0, a | 0)
                      : a
                      ? (O(c, a), N(d, a))
                      : v(d, 1, 1);
                    c.genMipmaps && (d.mipmask = (d.width << 1) - 1);
                    f.mipmask = d.mipmask;
                    r(f, d);
                    f.internalformat = d.internalformat;
                    e.width = d.width;
                    e.height = d.height;
                    T(f);
                    B(d, 3553);
                    R(c, 3553);
                    Aa();
                    ib(d);
                    n.profile &&
                      (f.stats.size = Ja(
                        f.internalformat,
                        f.type,
                        d.width,
                        d.height,
                        c.genMipmaps,
                        !1
                      ));
                    e.format = J[f.internalformat];
                    e.type = da[f.type];
                    e.mag = oa[c.magFilter];
                    e.min = za[c.minFilter];
                    e.wrapS = ka[c.wrapS];
                    e.wrapT = ka[c.wrapT];
                    return e;
                  }
                  var f = new F(3553);
                  X[f.id] = f;
                  d.textureCount++;
                  e(b, c);
                  e.subimage = function (a, b, c, d) {
                    b |= 0;
                    c |= 0;
                    d |= 0;
                    var p = h();
                    r(p, f);
                    p.width = 0;
                    p.height = 0;
                    C(p, a);
                    p.width = p.width || (f.width >> d) - b;
                    p.height = p.height || (f.height >> d) - c;
                    T(f);
                    k(p, 3553, b, c, d);
                    Aa();
                    l(p);
                    return e;
                  };
                  e.resize = function (b, c) {
                    var d = b | 0,
                      h = c | 0 || d;
                    if (d === f.width && h === f.height) return e;
                    e.width = f.width = d;
                    e.height = f.height = h;
                    T(f);
                    for (
                      var p, w = f.channels, z = f.type, I = 0;
                      f.mipmask >> I;
                      ++I
                    ) {
                      var fa = d >> I,
                        ga = h >> I;
                      if (!fa || !ga) break;
                      p = x.zero.allocType(z, fa * ga * w);
                      a.texImage2D(
                        3553,
                        I,
                        f.format,
                        fa,
                        ga,
                        0,
                        f.format,
                        f.type,
                        p
                      );
                      p && x.zero.freeType(p);
                    }
                    Aa();
                    n.profile &&
                      (f.stats.size = Ja(
                        f.internalformat,
                        f.type,
                        d,
                        h,
                        !1,
                        !1
                      ));
                    return e;
                  };
                  e._reglType = "texture2d";
                  e._texture = f;
                  n.profile && (e.stats = f.stats);
                  e.destroy = function () {
                    f.decRef();
                  };
                  return e;
                },
                createCube: function (b, c, e, f, g, ua) {
                  function A(a, b, c, d, e, f) {
                    var H,
                      Y = m.texInfo;
                    y.call(Y);
                    for (H = 0; 6 > H; ++H) p[H] = D();
                    if ("number" === typeof a || !a)
                      for (a = a | 0 || 1, H = 0; 6 > H; ++H) v(p[H], a, a);
                    else if ("object" === typeof a)
                      if (b)
                        N(p[0], a),
                          N(p[1], b),
                          N(p[2], c),
                          N(p[3], d),
                          N(p[4], e),
                          N(p[5], f);
                      else if ((O(Y, a), q(m, a), "faces" in a))
                        for (a = a.faces, H = 0; 6 > H; ++H)
                          r(p[H], m), N(p[H], a[H]);
                      else for (H = 0; 6 > H; ++H) N(p[H], a);
                    r(m, p[0]);
                    m.mipmask = Y.genMipmaps
                      ? (p[0].width << 1) - 1
                      : p[0].mipmask;
                    m.internalformat = p[0].internalformat;
                    A.width = p[0].width;
                    A.height = p[0].height;
                    T(m);
                    for (H = 0; 6 > H; ++H) B(p[H], 34069 + H);
                    R(Y, 34067);
                    Aa();
                    n.profile &&
                      (m.stats.size = Ja(
                        m.internalformat,
                        m.type,
                        A.width,
                        A.height,
                        Y.genMipmaps,
                        !0
                      ));
                    A.format = J[m.internalformat];
                    A.type = da[m.type];
                    A.mag = oa[Y.magFilter];
                    A.min = za[Y.minFilter];
                    A.wrapS = ka[Y.wrapS];
                    A.wrapT = ka[Y.wrapT];
                    for (H = 0; 6 > H; ++H) ib(p[H]);
                    return A;
                  }
                  var m = new F(34067);
                  X[m.id] = m;
                  d.cubeCount++;
                  var p = Array(6);
                  A(b, c, e, f, g, ua);
                  A.subimage = function (a, b, c, p, d) {
                    c |= 0;
                    p |= 0;
                    d |= 0;
                    var e = h();
                    r(e, m);
                    e.width = 0;
                    e.height = 0;
                    C(e, b);
                    e.width = e.width || (m.width >> d) - c;
                    e.height = e.height || (m.height >> d) - p;
                    T(m);
                    k(e, 34069 + a, c, p, d);
                    Aa();
                    l(e);
                    return A;
                  };
                  A.resize = function (b) {
                    b |= 0;
                    if (b !== m.width) {
                      A.width = m.width = b;
                      A.height = m.height = b;
                      T(m);
                      for (var c = 0; 6 > c; ++c)
                        for (var p = 0; m.mipmask >> p; ++p)
                          a.texImage2D(
                            34069 + c,
                            p,
                            m.format,
                            b >> p,
                            b >> p,
                            0,
                            m.format,
                            m.type,
                            null
                          );
                      Aa();
                      n.profile &&
                        (m.stats.size = Ja(
                          m.internalformat,
                          m.type,
                          A.width,
                          A.height,
                          !1,
                          !0
                        ));
                      return A;
                    }
                  };
                  A._reglType = "textureCube";
                  A._texture = m;
                  n.profile && (A.stats = m.stats);
                  A.destroy = function () {
                    m.decRef();
                  };
                  return A;
                },
                clear: function () {
                  for (var b = 0; b < ea; ++b)
                    a.activeTexture(33984 + b),
                      a.bindTexture(3553, null),
                      (ha[b] = null);
                  S(X).forEach(A);
                  d.cubeCount = 0;
                  d.textureCount = 0;
                },
                getTexture: function (a) {
                  return null;
                },
                restore: function () {
                  for (var b = 0; b < ea; ++b) {
                    var c = ha[b];
                    c && ((c.bindCount = 0), (c.unit = -1), (ha[b] = null));
                  }
                  S(X).forEach(function (b) {
                    b.texture = a.createTexture();
                    a.bindTexture(b.target, b.texture);
                    for (var c = 0; 32 > c; ++c)
                      if (0 !== (b.mipmask & (1 << c)))
                        if (3553 === b.target)
                          a.texImage2D(
                            3553,
                            c,
                            b.internalformat,
                            b.width >> c,
                            b.height >> c,
                            0,
                            b.internalformat,
                            b.type,
                            null
                          );
                        else
                          for (var d = 0; 6 > d; ++d)
                            a.texImage2D(
                              34069 + d,
                              c,
                              b.internalformat,
                              b.width >> c,
                              b.height >> c,
                              0,
                              b.internalformat,
                              b.type,
                              null
                            );
                    R(b.texInfo, b.target);
                  });
                },
              };
            }
            function Ob(a, b, c, e, g, d) {
              function n(a, b, c) {
                this.target = a;
                this.texture = b;
                this.renderbuffer = c;
                var d = (a = 0);
                b
                  ? ((a = b.width), (d = b.height))
                  : c && ((a = c.width), (d = c.height));
                this.width = a;
                this.height = d;
              }
              function f(a) {
                a &&
                  (a.texture && a.texture._texture.decRef(),
                  a.renderbuffer && a.renderbuffer._renderbuffer.decRef());
              }
              function r(a, b, c) {
                a &&
                  (a.texture
                    ? (a.texture._texture.refCount += 1)
                    : (a.renderbuffer._renderbuffer.refCount += 1));
              }
              function q(b, c) {
                c &&
                  (c.texture
                    ? a.framebufferTexture2D(
                        36160,
                        b,
                        c.target,
                        c.texture._texture.texture,
                        0
                      )
                    : a.framebufferRenderbuffer(
                        36160,
                        b,
                        36161,
                        c.renderbuffer._renderbuffer.renderbuffer
                      ));
              }
              function t(a) {
                var b = 3553,
                  c = null,
                  d = null,
                  e = a;
                "object" === typeof a &&
                  ((e = a.data), "target" in a && (b = a.target | 0));
                a = e._reglType;
                "texture2d" === a
                  ? (c = e)
                  : "textureCube" === a
                  ? (c = e)
                  : "renderbuffer" === a && ((d = e), (b = 36161));
                return new n(b, c, d);
              }
              function m(a, b, c, d, f) {
                if (c)
                  return (
                    (a = e.create2D({
                      width: a,
                      height: b,
                      format: d,
                      type: f,
                    })),
                    (a._texture.refCount = 0),
                    new n(3553, a, null)
                  );
                a = g.create({ width: a, height: b, format: d });
                a._renderbuffer.refCount = 0;
                return new n(36161, null, a);
              }
              function C(a) {
                return a && (a.texture || a.renderbuffer);
              }
              function k(a, b, c) {
                a &&
                  (a.texture
                    ? a.texture.resize(b, c)
                    : a.renderbuffer && a.renderbuffer.resize(b, c),
                  (a.width = b),
                  (a.height = c));
              }
              function h() {
                this.id = O++;
                R[this.id] = this;
                this.framebuffer = a.createFramebuffer();
                this.height = this.width = 0;
                this.colorAttachments = [];
                this.depthStencilAttachment =
                  this.stencilAttachment =
                  this.depthAttachment =
                    null;
              }
              function l(a) {
                a.colorAttachments.forEach(f);
                f(a.depthAttachment);
                f(a.stencilAttachment);
                f(a.depthStencilAttachment);
              }
              function u(b) {
                a.deleteFramebuffer(b.framebuffer);
                b.framebuffer = null;
                d.framebufferCount--;
                delete R[b.id];
              }
              function v(b) {
                var d;
                a.bindFramebuffer(36160, b.framebuffer);
                var e = b.colorAttachments;
                for (d = 0; d < e.length; ++d) q(36064 + d, e[d]);
                for (d = e.length; d < c.maxColorAttachments; ++d)
                  a.framebufferTexture2D(36160, 36064 + d, 3553, null, 0);
                a.framebufferTexture2D(36160, 33306, 3553, null, 0);
                a.framebufferTexture2D(36160, 36096, 3553, null, 0);
                a.framebufferTexture2D(36160, 36128, 3553, null, 0);
                q(36096, b.depthAttachment);
                q(36128, b.stencilAttachment);
                q(33306, b.depthStencilAttachment);
                a.checkFramebufferStatus(36160);
                a.isContextLost();
                a.bindFramebuffer(36160, B.next ? B.next.framebuffer : null);
                B.cur = B.next;
                a.getError();
              }
              function N(a, b) {
                function c(a, b) {
                  var d,
                    f = 0,
                    h = 0,
                    g = !0,
                    k = !0;
                  d = null;
                  var q = !0,
                    u = "rgba",
                    n = "uint8",
                    N = 1,
                    da = null,
                    oa = null,
                    B = null,
                    ka = !1;
                  if ("number" === typeof a) (f = a | 0), (h = b | 0 || f);
                  else if (a) {
                    "shape" in a
                      ? ((h = a.shape), (f = h[0]), (h = h[1]))
                      : ("radius" in a && (f = h = a.radius),
                        "width" in a && (f = a.width),
                        "height" in a && (h = a.height));
                    if ("color" in a || "colors" in a)
                      (d = a.color || a.colors), Array.isArray(d);
                    if (!d) {
                      "colorCount" in a && (N = a.colorCount | 0);
                      "colorTexture" in a &&
                        ((q = !!a.colorTexture), (u = "rgba4"));
                      if ("colorType" in a && ((n = a.colorType), !q))
                        if ("half float" === n || "float16" === n)
                          u = "rgba16f";
                        else if ("float" === n || "float32" === n)
                          u = "rgba32f";
                      "colorFormat" in a &&
                        ((u = a.colorFormat),
                        0 <= x.indexOf(u)
                          ? (q = !0)
                          : 0 <= D.indexOf(u) && (q = !1));
                    }
                    if ("depthTexture" in a || "depthStencilTexture" in a)
                      ka = !(!a.depthTexture && !a.depthStencilTexture);
                    "depth" in a &&
                      ("boolean" === typeof a.depth
                        ? (g = a.depth)
                        : ((da = a.depth), (k = !1)));
                    "stencil" in a &&
                      ("boolean" === typeof a.stencil
                        ? (k = a.stencil)
                        : ((oa = a.stencil), (g = !1)));
                    "depthStencil" in a &&
                      ("boolean" === typeof a.depthStencil
                        ? (g = k = a.depthStencil)
                        : ((B = a.depthStencil), (k = g = !1)));
                  } else f = h = 1;
                  var F = null,
                    y = null,
                    E = null,
                    T = null;
                  if (Array.isArray(d)) F = d.map(t);
                  else if (d) F = [t(d)];
                  else
                    for (F = Array(N), d = 0; d < N; ++d)
                      F[d] = m(f, h, q, u, n);
                  f = f || F[0].width;
                  h = h || F[0].height;
                  da
                    ? (y = t(da))
                    : g && !k && (y = m(f, h, ka, "depth", "uint32"));
                  oa
                    ? (E = t(oa))
                    : k && !g && (E = m(f, h, !1, "stencil", "uint8"));
                  B
                    ? (T = t(B))
                    : !da &&
                      !oa &&
                      k &&
                      g &&
                      (T = m(f, h, ka, "depth stencil", "depth stencil"));
                  g = null;
                  for (d = 0; d < F.length; ++d)
                    r(F[d], f, h),
                      F[d] &&
                        F[d].texture &&
                        ((k =
                          Wa[F[d].texture._texture.format] *
                          Na[F[d].texture._texture.type]),
                        null === g && (g = k));
                  r(y, f, h);
                  r(E, f, h);
                  r(T, f, h);
                  l(e);
                  e.width = f;
                  e.height = h;
                  e.colorAttachments = F;
                  e.depthAttachment = y;
                  e.stencilAttachment = E;
                  e.depthStencilAttachment = T;
                  c.color = F.map(C);
                  c.depth = C(y);
                  c.stencil = C(E);
                  c.depthStencil = C(T);
                  c.width = e.width;
                  c.height = e.height;
                  v(e);
                  return c;
                }
                var e = new h();
                d.framebufferCount++;
                c(a, b);
                return E(c, {
                  resize: function (a, b) {
                    var d = Math.max(a | 0, 1),
                      f = Math.max(b | 0 || d, 1);
                    if (d === e.width && f === e.height) return c;
                    for (var h = e.colorAttachments, g = 0; g < h.length; ++g)
                      k(h[g], d, f);
                    k(e.depthAttachment, d, f);
                    k(e.stencilAttachment, d, f);
                    k(e.depthStencilAttachment, d, f);
                    e.width = c.width = d;
                    e.height = c.height = f;
                    v(e);
                    return c;
                  },
                  _reglType: "framebuffer",
                  _framebuffer: e,
                  destroy: function () {
                    u(e);
                    l(e);
                  },
                  use: function (a) {
                    B.setFBO({ framebuffer: c }, a);
                  },
                });
              }
              var B = { cur: null, next: null, dirty: !1, setFBO: null },
                x = ["rgba"],
                D = ["rgba4", "rgb565", "rgb5 a1"];
              b.ext_srgb && D.push("srgba");
              b.ext_color_buffer_half_float && D.push("rgba16f", "rgb16f");
              b.webgl_color_buffer_float && D.push("rgba32f");
              var y = ["uint8"];
              b.oes_texture_half_float && y.push("half float", "float16");
              b.oes_texture_float && y.push("float", "float32");
              var O = 0,
                R = {};
              return E(B, {
                getFramebuffer: function (a) {
                  return "function" === typeof a &&
                    "framebuffer" === a._reglType &&
                    ((a = a._framebuffer), a instanceof h)
                    ? a
                    : null;
                },
                create: N,
                createCube: function (a) {
                  function b(a) {
                    var d,
                      f = { color: null },
                      h = 0,
                      g = null;
                    d = "rgba";
                    var l = "uint8",
                      m = 1;
                    if ("number" === typeof a) h = a | 0;
                    else if (a) {
                      "shape" in a
                        ? (h = a.shape[0])
                        : ("radius" in a && (h = a.radius | 0),
                          "width" in a
                            ? (h = a.width | 0)
                            : "height" in a && (h = a.height | 0));
                      if ("color" in a || "colors" in a)
                        (g = a.color || a.colors), Array.isArray(g);
                      g ||
                        ("colorCount" in a && (m = a.colorCount | 0),
                        "colorType" in a && (l = a.colorType),
                        "colorFormat" in a && (d = a.colorFormat));
                      "depth" in a && (f.depth = a.depth);
                      "stencil" in a && (f.stencil = a.stencil);
                      "depthStencil" in a && (f.depthStencil = a.depthStencil);
                    } else h = 1;
                    if (g)
                      if (Array.isArray(g))
                        for (a = [], d = 0; d < g.length; ++d) a[d] = g[d];
                      else a = [g];
                    else
                      for (
                        a = Array(m),
                          g = { radius: h, format: d, type: l },
                          d = 0;
                        d < m;
                        ++d
                      )
                        a[d] = e.createCube(g);
                    f.color = Array(a.length);
                    for (d = 0; d < a.length; ++d)
                      (m = a[d]),
                        (h = h || m.width),
                        (f.color[d] = { target: 34069, data: a[d] });
                    for (d = 0; 6 > d; ++d) {
                      for (m = 0; m < a.length; ++m)
                        f.color[m].target = 34069 + d;
                      0 < d &&
                        ((f.depth = c[0].depth),
                        (f.stencil = c[0].stencil),
                        (f.depthStencil = c[0].depthStencil));
                      if (c[d]) c[d](f);
                      else c[d] = N(f);
                    }
                    return E(b, { width: h, height: h, color: a });
                  }
                  var c = Array(6);
                  b(a);
                  return E(b, {
                    faces: c,
                    resize: function (a) {
                      var d = a | 0;
                      if (d === b.width) return b;
                      var e = b.color;
                      for (a = 0; a < e.length; ++a) e[a].resize(d);
                      for (a = 0; 6 > a; ++a) c[a].resize(d);
                      b.width = b.height = d;
                      return b;
                    },
                    _reglType: "framebufferCube",
                    destroy: function () {
                      c.forEach(function (a) {
                        a.destroy();
                      });
                    },
                  });
                },
                clear: function () {
                  S(R).forEach(u);
                },
                restore: function () {
                  B.cur = null;
                  B.next = null;
                  B.dirty = !0;
                  S(R).forEach(function (b) {
                    b.framebuffer = a.createFramebuffer();
                    v(b);
                  });
                },
              });
            }
            function ub() {
              this.w = this.z = this.y = this.x = this.state = 0;
              this.buffer = null;
              this.size = 0;
              this.normalized = !1;
              this.type = 5126;
              this.divisor = this.stride = this.offset = 0;
            }
            function Pb(a, b, c, e) {
              a = c.maxAttributes;
              b = Array(a);
              for (c = 0; c < a; ++c) b[c] = new ub();
              return { Record: ub, scope: {}, state: b };
            }
            function Qb(a, b, c, e) {
              function g(a, b, c, d) {
                this.name = a;
                this.id = b;
                this.location = c;
                this.info = d;
              }
              function d(a, b) {
                for (var c = 0; c < a.length; ++c)
                  if (a[c].id === b.id) {
                    a[c].location = b.location;
                    return;
                  }
                a.push(b);
              }
              function n(c, d, e) {
                e = 35632 === c ? q : t;
                var f = e[d];
                if (!f) {
                  var g = b.str(d),
                    f = a.createShader(c);
                  a.shaderSource(f, g);
                  a.compileShader(f);
                  e[d] = f;
                }
                return f;
              }
              function f(a, b) {
                this.id = k++;
                this.fragId = a;
                this.vertId = b;
                this.program = null;
                this.uniforms = [];
                this.attributes = [];
                e.profile &&
                  (this.stats = { uniformsCount: 0, attributesCount: 0 });
              }
              function r(c, f) {
                var m, k;
                m = n(35632, c.fragId);
                k = n(35633, c.vertId);
                var q = (c.program = a.createProgram());
                a.attachShader(q, m);
                a.attachShader(q, k);
                a.linkProgram(q);
                var r = a.getProgramParameter(q, 35718);
                e.profile && (c.stats.uniformsCount = r);
                var t = c.uniforms;
                for (m = 0; m < r; ++m)
                  if ((k = a.getActiveUniform(q, m)))
                    if (1 < k.size)
                      for (var C = 0; C < k.size; ++C) {
                        var y = k.name.replace("[0]", "[" + C + "]");
                        d(t, new g(y, b.id(y), a.getUniformLocation(q, y), k));
                      }
                    else
                      d(
                        t,
                        new g(
                          k.name,
                          b.id(k.name),
                          a.getUniformLocation(q, k.name),
                          k
                        )
                      );
                r = a.getProgramParameter(q, 35721);
                e.profile && (c.stats.attributesCount = r);
                t = c.attributes;
                for (m = 0; m < r; ++m)
                  (k = a.getActiveAttrib(q, m)) &&
                    d(
                      t,
                      new g(
                        k.name,
                        b.id(k.name),
                        a.getAttribLocation(q, k.name),
                        k
                      )
                    );
              }
              var q = {},
                t = {},
                m = {},
                C = [],
                k = 0;
              e.profile &&
                ((c.getMaxUniformsCount = function () {
                  var a = 0;
                  C.forEach(function (b) {
                    b.stats.uniformsCount > a && (a = b.stats.uniformsCount);
                  });
                  return a;
                }),
                (c.getMaxAttributesCount = function () {
                  var a = 0;
                  C.forEach(function (b) {
                    b.stats.attributesCount > a &&
                      (a = b.stats.attributesCount);
                  });
                  return a;
                }));
              return {
                clear: function () {
                  var b = a.deleteShader.bind(a);
                  S(q).forEach(b);
                  q = {};
                  S(t).forEach(b);
                  t = {};
                  C.forEach(function (b) {
                    a.deleteProgram(b.program);
                  });
                  C.length = 0;
                  m = {};
                  c.shaderCount = 0;
                },
                program: function (a, b, d) {
                  var e = m[b];
                  e || (e = m[b] = {});
                  var g = e[a];
                  g ||
                    ((g = new f(b, a)),
                    c.shaderCount++,
                    r(g, d),
                    (e[a] = g),
                    C.push(g));
                  return g;
                },
                restore: function () {
                  q = {};
                  t = {};
                  for (var a = 0; a < C.length; ++a) r(C[a]);
                },
                shader: n,
                frag: -1,
                vert: -1,
              };
            }
            function Rb(a, b, c, e, g, d, n) {
              function f(d) {
                var f;
                f =
                  null === b.next
                    ? 5121
                    : b.next.colorAttachments[0].texture._texture.type;
                var g = 0,
                  r = 0,
                  k = e.framebufferWidth,
                  h = e.framebufferHeight,
                  l = null;
                M(d)
                  ? (l = d)
                  : d &&
                    ((g = d.x | 0),
                    (r = d.y | 0),
                    (k = (d.width || e.framebufferWidth - g) | 0),
                    (h = (d.height || e.framebufferHeight - r) | 0),
                    (l = d.data || null));
                c();
                d = k * h * 4;
                l ||
                  (5121 === f
                    ? (l = new Uint8Array(d))
                    : 5126 === f && (l = l || new Float32Array(d)));
                a.pixelStorei(3333, 4);
                a.readPixels(g, r, k, h, 6408, f, l);
                return l;
              }
              function r(a) {
                var c;
                b.setFBO({ framebuffer: a.framebuffer }, function () {
                  c = f(a);
                });
                return c;
              }
              return function (a) {
                return a && "framebuffer" in a ? r(a) : f(a);
              };
            }
            function Ba(a) {
              return Array.prototype.slice.call(a);
            }
            function Ca(a) {
              return Ba(a).join("");
            }
            function Sb() {
              function a() {
                var a = [],
                  b = [];
                return E(
                  function () {
                    a.push.apply(a, Ba(arguments));
                  },
                  {
                    def: function () {
                      var d = "v" + c++;
                      b.push(d);
                      0 < arguments.length &&
                        (a.push(d, "="),
                        a.push.apply(a, Ba(arguments)),
                        a.push(";"));
                      return d;
                    },
                    toString: function () {
                      return Ca([0 < b.length ? "var " + b + ";" : "", Ca(a)]);
                    },
                  }
                );
              }
              function b() {
                function b(a, e) {
                  d(a, e, "=", c.def(a, e), ";");
                }
                var c = a(),
                  d = a(),
                  e = c.toString,
                  g = d.toString;
                return E(
                  function () {
                    c.apply(c, Ba(arguments));
                  },
                  {
                    def: c.def,
                    entry: c,
                    exit: d,
                    save: b,
                    set: function (a, d, e) {
                      b(a, d);
                      c(a, d, "=", e, ";");
                    },
                    toString: function () {
                      return e() + g();
                    },
                  }
                );
              }
              var c = 0,
                e = [],
                g = [],
                d = a(),
                n = {};
              return {
                global: d,
                link: function (a) {
                  for (var b = 0; b < g.length; ++b)
                    if (g[b] === a) return e[b];
                  b = "g" + c++;
                  e.push(b);
                  g.push(a);
                  return b;
                },
                block: a,
                proc: function (a, c) {
                  function d() {
                    var a = "a" + e.length;
                    e.push(a);
                    return a;
                  }
                  var e = [];
                  c = c || 0;
                  for (var g = 0; g < c; ++g) d();
                  var g = b(),
                    C = g.toString;
                  return (n[a] = E(g, {
                    arg: d,
                    toString: function () {
                      return Ca(["function(", e.join(), "){", C(), "}"]);
                    },
                  }));
                },
                scope: b,
                cond: function () {
                  var a = Ca(arguments),
                    c = b(),
                    d = b(),
                    e = c.toString,
                    g = d.toString;
                  return E(c, {
                    then: function () {
                      c.apply(c, Ba(arguments));
                      return this;
                    },
                    else: function () {
                      d.apply(d, Ba(arguments));
                      return this;
                    },
                    toString: function () {
                      var b = g();
                      b && (b = "else{" + b + "}");
                      return Ca(["if(", a, "){", e(), "}", b]);
                    },
                  });
                },
                compile: function () {
                  var a = ['"use strict";', d, "return {"];
                  Object.keys(n).forEach(function (b) {
                    a.push('"', b, '":', n[b].toString(), ",");
                  });
                  a.push("}");
                  var b = Ca(a)
                    .replace(/;/g, ";\n")
                    .replace(/}/g, "}\n")
                    .replace(/{/g, "{\n");
                  return Function.apply(null, e.concat(b)).apply(null, g);
                },
              };
            }
            function Oa(a) {
              return Array.isArray(a) || M(a) || ma(a);
            }
            function vb(a) {
              return a.sort(function (a, c) {
                return "viewport" === a
                  ? -1
                  : "viewport" === c
                  ? 1
                  : a < c
                  ? -1
                  : 1;
              });
            }
            function Z(a, b, c, e) {
              this.thisDep = a;
              this.contextDep = b;
              this.propDep = c;
              this.append = e;
            }
            function va(a) {
              return a && !(a.thisDep || a.contextDep || a.propDep);
            }
            function D(a) {
              return new Z(!1, !1, !1, a);
            }
            function P(a, b) {
              var c = a.type;
              return 0 === c
                ? ((c = a.data.length), new Z(!0, 1 <= c, 2 <= c, b))
                : 4 === c
                ? ((c = a.data), new Z(c.thisDep, c.contextDep, c.propDep, b))
                : new Z(3 === c, 2 === c, 1 === c, b);
            }
            function Tb(a, b, c, e, g, d, n, f, r, q, t, m, C, k, h) {
              function l(a) {
                return a.replace(".", "_");
              }
              function u(a, b, c) {
                var d = l(a);
                Ka.push(a);
                Fa[d] = ra[d] = !!c;
                sa[d] = b;
              }
              function v(a, b, c) {
                var d = l(a);
                Ka.push(a);
                Array.isArray(c)
                  ? ((ra[d] = c.slice()), (Fa[d] = c.slice()))
                  : (ra[d] = Fa[d] = c);
                ta[d] = b;
              }
              function N() {
                var a = Sb(),
                  c = a.link,
                  d = a.global;
                a.id = qa++;
                a.batchId = "0";
                var e = c(na),
                  f = (a.shared = { props: "a0" });
                Object.keys(na).forEach(function (a) {
                  f[a] = d.def(e, ".", a);
                });
                var g = (a.next = {}),
                  xa = (a.current = {});
                Object.keys(ta).forEach(function (a) {
                  Array.isArray(ra[a]) &&
                    ((g[a] = d.def(f.next, ".", a)),
                    (xa[a] = d.def(f.current, ".", a)));
                });
                var H = (a.constants = {});
                Object.keys(aa).forEach(function (a) {
                  H[a] = d.def(JSON.stringify(aa[a]));
                });
                a.invoke = function (b, d) {
                  switch (d.type) {
                    case 0:
                      var e = ["this", f.context, f.props, a.batchId];
                      return b.def(
                        c(d.data),
                        ".call(",
                        e.slice(0, Math.max(d.data.length + 1, 4)),
                        ")"
                      );
                    case 1:
                      return b.def(f.props, d.data);
                    case 2:
                      return b.def(f.context, d.data);
                    case 3:
                      return b.def("this", d.data);
                    case 4:
                      return d.data.append(a, b), d.data.ref;
                  }
                };
                a.attribCache = {};
                var Y = {};
                a.scopeAttrib = function (a) {
                  a = b.id(a);
                  if (a in Y) return Y[a];
                  var d = q.scope[a];
                  d || (d = q.scope[a] = new ya());
                  return (Y[a] = c(d));
                };
                return a;
              }
              function B(a) {
                var b = a["static"];
                a = a.dynamic;
                var c;
                if ("profile" in b) {
                  var d = !!b.profile;
                  c = D(function (a, b) {
                    return d;
                  });
                  c.enable = d;
                } else if ("profile" in a) {
                  var e = a.profile;
                  c = P(e, function (a, b) {
                    return a.invoke(b, e);
                  });
                }
                return c;
              }
              function y(a, b) {
                var c = a["static"],
                  d = a.dynamic;
                if ("framebuffer" in c) {
                  var e = c.framebuffer;
                  return e
                    ? ((e = f.getFramebuffer(e)),
                      D(function (a, b) {
                        var c = a.link(e),
                          d = a.shared;
                        b.set(d.framebuffer, ".next", c);
                        d = d.context;
                        b.set(d, ".framebufferWidth", c + ".width");
                        b.set(d, ".framebufferHeight", c + ".height");
                        return c;
                      }))
                    : D(function (a, b) {
                        var c = a.shared;
                        b.set(c.framebuffer, ".next", "null");
                        c = c.context;
                        b.set(
                          c,
                          ".framebufferWidth",
                          c + ".drawingBufferWidth"
                        );
                        b.set(
                          c,
                          ".framebufferHeight",
                          c + ".drawingBufferHeight"
                        );
                        return "null";
                      });
                }
                if ("framebuffer" in d) {
                  var g = d.framebuffer;
                  return P(g, function (a, b) {
                    var c = a.invoke(b, g),
                      d = a.shared,
                      e = d.framebuffer,
                      c = b.def(e, ".getFramebuffer(", c, ")");
                    b.set(e, ".next", c);
                    d = d.context;
                    b.set(
                      d,
                      ".framebufferWidth",
                      c + "?" + c + ".width:" + d + ".drawingBufferWidth"
                    );
                    b.set(
                      d,
                      ".framebufferHeight",
                      c + "?" + c + ".height:" + d + ".drawingBufferHeight"
                    );
                    return c;
                  });
                }
                return null;
              }
              function x(a, b, c) {
                function d(a) {
                  if (a in e) {
                    var c = e[a];
                    a = !0;
                    var p = c.x | 0,
                      ba = c.y | 0,
                      g,
                      h;
                    "width" in c ? (g = c.width | 0) : (a = !1);
                    "height" in c ? (h = c.height | 0) : (a = !1);
                    return new Z(
                      !a && b && b.thisDep,
                      !a && b && b.contextDep,
                      !a && b && b.propDep,
                      function (a, b) {
                        var d = a.shared.context,
                          e = g;
                        "width" in c ||
                          (e = b.def(d, ".", "framebufferWidth", "-", p));
                        var f = h;
                        "height" in c ||
                          (f = b.def(d, ".", "framebufferHeight", "-", ba));
                        return [p, ba, e, f];
                      }
                    );
                  }
                  if (a in f) {
                    var z = f[a];
                    a = P(z, function (a, b) {
                      var c = a.invoke(b, z),
                        d = a.shared.context,
                        e = b.def(c, ".x|0"),
                        p = b.def(c, ".y|0"),
                        Y = b.def(
                          '"width" in ',
                          c,
                          "?",
                          c,
                          ".width|0:",
                          "(",
                          d,
                          ".",
                          "framebufferWidth",
                          "-",
                          e,
                          ")"
                        ),
                        c = b.def(
                          '"height" in ',
                          c,
                          "?",
                          c,
                          ".height|0:",
                          "(",
                          d,
                          ".",
                          "framebufferHeight",
                          "-",
                          p,
                          ")"
                        );
                      return [e, p, Y, c];
                    });
                    b &&
                      ((a.thisDep = a.thisDep || b.thisDep),
                      (a.contextDep = a.contextDep || b.contextDep),
                      (a.propDep = a.propDep || b.propDep));
                    return a;
                  }
                  return b
                    ? new Z(b.thisDep, b.contextDep, b.propDep, function (
                        a,
                        b
                      ) {
                        var c = a.shared.context;
                        return [
                          0,
                          0,
                          b.def(c, ".", "framebufferWidth"),
                          b.def(c, ".", "framebufferHeight"),
                        ];
                      })
                    : null;
                }
                var e = a["static"],
                  f = a.dynamic;
                if ((a = d("viewport"))) {
                  var g = a;
                  a = new Z(a.thisDep, a.contextDep, a.propDep, function (
                    a,
                    b
                  ) {
                    var c = g.append(a, b),
                      d = a.shared.context;
                    b.set(d, ".viewportWidth", c[2]);
                    b.set(d, ".viewportHeight", c[3]);
                    return c;
                  });
                }
                return { viewport: a, scissor_box: d("scissor.box") };
              }
              function E(a) {
                function c(a) {
                  if (a in d) {
                    var p = b.id(d[a]);
                    a = D(function () {
                      return p;
                    });
                    a.id = p;
                    return a;
                  }
                  if (a in e) {
                    var f = e[a];
                    return P(f, function (a, b) {
                      var c = a.invoke(b, f);
                      return b.def(a.shared.strings, ".id(", c, ")");
                    });
                  }
                  return null;
                }
                var d = a["static"],
                  e = a.dynamic,
                  f = c("frag"),
                  g = c("vert"),
                  h = null;
                va(f) && va(g)
                  ? ((h = t.program(g.id, f.id)),
                    (a = D(function (a, b) {
                      return a.link(h);
                    })))
                  : (a = new Z(
                      (f && f.thisDep) || (g && g.thisDep),
                      (f && f.contextDep) || (g && g.contextDep),
                      (f && f.propDep) || (g && g.propDep),
                      function (a, b) {
                        var c = a.shared.shader,
                          d;
                        d = f ? f.append(a, b) : b.def(c, ".", "frag");
                        var e;
                        e = g ? g.append(a, b) : b.def(c, ".", "vert");
                        return b.def(c + ".program(" + e + "," + d + ")");
                      }
                    ));
                return { frag: f, vert: g, progVar: a, program: h };
              }
              function O(a, b) {
                function c(a, b) {
                  if (a in e) {
                    var d = e[a] | 0;
                    return D(function (a, c) {
                      b && (a.OFFSET = d);
                      return d;
                    });
                  }
                  if (a in f) {
                    var p = f[a];
                    return P(p, function (a, c) {
                      var d = a.invoke(c, p);
                      b && (a.OFFSET = d);
                      return d;
                    });
                  }
                  return b && g
                    ? D(function (a, b) {
                        a.OFFSET = "0";
                        return 0;
                      })
                    : null;
                }
                var e = a["static"],
                  f = a.dynamic,
                  g = (function () {
                    if ("elements" in e) {
                      var a = e.elements;
                      Oa(a)
                        ? (a = d.getElements(d.create(a, !0)))
                        : a && (a = d.getElements(a));
                      var b = D(function (b, c) {
                        if (a) {
                          var d = b.link(a);
                          return (b.ELEMENTS = d);
                        }
                        return (b.ELEMENTS = null);
                      });
                      b.value = a;
                      return b;
                    }
                    if ("elements" in f) {
                      var c = f.elements;
                      return P(c, function (a, b) {
                        var d = a.shared,
                          e = d.isBufferArgs,
                          d = d.elements,
                          p = a.invoke(b, c),
                          f = b.def("null"),
                          e = b.def(e, "(", p, ")"),
                          p = a
                            .cond(e)
                            .then(f, "=", d, ".createStream(", p, ");")
                            ["else"](f, "=", d, ".getElements(", p, ");");
                        b.entry(p);
                        b.exit(a.cond(e).then(d, ".destroyStream(", f, ");"));
                        return (a.ELEMENTS = f);
                      });
                    }
                    return null;
                  })(),
                  h = c("offset", !0);
                return {
                  elements: g,
                  primitive: (function () {
                    if ("primitive" in e) {
                      var a = e.primitive;
                      return D(function (b, c) {
                        return Sa[a];
                      });
                    }
                    if ("primitive" in f) {
                      var b = f.primitive;
                      return P(b, function (a, c) {
                        var d = a.constants.primTypes,
                          e = a.invoke(c, b);
                        return c.def(d, "[", e, "]");
                      });
                    }
                    return g
                      ? va(g)
                        ? g.value
                          ? D(function (a, b) {
                              return b.def(a.ELEMENTS, ".primType");
                            })
                          : D(function () {
                              return 4;
                            })
                        : new Z(g.thisDep, g.contextDep, g.propDep, function (
                            a,
                            b
                          ) {
                            var c = a.ELEMENTS;
                            return b.def(c, "?", c, ".primType:", 4);
                          })
                      : null;
                  })(),
                  count: (function () {
                    if ("count" in e) {
                      var a = e.count | 0;
                      return D(function () {
                        return a;
                      });
                    }
                    if ("count" in f) {
                      var b = f.count;
                      return P(b, function (a, c) {
                        return a.invoke(c, b);
                      });
                    }
                    return g
                      ? va(g)
                        ? g
                          ? h
                            ? new Z(
                                h.thisDep,
                                h.contextDep,
                                h.propDep,
                                function (a, b) {
                                  return b.def(
                                    a.ELEMENTS,
                                    ".vertCount-",
                                    a.OFFSET
                                  );
                                }
                              )
                            : D(function (a, b) {
                                return b.def(a.ELEMENTS, ".vertCount");
                              })
                          : D(function () {
                              return -1;
                            })
                        : new Z(
                            g.thisDep || h.thisDep,
                            g.contextDep || h.contextDep,
                            g.propDep || h.propDep,
                            function (a, b) {
                              var c = a.ELEMENTS;
                              return a.OFFSET
                                ? b.def(
                                    c,
                                    "?",
                                    c,
                                    ".vertCount-",
                                    a.OFFSET,
                                    ":-1"
                                  )
                                : b.def(c, "?", c, ".vertCount:-1");
                            }
                          )
                      : null;
                  })(),
                  instances: c("instances", !1),
                  offset: h,
                };
              }
              function R(a, b) {
                var c = a["static"],
                  d = a.dynamic,
                  e = {};
                Ka.forEach(function (a) {
                  function b(f, g) {
                    if (a in c) {
                      var w = f(c[a]);
                      e[p] = D(function () {
                        return w;
                      });
                    } else if (a in d) {
                      var h = d[a];
                      e[p] = P(h, function (a, b) {
                        return g(a, b, a.invoke(b, h));
                      });
                    }
                  }
                  var p = l(a);
                  switch (a) {
                    case "cull.enable":
                    case "blend.enable":
                    case "dither":
                    case "stencil.enable":
                    case "depth.enable":
                    case "scissor.enable":
                    case "polygonOffset.enable":
                    case "sample.alpha":
                    case "sample.enable":
                    case "depth.mask":
                      return b(
                        function (a) {
                          return a;
                        },
                        function (a, b, c) {
                          return c;
                        }
                      );
                    case "depth.func":
                      return b(
                        function (a) {
                          return Xa[a];
                        },
                        function (a, b, c) {
                          return b.def(a.constants.compareFuncs, "[", c, "]");
                        }
                      );
                    case "depth.range":
                      return b(
                        function (a) {
                          return a;
                        },
                        function (a, b, c) {
                          a = b.def("+", c, "[0]");
                          b = b.def("+", c, "[1]");
                          return [a, b];
                        }
                      );
                    case "blend.func":
                      return b(
                        function (a) {
                          return [
                            Ga["srcRGB" in a ? a.srcRGB : a.src],
                            Ga["dstRGB" in a ? a.dstRGB : a.dst],
                            Ga["srcAlpha" in a ? a.srcAlpha : a.src],
                            Ga["dstAlpha" in a ? a.dstAlpha : a.dst],
                          ];
                        },
                        function (a, b, c) {
                          function d(a, e) {
                            return b.def(
                              '"',
                              a,
                              e,
                              '" in ',
                              c,
                              "?",
                              c,
                              ".",
                              a,
                              e,
                              ":",
                              c,
                              ".",
                              a
                            );
                          }
                          a = a.constants.blendFuncs;
                          var e = d("src", "RGB"),
                            p = d("dst", "RGB"),
                            e = b.def(a, "[", e, "]"),
                            f = b.def(a, "[", d("src", "Alpha"), "]"),
                            p = b.def(a, "[", p, "]");
                          a = b.def(a, "[", d("dst", "Alpha"), "]");
                          return [e, p, f, a];
                        }
                      );
                    case "blend.equation":
                      return b(
                        function (a) {
                          if ("string" === typeof a) return [X[a], X[a]];
                          if ("object" === typeof a)
                            return [X[a.rgb], X[a.alpha]];
                        },
                        function (a, b, c) {
                          var d = a.constants.blendEquations,
                            e = b.def(),
                            p = b.def();
                          a = a.cond("typeof ", c, '==="string"');
                          a.then(e, "=", p, "=", d, "[", c, "];");
                          a["else"](
                            e,
                            "=",
                            d,
                            "[",
                            c,
                            ".rgb];",
                            p,
                            "=",
                            d,
                            "[",
                            c,
                            ".alpha];"
                          );
                          b(a);
                          return [e, p];
                        }
                      );
                    case "blend.color":
                      return b(
                        function (a) {
                          return J(4, function (b) {
                            return +a[b];
                          });
                        },
                        function (a, b, c) {
                          return J(4, function (a) {
                            return b.def("+", c, "[", a, "]");
                          });
                        }
                      );
                    case "stencil.mask":
                      return b(
                        function (a) {
                          return a | 0;
                        },
                        function (a, b, c) {
                          return b.def(c, "|0");
                        }
                      );
                    case "stencil.func":
                      return b(
                        function (a) {
                          return [
                            Xa[a.cmp || "keep"],
                            a.ref || 0,
                            "mask" in a ? a.mask : -1,
                          ];
                        },
                        function (a, b, c) {
                          a = b.def(
                            '"cmp" in ',
                            c,
                            "?",
                            a.constants.compareFuncs,
                            "[",
                            c,
                            ".cmp]",
                            ":",
                            7680
                          );
                          var d = b.def(c, ".ref|0");
                          b = b.def('"mask" in ', c, "?", c, ".mask|0:-1");
                          return [a, d, b];
                        }
                      );
                    case "stencil.opFront":
                    case "stencil.opBack":
                      return b(
                        function (b) {
                          return [
                            "stencil.opBack" === a ? 1029 : 1028,
                            Pa[b.fail || "keep"],
                            Pa[b.zfail || "keep"],
                            Pa[b.zpass || "keep"],
                          ];
                        },
                        function (b, c, d) {
                          function e(a) {
                            return c.def(
                              '"',
                              a,
                              '" in ',
                              d,
                              "?",
                              p,
                              "[",
                              d,
                              ".",
                              a,
                              "]:",
                              7680
                            );
                          }
                          var p = b.constants.stencilOps;
                          return [
                            "stencil.opBack" === a ? 1029 : 1028,
                            e("fail"),
                            e("zfail"),
                            e("zpass"),
                          ];
                        }
                      );
                    case "polygonOffset.offset":
                      return b(
                        function (a) {
                          return [a.factor | 0, a.units | 0];
                        },
                        function (a, b, c) {
                          a = b.def(c, ".factor|0");
                          b = b.def(c, ".units|0");
                          return [a, b];
                        }
                      );
                    case "cull.face":
                      return b(
                        function (a) {
                          var b = 0;
                          "front" === a
                            ? (b = 1028)
                            : "back" === a && (b = 1029);
                          return b;
                        },
                        function (a, b, c) {
                          return b.def(c, '==="front"?', 1028, ":", 1029);
                        }
                      );
                    case "lineWidth":
                      return b(
                        function (a) {
                          return a;
                        },
                        function (a, b, c) {
                          return c;
                        }
                      );
                    case "frontFace":
                      return b(
                        function (a) {
                          return wb[a];
                        },
                        function (a, b, c) {
                          return b.def(c + '==="cw"?2304:2305');
                        }
                      );
                    case "colorMask":
                      return b(
                        function (a) {
                          return a.map(function (a) {
                            return !!a;
                          });
                        },
                        function (a, b, c) {
                          return J(4, function (a) {
                            return "!!" + c + "[" + a + "]";
                          });
                        }
                      );
                    case "sample.coverage":
                      return b(
                        function (a) {
                          return ["value" in a ? a.value : 1, !!a.invert];
                        },
                        function (a, b, c) {
                          a = b.def('"value" in ', c, "?+", c, ".value:1");
                          b = b.def("!!", c, ".invert");
                          return [a, b];
                        }
                      );
                  }
                });
                return e;
              }
              function F(a, b) {
                var c = a["static"],
                  d = a.dynamic,
                  e = {};
                Object.keys(c).forEach(function (a) {
                  var b = c[a],
                    d;
                  if ("number" === typeof b || "boolean" === typeof b)
                    d = D(function () {
                      return b;
                    });
                  else if ("function" === typeof b) {
                    var p = b._reglType;
                    if ("texture2d" === p || "textureCube" === p)
                      d = D(function (a) {
                        return a.link(b);
                      });
                    else if ("framebuffer" === p || "framebufferCube" === p)
                      d = D(function (a) {
                        return a.link(b.color[0]);
                      });
                  } else
                    pa(b) &&
                      (d = D(function (a) {
                        return a.global.def(
                          "[",
                          J(b.length, function (a) {
                            return b[a];
                          }),
                          "]"
                        );
                      }));
                  d.value = b;
                  e[a] = d;
                });
                Object.keys(d).forEach(function (a) {
                  var b = d[a];
                  e[a] = P(b, function (a, c) {
                    return a.invoke(c, b);
                  });
                });
                return e;
              }
              function T(a, c) {
                var d = a["static"],
                  e = a.dynamic,
                  f = {};
                Object.keys(d).forEach(function (a) {
                  var c = d[a],
                    e = b.id(a),
                    p = new ya();
                  if (Oa(c))
                    (p.state = 1),
                      (p.buffer = g.getBuffer(g.create(c, 34962, !1, !0))),
                      (p.type = 0);
                  else {
                    var w = g.getBuffer(c);
                    if (w) (p.state = 1), (p.buffer = w), (p.type = 0);
                    else if ("constant" in c) {
                      var h = c.constant;
                      p.buffer = "null";
                      p.state = 2;
                      "number" === typeof h
                        ? (p.x = h)
                        : Da.forEach(function (a, b) {
                            b < h.length && (p[a] = h[b]);
                          });
                    } else {
                      var w = Oa(c.buffer)
                          ? g.getBuffer(g.create(c.buffer, 34962, !1, !0))
                          : g.getBuffer(c.buffer),
                        k = c.offset | 0,
                        m = c.stride | 0,
                        I = c.size | 0,
                        l = !!c.normalized,
                        n = 0;
                      "type" in c && (n = Ra[c.type]);
                      c = c.divisor | 0;
                      p.buffer = w;
                      p.state = 1;
                      p.size = I;
                      p.normalized = l;
                      p.type = n || w.dtype;
                      p.offset = k;
                      p.stride = m;
                      p.divisor = c;
                    }
                  }
                  f[a] = D(function (a, b) {
                    var c = a.attribCache;
                    if (e in c) return c[e];
                    var d = { isStream: !1 };
                    Object.keys(p).forEach(function (a) {
                      d[a] = p[a];
                    });
                    p.buffer &&
                      ((d.buffer = a.link(p.buffer)),
                      (d.type = d.type || d.buffer + ".dtype"));
                    return (c[e] = d);
                  });
                });
                Object.keys(e).forEach(function (a) {
                  var b = e[a];
                  f[a] = P(b, function (a, c) {
                    function d(a) {
                      c(w[a], "=", e, ".", a, "|0;");
                    }
                    var e = a.invoke(c, b),
                      p = a.shared,
                      f = p.isBufferArgs,
                      g = p.buffer,
                      w = { isStream: c.def(!1) },
                      h = new ya();
                    h.state = 1;
                    Object.keys(h).forEach(function (a) {
                      w[a] = c.def("" + h[a]);
                    });
                    var z = w.buffer,
                      k = w.type;
                    c(
                      "if(",
                      f,
                      "(",
                      e,
                      ")){",
                      w.isStream,
                      "=true;",
                      z,
                      "=",
                      g,
                      ".createStream(",
                      34962,
                      ",",
                      e,
                      ");",
                      k,
                      "=",
                      z,
                      ".dtype;",
                      "}else{",
                      z,
                      "=",
                      g,
                      ".getBuffer(",
                      e,
                      ");",
                      "if(",
                      z,
                      "){",
                      k,
                      "=",
                      z,
                      ".dtype;",
                      '}else if("constant" in ',
                      e,
                      "){",
                      w.state,
                      "=",
                      2,
                      ";",
                      "if(typeof " + e + '.constant === "number"){',
                      w[Da[0]],
                      "=",
                      e,
                      ".constant;",
                      Da.slice(1)
                        .map(function (a) {
                          return w[a];
                        })
                        .join("="),
                      "=0;",
                      "}else{",
                      Da.map(function (a, b) {
                        return (
                          w[a] +
                          "=" +
                          e +
                          ".constant.length>" +
                          b +
                          "?" +
                          e +
                          ".constant[" +
                          b +
                          "]:0;"
                        );
                      }).join(""),
                      "}}else{",
                      "if(",
                      f,
                      "(",
                      e,
                      ".buffer)){",
                      z,
                      "=",
                      g,
                      ".createStream(",
                      34962,
                      ",",
                      e,
                      ".buffer);",
                      "}else{",
                      z,
                      "=",
                      g,
                      ".getBuffer(",
                      e,
                      ".buffer);",
                      "}",
                      k,
                      '="type" in ',
                      e,
                      "?",
                      p.glTypes,
                      "[",
                      e,
                      ".type]:",
                      z,
                      ".dtype;",
                      w.normalized,
                      "=!!",
                      e,
                      ".normalized;"
                    );
                    d("size");
                    d("offset");
                    d("stride");
                    d("divisor");
                    c("}}");
                    c.exit(
                      "if(",
                      w.isStream,
                      "){",
                      g,
                      ".destroyStream(",
                      z,
                      ");",
                      "}"
                    );
                    return w;
                  });
                });
                return f;
              }
              function M(a) {
                var b = a["static"],
                  c = a.dynamic,
                  d = {};
                Object.keys(b).forEach(function (a) {
                  var c = b[a];
                  d[a] = D(function (a, b) {
                    return "number" === typeof c || "boolean" === typeof c
                      ? "" + c
                      : a.link(c);
                  });
                });
                Object.keys(c).forEach(function (a) {
                  var b = c[a];
                  d[a] = P(b, function (a, c) {
                    return a.invoke(c, b);
                  });
                });
                return d;
              }
              function A(a, b, c, d, e) {
                var f = y(a, e),
                  g = x(a, f, e),
                  h = O(a, e),
                  k = R(a, e),
                  m = E(a, e),
                  ba = g.viewport;
                ba && (k.viewport = ba);
                ba = l("scissor.box");
                (g = g[ba]) && (k[ba] = g);
                g = 0 < Object.keys(k).length;
                f = { framebuffer: f, draw: h, shader: m, state: k, dirty: g };
                f.profile = B(a, e);
                f.uniforms = F(c, e);
                f.attributes = T(b, e);
                f.context = M(d, e);
                return f;
              }
              function ua(a, b, c) {
                var d = a.shared.context,
                  e = a.scope();
                Object.keys(c).forEach(function (f) {
                  b.save(d, "." + f);
                  e(d, ".", f, "=", c[f].append(a, b), ";");
                });
                b(e);
              }
              function K(a, b, c, d) {
                var e = a.shared,
                  f = e.gl,
                  g = e.framebuffer,
                  h;
                ha && (h = b.def(e.extensions, ".webgl_draw_buffers"));
                var k = a.constants,
                  e = k.drawBuffer,
                  k = k.backBuffer;
                a = c ? c.append(a, b) : b.def(g, ".next");
                d || b("if(", a, "!==", g, ".cur){");
                b(
                  "if(",
                  a,
                  "){",
                  f,
                  ".bindFramebuffer(",
                  36160,
                  ",",
                  a,
                  ".framebuffer);"
                );
                ha &&
                  b(
                    h,
                    ".drawBuffersWEBGL(",
                    e,
                    "[",
                    a,
                    ".colorAttachments.length]);"
                  );
                b("}else{", f, ".bindFramebuffer(", 36160, ",null);");
                ha && b(h, ".drawBuffersWEBGL(", k, ");");
                b("}", g, ".cur=", a, ";");
                d || b("}");
              }
              function V(a, b, c) {
                var d = a.shared,
                  e = d.gl,
                  f = a.current,
                  g = a.next,
                  h = d.current,
                  k = d.next,
                  m = a.cond(h, ".dirty");
                Ka.forEach(function (b) {
                  b = l(b);
                  if (!(b in c.state)) {
                    var d, w;
                    if (b in g) {
                      d = g[b];
                      w = f[b];
                      var I = J(ra[b].length, function (a) {
                        return m.def(d, "[", a, "]");
                      });
                      m(
                        a
                          .cond(
                            I.map(function (a, b) {
                              return a + "!==" + w + "[" + b + "]";
                            }).join("||")
                          )
                          .then(
                            e,
                            ".",
                            ta[b],
                            "(",
                            I,
                            ");",
                            I.map(function (a, b) {
                              return w + "[" + b + "]=" + a;
                            }).join(";"),
                            ";"
                          )
                      );
                    } else
                      (d = m.def(k, ".", b)),
                        (I = a.cond(d, "!==", h, ".", b)),
                        m(I),
                        b in sa
                          ? I(
                              a
                                .cond(d)
                                .then(e, ".enable(", sa[b], ");")
                                ["else"](e, ".disable(", sa[b], ");"),
                              h,
                              ".",
                              b,
                              "=",
                              d,
                              ";"
                            )
                          : I(
                              e,
                              ".",
                              ta[b],
                              "(",
                              d,
                              ");",
                              h,
                              ".",
                              b,
                              "=",
                              d,
                              ";"
                            );
                  }
                });
                0 === Object.keys(c.state).length && m(h, ".dirty=false;");
                b(m);
              }
              function Q(a, b, c, d) {
                var e = a.shared,
                  f = a.current,
                  g = e.current,
                  h = e.gl;
                vb(Object.keys(c)).forEach(function (e) {
                  var k = c[e];
                  if (!d || d(k)) {
                    var m = k.append(a, b);
                    if (sa[e]) {
                      var l = sa[e];
                      va(k)
                        ? m
                          ? b(h, ".enable(", l, ");")
                          : b(h, ".disable(", l, ");")
                        : b(
                            a
                              .cond(m)
                              .then(h, ".enable(", l, ");")
                              ["else"](h, ".disable(", l, ");")
                          );
                      b(g, ".", e, "=", m, ";");
                    } else if (pa(m)) {
                      var n = f[e];
                      b(
                        h,
                        ".",
                        ta[e],
                        "(",
                        m,
                        ");",
                        m
                          .map(function (a, b) {
                            return n + "[" + b + "]=" + a;
                          })
                          .join(";"),
                        ";"
                      );
                    } else
                      b(h, ".", ta[e], "(", m, ");", g, ".", e, "=", m, ";");
                  }
                });
              }
              function wa(a, b) {
                ea &&
                  (a.instancing = b.def(
                    a.shared.extensions,
                    ".angle_instanced_arrays"
                  ));
              }
              function G(a, b, c, d, e) {
                function f() {
                  return "undefined" === typeof performance
                    ? "Date.now()"
                    : "performance.now()";
                }
                function g(a) {
                  t = b.def();
                  a(t, "=", f(), ";");
                  "string" === typeof e
                    ? a(ba, ".count+=", e, ";")
                    : a(ba, ".count++;");
                  k &&
                    (d
                      ? ((r = b.def()),
                        a(r, "=", q, ".getNumPendingQueries();"))
                      : a(q, ".beginQuery(", ba, ");"));
                }
                function h(a) {
                  a(ba, ".cpuTime+=", f(), "-", t, ";");
                  k &&
                    (d
                      ? a(
                          q,
                          ".pushScopeStats(",
                          r,
                          ",",
                          q,
                          ".getNumPendingQueries(),",
                          ba,
                          ");"
                        )
                      : a(q, ".endQuery();"));
                }
                function m(a) {
                  var c = b.def(n, ".profile");
                  b(n, ".profile=", a, ";");
                  b.exit(n, ".profile=", c, ";");
                }
                var l = a.shared,
                  ba = a.stats,
                  n = l.current,
                  q = l.timer;
                c = c.profile;
                var t, r;
                if (c) {
                  if (va(c)) {
                    c.enable ? (g(b), h(b.exit), m("true")) : m("false");
                    return;
                  }
                  c = c.append(a, b);
                  m(c);
                } else c = b.def(n, ".profile");
                l = a.block();
                g(l);
                b("if(", c, "){", l, "}");
                a = a.block();
                h(a);
                b.exit("if(", c, "){", a, "}");
              }
              function U(a, b, c, d, e) {
                function f(a) {
                  switch (a) {
                    case 35664:
                    case 35667:
                    case 35671:
                      return 2;
                    case 35665:
                    case 35668:
                    case 35672:
                      return 3;
                    case 35666:
                    case 35669:
                    case 35673:
                      return 4;
                    default:
                      return 1;
                  }
                }
                function g(c, d, e) {
                  function f() {
                    b(
                      "if(!",
                      z,
                      ".buffer){",
                      k,
                      ".enableVertexAttribArray(",
                      l,
                      ");}"
                    );
                    var c = e.type,
                      g;
                    g = e.size ? b.def(e.size, "||", d) : d;
                    b(
                      "if(",
                      z,
                      ".type!==",
                      c,
                      "||",
                      z,
                      ".size!==",
                      g,
                      "||",
                      q
                        .map(function (a) {
                          return z + "." + a + "!==" + e[a];
                        })
                        .join("||"),
                      "){",
                      k,
                      ".bindBuffer(",
                      34962,
                      ",",
                      I,
                      ".buffer);",
                      k,
                      ".vertexAttribPointer(",
                      [l, g, c, e.normalized, e.stride, e.offset],
                      ");",
                      z,
                      ".type=",
                      c,
                      ";",
                      z,
                      ".size=",
                      g,
                      ";",
                      q
                        .map(function (a) {
                          return z + "." + a + "=" + e[a] + ";";
                        })
                        .join(""),
                      "}"
                    );
                    ea &&
                      ((c = e.divisor),
                      b(
                        "if(",
                        z,
                        ".divisor!==",
                        c,
                        "){",
                        a.instancing,
                        ".vertexAttribDivisorANGLE(",
                        [l, c],
                        ");",
                        z,
                        ".divisor=",
                        c,
                        ";}"
                      ));
                  }
                  function m() {
                    b(
                      "if(",
                      z,
                      ".buffer){",
                      k,
                      ".disableVertexAttribArray(",
                      l,
                      ");",
                      "}if(",
                      Da.map(function (a, b) {
                        return z + "." + a + "!==" + n[b];
                      }).join("||"),
                      "){",
                      k,
                      ".vertexAttrib4f(",
                      l,
                      ",",
                      n,
                      ");",
                      Da.map(function (a, b) {
                        return z + "." + a + "=" + n[b] + ";";
                      }).join(""),
                      "}"
                    );
                  }
                  var k = h.gl,
                    l = b.def(c, ".location"),
                    z = b.def(h.attributes, "[", l, "]");
                  c = e.state;
                  var I = e.buffer,
                    n = [e.x, e.y, e.z, e.w],
                    q = ["buffer", "normalized", "offset", "stride"];
                  1 === c
                    ? f()
                    : 2 === c
                    ? m()
                    : (b("if(", c, "===", 1, "){"),
                      f(),
                      b("}else{"),
                      m(),
                      b("}"));
                }
                var h = a.shared;
                d.forEach(function (d) {
                  var h = d.name,
                    k = c.attributes[h],
                    m;
                  if (k) {
                    if (!e(k)) return;
                    m = k.append(a, b);
                  } else {
                    if (!e(xb)) return;
                    var l = a.scopeAttrib(h);
                    m = {};
                    Object.keys(new ya()).forEach(function (a) {
                      m[a] = b.def(l, ".", a);
                    });
                  }
                  g(a.link(d), f(d.info.type), m);
                });
              }
              function W(a, c, d, e, f) {
                for (var g = a.shared, h = g.gl, k, m = 0; m < e.length; ++m) {
                  var l = e[m],
                    n = l.name,
                    q = l.info.type,
                    t = d.uniforms[n],
                    l = a.link(l) + ".location",
                    r;
                  if (t) {
                    if (!f(t)) continue;
                    if (va(t)) {
                      n = t.value;
                      if (35678 === q || 35680 === q)
                        (q = a.link(n._texture || n.color[0]._texture)),
                          c(h, ".uniform1i(", l, ",", q + ".bind());"),
                          c.exit(q, ".unbind();");
                      else if (35674 === q || 35675 === q || 35676 === q)
                        (n = a.global.def(
                          "new Float32Array([" +
                            Array.prototype.slice.call(n) +
                            "])"
                        )),
                          (t = 2),
                          35675 === q ? (t = 3) : 35676 === q && (t = 4),
                          c(
                            h,
                            ".uniformMatrix",
                            t,
                            "fv(",
                            l,
                            ",false,",
                            n,
                            ");"
                          );
                      else {
                        switch (q) {
                          case 5126:
                            k = "1f";
                            break;
                          case 35664:
                            k = "2f";
                            break;
                          case 35665:
                            k = "3f";
                            break;
                          case 35666:
                            k = "4f";
                            break;
                          case 35670:
                            k = "1i";
                            break;
                          case 5124:
                            k = "1i";
                            break;
                          case 35671:
                            k = "2i";
                            break;
                          case 35667:
                            k = "2i";
                            break;
                          case 35672:
                            k = "3i";
                            break;
                          case 35668:
                            k = "3i";
                            break;
                          case 35673:
                            k = "4i";
                            break;
                          case 35669:
                            k = "4i";
                        }
                        c(
                          h,
                          ".uniform",
                          k,
                          "(",
                          l,
                          ",",
                          pa(n) ? Array.prototype.slice.call(n) : n,
                          ");"
                        );
                      }
                      continue;
                    } else r = t.append(a, c);
                  } else {
                    if (!f(xb)) continue;
                    r = c.def(g.uniforms, "[", b.id(n), "]");
                  }
                  35678 === q
                    ? c(
                        "if(",
                        r,
                        "&&",
                        r,
                        '._reglType==="framebuffer"){',
                        r,
                        "=",
                        r,
                        ".color[0];",
                        "}"
                      )
                    : 35680 === q &&
                      c(
                        "if(",
                        r,
                        "&&",
                        r,
                        '._reglType==="framebufferCube"){',
                        r,
                        "=",
                        r,
                        ".color[0];",
                        "}"
                      );
                  n = 1;
                  switch (q) {
                    case 35678:
                    case 35680:
                      q = c.def(r, "._texture");
                      c(h, ".uniform1i(", l, ",", q, ".bind());");
                      c.exit(q, ".unbind();");
                      continue;
                    case 5124:
                    case 35670:
                      k = "1i";
                      break;
                    case 35667:
                    case 35671:
                      k = "2i";
                      n = 2;
                      break;
                    case 35668:
                    case 35672:
                      k = "3i";
                      n = 3;
                      break;
                    case 35669:
                    case 35673:
                      k = "4i";
                      n = 4;
                      break;
                    case 5126:
                      k = "1f";
                      break;
                    case 35664:
                      k = "2f";
                      n = 2;
                      break;
                    case 35665:
                      k = "3f";
                      n = 3;
                      break;
                    case 35666:
                      k = "4f";
                      n = 4;
                      break;
                    case 35674:
                      k = "Matrix2fv";
                      break;
                    case 35675:
                      k = "Matrix3fv";
                      break;
                    case 35676:
                      k = "Matrix4fv";
                  }
                  c(h, ".uniform", k, "(", l, ",");
                  if ("M" === k.charAt(0)) {
                    var l = Math.pow(q - 35674 + 2, 2),
                      v = a.global.def("new Float32Array(", l, ")");
                    c(
                      "false,(Array.isArray(",
                      r,
                      ")||",
                      r,
                      " instanceof Float32Array)?",
                      r,
                      ":(",
                      J(l, function (a) {
                        return v + "[" + a + "]=" + r + "[" + a + "]";
                      }),
                      ",",
                      v,
                      ")"
                    );
                  } else
                    1 < n
                      ? c(
                          J(n, function (a) {
                            return r + "[" + a + "]";
                          })
                        )
                      : c(r);
                  c(");");
                }
              }
              function S(a, b, c, d) {
                function e(f) {
                  var g = l[f];
                  return g
                    ? (g.contextDep && d.contextDynamic) || g.propDep
                      ? g.append(a, c)
                      : g.append(a, b)
                    : b.def(m, ".", f);
                }
                function f() {
                  function a() {
                    c(
                      u,
                      ".drawElementsInstancedANGLE(",
                      [q, t, C, r + "<<((" + C + "-5121)>>1)", v],
                      ");"
                    );
                  }
                  function b() {
                    c(u, ".drawArraysInstancedANGLE(", [q, r, t, v], ");");
                  }
                  n
                    ? da
                      ? a()
                      : (c("if(", n, "){"), a(), c("}else{"), b(), c("}"))
                    : b();
                }
                function g() {
                  function a() {
                    c(
                      k +
                        ".drawElements(" +
                        [q, t, C, r + "<<((" + C + "-5121)>>1)"] +
                        ");"
                    );
                  }
                  function b() {
                    c(k + ".drawArrays(" + [q, r, t] + ");");
                  }
                  n
                    ? da
                      ? a()
                      : (c("if(", n, "){"), a(), c("}else{"), b(), c("}"))
                    : b();
                }
                var h = a.shared,
                  k = h.gl,
                  m = h.draw,
                  l = d.draw,
                  n = (function () {
                    var e = l.elements,
                      f = b;
                    if (e) {
                      if ((e.contextDep && d.contextDynamic) || e.propDep)
                        f = c;
                      e = e.append(a, f);
                    } else e = f.def(m, ".", "elements");
                    e &&
                      f(
                        "if(" +
                          e +
                          ")" +
                          k +
                          ".bindBuffer(34963," +
                          e +
                          ".buffer.buffer);"
                      );
                    return e;
                  })(),
                  q = e("primitive"),
                  r = e("offset"),
                  t = (function () {
                    var e = l.count,
                      f = b;
                    if (e) {
                      if ((e.contextDep && d.contextDynamic) || e.propDep)
                        f = c;
                      e = e.append(a, f);
                    } else e = f.def(m, ".", "count");
                    return e;
                  })();
                if ("number" === typeof t) {
                  if (0 === t) return;
                } else c("if(", t, "){"), c.exit("}");
                var v, u;
                ea && ((v = e("instances")), (u = a.instancing));
                var C = n + ".type",
                  da = l.elements && va(l.elements);
                ea && ("number" !== typeof v || 0 <= v)
                  ? "string" === typeof v
                    ? (c("if(", v, ">0){"),
                      f(),
                      c("}else if(", v, "<0){"),
                      g(),
                      c("}"))
                    : f()
                  : g();
              }
              function ca(a, b, c, d, e) {
                b = N();
                e = b.proc("body", e);
                ea &&
                  (b.instancing = e.def(
                    b.shared.extensions,
                    ".angle_instanced_arrays"
                  ));
                a(b, e, c, d);
                return b.compile().body;
              }
              function L(a, b, c, d) {
                wa(a, b);
                U(a, b, c, d.attributes, function () {
                  return !0;
                });
                W(a, b, c, d.uniforms, function () {
                  return !0;
                });
                S(a, b, b, c);
              }
              function da(a, b) {
                var c = a.proc("draw", 1);
                wa(a, c);
                ua(a, c, b.context);
                K(a, c, b.framebuffer);
                V(a, c, b);
                Q(a, c, b.state);
                G(a, c, b, !1, !0);
                var d = b.shader.progVar.append(a, c);
                c(a.shared.gl, ".useProgram(", d, ".program);");
                if (b.shader.program) L(a, c, b, b.shader.program);
                else {
                  var e = a.global.def("{}"),
                    f = c.def(d, ".id"),
                    g = c.def(e, "[", f, "]");
                  c(
                    a
                      .cond(g)
                      .then(g, ".call(this,a0);")
                      ["else"](
                        g,
                        "=",
                        e,
                        "[",
                        f,
                        "]=",
                        a.link(function (c) {
                          return ca(L, a, b, c, 1);
                        }),
                        "(",
                        d,
                        ");",
                        g,
                        ".call(this,a0);"
                      )
                  );
                }
                0 < Object.keys(b.state).length &&
                  c(a.shared.current, ".dirty=true;");
              }
              function oa(a, b, c, d) {
                function e() {
                  return !0;
                }
                a.batchId = "a1";
                wa(a, b);
                U(a, b, c, d.attributes, e);
                W(a, b, c, d.uniforms, e);
                S(a, b, b, c);
              }
              function za(a, b, c, d) {
                function e(a) {
                  return (a.contextDep && g) || a.propDep;
                }
                function f(a) {
                  return !e(a);
                }
                wa(a, b);
                var g = c.contextDep,
                  h = b.def(),
                  k = b.def();
                a.shared.props = k;
                a.batchId = h;
                var m = a.scope(),
                  l = a.scope();
                b(
                  m.entry,
                  "for(",
                  h,
                  "=0;",
                  h,
                  "<",
                  "a1",
                  ";++",
                  h,
                  "){",
                  k,
                  "=",
                  "a0",
                  "[",
                  h,
                  "];",
                  l,
                  "}",
                  m.exit
                );
                c.needsContext && ua(a, l, c.context);
                c.needsFramebuffer && K(a, l, c.framebuffer);
                Q(a, l, c.state, e);
                c.profile && e(c.profile) && G(a, l, c, !1, !0);
                d
                  ? (U(a, m, c, d.attributes, f),
                    U(a, l, c, d.attributes, e),
                    W(a, m, c, d.uniforms, f),
                    W(a, l, c, d.uniforms, e),
                    S(a, m, l, c))
                  : ((b = a.global.def("{}")),
                    (d = c.shader.progVar.append(a, l)),
                    (k = l.def(d, ".id")),
                    (m = l.def(b, "[", k, "]")),
                    l(
                      a.shared.gl,
                      ".useProgram(",
                      d,
                      ".program);",
                      "if(!",
                      m,
                      "){",
                      m,
                      "=",
                      b,
                      "[",
                      k,
                      "]=",
                      a.link(function (b) {
                        return ca(oa, a, c, b, 2);
                      }),
                      "(",
                      d,
                      ");}",
                      m,
                      ".call(this,a0[",
                      h,
                      "],",
                      h,
                      ");"
                    ));
              }
              function ka(a, b) {
                function c(a) {
                  return (a.contextDep && e) || a.propDep;
                }
                var d = a.proc("batch", 2);
                a.batchId = "0";
                wa(a, d);
                var e = !1,
                  f = !0;
                Object.keys(b.context).forEach(function (a) {
                  e = e || b.context[a].propDep;
                });
                e || (ua(a, d, b.context), (f = !1));
                var g = b.framebuffer,
                  h = !1;
                g
                  ? (g.propDep ? (e = h = !0) : g.contextDep && e && (h = !0),
                    h || K(a, d, g))
                  : K(a, d, null);
                b.state.viewport && b.state.viewport.propDep && (e = !0);
                V(a, d, b);
                Q(a, d, b.state, function (a) {
                  return !c(a);
                });
                (b.profile && c(b.profile)) || G(a, d, b, !1, "a1");
                b.contextDep = e;
                b.needsContext = f;
                b.needsFramebuffer = h;
                f = b.shader.progVar;
                if ((f.contextDep && e) || f.propDep) za(a, d, b, null);
                else if (
                  ((f = f.append(a, d)),
                  d(a.shared.gl, ".useProgram(", f, ".program);"),
                  b.shader.program)
                )
                  za(a, d, b, b.shader.program);
                else {
                  var g = a.global.def("{}"),
                    h = d.def(f, ".id"),
                    k = d.def(g, "[", h, "]");
                  d(
                    a
                      .cond(k)
                      .then(k, ".call(this,a0,a1);")
                      ["else"](
                        k,
                        "=",
                        g,
                        "[",
                        h,
                        "]=",
                        a.link(function (c) {
                          return ca(za, a, b, c, 2);
                        }),
                        "(",
                        f,
                        ");",
                        k,
                        ".call(this,a0,a1);"
                      )
                  );
                }
                0 < Object.keys(b.state).length &&
                  d(a.shared.current, ".dirty=true;");
              }
              function ia(a, c) {
                function d(b) {
                  var g = c.shader[b];
                  g && e.set(f.shader, "." + b, g.append(a, e));
                }
                var e = a.proc("scope", 3);
                a.batchId = "a2";
                var f = a.shared,
                  g = f.current;
                ua(a, e, c.context);
                c.framebuffer && c.framebuffer.append(a, e);
                vb(Object.keys(c.state)).forEach(function (b) {
                  var d = c.state[b].append(a, e);
                  pa(d)
                    ? d.forEach(function (c, d) {
                        e.set(a.next[b], "[" + d + "]", c);
                      })
                    : e.set(f.next, "." + b, d);
                });
                G(a, e, c, !0, !0);
                [
                  "elements",
                  "offset",
                  "count",
                  "instances",
                  "primitive",
                ].forEach(function (b) {
                  var d = c.draw[b];
                  d && e.set(f.draw, "." + b, "" + d.append(a, e));
                });
                Object.keys(c.uniforms).forEach(function (d) {
                  e.set(
                    f.uniforms,
                    "[" + b.id(d) + "]",
                    c.uniforms[d].append(a, e)
                  );
                });
                Object.keys(c.attributes).forEach(function (b) {
                  var d = c.attributes[b].append(a, e),
                    f = a.scopeAttrib(b);
                  Object.keys(new ya()).forEach(function (a) {
                    e.set(f, "." + a, d[a]);
                  });
                });
                d("vert");
                d("frag");
                0 < Object.keys(c.state).length &&
                  (e(g, ".dirty=true;"), e.exit(g, ".dirty=true;"));
                e("a1(", a.shared.context, ",a0,", a.batchId, ");");
              }
              function ma(a) {
                if ("object" === typeof a && !pa(a)) {
                  for (var b = Object.keys(a), c = 0; c < b.length; ++c)
                    if (la.isDynamic(a[b[c]])) return !0;
                  return !1;
                }
              }
              function ja(a, b, c) {
                function d(a, b) {
                  g.forEach(function (c) {
                    var d = e[c];
                    la.isDynamic(d) &&
                      ((d = a.invoke(b, d)), b(l, ".", c, "=", d, ";"));
                  });
                }
                var e = b["static"][c];
                if (e && ma(e)) {
                  var f = a.global,
                    g = Object.keys(e),
                    h = !1,
                    k = !1,
                    m = !1,
                    l = a.global.def("{}");
                  g.forEach(function (b) {
                    var c = e[b];
                    if (la.isDynamic(c))
                      "function" === typeof c && (c = e[b] = la.unbox(c)),
                        (b = P(c, null)),
                        (h = h || b.thisDep),
                        (m = m || b.propDep),
                        (k = k || b.contextDep);
                    else {
                      f(l, ".", b, "=");
                      switch (typeof c) {
                        case "number":
                          f(c);
                          break;
                        case "string":
                          f('"', c, '"');
                          break;
                        case "object":
                          Array.isArray(c) && f("[", c.join(), "]");
                          break;
                        default:
                          f(a.link(c));
                      }
                      f(";");
                    }
                  });
                  b.dynamic[c] = new la.DynamicVariable(4, {
                    thisDep: h,
                    contextDep: k,
                    propDep: m,
                    ref: l,
                    append: d,
                  });
                  delete b["static"][c];
                }
              }
              var ya = q.Record,
                X = { add: 32774, subtract: 32778, "reverse subtract": 32779 };
              c.ext_blend_minmax && ((X.min = 32775), (X.max = 32776));
              var ea = c.angle_instanced_arrays,
                ha = c.webgl_draw_buffers,
                ra = { dirty: !0, profile: h.profile },
                Fa = {},
                Ka = [],
                sa = {},
                ta = {};
              u("dither", 3024);
              u("blend.enable", 3042);
              v("blend.color", "blendColor", [0, 0, 0, 0]);
              v("blend.equation", "blendEquationSeparate", [32774, 32774]);
              v("blend.func", "blendFuncSeparate", [1, 0, 1, 0]);
              u("depth.enable", 2929, !0);
              v("depth.func", "depthFunc", 513);
              v("depth.range", "depthRange", [0, 1]);
              v("depth.mask", "depthMask", !0);
              v("colorMask", "colorMask", [!0, !0, !0, !0]);
              u("cull.enable", 2884);
              v("cull.face", "cullFace", 1029);
              v("frontFace", "frontFace", 2305);
              v("lineWidth", "lineWidth", 1);
              u("polygonOffset.enable", 32823);
              v("polygonOffset.offset", "polygonOffset", [0, 0]);
              u("sample.alpha", 32926);
              u("sample.enable", 32928);
              v("sample.coverage", "sampleCoverage", [1, !1]);
              u("stencil.enable", 2960);
              v("stencil.mask", "stencilMask", -1);
              v("stencil.func", "stencilFunc", [519, 0, -1]);
              v(
                "stencil.opFront",
                "stencilOpSeparate",
                [1028, 7680, 7680, 7680]
              );
              v(
                "stencil.opBack",
                "stencilOpSeparate",
                [1029, 7680, 7680, 7680]
              );
              u("scissor.enable", 3089);
              v("scissor.box", "scissor", [
                0,
                0,
                a.drawingBufferWidth,
                a.drawingBufferHeight,
              ]);
              v("viewport", "viewport", [
                0,
                0,
                a.drawingBufferWidth,
                a.drawingBufferHeight,
              ]);
              var na = {
                  gl: a,
                  context: C,
                  strings: b,
                  next: Fa,
                  current: ra,
                  draw: m,
                  elements: d,
                  buffer: g,
                  shader: t,
                  attributes: q.state,
                  uniforms: r,
                  framebuffer: f,
                  extensions: c,
                  timer: k,
                  isBufferArgs: Oa,
                },
                aa = {
                  primTypes: Sa,
                  compareFuncs: Xa,
                  blendFuncs: Ga,
                  blendEquations: X,
                  stencilOps: Pa,
                  glTypes: Ra,
                  orientationType: wb,
                };
              ha &&
                ((aa.backBuffer = [1029]),
                (aa.drawBuffer = J(e.maxDrawbuffers, function (a) {
                  return 0 === a
                    ? [0]
                    : J(a, function (a) {
                        return 36064 + a;
                      });
                })));
              var qa = 0;
              return {
                next: Fa,
                current: ra,
                procs: (function () {
                  var a = N(),
                    b = a.proc("poll"),
                    c = a.proc("refresh"),
                    d = a.block();
                  b(d);
                  c(d);
                  var f = a.shared,
                    g = f.gl,
                    h = f.next,
                    k = f.current;
                  d(k, ".dirty=false;");
                  K(a, b);
                  K(a, c, null, !0);
                  var m;
                  ea && (m = a.link(ea));
                  for (var l = 0; l < e.maxAttributes; ++l) {
                    var n = c.def(f.attributes, "[", l, "]"),
                      q = a.cond(n, ".buffer");
                    q.then(
                      g,
                      ".enableVertexAttribArray(",
                      l,
                      ");",
                      g,
                      ".bindBuffer(",
                      34962,
                      ",",
                      n,
                      ".buffer.buffer);",
                      g,
                      ".vertexAttribPointer(",
                      l,
                      ",",
                      n,
                      ".size,",
                      n,
                      ".type,",
                      n,
                      ".normalized,",
                      n,
                      ".stride,",
                      n,
                      ".offset);"
                    )["else"](
                      g,
                      ".disableVertexAttribArray(",
                      l,
                      ");",
                      g,
                      ".vertexAttrib4f(",
                      l,
                      ",",
                      n,
                      ".x,",
                      n,
                      ".y,",
                      n,
                      ".z,",
                      n,
                      ".w);",
                      n,
                      ".buffer=null;"
                    );
                    c(q);
                    ea &&
                      c(
                        m,
                        ".vertexAttribDivisorANGLE(",
                        l,
                        ",",
                        n,
                        ".divisor);"
                      );
                  }
                  Object.keys(sa).forEach(function (e) {
                    var f = sa[e],
                      m = d.def(h, ".", e),
                      l = a.block();
                    l(
                      "if(",
                      m,
                      "){",
                      g,
                      ".enable(",
                      f,
                      ")}else{",
                      g,
                      ".disable(",
                      f,
                      ")}",
                      k,
                      ".",
                      e,
                      "=",
                      m,
                      ";"
                    );
                    c(l);
                    b("if(", m, "!==", k, ".", e, "){", l, "}");
                  });
                  Object.keys(ta).forEach(function (e) {
                    var f = ta[e],
                      m = ra[e],
                      l,
                      n,
                      q = a.block();
                    q(g, ".", f, "(");
                    pa(m)
                      ? ((f = m.length),
                        (l = a.global.def(h, ".", e)),
                        (n = a.global.def(k, ".", e)),
                        q(
                          J(f, function (a) {
                            return l + "[" + a + "]";
                          }),
                          ");",
                          J(f, function (a) {
                            return n + "[" + a + "]=" + l + "[" + a + "];";
                          }).join("")
                        ),
                        b(
                          "if(",
                          J(f, function (a) {
                            return l + "[" + a + "]!==" + n + "[" + a + "]";
                          }).join("||"),
                          "){",
                          q,
                          "}"
                        ))
                      : ((l = d.def(h, ".", e)),
                        (n = d.def(k, ".", e)),
                        q(l, ");", k, ".", e, "=", l, ";"),
                        b("if(", l, "!==", n, "){", q, "}"));
                    c(q);
                  });
                  return a.compile();
                })(),
                compile: function (a, b, c, d, e) {
                  var f = N();
                  f.stats = f.link(e);
                  Object.keys(b["static"]).forEach(function (a) {
                    ja(f, b, a);
                  });
                  Ub.forEach(function (b) {
                    ja(f, a, b);
                  });
                  c = A(a, b, c, d, f);
                  da(f, c);
                  ia(f, c);
                  ka(f, c);
                  return f.compile();
                },
              };
            }
            function yb(a, b) {
              for (var c = 0; c < a.length; ++c) if (a[c] === b) return c;
              return -1;
            }
            var E = function (a, b) {
                for (var c = Object.keys(b), e = 0; e < c.length; ++e)
                  a[c[e]] = b[c[e]];
                return a;
              },
              Ab = 0,
              la = {
                DynamicVariable: aa,
                define: function (a, b) {
                  return new aa(a, Za(b + ""));
                },
                isDynamic: function (a) {
                  return (
                    ("function" === typeof a && !a._reglType) || a instanceof aa
                  );
                },
                unbox: function (a, b) {
                  return "function" === typeof a ? new aa(0, a) : a;
                },
                accessor: Za,
              },
              Ya = {
                next:
                  "function" === typeof requestAnimationFrame
                    ? function (a) {
                        return requestAnimationFrame(a);
                      }
                    : function (a) {
                        return setTimeout(a, 16);
                      },
                cancel:
                  "function" === typeof cancelAnimationFrame
                    ? function (a) {
                        return cancelAnimationFrame(a);
                      }
                    : clearTimeout,
              },
              zb =
                "undefined" !== typeof performance && performance.now
                  ? function () {
                      return performance.now();
                    }
                  : function () {
                      return +new Date();
                    },
              x = cb();
            x.zero = cb();
            var Vb = function (a, b) {
                var c = 1;
                b.ext_texture_filter_anisotropic && (c = a.getParameter(34047));
                var e = 1,
                  g = 1;
                b.webgl_draw_buffers &&
                  ((e = a.getParameter(34852)), (g = a.getParameter(36063)));
                var d = !!b.oes_texture_float;
                if (d) {
                  d = a.createTexture();
                  a.bindTexture(3553, d);
                  a.texImage2D(3553, 0, 6408, 1, 1, 0, 6408, 5126, null);
                  var n = a.createFramebuffer();
                  a.bindFramebuffer(36160, n);
                  a.framebufferTexture2D(36160, 36064, 3553, d, 0);
                  a.bindTexture(3553, null);
                  if (36053 !== a.checkFramebufferStatus(36160)) d = !1;
                  else {
                    a.viewport(0, 0, 1, 1);
                    a.clearColor(1, 0, 0, 1);
                    a.clear(16384);
                    var f = x.allocType(5126, 4);
                    a.readPixels(0, 0, 1, 1, 6408, 5126, f);
                    a.getError()
                      ? (d = !1)
                      : (a.deleteFramebuffer(n),
                        a.deleteTexture(d),
                        (d = 1 === f[0]));
                    x.freeType(f);
                  }
                }
                f = !0;
                f = a.createTexture();
                n = x.allocType(5121, 36);
                a.activeTexture(33984);
                a.bindTexture(34067, f);
                a.texImage2D(34069, 0, 6408, 3, 3, 0, 6408, 5121, n);
                x.freeType(n);
                a.bindTexture(34067, null);
                a.deleteTexture(f);
                f = !a.getError();
                return {
                  colorBits: [
                    a.getParameter(3410),
                    a.getParameter(3411),
                    a.getParameter(3412),
                    a.getParameter(3413),
                  ],
                  depthBits: a.getParameter(3414),
                  stencilBits: a.getParameter(3415),
                  subpixelBits: a.getParameter(3408),
                  extensions: Object.keys(b).filter(function (a) {
                    return !!b[a];
                  }),
                  maxAnisotropic: c,
                  maxDrawbuffers: e,
                  maxColorAttachments: g,
                  pointSizeDims: a.getParameter(33901),
                  lineWidthDims: a.getParameter(33902),
                  maxViewportDims: a.getParameter(3386),
                  maxCombinedTextureUnits: a.getParameter(35661),
                  maxCubeMapSize: a.getParameter(34076),
                  maxRenderbufferSize: a.getParameter(34024),
                  maxTextureUnits: a.getParameter(34930),
                  maxTextureSize: a.getParameter(3379),
                  maxAttributes: a.getParameter(34921),
                  maxVertexUniforms: a.getParameter(36347),
                  maxVertexTextureUnits: a.getParameter(35660),
                  maxVaryingVectors: a.getParameter(36348),
                  maxFragmentUniforms: a.getParameter(36349),
                  glsl: a.getParameter(35724),
                  renderer: a.getParameter(7937),
                  vendor: a.getParameter(7936),
                  version: a.getParameter(7938),
                  readFloat: d,
                  npotTextureCube: f,
                };
              },
              M = function (a) {
                return (
                  a instanceof Uint8Array ||
                  a instanceof Uint16Array ||
                  a instanceof Uint32Array ||
                  a instanceof Int8Array ||
                  a instanceof Int16Array ||
                  a instanceof Int32Array ||
                  a instanceof Float32Array ||
                  a instanceof Float64Array ||
                  a instanceof Uint8ClampedArray
                );
              },
              S = function (a) {
                return Object.keys(a).map(function (b) {
                  return a[b];
                });
              },
              Ma = {
                shape: function (a) {
                  for (var b = []; a.length; a = a[0]) b.push(a.length);
                  return b;
                },
                flatten: function (a, b, c, e) {
                  var g = 1;
                  if (b.length) for (var d = 0; d < b.length; ++d) g *= b[d];
                  else g = 0;
                  c = e || x.allocType(c, g);
                  switch (b.length) {
                    case 0:
                      break;
                    case 1:
                      e = b[0];
                      for (b = 0; b < e; ++b) c[b] = a[b];
                      break;
                    case 2:
                      e = b[0];
                      b = b[1];
                      for (d = g = 0; d < e; ++d)
                        for (var n = a[d], f = 0; f < b; ++f) c[g++] = n[f];
                      break;
                    case 3:
                      db(a, b[0], b[1], b[2], c, 0);
                      break;
                    default:
                      eb(a, b, 0, c, 0);
                  }
                  return c;
                },
              },
              Ia = {
                "[object Int8Array]": 5120,
                "[object Int16Array]": 5122,
                "[object Int32Array]": 5124,
                "[object Uint8Array]": 5121,
                "[object Uint8ClampedArray]": 5121,
                "[object Uint16Array]": 5123,
                "[object Uint32Array]": 5125,
                "[object Float32Array]": 5126,
                "[object Float64Array]": 5121,
                "[object ArrayBuffer]": 5121,
              },
              Ra = {
                int8: 5120,
                int16: 5122,
                int32: 5124,
                uint8: 5121,
                uint16: 5123,
                uint32: 5125,
                float: 5126,
                float32: 5126,
              },
              jb = { dynamic: 35048, stream: 35040, static: 35044 },
              Qa = Ma.flatten,
              hb = Ma.shape,
              ja = [];
            ja[5120] = 1;
            ja[5122] = 2;
            ja[5124] = 4;
            ja[5121] = 1;
            ja[5123] = 2;
            ja[5125] = 4;
            ja[5126] = 4;
            var Sa = {
                points: 0,
                point: 0,
                lines: 1,
                line: 1,
                triangles: 4,
                triangle: 4,
                "line loop": 2,
                "line strip": 3,
                "triangle strip": 5,
                "triangle fan": 6,
              },
              lb = new Float32Array(1),
              Ib = new Uint32Array(lb.buffer),
              Mb = [9984, 9986, 9985, 9987],
              La = [0, 6409, 6410, 6407, 6408],
              L = {};
            L[6409] = L[6406] = L[6402] = 1;
            L[34041] = L[6410] = 2;
            L[6407] = L[35904] = 3;
            L[6408] = L[35906] = 4;
            var Ua = Ea("HTMLCanvasElement"),
              pb = Ea("CanvasRenderingContext2D"),
              qb = Ea("ImageBitmap"),
              rb = Ea("HTMLImageElement"),
              sb = Ea("HTMLVideoElement"),
              Jb = Object.keys(Ia).concat([Ua, pb, qb, rb, sb]),
              qa = [];
            qa[5121] = 1;
            qa[5126] = 4;
            qa[36193] = 2;
            qa[5123] = 2;
            qa[5125] = 4;
            var y = [];
            y[32854] = 2;
            y[32855] = 2;
            y[36194] = 2;
            y[34041] = 4;
            y[33776] = 0.5;
            y[33777] = 0.5;
            y[33778] = 1;
            y[33779] = 1;
            y[35986] = 0.5;
            y[35987] = 1;
            y[34798] = 1;
            y[35840] = 0.5;
            y[35841] = 0.25;
            y[35842] = 0.5;
            y[35843] = 0.25;
            y[36196] = 0.5;
            var Q = [];
            Q[32854] = 2;
            Q[32855] = 2;
            Q[36194] = 2;
            Q[33189] = 2;
            Q[36168] = 1;
            Q[34041] = 4;
            Q[35907] = 4;
            Q[34836] = 16;
            Q[34842] = 8;
            Q[34843] = 6;
            var Wb = function (a, b, c, e, g) {
                function d(a) {
                  this.id = q++;
                  this.refCount = 1;
                  this.renderbuffer = a;
                  this.format = 32854;
                  this.height = this.width = 0;
                  g.profile && (this.stats = { size: 0 });
                }
                function n(b) {
                  var c = b.renderbuffer;
                  a.bindRenderbuffer(36161, null);
                  a.deleteRenderbuffer(c);
                  b.renderbuffer = null;
                  b.refCount = 0;
                  delete t[b.id];
                  e.renderbufferCount--;
                }
                var f = {
                  rgba4: 32854,
                  rgb565: 36194,
                  "rgb5 a1": 32855,
                  depth: 33189,
                  stencil: 36168,
                  "depth stencil": 34041,
                };
                b.ext_srgb && (f.srgba = 35907);
                b.ext_color_buffer_half_float &&
                  ((f.rgba16f = 34842), (f.rgb16f = 34843));
                b.webgl_color_buffer_float && (f.rgba32f = 34836);
                var r = [];
                Object.keys(f).forEach(function (a) {
                  r[f[a]] = a;
                });
                var q = 0,
                  t = {};
                d.prototype.decRef = function () {
                  0 >= --this.refCount && n(this);
                };
                g.profile &&
                  (e.getTotalRenderbufferSize = function () {
                    var a = 0;
                    Object.keys(t).forEach(function (b) {
                      a += t[b].stats.size;
                    });
                    return a;
                  });
                return {
                  create: function (b, c) {
                    function k(b, c) {
                      var d = 0,
                        e = 0,
                        m = 32854;
                      "object" === typeof b && b
                        ? ("shape" in b
                            ? ((e = b.shape), (d = e[0] | 0), (e = e[1] | 0))
                            : ("radius" in b && (d = e = b.radius | 0),
                              "width" in b && (d = b.width | 0),
                              "height" in b && (e = b.height | 0)),
                          "format" in b && (m = f[b.format]))
                        : "number" === typeof b
                        ? ((d = b | 0), (e = "number" === typeof c ? c | 0 : d))
                        : b || (d = e = 1);
                      if (d !== h.width || e !== h.height || m !== h.format)
                        return (
                          (k.width = h.width = d),
                          (k.height = h.height = e),
                          (h.format = m),
                          a.bindRenderbuffer(36161, h.renderbuffer),
                          a.renderbufferStorage(36161, m, d, e),
                          g.profile &&
                            (h.stats.size = Q[h.format] * h.width * h.height),
                          (k.format = r[h.format]),
                          k
                        );
                    }
                    var h = new d(a.createRenderbuffer());
                    t[h.id] = h;
                    e.renderbufferCount++;
                    k(b, c);
                    k.resize = function (b, c) {
                      var d = b | 0,
                        e = c | 0 || d;
                      if (d === h.width && e === h.height) return k;
                      k.width = h.width = d;
                      k.height = h.height = e;
                      a.bindRenderbuffer(36161, h.renderbuffer);
                      a.renderbufferStorage(36161, h.format, d, e);
                      g.profile &&
                        (h.stats.size = Q[h.format] * h.width * h.height);
                      return k;
                    };
                    k._reglType = "renderbuffer";
                    k._renderbuffer = h;
                    g.profile && (k.stats = h.stats);
                    k.destroy = function () {
                      h.decRef();
                    };
                    return k;
                  },
                  clear: function () {
                    S(t).forEach(n);
                  },
                  restore: function () {
                    S(t).forEach(function (b) {
                      b.renderbuffer = a.createRenderbuffer();
                      a.bindRenderbuffer(36161, b.renderbuffer);
                      a.renderbufferStorage(36161, b.format, b.width, b.height);
                    });
                    a.bindRenderbuffer(36161, null);
                  },
                };
              },
              Wa = [];
            Wa[6408] = 4;
            Wa[6407] = 3;
            var Na = [];
            Na[5121] = 1;
            Na[5126] = 4;
            Na[36193] = 2;
            var Da = ["x", "y", "z", "w"],
              Ub =
                "blend.func blend.equation stencil.func stencil.opFront stencil.opBack sample.coverage viewport scissor.box polygonOffset.offset".split(
                  " "
                ),
              Ga = {
                0: 0,
                1: 1,
                zero: 0,
                one: 1,
                "src color": 768,
                "one minus src color": 769,
                "src alpha": 770,
                "one minus src alpha": 771,
                "dst color": 774,
                "one minus dst color": 775,
                "dst alpha": 772,
                "one minus dst alpha": 773,
                "constant color": 32769,
                "one minus constant color": 32770,
                "constant alpha": 32771,
                "one minus constant alpha": 32772,
                "src alpha saturate": 776,
              },
              Xa = {
                never: 512,
                less: 513,
                "<": 513,
                equal: 514,
                "=": 514,
                "==": 514,
                "===": 514,
                lequal: 515,
                "<=": 515,
                greater: 516,
                ">": 516,
                notequal: 517,
                "!=": 517,
                "!==": 517,
                gequal: 518,
                ">=": 518,
                always: 519,
              },
              Pa = {
                0: 0,
                zero: 0,
                keep: 7680,
                replace: 7681,
                increment: 7682,
                decrement: 7683,
                "increment wrap": 34055,
                "decrement wrap": 34056,
                invert: 5386,
              },
              wb = { cw: 2304, ccw: 2305 },
              xb = new Z(!1, !1, !1, function () {}),
              Xb = function (a, b) {
                function c() {
                  this.endQueryIndex = this.startQueryIndex = -1;
                  this.sum = 0;
                  this.stats = null;
                }
                function e(a, b, d) {
                  var e = n.pop() || new c();
                  e.startQueryIndex = a;
                  e.endQueryIndex = b;
                  e.sum = 0;
                  e.stats = d;
                  f.push(e);
                }
                if (!b.ext_disjoint_timer_query) return null;
                var g = [],
                  d = [],
                  n = [],
                  f = [],
                  r = [],
                  q = [];
                return {
                  beginQuery: function (a) {
                    var c =
                      g.pop() || b.ext_disjoint_timer_query.createQueryEXT();
                    b.ext_disjoint_timer_query.beginQueryEXT(35007, c);
                    d.push(c);
                    e(d.length - 1, d.length, a);
                  },
                  endQuery: function () {
                    b.ext_disjoint_timer_query.endQueryEXT(35007);
                  },
                  pushScopeStats: e,
                  update: function () {
                    var a, c;
                    a = d.length;
                    if (0 !== a) {
                      q.length = Math.max(q.length, a + 1);
                      r.length = Math.max(r.length, a + 1);
                      r[0] = 0;
                      var e = (q[0] = 0);
                      for (c = a = 0; c < d.length; ++c) {
                        var k = d[c];
                        b.ext_disjoint_timer_query.getQueryObjectEXT(k, 34919)
                          ? ((e += b.ext_disjoint_timer_query.getQueryObjectEXT(
                              k,
                              34918
                            )),
                            g.push(k))
                          : (d[a++] = k);
                        r[c + 1] = e;
                        q[c + 1] = a;
                      }
                      d.length = a;
                      for (c = a = 0; c < f.length; ++c) {
                        var e = f[c],
                          h = e.startQueryIndex,
                          k = e.endQueryIndex;
                        e.sum += r[k] - r[h];
                        h = q[h];
                        k = q[k];
                        k === h
                          ? ((e.stats.gpuTime += e.sum / 1e6), n.push(e))
                          : ((e.startQueryIndex = h),
                            (e.endQueryIndex = k),
                            (f[a++] = e));
                      }
                      f.length = a;
                    }
                  },
                  getNumPendingQueries: function () {
                    return d.length;
                  },
                  clear: function () {
                    g.push.apply(g, d);
                    for (var a = 0; a < g.length; a++)
                      b.ext_disjoint_timer_query.deleteQueryEXT(g[a]);
                    d.length = 0;
                    g.length = 0;
                  },
                  restore: function () {
                    d.length = 0;
                    g.length = 0;
                  },
                };
              };
            return function (a) {
              function b() {
                if (0 === G.length) B && B.update(), (ca = null);
                else {
                  ca = Ya.next(b);
                  t();
                  for (var a = G.length - 1; 0 <= a; --a) {
                    var c = G[a];
                    c && c(O, null, 0);
                  }
                  k.flush();
                  B && B.update();
                }
              }
              function c() {
                !ca && 0 < G.length && (ca = Ya.next(b));
              }
              function e() {
                ca && (Ya.cancel(b), (ca = null));
              }
              function g(a) {
                a.preventDefault();
                e();
                U.forEach(function (a) {
                  a();
                });
              }
              function d(a) {
                k.getError();
                l.restore();
                Q.restore();
                F.restore();
                A.restore();
                M.restore();
                K.restore();
                B && B.restore();
                V.procs.refresh();
                c();
                W.forEach(function (a) {
                  a();
                });
              }
              function n(a) {
                function b(a) {
                  var c = {},
                    d = {};
                  Object.keys(a).forEach(function (b) {
                    var e = a[b];
                    la.isDynamic(e) ? (d[b] = la.unbox(e, b)) : (c[b] = e);
                  });
                  return { dynamic: d, static: c };
                }
                function c(a) {
                  for (; m.length < a; ) m.push(null);
                  return m;
                }
                var d = b(a.context || {}),
                  e = b(a.uniforms || {}),
                  f = b(a.attributes || {}),
                  g = b(
                    (function (a) {
                      function b(a) {
                        if (a in c) {
                          var d = c[a];
                          delete c[a];
                          Object.keys(d).forEach(function (b) {
                            c[a + "." + b] = d[b];
                          });
                        }
                      }
                      var c = E({}, a);
                      delete c.uniforms;
                      delete c.attributes;
                      delete c.context;
                      "stencil" in c &&
                        c.stencil.op &&
                        ((c.stencil.opBack = c.stencil.opFront = c.stencil.op),
                        delete c.stencil.op);
                      b("blend");
                      b("depth");
                      b("cull");
                      b("stencil");
                      b("polygonOffset");
                      b("scissor");
                      b("sample");
                      return c;
                    })(a)
                  );
                a = { gpuTime: 0, cpuTime: 0, count: 0 };
                var d = V.compile(g, f, e, d, a),
                  h = d.draw,
                  k = d.batch,
                  l = d.scope,
                  m = [];
                return E(
                  function (a, b) {
                    var d;
                    if ("function" === typeof a)
                      return l.call(this, null, a, 0);
                    if ("function" === typeof b)
                      if ("number" === typeof a)
                        for (d = 0; d < a; ++d) l.call(this, null, b, d);
                      else if (Array.isArray(a))
                        for (d = 0; d < a.length; ++d) l.call(this, a[d], b, d);
                      else return l.call(this, a, b, 0);
                    else if ("number" === typeof a) {
                      if (0 < a) return k.call(this, c(a | 0), a | 0);
                    } else if (Array.isArray(a)) {
                      if (a.length) return k.call(this, a, a.length);
                    } else return h.call(this, a);
                  },
                  { stats: a }
                );
              }
              function f(a, b) {
                var c = 0;
                V.procs.poll();
                var d = b.color;
                d &&
                  (k.clearColor(+d[0] || 0, +d[1] || 0, +d[2] || 0, +d[3] || 0),
                  (c |= 16384));
                "depth" in b && (k.clearDepth(+b.depth), (c |= 256));
                "stencil" in b && (k.clearStencil(b.stencil | 0), (c |= 1024));
                k.clear(c);
              }
              function r(a) {
                G.push(a);
                c();
                return {
                  cancel: function () {
                    function b() {
                      var a = yb(G, b);
                      G[a] = G[G.length - 1];
                      --G.length;
                      0 >= G.length && e();
                    }
                    var c = yb(G, a);
                    G[c] = b;
                  },
                };
              }
              function q() {
                var a = S.viewport,
                  b = S.scissor_box;
                a[0] = a[1] = b[0] = b[1] = 0;
                O.viewportWidth =
                  O.framebufferWidth =
                  O.drawingBufferWidth =
                  a[2] =
                  b[2] =
                    k.drawingBufferWidth;
                O.viewportHeight =
                  O.framebufferHeight =
                  O.drawingBufferHeight =
                  a[3] =
                  b[3] =
                    k.drawingBufferHeight;
              }
              function t() {
                O.tick += 1;
                O.time = y();
                q();
                V.procs.poll();
              }
              function m() {
                q();
                V.procs.refresh();
                B && B.update();
              }
              function y() {
                return (zb() - D) / 1e3;
              }
              a = Eb(a);
              if (!a) return null;
              var k = a.gl,
                h = k.getContextAttributes();
              k.isContextLost();
              var l = Fb(k, a);
              if (!l) return null;
              var u = Bb(),
                v = {
                  bufferCount: 0,
                  elementsCount: 0,
                  framebufferCount: 0,
                  shaderCount: 0,
                  textureCount: 0,
                  cubeCount: 0,
                  renderbufferCount: 0,
                  maxTextureUnits: 0,
                },
                x = l.extensions,
                B = Xb(k, x),
                D = zb(),
                J = k.drawingBufferWidth,
                P = k.drawingBufferHeight,
                O = {
                  tick: 0,
                  time: 0,
                  viewportWidth: J,
                  viewportHeight: P,
                  framebufferWidth: J,
                  framebufferHeight: P,
                  drawingBufferWidth: J,
                  drawingBufferHeight: P,
                  pixelRatio: a.pixelRatio,
                },
                R = Vb(k, x),
                J = Pb(k, x, R, u),
                F = Gb(k, v, a, J),
                T = Hb(k, x, F, v),
                Q = Qb(k, u, v, a),
                A = Kb(
                  k,
                  x,
                  R,
                  function () {
                    V.procs.poll();
                  },
                  O,
                  v,
                  a
                ),
                M = Wb(k, x, R, v, a),
                K = Ob(k, x, R, A, M, v),
                V = Tb(
                  k,
                  u,
                  x,
                  R,
                  F,
                  T,
                  A,
                  K,
                  {},
                  J,
                  Q,
                  {
                    elements: null,
                    primitive: 4,
                    count: -1,
                    offset: 0,
                    instances: -1,
                  },
                  O,
                  B,
                  a
                ),
                u = Rb(k, K, V.procs.poll, O, h, x, R),
                S = V.next,
                L = k.canvas,
                G = [],
                U = [],
                W = [],
                Z = [a.onDestroy],
                ca = null;
              L &&
                (L.addEventListener("webglcontextlost", g, !1),
                L.addEventListener("webglcontextrestored", d, !1));
              var aa = (K.setFBO = n({
                framebuffer: la.define.call(null, 1, "framebuffer"),
              }));
              m();
              h = E(n, {
                clear: function (a) {
                  if ("framebuffer" in a)
                    if (
                      a.framebuffer &&
                      "framebufferCube" === a.framebuffer_reglType
                    )
                      for (var b = 0; 6 > b; ++b)
                        aa(E({ framebuffer: a.framebuffer.faces[b] }, a), f);
                    else aa(a, f);
                  else f(null, a);
                },
                prop: la.define.bind(null, 1),
                context: la.define.bind(null, 2),
                this: la.define.bind(null, 3),
                draw: n({}),
                buffer: function (a) {
                  return F.create(a, 34962, !1, !1);
                },
                elements: function (a) {
                  return T.create(a, !1);
                },
                texture: A.create2D,
                cube: A.createCube,
                renderbuffer: M.create,
                framebuffer: K.create,
                framebufferCube: K.createCube,
                attributes: h,
                frame: r,
                on: function (a, b) {
                  var c;
                  switch (a) {
                    case "frame":
                      return r(b);
                    case "lost":
                      c = U;
                      break;
                    case "restore":
                      c = W;
                      break;
                    case "destroy":
                      c = Z;
                  }
                  c.push(b);
                  return {
                    cancel: function () {
                      for (var a = 0; a < c.length; ++a)
                        if (c[a] === b) {
                          c[a] = c[c.length - 1];
                          c.pop();
                          break;
                        }
                    },
                  };
                },
                limits: R,
                hasExtension: function (a) {
                  return 0 <= R.extensions.indexOf(a.toLowerCase());
                },
                read: u,
                destroy: function () {
                  G.length = 0;
                  e();
                  L &&
                    (L.removeEventListener("webglcontextlost", g),
                    L.removeEventListener("webglcontextrestored", d));
                  Q.clear();
                  K.clear();
                  M.clear();
                  A.clear();
                  T.clear();
                  F.clear();
                  B && B.clear();
                  Z.forEach(function (a) {
                    a();
                  });
                },
                _gl: k,
                _refresh: m,
                poll: function () {
                  t();
                  B && B.update();
                },
                now: y,
                stats: v,
              });
              a.onDone(null, h);
              return h;
            };
          });
        },
        {},
      ],
      9: [
        function (require, module, exports) {
          (function (global) {
            (function () {
              module.exports =
                global.performance && global.performance.now
                  ? function now() {
                      return performance.now();
                    }
                  : Date.now ||
                    function now() {
                      return +new Date();
                    };
            }.call(this));
          }.call(
            this,
            typeof global !== "undefined"
              ? global
              : typeof self !== "undefined"
              ? self
              : typeof window !== "undefined"
              ? window
              : {}
          ));
        },
        {},
      ],
      10: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = void 0;

          var _sandbox = _interopRequireDefault(require("./lib/sandbox.js"));

          var _arrayUtils = _interopRequireDefault(
            require("./lib/array-utils.js")
          );

          function _interopRequireDefault(obj) {
            return obj && obj.__esModule ? obj : { default: obj };
          }

          // handles code evaluation and attaching relevant objects to global and evaluation contexts
          class EvalSandbox {
            constructor(parent, makeGlobal, userProps = []) {
              this.makeGlobal = makeGlobal;
              this.sandbox = (0, _sandbox.default)(parent);
              this.parent = parent;
              var properties = Object.keys(parent);
              properties.forEach((property) => this.add(property));
              this.userProps = userProps;
            }

            add(name) {
              if (this.makeGlobal) window[name] = this.parent[name];
              this.sandbox.addToContext(name, `parent.${name}`);
            } // sets on window as well as synth object if global (not needed for objects, which can be set directly)

            set(property, value) {
              if (this.makeGlobal) {
                window[property] = value;
              }

              this.parent[property] = value;
            }

            tick() {
              if (this.makeGlobal) {
                this.userProps.forEach((property) => {
                  this.parent[property] = window[property];
                }); //  this.parent.speed = window.speed
              } else {
              }
            }

            eval(code) {
              this.sandbox.eval(code);
            }
          }

          var _default = EvalSandbox;
          exports.default = _default;
        },
        { "./lib/array-utils.js": 20, "./lib/sandbox.js": 25 },
      ],
      11: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = formatArguments;

          var _arrayUtils = _interopRequireDefault(
            require("./lib/array-utils.js")
          );

          function _interopRequireDefault(obj) {
            return obj && obj.__esModule ? obj : { default: obj };
          }

          // [WIP] how to treat different dimensions (?)
          const DEFAULT_CONVERSIONS = {
            float: {
              vec4: {
                name: "sum",
                args: [[1, 1, 1, 1]],
              },
              vec2: {
                name: "sum",
                args: [[1, 1]],
              },
            },
          };

          function fillArrayWithDefaults(arr, len) {
            // fill the array with default values if it's too short
            while (arr.length < len) {
              if (arr.length === 3) {
                // push a 1 as the default for .a in vec4
                arr.push(1.0);
              } else {
                arr.push(0.0);
              }
            }

            return arr.slice(0, len);
          }

          const ensure_decimal_dot = (val) => {
            val = val.toString();

            if (val.indexOf(".") < 0) {
              val += ".";
            }

            return val;
          };

          function formatArguments(transform, startIndex, synthContext) {
            const defaultArgs = transform.transform.inputs;
            const userArgs = transform.userArgs;
            const { generators } = transform.synth;
            const { src } = generators; // depends on synth having src() function

            return defaultArgs.map((input, index) => {
              const typedArg = {
                value: input.default,
                type: input.type,
                //
                isUniform: false,
                name: input.name,
                vecLen: 0, //  generateGlsl: null // function for creating glsl
              };
              if (typedArg.type === "float")
                typedArg.value = ensure_decimal_dot(input.default);

              if (input.type.startsWith("vec")) {
                try {
                  typedArg.vecLen = Number.parseInt(input.type.substr(3));
                } catch (e) {
                  console.log(
                    `Error determining length of vector input type ${input.type} (${input.name})`
                  );
                }
              } // if user has input something for this argument

              if (userArgs.length > index) {
                typedArg.value = userArgs[index]; // do something if a composite or transform

                if (typeof userArgs[index] === "function") {
                  // if (typedArg.vecLen > 0) { // expected input is a vector, not a scalar
                  //    typedArg.value = (context, props, batchId) => (fillArrayWithDefaults(userArgs[index](props), typedArg.vecLen))
                  // } else {
                  typedArg.value = (context, props, batchId) => {
                    try {
                      const val = userArgs[index](props);

                      if (typeof val === "number") {
                        return val;
                      } else {
                        console.warn(
                          "function does not return a number",
                          userArgs[index]
                        );
                      }

                      return input.default;
                    } catch (e) {
                      console.warn("ERROR", e);
                      return input.default;
                    }
                  }; //  }

                  typedArg.isUniform = true;
                } else if (userArgs[index].constructor === Array) {
                  //   if (typedArg.vecLen > 0) { // expected input is a vector, not a scalar
                  //     typedArg.isUniform = true
                  //     typedArg.value = fillArrayWithDefaults(typedArg.value, typedArg.vecLen)
                  //  } else {
                  //  console.log("is Array")
                  // filter out values that are not a number
                  // const filteredArray = userArgs[index].filter((val) => typeof val === 'number')
                  // typedArg.value = (context, props, batchId) => arrayUtils.getValue(filteredArray)(props)
                  typedArg.value = (context, props, batchId) =>
                    _arrayUtils.default.getValue(userArgs[index])(props);

                  typedArg.isUniform = true; // }
                }
              }

              if (startIndex < 0) {
              } else {
                if (typedArg.value && typedArg.value.transforms) {
                  const final_transform =
                    typedArg.value.transforms[
                      typedArg.value.transforms.length - 1
                    ];

                  if (
                    final_transform.transform.glsl_return_type !== input.type
                  ) {
                    const defaults = DEFAULT_CONVERSIONS[input.type];

                    if (typeof defaults !== "undefined") {
                      const default_def =
                        defaults[final_transform.transform.glsl_return_type];

                      if (typeof default_def !== "undefined") {
                        const { name, args } = default_def;
                        typedArg.value = typedArg.value[name](...args);
                      }
                    }
                  }

                  typedArg.isUniform = false;
                } else if (
                  typedArg.type === "float" &&
                  typeof typedArg.value === "number"
                ) {
                  typedArg.value = ensure_decimal_dot(typedArg.value);
                } else if (
                  typedArg.type.startsWith("vec") &&
                  typeof typedArg.value === "object" &&
                  Array.isArray(typedArg.value)
                ) {
                  typedArg.isUniform = false;
                  typedArg.value = `${typedArg.type}(${typedArg.value
                    .map(ensure_decimal_dot)
                    .join(", ")})`;
                } else if (input.type === "sampler2D") {
                  // typedArg.tex = typedArg.value
                  var x = typedArg.value;

                  typedArg.value = () => x.getTexture();

                  typedArg.isUniform = true;
                } else {
                  // if passing in a texture reference, when function asks for vec4, convert to vec4
                  if (typedArg.value.getTexture && input.type === "vec4") {
                    var x1 = typedArg.value;
                    typedArg.value = src(x1);
                    typedArg.isUniform = false;
                  }
                } // add tp uniform array if is a function that will pass in a different value on each render frame,
                // or a texture/ external source

                if (typedArg.isUniform) {
                  typedArg.name += startIndex; //  shaderParams.uniforms.push(typedArg)
                }
              }

              return typedArg;
            });
          }
        },
        { "./lib/array-utils.js": 20 },
      ],
      12: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = _default;

          var _formatArguments = _interopRequireDefault(
            require("./format-arguments.js")
          );

          var _arrayUtils = _interopRequireDefault(
            require("./lib/array-utils.js")
          );

          function _interopRequireDefault(obj) {
            return obj && obj.__esModule ? obj : { default: obj };
          }

          // Add extra functionality to Array.prototype for generating sequences in time
          // converts a tree of javascript functions to a shader
          function _default(transforms) {
            var shaderParams = {
              uniforms: [],
              // list of uniforms used in shader
              glslFunctions: [],
              // list of functions used in shader
              fragColor: "",
            };
            var gen = generateGlsl(transforms, shaderParams)("st");
            shaderParams.fragColor = gen; // remove uniforms with duplicate names

            let uniforms = {};
            shaderParams.uniforms.forEach(
              (uniform) => (uniforms[uniform.name] = uniform)
            );
            shaderParams.uniforms = Object.values(uniforms);
            return shaderParams;
          } // recursive function for generating shader string from object containing functions and user arguments. Order of functions in string depends on type of function
          // to do: improve variable names

          function generateGlsl(transforms, shaderParams) {
            // transform function that outputs a shader string corresponding to gl_FragColor
            var fragColor = () => ""; // var uniforms = []
            // var glslFunctions = []

            transforms.forEach((transform) => {
              var inputs = (0, _formatArguments.default)(
                transform,
                shaderParams.uniforms.length
              );
              inputs.forEach((input) => {
                if (input.isUniform) shaderParams.uniforms.push(input);
              }); // add new glsl function to running list of functions

              if (!contains(transform, shaderParams.glslFunctions))
                shaderParams.glslFunctions.push(transform); // current function for generating frag color shader code

              var f0 = fragColor;

              if (transform.transform.type === "src") {
                fragColor = (uv) =>
                  `${shaderString(uv, transform.name, inputs, shaderParams)}`;
              } else if (transform.transform.type === "coord") {
                fragColor = (uv) =>
                  `${f0(
                    `${shaderString(uv, transform.name, inputs, shaderParams)}`
                  )}`;
              } else if (transform.transform.type === "color") {
                fragColor = (uv) =>
                  `${shaderString(
                    `${f0(uv)}`,
                    transform.name,
                    inputs,
                    shaderParams
                  )}`;
              } else if (transform.transform.type === "combine") {
                // combining two generated shader strings (i.e. for blend, mult, add funtions)
                var f1 =
                  inputs[0].value && inputs[0].value.transforms
                    ? (uv) =>
                        `${generateGlsl(
                          inputs[0].value.transforms,
                          shaderParams
                        )(uv)}`
                    : inputs[0].isUniform
                    ? () => inputs[0].name
                    : () => inputs[0].value;

                fragColor = (uv) =>
                  `${shaderString(
                    `${f0(uv)}, ${f1(uv)}`,
                    transform.name,
                    inputs.slice(1),
                    shaderParams
                  )}`;
              } else if (transform.transform.type === "combineCoord") {
                // combining two generated shader strings (i.e. for modulate functions)
                var f1 =
                  inputs[0].value && inputs[0].value.transforms
                    ? (uv) =>
                        `${generateGlsl(
                          inputs[0].value.transforms,
                          shaderParams
                        )(uv)}`
                    : inputs[0].isUniform
                    ? () => inputs[0].name
                    : () => inputs[0].value;

                fragColor = (uv) =>
                  `${f0(
                    `${shaderString(
                      `${uv}, ${f1(uv)}`,
                      transform.name,
                      inputs.slice(1),
                      shaderParams
                    )}`
                  )}`;
              }
            }); //  console.log(fragColor)
            //  break;

            return fragColor;
          } // assembles a shader string containing the arguments and the function name, i.e. 'osc(uv, frequency)'

          function shaderString(uv, method, inputs, shaderParams) {
            const str = inputs
              .map((input) => {
                if (input.isUniform) {
                  return input.name;
                } else if (input.value && input.value.transforms) {
                  // this by definition needs to be a generator, hence we start with 'st' as the initial value for generating the glsl fragment
                  return `${generateGlsl(
                    input.value.transforms,
                    shaderParams
                  )("st")}`;
                }

                return input.value;
              })
              .reduce((p, c) => `${p}, ${c}`, "");
            return `${method}(${uv}${str})`;
          } // merge two arrays and remove duplicates

          function mergeArrays(a, b) {
            return a.concat(
              b.filter(function (item) {
                return a.indexOf(item) < 0;
              })
            );
          } // check whether array

          function contains(object, arr) {
            for (var i = 0; i < arr.length; i++) {
              if (object.name == arr[i].name) return true;
            }

            return false;
          }
        },
        { "./format-arguments.js": 11, "./lib/array-utils.js": 20 },
      ],
      13: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = void 0;

          var _glslSource = _interopRequireDefault(require("./glsl-source.js"));

          var _glslFunctions = _interopRequireDefault(
            require("./glsl/glsl-functions.js")
          );

          function _interopRequireDefault(obj) {
            return obj && obj.__esModule ? obj : { default: obj };
          }

          class GeneratorFactory {
            constructor({
              defaultUniforms,
              defaultOutput,
              extendTransforms = [],
              changeListener = () => {},
            } = {}) {
              this.defaultOutput = defaultOutput;
              this.defaultUniforms = defaultUniforms;
              this.changeListener = changeListener;
              this.extendTransforms = extendTransforms;
              this.generators = {};
              this.init();
            }

            init() {
              const functions = (0, _glslFunctions.default)();
              this.glslTransforms = {};
              this.generators = Object.entries(this.generators).reduce(
                (prev, [method, transform]) => {
                  this.changeListener({
                    type: "remove",
                    synth: this,
                    method,
                  });
                  return prev;
                },
                {}
              );

              this.sourceClass = (() => {
                return class extends _glslSource.default {};
              })(); // add user definied transforms

              if (Array.isArray(this.extendTransforms)) {
                functions.concat(this.extendTransforms);
              } else if (
                typeof this.extendTransforms === "object" &&
                this.extendTransforms.type
              ) {
                functions.push(this.extendTransforms);
              }

              return functions.map((transform) => this.setFunction(transform));
            }

            _addMethod(method, transform) {
              const self = this;
              this.glslTransforms[method] = transform;

              if (transform.type === "src") {
                const func = (...args) =>
                  new this.sourceClass({
                    name: method,
                    transform: transform,
                    userArgs: args,
                    defaultOutput: this.defaultOutput,
                    defaultUniforms: this.defaultUniforms,
                    synth: self,
                  });

                this.generators[method] = func;
                this.changeListener({
                  type: "add",
                  synth: this,
                  method,
                });
                return func;
              } else {
                this.sourceClass.prototype[method] = function (...args) {
                  this.transforms.push({
                    name: method,
                    transform: transform,
                    userArgs: args,
                    synth: self,
                  });
                  return this;
                };
              }

              return undefined;
            }

            setFunction(obj) {
              var processedGlsl = processGlsl(obj);
              if (processedGlsl) this._addMethod(obj.name, processedGlsl);
            }
          }

          const typeLookup = {
            src: {
              returnType: "vec4",
              args: ["vec2 _st"],
            },
            coord: {
              returnType: "vec2",
              args: ["vec2 _st"],
            },
            color: {
              returnType: "vec4",
              args: ["vec4 _c0"],
            },
            combine: {
              returnType: "vec4",
              args: ["vec4 _c0", "vec4 _c1"],
            },
            combineCoord: {
              returnType: "vec2",
              args: ["vec2 _st", "vec4 _c0"],
            },
          }; // expects glsl of format
          // {
          //   name: 'osc', // name that will be used to access function as well as within glsl
          //   type: 'src', // can be src: vec4(vec2 _st), coord: vec2(vec2 _st), color: vec4(vec4 _c0), combine: vec4(vec4 _c0, vec4 _c1), combineCoord: vec2(vec2 _st, vec4 _c0)
          //   inputs: [
          //     {
          //       name: 'freq',
          //       type: 'float', // 'float'   //, 'texture', 'vec4'
          //       default: 0.2
          //     },
          //     {
          //           name: 'sync',
          //           type: 'float',
          //           default: 0.1
          //         },
          //         {
          //           name: 'offset',
          //           type: 'float',
          //           default: 0.0
          //         }
          //   ],
          //  glsl: `
          //    vec2 st = _st;
          //    float r = sin((st.x-offset*2/freq+time*sync)*freq)*0.5  + 0.5;
          //    float g = sin((st.x+time*sync)*freq)*0.5 + 0.5;
          //    float b = sin((st.x+offset/freq+time*sync)*freq)*0.5  + 0.5;
          //    return vec4(r, g, b, 1.0);
          // `
          // }
          // // generates glsl function:
          // `vec4 osc(vec2 _st, float freq, float sync, float offset){
          //  vec2 st = _st;
          //  float r = sin((st.x-offset*2/freq+time*sync)*freq)*0.5  + 0.5;
          //  float g = sin((st.x+time*sync)*freq)*0.5 + 0.5;
          //  float b = sin((st.x+offset/freq+time*sync)*freq)*0.5  + 0.5;
          //  return vec4(r, g, b, 1.0);
          // }`

          function processGlsl(obj) {
            let t = typeLookup[obj.type];

            if (t) {
              let baseArgs = t.args.map((arg) => arg).join(", "); // @todo: make sure this works for all input types, add validation

              let customArgs = obj.inputs
                .map((input) => `${input.type} ${input.name}`)
                .join(", ");
              let args = `${baseArgs}${
                customArgs.length > 0 ? ", " + customArgs : ""
              }`; //  console.log('args are ', args)

              let glslFunction = `
    ${t.returnType} ${obj.name}(${args}) {
        ${obj.glsl}
    }
  `; // add extra input to beginning for backward combatibility @todo update compiler so this is no longer necessary

              if (obj.type === "combine" || obj.type === "combineCoord")
                obj.inputs.unshift({
                  name: "color",
                  type: "vec4",
                });
              return Object.assign({}, obj, {
                glsl: glslFunction,
              });
            } else {
              console.warn(`type ${obj.type} not recognized`, obj);
            }
          }

          var _default = GeneratorFactory;
          exports.default = _default;
        },
        { "./glsl-source.js": 14, "./glsl/glsl-functions.js": 15 },
      ],
      14: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = void 0;

          var _generateGlsl = _interopRequireDefault(
            require("./generate-glsl.js")
          );

          var _utilityFunctions = _interopRequireDefault(
            require("./glsl/utility-functions.js")
          );

          function _interopRequireDefault(obj) {
            return obj && obj.__esModule ? obj : { default: obj };
          }

          // const formatArguments = require('./glsl-utils.js').formatArguments
          // const glslTransforms = require('./glsl/composable-glsl-functions.js')
          var GlslSource = function (obj) {
            this.transforms = [];
            this.transforms.push(obj);
            this.defaultOutput = obj.defaultOutput;
            this.synth = obj.synth;
            this.type = "GlslSource";
            this.defaultUniforms = obj.defaultUniforms;
            return this;
          };

          GlslSource.prototype.addTransform = function (obj) {
            this.transforms.push(obj);
          };

          GlslSource.prototype.out = function (_output) {
            var output = _output || this.defaultOutput;
            var glsl = this.glsl(output);
            this.synth.currentFunctions = []; // output.renderPasses(glsl)

            if (output)
              try {
                output.render(glsl);
              } catch (error) {
                console.log("shader could not compile", error);
              }
          };

          GlslSource.prototype.glsl = function () {
            //var output = _output || this.defaultOutput
            var self = this; // uniforms included in all shaders
            //  this.defaultUniforms = output.uniforms

            var passes = [];
            var transforms = []; //  console.log('output', output)

            this.transforms.forEach((transform) => {
              if (transform.transform.type === "renderpass") {
                // if (transforms.length > 0) passes.push(this.compile(transforms, output))
                // transforms = []
                // var uniforms = {}
                // const inputs = formatArguments(transform, -1)
                // inputs.forEach((uniform) => { uniforms[uniform.name] = uniform.value })
                //
                // passes.push({
                //   frag: transform.transform.frag,
                //   uniforms: Object.assign({}, self.defaultUniforms, uniforms)
                // })
                // transforms.push({name: 'prev', transform:  glslTransforms['prev'], synth: this.synth})
                console.warn("no support for renderpass");
              } else {
                transforms.push(transform);
              }
            });
            if (transforms.length > 0) passes.push(this.compile(transforms));
            return passes;
          };

          GlslSource.prototype.compile = function (transforms) {
            var shaderInfo = (0, _generateGlsl.default)(transforms, this.synth);
            var uniforms = {};
            shaderInfo.uniforms.forEach((uniform) => {
              uniforms[uniform.name] = uniform.value;
            });
            var frag = `
    precision ${this.defaultOutput.precision} float;
    ${Object.values(shaderInfo.uniforms)
      .map((uniform) => {
        let type = uniform.type;

        switch (uniform.type) {
          case "texture":
            type = "sampler2D";
            break;
        }

        return `
        uniform ${type} ${uniform.name};`;
      })
      .join("")}
    uniform float time;
    uniform vec2 resolution;
    varying vec2 uv;
    uniform sampler2D prevBuffer;

    ${Object.values(_utilityFunctions.default)
      .map((transform) => {
        //  console.log(transform.glsl)
        return `
              ${transform.glsl}
            `;
      })
      .join("")}

    ${shaderInfo.glslFunctions
      .map((transform) => {
        return `
              ${transform.transform.glsl}
            `;
      })
      .join("")}

    void main () {
      vec4 c = vec4(1, 0, 0, 1);
      vec2 st = gl_FragCoord.xy/resolution.xy;
      gl_FragColor = ${shaderInfo.fragColor};
    }
    `;
            return {
              frag: frag,
              uniforms: Object.assign({}, this.defaultUniforms, uniforms),
            };
          };

          var _default = GlslSource;
          exports.default = _default;
        },
        { "./generate-glsl.js": 12, "./glsl/utility-functions.js": 16 },
      ],
      15: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = void 0;

          /*
  Format for adding functions to hydra. For each entry in this file, hydra automatically generates a glsl function and javascript function with the same name. You can also ass functions dynamically using setFunction(object).

  {
    name: 'osc', // name that will be used to access function in js as well as in glsl
    type: 'src', // can be 'src', 'color', 'combine', 'combineCoords'. see below for more info
    inputs: [
      {
        name: 'freq',
        type: 'float',
        default: 0.2
      },
      {
        name: 'sync',
        type: 'float',
        default: 0.1
      },
      {
        name: 'offset',
        type: 'float',
        default: 0.0
      }
    ],
      glsl: `
        vec2 st = _st;
        float r = sin((st.x-offset*2/freq+time*sync)*freq)*0.5  + 0.5;
        float g = sin((st.x+time*sync)*freq)*0.5 + 0.5;
        float b = sin((st.x+offset/freq+time*sync)*freq)*0.5  + 0.5;
        return vec4(r, g, b, 1.0);
     `
  }

  // The above code generates the glsl function:
  `vec4 osc(vec2 _st, float freq, float sync, float offset){
   vec2 st = _st;
   float r = sin((st.x-offset*2/freq+time*sync)*freq)*0.5  + 0.5;
   float g = sin((st.x+time*sync)*freq)*0.5 + 0.5;
   float b = sin((st.x+offset/freq+time*sync)*freq)*0.5  + 0.5;
   return vec4(r, g, b, 1.0);
  }`


  Types and default arguments for hydra functions.
  The value in the 'type' field lets the parser know which type the function will be returned as well as default arguments.

  const types = {
    'src': {
      returnType: 'vec4',
      args: ['vec2 _st']
    },
    'coord': {
      returnType: 'vec2',
      args: ['vec2 _st']
    },
    'color': {
      returnType: 'vec4',
      args: ['vec4 _c0']
    },
    'combine': {
      returnType: 'vec4',
      args: ['vec4 _c0', 'vec4 _c1']
    },
    'combineCoord': {
      returnType: 'vec2',
      args: ['vec2 _st', 'vec4 _c0']
    }
  }

  */
          var _default = () => [
            {
              name: "noise",
              type: "src",
              inputs: [
                {
                  type: "float",
                  name: "scale",
                  default: 10,
                },
                {
                  type: "float",
                  name: "offset",
                  default: 0.1,
                },
              ],
              glsl: `   return vec4(vec3(_noise(vec3(_st*scale, offset*time))), 1.0);`,
            },
            {
              name: "voronoi",
              type: "src",
              inputs: [
                {
                  type: "float",
                  name: "scale",
                  default: 5,
                },
                {
                  type: "float",
                  name: "speed",
                  default: 0.3,
                },
                {
                  type: "float",
                  name: "blending",
                  default: 0.3,
                },
              ],
              glsl: `   vec3 color = vec3(.0);
     // Scale
     _st *= scale;
     // Tile the space
     vec2 i_st = floor(_st);
     vec2 f_st = fract(_st);
     float m_dist = 10.;  // minimun distance
     vec2 m_point;        // minimum point
     for (int j=-1; j<=1; j++ ) {
     for (int i=-1; i<=1; i++ ) {
     vec2 neighbor = vec2(float(i),float(j));
     vec2 p = i_st + neighbor;
     vec2 point = fract(sin(vec2(dot(p,vec2(127.1,311.7)),dot(p,vec2(269.5,183.3))))*43758.5453);
     point = 0.5 + 0.5*sin(time*speed + 6.2831*point);
     vec2 diff = neighbor + point - f_st;
     float dist = length(diff);
     if( dist < m_dist ) {
     m_dist = dist;
     m_point = point;
     }
     }
     }
     // Assign a color using the closest point position
     color += dot(m_point,vec2(.3,.6));
     color *= 1.0 - blending*m_dist;
     return vec4(color, 1.0);`,
            },
            {
              name: "osc",
              type: "src",
              inputs: [
                {
                  type: "float",
                  name: "frequency",
                  default: 60,
                },
                {
                  type: "float",
                  name: "sync",
                  default: 0.1,
                },
                {
                  type: "float",
                  name: "offset",
                  default: 0,
                },
              ],
              glsl: `   vec2 st = _st;
     float r = sin((st.x-offset/frequency+time*sync)*frequency)*0.5  + 0.5;
     float g = sin((st.x+time*sync)*frequency)*0.5 + 0.5;
     float b = sin((st.x+offset/frequency+time*sync)*frequency)*0.5  + 0.5;
     return vec4(r, g, b, 1.0);`,
            },
            {
              name: "shape",
              type: "src",
              inputs: [
                {
                  type: "float",
                  name: "sides",
                  default: 3,
                },
                {
                  type: "float",
                  name: "radius",
                  default: 0.3,
                },
                {
                  type: "float",
                  name: "smoothing",
                  default: 0.01,
                },
              ],
              glsl: `   vec2 st = _st * 2. - 1.;
     // Angle and radius from the current pixel
     float a = atan(st.x,st.y)+3.1416;
     float r = (2.*3.1416)/sides;
     float d = cos(floor(.5+a/r)*r-a)*length(st);
     return vec4(vec3(1.0-smoothstep(radius,radius + smoothing + 0.0000001,d)), 1.0);`,
            },
            {
              name: "gradient",
              type: "src",
              inputs: [
                {
                  type: "float",
                  name: "speed",
                  default: 0,
                },
              ],
              glsl: `   return vec4(_st, sin(time*speed), 1.0);`,
            },
            {
              name: "src",
              type: "src",
              inputs: [
                {
                  type: "sampler2D",
                  name: "tex",
                  default: NaN,
                },
              ],
              glsl: `   //  vec2 uv = gl_FragCoord.xy/vec2(1280., 720.);
     return texture2D(tex, fract(_st));`,
            },
            {
              name: "solid",
              type: "src",
              inputs: [
                {
                  type: "float",
                  name: "r",
                  default: 0,
                },
                {
                  type: "float",
                  name: "g",
                  default: 0,
                },
                {
                  type: "float",
                  name: "b",
                  default: 0,
                },
                {
                  type: "float",
                  name: "a",
                  default: 1,
                },
              ],
              glsl: `   return vec4(r, g, b, a);`,
            },
            {
              name: "rotate",
              type: "coord",
              inputs: [
                {
                  type: "float",
                  name: "angle",
                  default: 10,
                },
                {
                  type: "float",
                  name: "speed",
                  default: 0,
                },
              ],
              glsl: `   vec2 xy = _st - vec2(0.5);
     float ang = angle + speed *time;
     xy = mat2(cos(ang),-sin(ang), sin(ang),cos(ang))*xy;
     xy += 0.5;
     return xy;`,
            },
            {
              name: "scale",
              type: "coord",
              inputs: [
                {
                  type: "float",
                  name: "amount",
                  default: 1.5,
                },
                {
                  type: "float",
                  name: "xMult",
                  default: 1,
                },
                {
                  type: "float",
                  name: "yMult",
                  default: 1,
                },
                {
                  type: "float",
                  name: "offsetX",
                  default: 0.5,
                },
                {
                  type: "float",
                  name: "offsetY",
                  default: 0.5,
                },
              ],
              glsl: `   vec2 xy = _st - vec2(offsetX, offsetY);
     xy*=(1.0/vec2(amount*xMult, amount*yMult));
     xy+=vec2(offsetX, offsetY);
     return xy;
     `,
            },
            {
              name: "pixelate",
              type: "coord",
              inputs: [
                {
                  type: "float",
                  name: "pixelX",
                  default: 20,
                },
                {
                  type: "float",
                  name: "pixelY",
                  default: 20,
                },
              ],
              glsl: `   vec2 xy = vec2(pixelX, pixelY);
     return (floor(_st * xy) + 0.5)/xy;`,
            },
            {
              name: "posterize",
              type: "color",
              inputs: [
                {
                  type: "float",
                  name: "bins",
                  default: 3,
                },
                {
                  type: "float",
                  name: "gamma",
                  default: 0.6,
                },
              ],
              glsl: `   vec4 c2 = pow(_c0, vec4(gamma));
     c2 *= vec4(bins);
     c2 = floor(c2);
     c2/= vec4(bins);
     c2 = pow(c2, vec4(1.0/gamma));
     return vec4(c2.xyz, _c0.a);`,
            },
            {
              name: "shift",
              type: "color",
              inputs: [
                {
                  type: "float",
                  name: "r",
                  default: 0.5,
                },
                {
                  type: "float",
                  name: "g",
                  default: 0,
                },
                {
                  type: "float",
                  name: "b",
                  default: 0,
                },
                {
                  type: "float",
                  name: "a",
                  default: 0,
                },
              ],
              glsl: `   vec4 c2 = vec4(_c0);
     c2.r = fract(c2.r + r);
     c2.g = fract(c2.g + g);
     c2.b = fract(c2.b + b);
     c2.a = fract(c2.a + a);
     return vec4(c2.rgba);`,
            },
            {
              name: "repeat",
              type: "coord",
              inputs: [
                {
                  type: "float",
                  name: "repeatX",
                  default: 3,
                },
                {
                  type: "float",
                  name: "repeatY",
                  default: 3,
                },
                {
                  type: "float",
                  name: "offsetX",
                  default: 0,
                },
                {
                  type: "float",
                  name: "offsetY",
                  default: 0,
                },
              ],
              glsl: `   vec2 st = _st * vec2(repeatX, repeatY);
     st.x += step(1., mod(st.y,2.0)) * offsetX;
     st.y += step(1., mod(st.x,2.0)) * offsetY;
     return fract(st);`,
            },
            {
              name: "modulateRepeat",
              type: "combineCoord",
              inputs: [
                {
                  type: "float",
                  name: "repeatX",
                  default: 3,
                },
                {
                  type: "float",
                  name: "repeatY",
                  default: 3,
                },
                {
                  type: "float",
                  name: "offsetX",
                  default: 0.5,
                },
                {
                  type: "float",
                  name: "offsetY",
                  default: 0.5,
                },
              ],
              glsl: `   vec2 st = _st * vec2(repeatX, repeatY);
     st.x += step(1., mod(st.y,2.0)) + _c0.r * offsetX;
     st.y += step(1., mod(st.x,2.0)) + _c0.g * offsetY;
     return fract(st);`,
            },
            {
              name: "repeatX",
              type: "coord",
              inputs: [
                {
                  type: "float",
                  name: "reps",
                  default: 3,
                },
                {
                  type: "float",
                  name: "offset",
                  default: 0,
                },
              ],
              glsl: `   vec2 st = _st * vec2(reps, 1.0);
     //  float f =  mod(_st.y,2.0);
     st.y += step(1., mod(st.x,2.0))* offset;
     return fract(st);`,
            },
            {
              name: "modulateRepeatX",
              type: "combineCoord",
              inputs: [
                {
                  type: "float",
                  name: "reps",
                  default: 3,
                },
                {
                  type: "float",
                  name: "offset",
                  default: 0.5,
                },
              ],
              glsl: `   vec2 st = _st * vec2(reps, 1.0);
     //  float f =  mod(_st.y,2.0);
     st.y += step(1., mod(st.x,2.0)) + _c0.r * offset;
     return fract(st);`,
            },
            {
              name: "repeatY",
              type: "coord",
              inputs: [
                {
                  type: "float",
                  name: "reps",
                  default: 3,
                },
                {
                  type: "float",
                  name: "offset",
                  default: 0,
                },
              ],
              glsl: `   vec2 st = _st * vec2(1.0, reps);
     //  float f =  mod(_st.y,2.0);
     st.x += step(1., mod(st.y,2.0))* offset;
     return fract(st);`,
            },
            {
              name: "modulateRepeatY",
              type: "combineCoord",
              inputs: [
                {
                  type: "float",
                  name: "reps",
                  default: 3,
                },
                {
                  type: "float",
                  name: "offset",
                  default: 0.5,
                },
              ],
              glsl: `   vec2 st = _st * vec2(reps, 1.0);
     //  float f =  mod(_st.y,2.0);
     st.x += step(1., mod(st.y,2.0)) + _c0.r * offset;
     return fract(st);`,
            },
            {
              name: "kaleid",
              type: "coord",
              inputs: [
                {
                  type: "float",
                  name: "nSides",
                  default: 4,
                },
              ],
              glsl: `   vec2 st = _st;
     st -= 0.5;
     float r = length(st);
     float a = atan(st.y, st.x);
     float pi = 2.*3.1416;
     a = mod(a,pi/nSides);
     a = abs(a-pi/nSides/2.);
     return r*vec2(cos(a), sin(a));`,
            },
            {
              name: "modulateKaleid",
              type: "combineCoord",
              inputs: [
                {
                  type: "float",
                  name: "nSides",
                  default: 4,
                },
              ],
              glsl: `   vec2 st = _st - 0.5;
     float r = length(st);
     float a = atan(st.y, st.x);
     float pi = 2.*3.1416;
     a = mod(a,pi/nSides);
     a = abs(a-pi/nSides/2.);
     return (_c0.r+r)*vec2(cos(a), sin(a));`,
            },
            {
              name: "scroll",
              type: "coord",
              inputs: [
                {
                  type: "float",
                  name: "scrollX",
                  default: 0.5,
                },
                {
                  type: "float",
                  name: "scrollY",
                  default: 0.5,
                },
                {
                  type: "float",
                  name: "speedX",
                  default: 0,
                },
                {
                  type: "float",
                  name: "speedY",
                  default: 0,
                },
              ],
              glsl: `
     _st.x += scrollX + time*speedX;
     _st.y += scrollY + time*speedY;
     return fract(_st);`,
            },
            {
              name: "scrollX",
              type: "coord",
              inputs: [
                {
                  type: "float",
                  name: "scrollX",
                  default: 0.5,
                },
                {
                  type: "float",
                  name: "speed",
                  default: 0,
                },
              ],
              glsl: `   _st.x += scrollX + time*speed;
     return fract(_st);`,
            },
            {
              name: "modulateScrollX",
              type: "combineCoord",
              inputs: [
                {
                  type: "float",
                  name: "scrollX",
                  default: 0.5,
                },
                {
                  type: "float",
                  name: "speed",
                  default: 0,
                },
              ],
              glsl: `   _st.x += _c0.r*scrollX + time*speed;
     return fract(_st);`,
            },
            {
              name: "scrollY",
              type: "coord",
              inputs: [
                {
                  type: "float",
                  name: "scrollY",
                  default: 0.5,
                },
                {
                  type: "float",
                  name: "speed",
                  default: 0,
                },
              ],
              glsl: `   _st.y += scrollY + time*speed;
     return fract(_st);`,
            },
            {
              name: "modulateScrollY",
              type: "combineCoord",
              inputs: [
                {
                  type: "float",
                  name: "scrollY",
                  default: 0.5,
                },
                {
                  type: "float",
                  name: "speed",
                  default: 0,
                },
              ],
              glsl: `   _st.y += _c0.r*scrollY + time*speed;
     return fract(_st);`,
            },
            {
              name: "add",
              type: "combine",
              inputs: [
                {
                  type: "float",
                  name: "amount",
                  default: 1,
                },
              ],
              glsl: `   return (_c0+_c1)*amount + _c0*(1.0-amount);`,
            },
            {
              name: "sub",
              type: "combine",
              inputs: [
                {
                  type: "float",
                  name: "amount",
                  default: 1,
                },
              ],
              glsl: `   return (_c0-_c1)*amount + _c0*(1.0-amount);`,
            },
            {
              name: "layer",
              type: "combine",
              inputs: [],
              glsl: `   return vec4(mix(_c0.rgb, _c1.rgb, _c1.a), clamp(_c0.a + _c1.a, 0.0, 1.0));`,
            },
            {
              name: "blend",
              type: "combine",
              inputs: [
                {
                  type: "float",
                  name: "amount",
                  default: 0.5,
                },
              ],
              glsl: `   return _c0*(1.0-amount)+_c1*amount;`,
            },
            {
              name: "mult",
              type: "combine",
              inputs: [
                {
                  type: "float",
                  name: "amount",
                  default: 1,
                },
              ],
              glsl: `   return _c0*(1.0-amount)+(_c0*_c1)*amount;`,
            },
            {
              name: "diff",
              type: "combine",
              inputs: [],
              glsl: `   return vec4(abs(_c0.rgb-_c1.rgb), max(_c0.a, _c1.a));`,
            },
            {
              name: "modulate",
              type: "combineCoord",
              inputs: [
                {
                  type: "float",
                  name: "amount",
                  default: 0.1,
                },
              ],
              glsl: `   //  return fract(st+(_c0.xy-0.5)*amount);
     return _st + _c0.xy*amount;`,
            },
            {
              name: "modulateScale",
              type: "combineCoord",
              inputs: [
                {
                  type: "float",
                  name: "multiple",
                  default: 1,
                },
                {
                  type: "float",
                  name: "offset",
                  default: 1,
                },
              ],
              glsl: `   vec2 xy = _st - vec2(0.5);
     xy*=(1.0/vec2(offset + multiple*_c0.r, offset + multiple*_c0.g));
     xy+=vec2(0.5);
     return xy;`,
            },
            {
              name: "modulatePixelate",
              type: "combineCoord",
              inputs: [
                {
                  type: "float",
                  name: "multiple",
                  default: 10,
                },
                {
                  type: "float",
                  name: "offset",
                  default: 3,
                },
              ],
              glsl: `   vec2 xy = vec2(offset + _c0.x*multiple, offset + _c0.y*multiple);
     return (floor(_st * xy) + 0.5)/xy;`,
            },
            {
              name: "modulateRotate",
              type: "combineCoord",
              inputs: [
                {
                  type: "float",
                  name: "multiple",
                  default: 1,
                },
                {
                  type: "float",
                  name: "offset",
                  default: 0,
                },
              ],
              glsl: `   vec2 xy = _st - vec2(0.5);
     float angle = offset + _c0.x * multiple;
     xy = mat2(cos(angle),-sin(angle), sin(angle),cos(angle))*xy;
     xy += 0.5;
     return xy;`,
            },
            {
              name: "modulateHue",
              type: "combineCoord",
              inputs: [
                {
                  type: "float",
                  name: "amount",
                  default: 1,
                },
              ],
              glsl: `   return _st + (vec2(_c0.g - _c0.r, _c0.b - _c0.g) * amount * 1.0/resolution);`,
            },
            {
              name: "invert",
              type: "color",
              inputs: [
                {
                  type: "float",
                  name: "amount",
                  default: 1,
                },
              ],
              glsl: `   return vec4((1.0-_c0.rgb)*amount + _c0.rgb*(1.0-amount), _c0.a);`,
            },
            {
              name: "contrast",
              type: "color",
              inputs: [
                {
                  type: "float",
                  name: "amount",
                  default: 1.6,
                },
              ],
              glsl: `   vec4 c = (_c0-vec4(0.5))*vec4(amount) + vec4(0.5);
     return vec4(c.rgb, _c0.a);`,
            },
            {
              name: "brightness",
              type: "color",
              inputs: [
                {
                  type: "float",
                  name: "amount",
                  default: 0.4,
                },
              ],
              glsl: `   return vec4(_c0.rgb + vec3(amount), _c0.a);`,
            },
            {
              name: "mask",
              type: "combine",
              inputs: [],
              glsl: `   float a = _luminance(_c1.rgb);
    return vec4(_c0.rgb*a, a*_c0.a);`,
            },
            {
              name: "luma",
              type: "color",
              inputs: [
                {
                  type: "float",
                  name: "threshold",
                  default: 0.5,
                },
                {
                  type: "float",
                  name: "tolerance",
                  default: 0.1,
                },
              ],
              glsl: `   float a = smoothstep(threshold-(tolerance+0.0000001), threshold+(tolerance+0.0000001), _luminance(_c0.rgb));
     return vec4(_c0.rgb*a, a);`,
            },
            {
              name: "thresh",
              type: "color",
              inputs: [
                {
                  type: "float",
                  name: "threshold",
                  default: 0.5,
                },
                {
                  type: "float",
                  name: "tolerance",
                  default: 0.04,
                },
              ],
              glsl: `   return vec4(vec3(smoothstep(threshold-(tolerance+0.0000001), threshold+(tolerance+0.0000001), _luminance(_c0.rgb))), _c0.a);`,
            },
            {
              name: "color",
              type: "color",
              inputs: [
                {
                  type: "float",
                  name: "r",
                  default: 1,
                },
                {
                  type: "float",
                  name: "g",
                  default: 1,
                },
                {
                  type: "float",
                  name: "b",
                  default: 1,
                },
                {
                  type: "float",
                  name: "a",
                  default: 1,
                },
              ],
              glsl: `   vec4 c = vec4(r, g, b, a);
     vec4 pos = step(0.0, c); // detect whether negative
     // if > 0, return r * _c0
     // if < 0 return (1.0-r) * _c0
     return vec4(mix((1.0-_c0)*abs(c), c*_c0, pos));`,
            },
            {
              name: "saturate",
              type: "color",
              inputs: [
                {
                  type: "float",
                  name: "amount",
                  default: 2,
                },
              ],
              glsl: `   const vec3 W = vec3(0.2125, 0.7154, 0.0721);
     vec3 intensity = vec3(dot(_c0.rgb, W));
     return vec4(mix(intensity, _c0.rgb, amount), _c0.a);`,
            },
            {
              name: "hue",
              type: "color",
              inputs: [
                {
                  type: "float",
                  name: "hue",
                  default: 0.4,
                },
              ],
              glsl: `   vec3 c = _rgbToHsv(_c0.rgb);
     c.r += hue;
     //  c.r = fract(c.r);
     return vec4(_hsvToRgb(c), _c0.a);`,
            },
            {
              name: "colorama",
              type: "color",
              inputs: [
                {
                  type: "float",
                  name: "amount",
                  default: 0.005,
                },
              ],
              glsl: `   vec3 c = _rgbToHsv(_c0.rgb);
     c += vec3(amount);
     c = _hsvToRgb(c);
     c = fract(c);
     return vec4(c, _c0.a);`,
            },
            {
              name: "prev",
              type: "src",
              inputs: [],
              glsl: `   return texture2D(prevBuffer, fract(_st));`,
            },
            {
              name: "sum",
              type: "color",
              inputs: [
                {
                  type: "vec4",
                  name: "scale",
                  default: 1,
                },
              ],
              glsl: `   vec4 v = _c0 * s;
     return v.r + v.g + v.b + v.a;
     }
     float sum(vec2 _st, vec4 s) { // vec4 is not a typo, because argument type is not overloaded
     vec2 v = _st.xy * s.xy;
     return v.x + v.y;`,
            },
            {
              name: "r",
              type: "color",
              inputs: [
                {
                  type: "float",
                  name: "scale",
                  default: 1,
                },
                {
                  type: "float",
                  name: "offset",
                  default: 0,
                },
              ],
              // glsl: `   return vec4( _c0.r, _c0.r, _c0.r, 0.0);`,
              glsl: `   return vec4( 1.0, 1.0, 1.0, 0.0);`,
            },
            {
              name: "g",
              type: "color",
              inputs: [
                {
                  type: "float",
                  name: "scale",
                  default: 1,
                },
                {
                  // type: "flo1.0, 1.0, 1.0 "float",
                  name: "offset",
                  default: 0,
                },
              ],
              glsl: `   return vec4(_c0.g * scale + offset);`,
            },
            {
              name: "b",
              type: "color",
              inputs: [
                {
                  type: "float",
                  name: "scale",
                  default: 1,
                },
                {
                  type: "float",
                  name: "offset",
                  default: 0,
                },
              ],
              glsl: `   return vec4(_c0.b * scale + offset);`,
            },
            {
              name: "a",
              type: "color",
              inputs: [
                {
                  type: "float",
                  name: "scale",
                  default: 1,
                },
                {
                  type: "float",
                  name: "offset",
                  default: 0,
                },
              ],
              glsl: `   return vec4(_c0.a * scale + offset);`,
            },
          ];

          exports.default = _default;
        },
        {},
      ],
      16: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = void 0;
          // functions that are only used within other functions
          var _default = {
            _luminance: {
              type: "util",
              glsl: `float _luminance(vec3 rgb){
        const vec3 W = vec3(0.2125, 0.7154, 0.0721);
        return dot(rgb, W);
      }`,
            },
            _noise: {
              type: "util",
              glsl: `
      //	Simplex 3D Noise
      //	by Ian McEwan, Ashima Arts
      vec4 permute(vec4 x){return mod(((x*34.0)+1.0)*x, 289.0);}
    vec4 taylorInvSqrt(vec4 r){return 1.79284291400159 - 0.85373472095314 * r;}

    float _noise(vec3 v){
      const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
      const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

    // First corner
      vec3 i  = floor(v + dot(v, C.yyy) );
      vec3 x0 =   v - i + dot(i, C.xxx) ;

    // Other corners
      vec3 g = step(x0.yzx, x0.xyz);
      vec3 l = 1.0 - g;
      vec3 i1 = min( g.xyz, l.zxy );
      vec3 i2 = max( g.xyz, l.zxy );

      //  x0 = x0 - 0. + 0.0 * C
      vec3 x1 = x0 - i1 + 1.0 * C.xxx;
      vec3 x2 = x0 - i2 + 2.0 * C.xxx;
      vec3 x3 = x0 - 1. + 3.0 * C.xxx;

    // Permutations
      i = mod(i, 289.0 );
      vec4 p = permute( permute( permute(
                 i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
               + i.y + vec4(0.0, i1.y, i2.y, 1.0 ))
               + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

    // Gradients
    // ( N*N points uniformly over a square, mapped onto an octahedron.)
      float n_ = 1.0/7.0; // N=7
      vec3  ns = n_ * D.wyz - D.xzx;

      vec4 j = p - 49.0 * floor(p * ns.z *ns.z);  //  mod(p,N*N)

      vec4 x_ = floor(j * ns.z);
      vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)

      vec4 x = x_ *ns.x + ns.yyyy;
      vec4 y = y_ *ns.x + ns.yyyy;
      vec4 h = 1.0 - abs(x) - abs(y);

      vec4 b0 = vec4( x.xy, y.xy );
      vec4 b1 = vec4( x.zw, y.zw );

      vec4 s0 = floor(b0)*2.0 + 1.0;
      vec4 s1 = floor(b1)*2.0 + 1.0;
      vec4 sh = -step(h, vec4(0.0));

      vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
      vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;

      vec3 p0 = vec3(a0.xy,h.x);
      vec3 p1 = vec3(a0.zw,h.y);
      vec3 p2 = vec3(a1.xy,h.z);
      vec3 p3 = vec3(a1.zw,h.w);

    //Normalise gradients
      vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
      p0 *= norm.x;
      p1 *= norm.y;
      p2 *= norm.z;
      p3 *= norm.w;

    // Mix final noise value
      vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
      m = m * m;
      return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1),
                                    dot(p2,x2), dot(p3,x3) ) );
    }
      `,
            },
            _rgbToHsv: {
              type: "util",
              glsl: `vec3 _rgbToHsv(vec3 c){
              vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
              vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
              vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

              float d = q.x - min(q.w, q.y);
              float e = 1.0e-10;
              return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
          }`,
            },
            _hsvToRgb: {
              type: "util",
              glsl: `vec3 _hsvToRgb(vec3 c){
          vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
          vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
          return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
      }`,
            },
          };
          exports.default = _default;
        },
        {},
      ],
      17: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = void 0;

          var _webcam = _interopRequireDefault(require("./lib/webcam.js"));

          var _screenmedia = _interopRequireDefault(
            require("./lib/screenmedia.js")
          );

          function _interopRequireDefault(obj) {
            return obj && obj.__esModule ? obj : { default: obj };
          }

          class HydraSource {
            constructor({ regl, width, height, pb, label = "" }) {
              this.label = label;
              this.regl = regl;
              this.src = null;
              this.dynamic = true;
              this.width = width;
              this.height = height;
              this.tex = this.regl.texture({
                //  shape: [width, height]
                shape: [1, 1],
              });
              this.pb = pb;
            }

            init(opts, params) {
              if ("src" in opts) {
                this.src = opts.src;
                this.tex = this.regl.texture({
                  data: this.src,
                  ...params,
                });
              }

              if ("dynamic" in opts) this.dynamic = opts.dynamic;
            }

            initCam(index, params) {
              const self = this;
              (0, _webcam.default)(index)
                .then((response) => {
                  self.src = response.video;
                  self.dynamic = true;
                  self.tex = self.regl.texture({
                    data: self.src,
                    ...params,
                  });
                })
                .catch((err) => console.log("could not get camera", err));
            }

            initVideo(url = "", params) {
              // const self = this
              const vid = document.createElement("video");
              vid.crossOrigin = "anonymous";
              vid.autoplay = true;
              vid.loop = true;
              vid.muted = true; // mute in order to load without user interaction

              const onload = vid.addEventListener("loadeddata", () => {
                this.src = vid;
                vid.play();
                this.tex = this.regl.texture({
                  data: this.src,
                  ...params,
                });
                this.dynamic = true;
              });
              vid.src = url;
            }

            initImage(url = "", params) {
              const img = document.createElement("img");
              img.crossOrigin = "anonymous";
              img.src = url;

              img.onload = () => {
                this.src = img;
                this.dynamic = false;
                this.tex = this.regl.texture({
                  data: this.src,
                  ...params,
                });
              };
            }

            initStream(streamName, params) {
              //  console.log("initing stream!", streamName)
              let self = this;

              if (streamName && this.pb) {
                this.pb.initSource(streamName);
                this.pb.on("got video", function (nick, video) {
                  if (nick === streamName) {
                    self.src = video;
                    self.dynamic = true;
                    self.tex = self.regl.texture({
                      data: self.src,
                      ...params,
                    });
                  }
                });
              }
            } // index only relevant in atom-hydra + desktop apps

            initScreen(index = 0, params) {
              const self = this;
              (0, _screenmedia.default)()
                .then(function (response) {
                  self.src = response.video;
                  self.tex = self.regl.texture({
                    data: self.src,
                    ...params,
                  });
                  self.dynamic = true; //  console.log("received screen input")
                })
                .catch((err) => console.log("could not get screen", err));
            }

            resize(width, height) {
              this.width = width;
              this.height = height;
            }

            clear() {
              if (this.src && this.src.srcObject) {
                if (this.src.srcObject.getTracks) {
                  this.src.srcObject
                    .getTracks()
                    .forEach((track) => track.stop());
                }
              }

              this.src = null;
              this.tex = this.regl.texture({
                shape: [1, 1],
              });
            }

            tick(time) {
              //  console.log(this.src, this.tex.width, this.tex.height)
              if (this.src !== null && this.dynamic === true) {
                if (
                  this.src.videoWidth &&
                  this.src.videoWidth !== this.tex.width
                ) {
                  console.log(
                    this.src.videoWidth,
                    this.src.videoHeight,
                    this.tex.width,
                    this.tex.height
                  );
                  this.tex.resize(this.src.videoWidth, this.src.videoHeight);
                }

                if (this.src.width && this.src.width !== this.tex.width) {
                  this.tex.resize(this.src.width, this.src.height);
                }

                this.tex.subimage(this.src);
              }
            }

            getTexture() {
              return this.tex;
            }
          }

          var _default = HydraSource;
          exports.default = _default;
        },
        { "./lib/screenmedia.js": 26, "./lib/webcam.js": 28 },
      ],
      18: [
        function (require, module, exports) {
          (function (global) {
            (function () {
              "use strict";

              Object.defineProperty(exports, "__esModule", {
                value: true,
              });
              exports.default = void 0;

              var _output = _interopRequireDefault(require("./output.js"));

              var _rafLoop = _interopRequireDefault(require("raf-loop"));

              var _hydraSource = _interopRequireDefault(
                require("./hydra-source.js")
              );

              var _mouse = _interopRequireDefault(require("./lib/mouse.js"));

              var _audio = _interopRequireDefault(require("./lib/audio.js"));

              var _videoRecorder = _interopRequireDefault(
                require("./lib/video-recorder.js")
              );

              var _arrayUtils = _interopRequireDefault(
                require("./lib/array-utils.js")
              );

              var _evalSandbox = _interopRequireDefault(
                require("./eval-sandbox.js")
              );

              var _generatorFactory = _interopRequireDefault(
                require("./generator-factory.js")
              );

              var _regl = _interopRequireDefault(require("regl"));

              function _interopRequireDefault(obj) {
                return obj && obj.__esModule ? obj : { default: obj };
              }

              // import strudel from './lib/strudel.js'
              // const window = global.window
              const Mouse = (0, _mouse.default)(); // to do: add ability to pass in certain uniforms and transforms

              class HydraRenderer {
                constructor({
                  pb = null,
                  width = 1280,
                  height = 720,
                  numSources = 4,
                  numOutputs = 4,
                  makeGlobal = true,
                  autoLoop = true,
                  detectAudio = true,
                  enableStreamCapture = true,
                  canvas,
                  precision,
                  extendTransforms = {}, // add your own functions on init
                } = {}) {
                  _arrayUtils.default.init();

                  this.pb = pb;
                  this.width = width;
                  this.height = height;
                  this.renderAll = false;
                  this.detectAudio = detectAudio;

                  this._initCanvas(canvas);

                  global.window.test = "hi"; // object that contains all properties that will be made available on the global context and during local evaluation

                  this.synth = {
                    time: 0,
                    bpm: 30,
                    width: this.width,
                    height: this.height,
                    fps: undefined,
                    stats: {
                      fps: 0,
                    },
                    speed: 1,
                    mouse: Mouse,
                    render: this._render.bind(this),
                    setResolution: this.setResolution.bind(this),
                    update: (dt) => {},
                    // user defined update function
                    hush: this.hush.bind(this),
                    tick: this.tick.bind(this),
                  };
                  if (makeGlobal) window.loadScript = this.loadScript;
                  this.timeSinceLastUpdate = 0;
                  this._time = 0; // for internal use, only to use for deciding when to render frames
                  // only allow valid precision options

                  let precisionOptions = ["lowp", "mediump", "highp"];

                  if (
                    precision &&
                    precisionOptions.includes(precision.toLowerCase())
                  ) {
                    this.precision = precision.toLowerCase(); //
                    // if(!precisionValid){
                    //   console.warn('[hydra-synth warning]\nConstructor was provided an invalid floating point precision value of "' + precision + '". Using default value of "mediump" instead.')
                    // }
                  } else {
                    let isIOS =
                      (/iPad|iPhone|iPod/.test(navigator.platform) ||
                        (navigator.platform === "MacIntel" &&
                          navigator.maxTouchPoints > 1)) &&
                      !window.MSStream;
                    this.precision = isIOS ? "highp" : "mediump";
                  }

                  this.extendTransforms = extendTransforms; // boolean to store when to save screenshot

                  this.saveFrame = false; // if stream capture is enabled, this object contains the capture stream

                  this.captureStream = null;
                  this.generator = undefined;

                  this._initRegl();

                  this._initOutputs(numOutputs);

                  this._initSources(numSources);

                  this._generateGlslTransforms();

                  this.synth.screencap = () => {
                    this.saveFrame = true;
                  };

                  if (enableStreamCapture) {
                    try {
                      this.captureStream = this.canvas.captureStream(25); // to do: enable capture stream of specific sources and outputs

                      this.synth.vidRecorder = new _videoRecorder.default(
                        this.captureStream
                      );
                    } catch (e) {
                      console.warn(
                        "[hydra-synth warning]\nnew MediaSource() is not currently supported on iOS."
                      );
                      console.error(e);
                    }
                  }

                  if (detectAudio) this._initAudio();
                  if (autoLoop)
                    (0, _rafLoop.default)(this.tick.bind(this)).start(); // final argument is properties that the user can set, all others are treated as read-only

                  this.sandbox = new _evalSandbox.default(
                    this.synth,
                    makeGlobal,
                    ["speed", "update", "bpm", "fps"]
                  );
                }

                eval(code) {
                  this.sandbox.eval(code);
                }

                getScreenImage(callback) {
                  this.imageCallback = callback;
                  this.saveFrame = true;
                }

                hush() {
                  this.s.forEach((source) => {
                    source.clear();
                  });
                  this.o.forEach((output) => {
                    this.synth.solid(0, 0, 0, 0).out(output);
                  });
                  this.synth.render(this.o[0]); // this.synth.update = (dt) => {}

                  this.sandbox.set("update", (dt) => {});
                }

                loadScript(url = "") {
                  const p = new Promise((res, rej) => {
                    var script = document.createElement("script");

                    script.onload = function () {
                      console.log(`loaded script ${url}`);
                      res();
                    };

                    script.onerror = (err) => {
                      console.log(`error loading script ${url}`, "log-error");
                      res();
                    };

                    script.src = url;
                    document.head.appendChild(script);
                  });
                  return p;
                }

                setResolution(width, height) {
                  //  console.log(width, height)
                  this.canvas.width = width;
                  this.canvas.height = height;
                  this.width = width; // is this necessary?

                  this.height = height; // ?

                  this.sandbox.set("width", width);
                  this.sandbox.set("height", height);
                  console.log(this.width);
                  this.o.forEach((output) => {
                    output.resize(width, height);
                  });
                  this.s.forEach((source) => {
                    source.resize(width, height);
                  });

                  this.regl._refresh();

                  console.log(this.canvas.width);
                }

                canvasToImage(callback) {
                  const a = document.createElement("a");
                  a.style.display = "none";
                  let d = new Date();
                  a.download = `hydra-${d.getFullYear()}-${
                    d.getMonth() + 1
                  }-${d.getDate()}-${d.getHours()}.${d.getMinutes()}.${d.getSeconds()}.png`;
                  document.body.appendChild(a);
                  var self = this;
                  this.canvas.toBlob((blob) => {
                    if (self.imageCallback) {
                      self.imageCallback(blob);
                      delete self.imageCallback;
                    } else {
                      a.href = URL.createObjectURL(blob);
                      console.log(a.href);
                      a.click();
                    }
                  }, "image/png");
                  setTimeout(() => {
                    document.body.removeChild(a);
                    window.URL.revokeObjectURL(a.href);
                  }, 300);
                }

                _initAudio() {
                  const that = this;
                  this.synth.a = new _audio.default({
                    numBins: 4,
                    parentEl: this.canvas.parentNode, // changeListener: ({audio}) => {
                    //   that.a = audio.bins.map((_, index) =>
                    //     (scale = 1, offset = 0) => () => (audio.fft[index] * scale + offset)
                    //   )
                    //
                    //   if (that.makeGlobal) {
                    //     that.a.forEach((a, index) => {
                    //       const aname = `a${index}`
                    //       window[aname] = a
                    //     })
                    //   }
                    // }
                  });
                } // create main output canvas and add to screen

                _initCanvas(canvas) {
                  if (canvas) {
                    this.canvas = canvas;
                    this.width = canvas.width;
                    this.height = canvas.height;
                  } else {
                    this.canvas = document.createElement("canvas");
                    this.canvas.width = this.width;
                    this.canvas.height = this.height;
                    this.canvas.style.width = "100%";
                    this.canvas.style.height = "100%";
                    this.canvas.style.imageRendering = "pixelated";
                    document.body.appendChild(this.canvas);
                  }
                }

                _initRegl() {
                  this.regl = (0, _regl.default)({
                    //  profile: true,
                    canvas: this.canvas,
                    pixelRatio: 1, //,
                    // extensions: [
                    //   'oes_texture_half_float',
                    //   'oes_texture_half_float_linear'
                    // ],
                    // optionalExtensions: [
                    //   'oes_texture_float',
                    //   'oes_texture_float_linear'
                    //]
                  }); // This clears the color buffer to black and the depth buffer to 1

                  this.regl.clear({
                    color: [0, 0, 0, 1],
                  });
                  this.renderAll = this.regl({
                    frag: `
        precision ${this.precision} float;
        varying vec2 uv;
        uniform sampler2D tex0;
        uniform sampler2D tex1;
        uniform sampler2D tex2;
        uniform sampler2D tex3;

        void main () {
          vec2 st = vec2(1.0 - uv.x, uv.y);
          st*= vec2(2);
          vec2 q = floor(st).xy*(vec2(2.0, 1.0));
          int quad = int(q.x) + int(q.y);
          st.x += step(1., mod(st.y,2.0));
          st.y += step(1., mod(st.x,2.0));
          st = fract(st);
          if(quad==0){
            gl_FragColor = texture2D(tex0, st);
          } else if(quad==1){
            gl_FragColor = texture2D(tex1, st);
          } else if (quad==2){
            gl_FragColor = texture2D(tex2, st);
          } else {
            gl_FragColor = texture2D(tex3, st);
          }

        }
        `,
                    vert: `
        precision ${this.precision} float;
        attribute vec2 position;
        varying vec2 uv;

        void main () {
          uv = position;
          gl_Position = vec4(1.0 - 2.0 * position, 0, 1);
        }`,
                    attributes: {
                      position: [
                        [-2, 0],
                        [0, -2],
                        [2, 2],
                      ],
                    },
                    uniforms: {
                      tex0: this.regl.prop("tex0"),
                      tex1: this.regl.prop("tex1"),
                      tex2: this.regl.prop("tex2"),
                      tex3: this.regl.prop("tex3"),
                    },
                    count: 3,
                    depth: {
                      enable: false,
                    },
                  });
                  this.renderFbo = this.regl({
                    frag: `
        precision ${this.precision} float;
        varying vec2 uv;
        uniform vec2 resolution;
        uniform sampler2D tex0;

        void main () {
          gl_FragColor = texture2D(tex0, vec2(1.0 - uv.x, uv.y));
        }
        `,
                    vert: `
        precision ${this.precision} float;
        attribute vec2 position;
        varying vec2 uv;

        void main () {
          uv = position;
          gl_Position = vec4(1.0 - 2.0 * position, 0, 1);
        }`,
                    attributes: {
                      position: [
                        [-2, 0],
                        [0, -2],
                        [2, 2],
                      ],
                    },
                    uniforms: {
                      tex0: this.regl.prop("tex0"),
                      resolution: this.regl.prop("resolution"),
                    },
                    count: 3,
                    depth: {
                      enable: false,
                    },
                  });
                }

                _initOutputs(numOutputs) {
                  const self = this;
                  this.o = Array(numOutputs)
                    .fill()
                    .map((el, index) => {
                      var o = new _output.default({
                        regl: this.regl,
                        width: this.width,
                        height: this.height,
                        precision: this.precision,
                        label: `o${index}`,
                      }); //  o.render()

                      o.id = index;
                      self.synth["o" + index] = o;
                      return o;
                    }); // set default output

                  this.output = this.o[0];
                }

                _initSources(numSources) {
                  this.s = [];

                  for (var i = 0; i < numSources; i++) {
                    this.createSource(i);
                  }
                }

                createSource(i) {
                  let s = new _hydraSource.default({
                    regl: this.regl,
                    pb: this.pb,
                    width: this.width,
                    height: this.height,
                    label: `s${i}`,
                  });
                  this.synth["s" + this.s.length] = s;
                  this.s.push(s);
                  return s;
                }

                _generateGlslTransforms() {
                  var self = this;
                  this.generator = new _generatorFactory.default({
                    defaultOutput: this.o[0],
                    defaultUniforms: this.o[0].uniforms,
                    extendTransforms: this.extendTransforms,
                    changeListener: ({ type, method, synth }) => {
                      if (type === "add") {
                        self.synth[method] = synth.generators[method];
                        if (self.sandbox) self.sandbox.add(method);
                      } else if (type === "remove") {
                        // what to do here? dangerously deleting window methods
                        //delete window[method]
                      } //  }
                    },
                  });
                  this.synth.setFunction = this.generator.setFunction.bind(
                    this.generator
                  );
                }

                _render(output) {
                  if (output) {
                    this.output = output;
                    this.isRenderingAll = false;
                  } else {
                    this.isRenderingAll = true;
                  }
                } // dt in ms

                tick(dt, uniforms) {
                  this.sandbox.tick();
                  if (this.detectAudio === true) this.synth.a.tick(); //  let updateInterval = 1000/this.synth.fps // ms

                  this.sandbox.set(
                    "time",
                    (this.synth.time += dt * 0.001 * this.synth.speed)
                  );
                  this.timeSinceLastUpdate += dt;

                  if (
                    !this.synth.fps ||
                    this.timeSinceLastUpdate >= 1000 / this.synth.fps
                  ) {
                    //  console.log(1000/this.timeSinceLastUpdate)
                    this.synth.stats.fps = Math.ceil(
                      1000 / this.timeSinceLastUpdate
                    );

                    if (this.synth.update) {
                      try {
                        this.synth.update(this.timeSinceLastUpdate);
                      } catch (e) {
                        console.log(e);
                      }
                    } //  console.log(this.synth.speed, this.synth.time)

                    for (let i = 0; i < this.s.length; i++) {
                      this.s[i].tick(this.synth.time);
                    } //  console.log(this.canvas.width, this.canvas.height)

                    for (let i = 0; i < this.o.length; i++) {
                      this.o[i].tick({
                        time: this.synth.time,
                        mouse: this.synth.mouse,
                        bpm: this.synth.bpm,
                        resolution: [this.canvas.width, this.canvas.height],
                      });
                    }

                    if (this.isRenderingAll) {
                      this.renderAll({
                        tex0: this.o[0].getCurrent(),
                        tex1: this.o[1].getCurrent(),
                        tex2: this.o[2].getCurrent(),
                        tex3: this.o[3].getCurrent(),
                        resolution: [this.canvas.width, this.canvas.height],
                      });
                    } else {
                      this.renderFbo({
                        tex0: this.output.getCurrent(),
                        resolution: [this.canvas.width, this.canvas.height],
                      });
                    }

                    this.timeSinceLastUpdate = 0;
                  }

                  if (this.saveFrame === true) {
                    this.canvasToImage();
                    this.saveFrame = false;
                  } //  this.regl.poll()
                }
              }

              var _default = HydraRenderer;
              exports.default = _default;
            }.call(this));
          }.call(
            this,
            typeof global !== "undefined"
              ? global
              : typeof self !== "undefined"
              ? self
              : typeof window !== "undefined"
              ? window
              : {}
          ));
        },
        {
          "./eval-sandbox.js": 10,
          "./generator-factory.js": 13,
          "./hydra-source.js": 17,
          "./lib/array-utils.js": 20,
          "./lib/audio.js": 21,
          "./lib/mouse.js": 24,
          "./lib/video-recorder.js": 27,
          "./output.js": 29,
          "raf-loop": 6,
          regl: 8,
        },
      ],
      19: [
        function (require, module, exports) {
          "use strict";

          var _hydraSynth = _interopRequireDefault(require("./hydra-synth.js"));

          function _interopRequireDefault(obj) {
            return obj && obj.__esModule ? obj : { default: obj };
          }

          //import ShaderGenerator = require('./shader-generator.js')
          // alert('hi')
          // export default Synth
          module.exports = _hydraSynth.default;
        },
        { "./hydra-synth.js": 18 },
      ],
      20: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = void 0;

          var _easingFunctions = _interopRequireDefault(
            require("./easing-functions.js")
          );

          function _interopRequireDefault(obj) {
            return obj && obj.__esModule ? obj : { default: obj };
          }

          // WIP utils for working with arrays
          // Possibly should be integrated with lfo extension, etc.
          // to do: transform time rather than array values, similar to working with coordinates in hydra
          var map = (num, in_min, in_max, out_min, out_max) => {
            return (
              ((num - in_min) * (out_max - out_min)) / (in_max - in_min) +
              out_min
            );
          };

          var _default = {
            init: () => {
              Array.prototype.fast = function (speed = 1) {
                this._speed = speed;
                return this;
              };

              Array.prototype.smooth = function (smooth = 1) {
                this._smooth = smooth;
                return this;
              };

              Array.prototype.ease = function (ease = "linear") {
                if (typeof ease == "function") {
                  this._smooth = 1;
                  this._ease = ease;
                } else if (_easingFunctions.default[ease]) {
                  this._smooth = 1;
                  this._ease = _easingFunctions.default[ease];
                }

                return this;
              };

              Array.prototype.offset = function (offset = 0.5) {
                this._offset = offset % 1.0;
                return this;
              }; // Array.prototype.bounce = function() {
              //   this.modifiers.bounce = true
              //   return this
              // }

              Array.prototype.fit = function (low = 0, high = 1) {
                let lowest = Math.min(...this);
                let highest = Math.max(...this);
                var newArr = this.map((num) =>
                  map(num, lowest, highest, low, high)
                );
                newArr._speed = this._speed;
                newArr._smooth = this._smooth;
                newArr._ease = this._ease;
                return newArr;
              };
            },
            getValue:
              (arr = []) =>
              ({ time, bpm }) => {
                let speed = arr._speed ? arr._speed : 1;
                let smooth = arr._smooth ? arr._smooth : 0;
                let index = time * speed * (bpm / 60) + (arr._offset || 0);

                if (smooth !== 0) {
                  let ease = arr._ease
                    ? arr._ease
                    : _easingFunctions.default["linear"];

                  let _index = index - smooth / 2;

                  let currValue = arr[Math.floor(_index % arr.length)];
                  let nextValue = arr[Math.floor((_index + 1) % arr.length)];
                  let t = Math.min((_index % 1) / smooth, 1);
                  return ease(t) * (nextValue - currValue) + currValue;
                } else {
                  const val = arr[Math.floor(index % arr.length)];
                  return arr[Math.floor(index % arr.length)];
                }
              },
          };
          exports.default = _default;
        },
        { "./easing-functions.js": 22 },
      ],
      21: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = void 0;

          var _meyda = _interopRequireDefault(require("meyda"));

          function _interopRequireDefault(obj) {
            return obj && obj.__esModule ? obj : { default: obj };
          }

          class Audio {
            constructor({
              numBins = 4,
              cutoff = 2,
              smooth = 0.4,
              max = 15,
              scale = 10,
              isDrawing = false,
              parentEl = document.body,
            }) {
              this.vol = 0;
              this.scale = scale;
              this.max = max;
              this.cutoff = cutoff;
              this.smooth = smooth;
              this.setBins(numBins); // beat detection from: https://github.com/therewasaguy/p5-music-viz/blob/gh-pages/demos/01d_beat_detect_amplitude/sketch.js

              this.beat = {
                holdFrames: 20,
                threshold: 40,
                _cutoff: 0,
                // adaptive based on sound state
                decay: 0.98,
                _framesSinceBeat: 0, // keeps track of frames
              };

              this.onBeat = () => {
                //  console.log("beat")
              };

              this.canvas = document.createElement("canvas");
              this.canvas.width = 100;
              this.canvas.height = 80;
              this.canvas.style.width = "100px";
              this.canvas.style.height = "80px";
              this.canvas.style.position = "absolute";
              this.canvas.style.right = "0px";
              this.canvas.style.bottom = "0px";
              parentEl.appendChild(this.canvas);
              this.isDrawing = isDrawing;
              this.ctx = this.canvas.getContext("2d");
              this.ctx.fillStyle = "#DFFFFF";
              this.ctx.strokeStyle = "#0ff";
              this.ctx.lineWidth = 0.5;

              if (window.navigator.mediaDevices) {
                window.navigator.mediaDevices
                  .getUserMedia({
                    video: false,
                    audio: true,
                  })
                  .then((stream) => {
                    //  console.log('got mic stream', stream)
                    this.stream = stream;
                    this.context = new AudioContext(); //  this.context = new AudioContext()

                    let audio_stream =
                      this.context.createMediaStreamSource(stream); //  console.log(this.context)

                    this.meyda = _meyda.default.createMeydaAnalyzer({
                      audioContext: this.context,
                      source: audio_stream,
                      featureExtractors: [
                        "loudness", //  'perceptualSpread',
                        //  'perceptualSharpness',
                        //  'spectralCentroid'
                      ],
                    });
                  })
                  .catch((err) => console.log("ERROR", err));
              }
            }

            detectBeat(level) {
              //console.log(level,   this.beat._cutoff)
              if (level > this.beat._cutoff && level > this.beat.threshold) {
                this.onBeat();
                this.beat._cutoff = level * 1.2;
                this.beat._framesSinceBeat = 0;
              } else {
                if (this.beat._framesSinceBeat <= this.beat.holdFrames) {
                  this.beat._framesSinceBeat++;
                } else {
                  this.beat._cutoff *= this.beat.decay;
                  this.beat._cutoff = Math.max(
                    this.beat._cutoff,
                    this.beat.threshold
                  );
                }
              }
            }

            tick() {
              if (this.meyda) {
                var features = this.meyda.get();

                if (features && features !== null) {
                  this.vol = features.loudness.total;
                  this.detectBeat(this.vol); // reduce loudness array to number of bins

                  const reducer = (accumulator, currentValue) =>
                    accumulator + currentValue;

                  let spacing = Math.floor(
                    features.loudness.specific.length / this.bins.length
                  );
                  this.prevBins = this.bins.slice(0);
                  this.bins = this.bins
                    .map((bin, index) => {
                      return features.loudness.specific
                        .slice(index * spacing, (index + 1) * spacing)
                        .reduce(reducer);
                    })
                    .map((bin, index) => {
                      // map to specified range
                      // return (bin * (1.0 - this.smooth) + this.prevBins[index] * this.smooth)
                      return (
                        bin * (1.0 - this.settings[index].smooth) +
                        this.prevBins[index] * this.settings[index].smooth
                      );
                    }); // var y = this.canvas.height - scale*this.settings[index].cutoff
                  // this.ctx.beginPath()
                  // this.ctx.moveTo(index*spacing, y)
                  // this.ctx.lineTo((index+1)*spacing, y)
                  // this.ctx.stroke()
                  //
                  // var yMax = this.canvas.height - scale*(this.settings[index].scale + this.settings[index].cutoff)

                  this.fft = this.bins.map(
                    (
                      bin,
                      index // Math.max(0, (bin - this.cutoff) / (this.max - this.cutoff))
                    ) =>
                      Math.max(
                        0,
                        (bin - this.settings[index].cutoff) /
                          this.settings[index].scale
                      )
                  );
                  if (this.isDrawing) this.draw();
                }
              }
            }

            setCutoff(cutoff) {
              this.cutoff = cutoff;
              this.settings = this.settings.map((el) => {
                el.cutoff = cutoff;
                return el;
              });
            }

            setSmooth(smooth) {
              this.smooth = smooth;
              this.settings = this.settings.map((el) => {
                el.smooth = smooth;
                return el;
              });
            }

            setBins(numBins) {
              this.bins = Array(numBins).fill(0);
              this.prevBins = Array(numBins).fill(0);
              this.fft = Array(numBins).fill(0);
              this.settings = Array(numBins)
                .fill(0)
                .map(() => ({
                  cutoff: this.cutoff,
                  scale: this.scale,
                  smooth: this.smooth,
                })); // to do: what to do in non-global mode?

              this.bins.forEach((bin, index) => {
                window["a" + index] =
                  (scale = 1, offset = 0) =>
                  () =>
                    a.fft[index] * scale + offset;
              }); //  console.log(this.settings)
            }

            setScale(scale) {
              this.scale = scale;
              this.settings = this.settings.map((el) => {
                el.scale = scale;
                return el;
              });
            }

            setMax(max) {
              this.max = max;
              console.log("set max is deprecated");
            }

            hide() {
              this.isDrawing = false;
              this.canvas.style.display = "none";
            }

            show() {
              this.isDrawing = true;
              this.canvas.style.display = "block";
            }

            draw() {
              this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
              var spacing = this.canvas.width / this.bins.length;
              var scale = this.canvas.height / (this.max * 2); //  console.log(this.bins)

              this.bins.forEach((bin, index) => {
                var height = bin * scale;
                this.ctx.fillRect(
                  index * spacing,
                  this.canvas.height - height,
                  spacing,
                  height
                ); //   console.log(this.settings[index])

                var y =
                  this.canvas.height - scale * this.settings[index].cutoff;
                this.ctx.beginPath();
                this.ctx.moveTo(index * spacing, y);
                this.ctx.lineTo((index + 1) * spacing, y);
                this.ctx.stroke();
                var yMax =
                  this.canvas.height -
                  scale *
                    (this.settings[index].scale + this.settings[index].cutoff);
                this.ctx.beginPath();
                this.ctx.moveTo(index * spacing, yMax);
                this.ctx.lineTo((index + 1) * spacing, yMax);
                this.ctx.stroke();
              });
              /*var y = this.canvas.height - scale*this.cutoff
      this.ctx.beginPath()
      this.ctx.moveTo(0, y)
      this.ctx.lineTo(this.canvas.width, y)
      this.ctx.stroke()
      var yMax = this.canvas.height - scale*this.max
      this.ctx.beginPath()
      this.ctx.moveTo(0, yMax)
      this.ctx.lineTo(this.canvas.width, yMax)
      this.ctx.stroke()*/
            }
          }

          var _default = Audio;
          exports.default = _default;
        },
        { meyda: 3 },
      ],
      22: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = void 0;
          // from https://gist.github.com/gre/1650294
          var _default = {
            // no easing, no acceleration
            linear: function (t) {
              return t;
            },
            // accelerating from zero velocity
            easeInQuad: function (t) {
              return t * t;
            },
            // decelerating to zero velocity
            easeOutQuad: function (t) {
              return t * (2 - t);
            },
            // acceleration until halfway, then deceleration
            easeInOutQuad: function (t) {
              return t < 0.5 ? 2 * t * t : -1 + (4 - 2 * t) * t;
            },
            // accelerating from zero velocity
            easeInCubic: function (t) {
              return t * t * t;
            },
            // decelerating to zero velocity
            easeOutCubic: function (t) {
              return --t * t * t + 1;
            },
            // acceleration until halfway, then deceleration
            easeInOutCubic: function (t) {
              return t < 0.5
                ? 4 * t * t * t
                : (t - 1) * (2 * t - 2) * (2 * t - 2) + 1;
            },
            // accelerating from zero velocity
            easeInQuart: function (t) {
              return t * t * t * t;
            },
            // decelerating to zero velocity
            easeOutQuart: function (t) {
              return 1 - --t * t * t * t;
            },
            // acceleration until halfway, then deceleration
            easeInOutQuart: function (t) {
              return t < 0.5 ? 8 * t * t * t * t : 1 - 8 * --t * t * t * t;
            },
            // accelerating from zero velocity
            easeInQuint: function (t) {
              return t * t * t * t * t;
            },
            // decelerating to zero velocity
            easeOutQuint: function (t) {
              return 1 + --t * t * t * t * t;
            },
            // acceleration until halfway, then deceleration
            easeInOutQuint: function (t) {
              return t < 0.5
                ? 16 * t * t * t * t * t
                : 1 + 16 * --t * t * t * t * t;
            },
            // sin shape
            sin: function (t) {
              return (1 + Math.sin(Math.PI * t - Math.PI / 2)) / 2;
            },
          };
          exports.default = _default;
        },
        {},
      ],
      23: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = void 0;
          // https://github.com/mikolalysenko/mouse-event
          const mouse = {};

          function mouseButtons(ev) {
            if (typeof ev === "object") {
              if ("buttons" in ev) {
                return ev.buttons;
              } else if ("which" in ev) {
                var b = ev.which;

                if (b === 2) {
                  return 4;
                } else if (b === 3) {
                  return 2;
                } else if (b > 0) {
                  return 1 << (b - 1);
                }
              } else if ("button" in ev) {
                var b = ev.button;

                if (b === 1) {
                  return 4;
                } else if (b === 2) {
                  return 2;
                } else if (b >= 0) {
                  return 1 << b;
                }
              }
            }

            return 0;
          }

          mouse.buttons = mouseButtons;

          function mouseElement(ev) {
            return ev.target || ev.srcElement || window;
          }

          mouse.element = mouseElement;

          function mouseRelativeX(ev) {
            if (typeof ev === "object") {
              if ("pageX" in ev) {
                return ev.pageX;
              }
            }

            return 0;
          }

          mouse.x = mouseRelativeX;

          function mouseRelativeY(ev) {
            if (typeof ev === "object") {
              if ("pageY" in ev) {
                return ev.pageY;
              }
            }

            return 0;
          }

          mouse.y = mouseRelativeY;
          var _default = mouse;
          exports.default = _default;
        },
        {},
      ],
      24: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = void 0;

          var _mouseEvent = _interopRequireDefault(require("./mouse-event.js"));

          function _interopRequireDefault(obj) {
            return obj && obj.__esModule ? obj : { default: obj };
          }

          // based on https://github.com/mikolalysenko/mouse-change
          var _default = mouseListen;
          exports.default = _default;

          function mouseListen(element, callback) {
            if (!callback) {
              callback = element;
              element = window;
            }

            var buttonState = 0;
            var x = 0;
            var y = 0;
            var mods = {
              shift: false,
              alt: false,
              control: false,
              meta: false,
            };
            var attached = false;

            function updateMods(ev) {
              var changed = false;

              if ("altKey" in ev) {
                changed = changed || ev.altKey !== mods.alt;
                mods.alt = !!ev.altKey;
              }

              if ("shiftKey" in ev) {
                changed = changed || ev.shiftKey !== mods.shift;
                mods.shift = !!ev.shiftKey;
              }

              if ("ctrlKey" in ev) {
                changed = changed || ev.ctrlKey !== mods.control;
                mods.control = !!ev.ctrlKey;
              }

              if ("metaKey" in ev) {
                changed = changed || ev.metaKey !== mods.meta;
                mods.meta = !!ev.metaKey;
              }

              return changed;
            }

            function handleEvent(nextButtons, ev) {
              var nextX = _mouseEvent.default.x(ev);

              var nextY = _mouseEvent.default.y(ev);

              if ("buttons" in ev) {
                nextButtons = ev.buttons | 0;
              }

              if (
                nextButtons !== buttonState ||
                nextX !== x ||
                nextY !== y ||
                updateMods(ev)
              ) {
                buttonState = nextButtons | 0;
                x = nextX || 0;
                y = nextY || 0;
                callback && callback(buttonState, x, y, mods);
              }
            }

            function clearState(ev) {
              handleEvent(0, ev);
            }

            function handleBlur() {
              if (
                buttonState ||
                x ||
                y ||
                mods.shift ||
                mods.alt ||
                mods.meta ||
                mods.control
              ) {
                x = y = 0;
                buttonState = 0;
                mods.shift = mods.alt = mods.control = mods.meta = false;
                callback && callback(0, 0, 0, mods);
              }
            }

            function handleMods(ev) {
              if (updateMods(ev)) {
                callback && callback(buttonState, x, y, mods);
              }
            }

            function handleMouseMove(ev) {
              if (_mouseEvent.default.buttons(ev) === 0) {
                handleEvent(0, ev);
              } else {
                handleEvent(buttonState, ev);
              }
            }

            function handleMouseDown(ev) {
              handleEvent(buttonState | _mouseEvent.default.buttons(ev), ev);
            }

            function handleMouseUp(ev) {
              handleEvent(buttonState & ~_mouseEvent.default.buttons(ev), ev);
            }

            function attachListeners() {
              if (attached) {
                return;
              }

              attached = true;
              element.addEventListener("mousemove", handleMouseMove);
              element.addEventListener("mousedown", handleMouseDown);
              element.addEventListener("mouseup", handleMouseUp);
              element.addEventListener("mouseleave", clearState);
              element.addEventListener("mouseenter", clearState);
              element.addEventListener("mouseout", clearState);
              element.addEventListener("mouseover", clearState);
              element.addEventListener("blur", handleBlur);
              element.addEventListener("keyup", handleMods);
              element.addEventListener("keydown", handleMods);
              element.addEventListener("keypress", handleMods);

              if (element !== window) {
                window.addEventListener("blur", handleBlur);
                window.addEventListener("keyup", handleMods);
                window.addEventListener("keydown", handleMods);
                window.addEventListener("keypress", handleMods);
              }
            }

            function detachListeners() {
              if (!attached) {
                return;
              }

              attached = false;
              element.removeEventListener("mousemove", handleMouseMove);
              element.removeEventListener("mousedown", handleMouseDown);
              element.removeEventListener("mouseup", handleMouseUp);
              element.removeEventListener("mouseleave", clearState);
              element.removeEventListener("mouseenter", clearState);
              element.removeEventListener("mouseout", clearState);
              element.removeEventListener("mouseover", clearState);
              element.removeEventListener("blur", handleBlur);
              element.removeEventListener("keyup", handleMods);
              element.removeEventListener("keydown", handleMods);
              element.removeEventListener("keypress", handleMods);

              if (element !== window) {
                window.removeEventListener("blur", handleBlur);
                window.removeEventListener("keyup", handleMods);
                window.removeEventListener("keydown", handleMods);
                window.removeEventListener("keypress", handleMods);
              }
            } // Attach listeners

            attachListeners();
            var result = {
              element: element,
            };
            Object.defineProperties(result, {
              enabled: {
                get: function () {
                  return attached;
                },
                set: function (f) {
                  if (f) {
                    attachListeners();
                  } else {
                    detachListeners();
                  }
                },
                enumerable: true,
              },
              buttons: {
                get: function () {
                  return buttonState;
                },
                enumerable: true,
              },
              x: {
                get: function () {
                  return x;
                },
                enumerable: true,
              },
              y: {
                get: function () {
                  return y;
                },
                enumerable: true,
              },
              mods: {
                get: function () {
                  return mods;
                },
                enumerable: true,
              },
            });
            return result;
          }
        },
        { "./mouse-event.js": 23 },
      ],
      25: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = void 0;

          // attempt custom evaluation sandbox for hydra functions
          // for now, just avoids polluting the global namespace
          // should probably be replaced with an abstract syntax tree
          var _default = (parent) => {
            var initialCode = ``;
            var sandbox = createSandbox(initialCode);

            var addToContext = (name, object) => {
              initialCode += `
        var ${name} = ${object}
      `;
              sandbox = createSandbox(initialCode);
            };

            return {
              addToContext: addToContext,
              eval: (code) => sandbox.eval(code),
            };

            function createSandbox(initial) {
              eval(initial); // optional params

              var localEval = function (code) {
                eval(code);
              }; // API/data for end-user

              return {
                eval: localEval,
              };
            }
          };

          exports.default = _default;
        },
        {},
      ],
      26: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = _default;

          function _default(options) {
            return new Promise(function (resolve, reject) {
              //  async function startCapture(displayMediaOptions) {
              navigator.mediaDevices
                .getDisplayMedia(options)
                .then((stream) => {
                  const video = document.createElement("video");
                  video.srcObject = stream;
                  video.addEventListener("loadedmetadata", () => {
                    video.play();
                    resolve({
                      video: video,
                    });
                  });
                })
                .catch((err) => reject(err));
            });
          }
        },
        {},
      ],
      27: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = void 0;

          class VideoRecorder {
            constructor(stream) {
              this.mediaSource = new MediaSource();
              this.stream = stream; // testing using a recording as input

              this.output = document.createElement("video");
              this.output.autoplay = true;
              this.output.loop = true;
              let self = this;
              this.mediaSource.addEventListener("sourceopen", () => {
                console.log("MediaSource opened");
                self.sourceBuffer = self.mediaSource.addSourceBuffer(
                  'video/webm; codecs="vp8"'
                );
                console.log("Source buffer: ", sourceBuffer);
              });
            }

            start() {
              //  let options = {mimeType: 'video/webm'};
              //   let options = {mimeType: 'video/webm;codecs=h264'};
              let options = {
                mimeType: "video/webm;codecs=vp9",
              };
              this.recordedBlobs = [];

              try {
                this.mediaRecorder = new MediaRecorder(this.stream, options);
              } catch (e0) {
                console.log(
                  "Unable to create MediaRecorder with options Object: ",
                  e0
                );

                try {
                  options = {
                    mimeType: "video/webm,codecs=vp9",
                  };
                  this.mediaRecorder = new MediaRecorder(this.stream, options);
                } catch (e1) {
                  console.log(
                    "Unable to create MediaRecorder with options Object: ",
                    e1
                  );

                  try {
                    options = "video/vp8"; // Chrome 47

                    this.mediaRecorder = new MediaRecorder(
                      this.stream,
                      options
                    );
                  } catch (e2) {
                    alert(
                      "MediaRecorder is not supported by this browser.\n\n" +
                        "Try Firefox 29 or later, or Chrome 47 or later, " +
                        "with Enable experimental Web Platform features enabled from chrome://flags."
                    );
                    console.error(
                      "Exception while creating MediaRecorder:",
                      e2
                    );
                    return;
                  }
                }
              }

              console.log(
                "Created MediaRecorder",
                this.mediaRecorder,
                "with options",
                options
              );
              this.mediaRecorder.onstop = this._handleStop.bind(this);
              this.mediaRecorder.ondataavailable =
                this._handleDataAvailable.bind(this);
              this.mediaRecorder.start(100); // collect 100ms of data

              console.log("MediaRecorder started", this.mediaRecorder);
            }

            stop() {
              this.mediaRecorder.stop();
            }

            _handleStop() {
              //const superBuffer = new Blob(recordedBlobs, {type: 'video/webm'})
              // const blob = new Blob(this.recordedBlobs, {type: 'video/webm;codecs=h264'})
              const blob = new Blob(this.recordedBlobs, {
                type: this.mediaRecorder.mimeType,
              });
              const url = window.URL.createObjectURL(blob);
              this.output.src = url;
              const a = document.createElement("a");
              a.style.display = "none";
              a.href = url;
              let d = new Date();
              a.download = `hydra-${d.getFullYear()}-${
                d.getMonth() + 1
              }-${d.getDate()}-${d.getHours()}.${d.getMinutes()}.${d.getSeconds()}.webm`;
              document.body.appendChild(a);
              a.click();
              setTimeout(() => {
                document.body.removeChild(a);
                window.URL.revokeObjectURL(url);
              }, 300);
            }

            _handleDataAvailable(event) {
              if (event.data && event.data.size > 0) {
                this.recordedBlobs.push(event.data);
              }
            }
          }

          var _default = VideoRecorder;
          exports.default = _default;
        },
        {},
      ],
      28: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = _default;

          //const enumerateDevices = require('enumerate-devices')
          function _default(deviceId) {
            return navigator.mediaDevices
              .enumerateDevices()
              .then((devices) =>
                devices.filter((devices) => devices.kind === "videoinput")
              )
              .then((cameras) => {
                let constraints = {
                  audio: false,
                  video: true,
                };

                if (cameras[deviceId]) {
                  constraints["video"] = {
                    deviceId: {
                      exact: cameras[deviceId].deviceId,
                    },
                  };
                } //  console.log(cameras)

                return window.navigator.mediaDevices.getUserMedia(constraints);
              })
              .then((stream) => {
                const video = document.createElement("video");
                video.setAttribute("autoplay", "");
                video.setAttribute("muted", "");
                video.setAttribute("playsinline", ""); //  video.src = window.URL.createObjectURL(stream)

                video.srcObject = stream;
                return new Promise((resolve, reject) => {
                  video.addEventListener("loadedmetadata", () => {
                    video.play().then(() =>
                      resolve({
                        video: video,
                      })
                    );
                  });
                });
              })
              .catch(console.log.bind(console));
          }
        },
        {},
      ],
      29: [
        function (require, module, exports) {
          "use strict";

          Object.defineProperty(exports, "__esModule", {
            value: true,
          });
          exports.default = void 0;

          //const transforms = require('./glsl-transforms.js')
          var Output = function ({
            regl,
            precision,
            label = "",
            width,
            height,
          }) {
            this.regl = regl;
            this.precision = precision;
            this.label = label;
            this.positionBuffer = this.regl.buffer([
              [-2, 0],
              [0, -2],
              [2, 2],
            ]);

            this.draw = () => {};

            this.init();
            this.pingPongIndex = 0; // for each output, create two fbos for pingponging

            this.fbos = Array(2)
              .fill()
              .map(() =>
                this.regl.framebuffer({
                  color: this.regl.texture({
                    mag: "nearest",
                    width: width,
                    height: height,
                    format: "rgba",
                  }),
                  depthStencil: false,
                })
              ); // array containing render passes
            //  this.passes = []
          };

          Output.prototype.resize = function (width, height) {
            this.fbos.forEach((fbo) => {
              fbo.resize(width, height);
            }); //  console.log(this)
          };

          Output.prototype.getCurrent = function () {
            return this.fbos[this.pingPongIndex];
          };

          Output.prototype.getTexture = function () {
            var index = this.pingPongIndex ? 0 : 1;
            return this.fbos[index];
          };

          Output.prototype.init = function () {
            //  console.log('clearing')
            this.transformIndex = 0;
            this.fragHeader = `
    precision ${this.precision} float;

    uniform float time;
    varying vec2 uv;
    `;
            this.fragBody = ``;
            this.vert = `
    precision ${this.precision} float;
    attribute vec2 position;
    varying vec2 uv;

    void main () {
      uv = position;
      gl_Position = vec4(2.0 * position - 1.0, 0, 1);
    }`;
            this.attributes = {
              position: this.positionBuffer,
            };
            this.uniforms = {
              time: this.regl.prop("time"),
              resolution: this.regl.prop("resolution"),
            };
            this.frag = `
         ${this.fragHeader}

        void main () {
          vec4 c = vec4(0, 0, 0, 0);
          vec2 st = uv;
          ${this.fragBody}
          gl_FragColor = c;
        }
    `;
            return this;
          };

          Output.prototype.render = function (passes) {
            let pass = passes[0]; //console.log('pass', pass, this.pingPongIndex)

            var self = this;
            var uniforms = Object.assign(pass.uniforms, {
              prevBuffer: () => {
                //var index = this.pingPongIndex ? 0 : 1
                //   var index = self.pingPong[(passIndex+1)%2]
                //  console.log('ping pong', self.pingPongIndex)
                return self.fbos[self.pingPongIndex];
              },
            });
            self.draw = self.regl({
              frag: pass.frag,
              vert: self.vert,
              attributes: self.attributes,
              uniforms: uniforms,
              count: 3,
              framebuffer: () => {
                self.pingPongIndex = self.pingPongIndex ? 0 : 1;
                return self.fbos[self.pingPongIndex];
              },
            });
          };

          Output.prototype.tick = function (props) {
            //  console.log(props)
            this.draw(props);
          };

          var _default = Output;
          exports.default = _default;
        },
        {},
      ],
    },
    {},
    [19]
  )(19);
});
