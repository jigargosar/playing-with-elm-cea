try {
  !(function(t, e) {
    'use strict'
    ;(e = e && e.hasOwnProperty('default') ? e.default : e),
      function() {
        var t,
          n = n || {},
          i = this
        function r(t) {
          return 'string' == typeof t
        }
        function o(t) {
          return 'boolean' == typeof t
        }
        function a() {}
        function s(t) {
          var e = typeof t
          if ('object' == e) {
            if (!t) return 'null'
            if (t instanceof Array) return 'array'
            if (t instanceof Object) return e
            var n = Object.prototype.toString.call(t)
            if ('[object Window]' == n) return 'object'
            if (
              '[object Array]' == n ||
              ('number' == typeof t.length &&
                void 0 !== t.splice &&
                void 0 !== t.propertyIsEnumerable &&
                !t.propertyIsEnumerable('splice'))
            )
              return 'array'
            if (
              '[object Function]' == n ||
              (void 0 !== t.call &&
                void 0 !== t.propertyIsEnumerable &&
                !t.propertyIsEnumerable('call'))
            )
              return 'function'
          } else if ('function' == e && void 0 === t.call) return 'object'
          return e
        }
        function u(t) {
          return null === t
        }
        function c(t) {
          return 'array' == s(t)
        }
        function h(t) {
          var e = s(t)
          return 'array' == e || ('object' == e && 'number' == typeof t.length)
        }
        function f(t) {
          return 'function' == s(t)
        }
        function l(t) {
          var e = typeof t
          return ('object' == e && null != t) || 'function' == e
        }
        var d = 'closure_uid_' + ((1e9 * Math.random()) >>> 0),
          p = 0
        function v(t, e, n) {
          return t.call.apply(t.bind, arguments)
        }
        function m(t, e, n) {
          if (!t) throw Error()
          if (2 < arguments.length) {
            var i = Array.prototype.slice.call(arguments, 2)
            return function() {
              var n = Array.prototype.slice.call(arguments)
              return Array.prototype.unshift.apply(n, i), t.apply(e, n)
            }
          }
          return function() {
            return t.apply(e, arguments)
          }
        }
        function g(t, e, n) {
          return (g =
            Function.prototype.bind &&
            -1 != Function.prototype.bind.toString().indexOf('native code')
              ? v
              : m).apply(null, arguments)
        }
        function b(t, e) {
          var n = Array.prototype.slice.call(arguments, 1)
          return function() {
            var e = n.slice()
            return e.push.apply(e, arguments), t.apply(this, e)
          }
        }
        var y =
          Date.now ||
          function() {
            return +new Date()
          }
        function w(t, e) {
          function n() {}
          ;(n.prototype = e.prototype),
            (t.nb = e.prototype),
            (t.prototype = new n()),
            (t.prototype.constructor = t),
            (t.dd = function(t, n, i) {
              for (
                var r = Array(arguments.length - 2), o = 2;
                o < arguments.length;
                o++
              )
                r[o - 2] = arguments[o]
              return e.prototype[n].apply(t, r)
            })
        }
        function I(t) {
          ;(t.prototype.then = t.prototype.then),
            (t.prototype.$goog_Thenable = !0)
        }
        function T(t) {
          if (!t) return !1
          try {
            return !!t.$goog_Thenable
          } catch (t) {
            return !1
          }
        }
        function E(t) {
          if (Error.captureStackTrace) Error.captureStackTrace(this, E)
          else {
            var e = Error().stack
            e && (this.stack = e)
          }
          t && (this.message = String(t))
        }
        function A(t, e) {
          for (
            var n = '', i = (t = t.split('%s')).length - 1, r = 0;
            r < i;
            r++
          )
            n += t[r] + (r < e.length ? e[r] : '%s')
          E.call(this, n + t[i])
        }
        function k(t, e) {
          throw new A(
            'Failure' + (t ? ': ' + t : ''),
            Array.prototype.slice.call(arguments, 1),
          )
        }
        function S(t, e) {
          ;(this.c = t), (this.f = e), (this.b = 0), (this.a = null)
        }
        function N(t, e) {
          t.f(e), 100 > t.b && (t.b++, (e.next = t.a), (t.a = e))
        }
        function _() {
          this.b = this.a = null
        }
        w(E, Error),
          (E.prototype.name = 'CustomError'),
          w(A, E),
          (A.prototype.name = 'AssertionError'),
          (S.prototype.get = function() {
            if (0 < this.b) {
              this.b--
              var t = this.a
              ;(this.a = t.next), (t.next = null)
            } else t = this.c()
            return t
          })
        var O = new S(
          function() {
            return new R()
          },
          function(t) {
            t.reset()
          },
        )
        function P() {
          var t = dt,
            e = null
          return (
            t.a &&
              ((e = t.a),
              (t.a = t.a.next),
              t.a || (t.b = null),
              (e.next = null)),
            e
          )
        }
        function R() {
          this.next = this.b = this.a = null
        }
        ;(_.prototype.add = function(t, e) {
          var n = O.get()
          n.set(t, e), this.b ? (this.b.next = n) : (this.a = n), (this.b = n)
        }),
          (R.prototype.set = function(t, e) {
            ;(this.a = t), (this.b = e), (this.next = null)
          }),
          (R.prototype.reset = function() {
            this.next = this.b = this.a = null
          })
        var C = Array.prototype.indexOf
            ? function(t, e) {
                return Array.prototype.indexOf.call(t, e, void 0)
              }
            : function(t, e) {
                if (r(t)) return r(e) && 1 == e.length ? t.indexOf(e, 0) : -1
                for (var n = 0; n < t.length; n++)
                  if (n in t && t[n] === e) return n
                return -1
              },
          D = Array.prototype.forEach
            ? function(t, e, n) {
                Array.prototype.forEach.call(t, e, n)
              }
            : function(t, e, n) {
                for (
                  var i = t.length, o = r(t) ? t.split('') : t, a = 0;
                  a < i;
                  a++
                )
                  a in o && e.call(n, o[a], a, t)
              }
        var L = Array.prototype.map
            ? function(t, e) {
                return Array.prototype.map.call(t, e, void 0)
              }
            : function(t, e) {
                for (
                  var n = t.length,
                    i = Array(n),
                    o = r(t) ? t.split('') : t,
                    a = 0;
                  a < n;
                  a++
                )
                  a in o && (i[a] = e.call(void 0, o[a], a, t))
                return i
              },
          x = Array.prototype.some
            ? function(t, e) {
                return Array.prototype.some.call(t, e, void 0)
              }
            : function(t, e) {
                for (
                  var n = t.length, i = r(t) ? t.split('') : t, o = 0;
                  o < n;
                  o++
                )
                  if (o in i && e.call(void 0, i[o], o, t)) return !0
                return !1
              }
        function M(t, e) {
          return 0 <= C(t, e)
        }
        function j(t, e) {
          var n
          return (
            (n = 0 <= (e = C(t, e))) && Array.prototype.splice.call(t, e, 1), n
          )
        }
        function U(t, e) {
          !(function(t, e) {
            var n = t.length,
              i = r(t) ? t.split('') : t
            for (--n; 0 <= n; --n) n in i && e.call(void 0, i[n], n, t)
          })(t, function(n, i) {
            e.call(void 0, n, i, t) &&
              1 == Array.prototype.splice.call(t, i, 1).length &&
              0
          })
        }
        function V(t) {
          return Array.prototype.concat.apply([], arguments)
        }
        function F(t) {
          var e = t.length
          if (0 < e) {
            for (var n = Array(e), i = 0; i < e; i++) n[i] = t[i]
            return n
          }
          return []
        }
        function K(t, e) {
          for (
            var n = t.split('%s'),
              i = '',
              r = Array.prototype.slice.call(arguments, 1);
            r.length && 1 < n.length;

          )
            i += n.shift() + r.shift()
          return i + n.join('%s')
        }
        var H = String.prototype.trim
          ? function(t) {
              return t.trim()
            }
          : function(t) {
              return /^[\s\xa0]*([\s\S]*?)[\s\xa0]*$/.exec(t)[1]
            }
        function q(t) {
          return $.test(t)
            ? (-1 != t.indexOf('&') && (t = t.replace(G, '&amp;')),
              -1 != t.indexOf('<') && (t = t.replace(B, '&lt;')),
              -1 != t.indexOf('>') && (t = t.replace(X, '&gt;')),
              -1 != t.indexOf('"') && (t = t.replace(z, '&quot;')),
              -1 != t.indexOf("'") && (t = t.replace(J, '&#39;')),
              -1 != t.indexOf('\0') && (t = t.replace(Y, '&#0;')),
              t)
            : t
        }
        var W,
          G = /&/g,
          B = /</g,
          X = />/g,
          z = /"/g,
          J = /'/g,
          Y = /\x00/g,
          $ = /[\x00&<>"']/
        function Z(t, e) {
          return -1 != t.indexOf(e)
        }
        function Q(t, e) {
          return t < e ? -1 : t > e ? 1 : 0
        }
        t: {
          var tt = i.navigator
          if (tt) {
            var et = tt.userAgent
            if (et) {
              W = et
              break t
            }
          }
          W = ''
        }
        function nt(t) {
          return Z(W, t)
        }
        function it(t, e) {
          for (var n in t) e.call(void 0, t[n], n, t)
        }
        function rt(t) {
          for (var e in t) return !1
          return !0
        }
        function ot(t) {
          var e,
            n = {}
          for (e in t) n[e] = t[e]
          return n
        }
        var at,
          st,
          ut = 'constructor hasOwnProperty isPrototypeOf propertyIsEnumerable toLocaleString toString valueOf'.split(
            ' ',
          )
        function ct(t, e) {
          for (var n, i, r = 1; r < arguments.length; r++) {
            for (n in (i = arguments[r])) t[n] = i[n]
            for (var o = 0; o < ut.length; o++)
              (n = ut[o]),
                Object.prototype.hasOwnProperty.call(i, n) && (t[n] = i[n])
          }
        }
        function ht(t) {
          i.setTimeout(function() {
            throw t
          }, 0)
        }
        function ft(t, e) {
          st ||
            (function() {
              if (i.Promise && i.Promise.resolve) {
                var t = i.Promise.resolve(void 0)
                st = function() {
                  t.then(pt)
                }
              } else
                st = function() {
                  var t = pt
                  !f(i.setImmediate) ||
                  (i.Window &&
                    i.Window.prototype &&
                    !nt('Edge') &&
                    i.Window.prototype.setImmediate == i.setImmediate)
                    ? (at ||
                        (at = (function() {
                          var t = i.MessageChannel
                          if (
                            (void 0 === t &&
                              'undefined' != typeof window &&
                              window.postMessage &&
                              window.addEventListener &&
                              !nt('Presto') &&
                              (t = function() {
                                var t = document.createElement('IFRAME')
                                ;(t.style.display = 'none'),
                                  (t.src = ''),
                                  document.documentElement.appendChild(t)
                                var e = t.contentWindow
                                ;(t = e.document).open(), t.write(''), t.close()
                                var n = 'callImmediate' + Math.random(),
                                  i =
                                    'file:' == e.location.protocol
                                      ? '*'
                                      : e.location.protocol +
                                        '//' +
                                        e.location.host
                                ;(t = g(function(t) {
                                  ;('*' != i && t.origin != i) ||
                                    t.data != n ||
                                    this.port1.onmessage()
                                }, this)),
                                  e.addEventListener('message', t, !1),
                                  (this.port1 = {}),
                                  (this.port2 = {
                                    postMessage: function() {
                                      e.postMessage(n, i)
                                    },
                                  })
                              }),
                            void 0 !== t && !nt('Trident') && !nt('MSIE'))
                          ) {
                            var e = new t(),
                              n = {},
                              r = n
                            return (
                              (e.port1.onmessage = function() {
                                if (void 0 !== n.next) {
                                  var t = (n = n.next).vb
                                  ;(n.vb = null), t()
                                }
                              }),
                              function(t) {
                                ;(r.next = { vb: t }),
                                  (r = r.next),
                                  e.port2.postMessage(0)
                              }
                            )
                          }
                          return 'undefined' != typeof document &&
                            'onreadystatechange' in
                              document.createElement('SCRIPT')
                            ? function(t) {
                                var e = document.createElement('SCRIPT')
                                ;(e.onreadystatechange = function() {
                                  ;(e.onreadystatechange = null),
                                    e.parentNode.removeChild(e),
                                    (e = null),
                                    t(),
                                    (t = null)
                                }),
                                  document.documentElement.appendChild(e)
                              }
                            : function(t) {
                                i.setTimeout(t, 0)
                              }
                        })()),
                      at(t))
                    : i.setImmediate(t)
                }
            })(),
            lt || (st(), (lt = !0)),
            dt.add(t, e)
        }
        var lt = !1,
          dt = new _()
        function pt() {
          for (var t; (t = P()); ) {
            try {
              t.a.call(t.b)
            } catch (t) {
              ht(t)
            }
            N(O, t)
          }
          lt = !1
        }
        function vt(t, e) {
          if (
            ((this.a = mt),
            (this.i = void 0),
            (this.f = this.b = this.c = null),
            (this.g = this.h = !1),
            t != a)
          )
            try {
              var n = this
              t.call(
                e,
                function(t) {
                  _t(n, gt, t)
                },
                function(t) {
                  if (!(t instanceof xt))
                    try {
                      if (t instanceof Error) throw t
                      throw Error('Promise rejected.')
                    } catch (t) {}
                  _t(n, bt, t)
                },
              )
            } catch (t) {
              _t(this, bt, t)
            }
        }
        var mt = 0,
          gt = 2,
          bt = 3
        function yt() {
          ;(this.next = this.f = this.b = this.g = this.a = null), (this.c = !1)
        }
        yt.prototype.reset = function() {
          ;(this.f = this.b = this.g = this.a = null), (this.c = !1)
        }
        var wt = new S(
          function() {
            return new yt()
          },
          function(t) {
            t.reset()
          },
        )
        function It(t, e, n) {
          var i = wt.get()
          return (i.g = t), (i.b = e), (i.f = n), i
        }
        function Tt(t) {
          if (t instanceof vt) return t
          var e = new vt(a)
          return _t(e, gt, t), e
        }
        function Et(t) {
          return new vt(function(e, n) {
            n(t)
          })
        }
        function At(t, e, n) {
          Ot(t, e, n, null) || ft(b(e, t))
        }
        function kt(t) {
          return new vt(function(e) {
            var n = t.length,
              i = []
            if (n)
              for (
                var r = function(t, r, o) {
                    n--,
                      (i[t] = r ? { Cb: !0, value: o } : { Cb: !1, reason: o }),
                      0 == n && e(i)
                  },
                  o = 0;
                o < t.length;
                o++
              )
                At(t[o], b(r, o, !0), b(r, o, !1))
            else e(i)
          })
        }
        function St(t, e) {
          t.b || (t.a != gt && t.a != bt) || Pt(t),
            t.f ? (t.f.next = e) : (t.b = e),
            (t.f = e)
        }
        function Nt(t, e, n, i) {
          var r = It(null, null, null)
          return (
            (r.a = new vt(function(t, o) {
              ;(r.g = e
                ? function(n) {
                    try {
                      var r = e.call(i, n)
                      t(r)
                    } catch (t) {
                      o(t)
                    }
                  }
                : t),
                (r.b = n
                  ? function(e) {
                      try {
                        var r = n.call(i, e)
                        void 0 === r && e instanceof xt ? o(e) : t(r)
                      } catch (t) {
                        o(t)
                      }
                    }
                  : o)
            })),
            (r.a.c = t),
            St(t, r),
            r.a
          )
        }
        function _t(t, e, n) {
          t.a == mt &&
            (t === n &&
              ((e = bt),
              (n = new TypeError('Promise cannot resolve to itself'))),
            (t.a = 1),
            Ot(n, t.Mc, t.Nc, t) ||
              ((t.i = n),
              (t.a = e),
              (t.c = null),
              Pt(t),
              e != bt ||
                n instanceof xt ||
                (function(t, e) {
                  ;(t.g = !0),
                    ft(function() {
                      t.g && Lt.call(null, e)
                    })
                })(t, n)))
        }
        function Ot(t, e, n, i) {
          if (t instanceof vt) return St(t, It(e || a, n || null, i)), !0
          if (T(t)) return t.then(e, n, i), !0
          if (l(t))
            try {
              var r = t.then
              if (f(r))
                return (
                  (function(t, e, n, i, r) {
                    function o(t) {
                      a || ((a = !0), i.call(r, t))
                    }
                    var a = !1
                    try {
                      e.call(
                        t,
                        function(t) {
                          a || ((a = !0), n.call(r, t))
                        },
                        o,
                      )
                    } catch (t) {
                      o(t)
                    }
                  })(t, r, e, n, i),
                  !0
                )
            } catch (t) {
              return n.call(i, t), !0
            }
          return !1
        }
        function Pt(t) {
          t.h || ((t.h = !0), ft(t.Xb, t))
        }
        function Rt(t) {
          var e = null
          return (
            t.b && ((e = t.b), (t.b = e.next), (e.next = null)),
            t.b || (t.f = null),
            e
          )
        }
        function Ct(t, e, n, i) {
          if (n == bt && e.b && !e.c) for (; t && t.g; t = t.c) t.g = !1
          if (e.a) (e.a.c = null), Dt(e, n, i)
          else
            try {
              e.c ? e.g.call(e.f) : Dt(e, n, i)
            } catch (t) {
              Lt.call(null, t)
            }
          N(wt, e)
        }
        function Dt(t, e, n) {
          e == gt ? t.g.call(t.f, n) : t.b && t.b.call(t.f, n)
        }
        ;(vt.prototype.then = function(t, e, n) {
          return Nt(this, f(t) ? t : null, f(e) ? e : null, n)
        }),
          I(vt),
          ((t = vt.prototype).ia = function(t, e) {
            return ((t = It(t, t, e)).c = !0), St(this, t), this
          }),
          (t.s = function(t, e) {
            return Nt(this, null, t, e)
          }),
          (t.cancel = function(t) {
            this.a == mt &&
              ft(function() {
                !(function t(e, n) {
                  if (e.a == mt)
                    if (e.c) {
                      var i = e.c
                      if (i.b) {
                        for (
                          var r = 0, o = null, a = null, s = i.b;
                          s &&
                          (s.c || (r++, s.a == e && (o = s), !(o && 1 < r)));
                          s = s.next
                        )
                          o || (a = s)
                        o &&
                          (i.a == mt && 1 == r
                            ? t(i, n)
                            : (a
                                ? ((r = a).next == i.f && (i.f = r),
                                  (r.next = r.next.next))
                                : Rt(i),
                              Ct(i, o, bt, n)))
                      }
                      e.c = null
                    } else _t(e, bt, n)
                })(this, new xt(t))
              }, this)
          }),
          (t.Mc = function(t) {
            ;(this.a = mt), _t(this, gt, t)
          }),
          (t.Nc = function(t) {
            ;(this.a = mt), _t(this, bt, t)
          }),
          (t.Xb = function() {
            for (var t; (t = Rt(this)); ) Ct(this, t, this.a, this.i)
            this.h = !1
          })
        var Lt = ht
        function xt(t) {
          E.call(this, t)
        }
        function Mt() {
          0 != jt && (Ut[this[d] || (this[d] = ++p)] = this),
            (this.pa = this.pa),
            (this.ja = this.ja)
        }
        w(xt, E), (xt.prototype.name = 'cancel')
        var jt = 0,
          Ut = {}
        function Vt(t) {
          if (!t.pa && ((t.pa = !0), t.ua(), 0 != jt)) {
            var e = t[d] || (t[d] = ++p)
            if (0 != jt && t.ja && 0 < t.ja.length)
              throw Error(
                t +
                  " did not empty its onDisposeCallbacks queue. This probably means it overrode dispose() or disposeInternal() without calling the superclass' method.",
              )
            delete Ut[e]
          }
        }
        function Ft(t) {
          return Ft[' '](t), t
        }
        ;(Mt.prototype.pa = !1),
          (Mt.prototype.ua = function() {
            if (this.ja) for (; this.ja.length; ) this.ja.shift()()
          }),
          (Ft[' '] = a)
        var Kt,
          Ht,
          qt = nt('Opera'),
          Wt = nt('Trident') || nt('MSIE'),
          Gt = nt('Edge'),
          Bt = Gt || Wt,
          Xt =
            nt('Gecko') &&
            !(Z(W.toLowerCase(), 'webkit') && !nt('Edge')) &&
            !(nt('Trident') || nt('MSIE')) &&
            !nt('Edge'),
          zt = Z(W.toLowerCase(), 'webkit') && !nt('Edge')
        function Jt() {
          var t = i.document
          return t ? t.documentMode : void 0
        }
        t: {
          var Yt = '',
            $t = ((Ht = W),
            Xt
              ? /rv:([^\);]+)(\)|;)/.exec(Ht)
              : Gt
                ? /Edge\/([\d\.]+)/.exec(Ht)
                : Wt
                  ? /\b(?:MSIE|rv)[: ]([^\);]+)(\)|;)/.exec(Ht)
                  : zt
                    ? /WebKit\/(\S+)/.exec(Ht)
                    : qt
                      ? /(?:Version)[ \/]?(\S+)/.exec(Ht)
                      : void 0)
          if (($t && (Yt = $t ? $t[1] : ''), Wt)) {
            var Zt = Jt()
            if (null != Zt && Zt > parseFloat(Yt)) {
              Kt = String(Zt)
              break t
            }
          }
          Kt = Yt
        }
        var Qt,
          te = {}
        function ee(t) {
          return (function(t, e) {
            var n = te
            return Object.prototype.hasOwnProperty.call(n, t)
              ? n[t]
              : (n[t] = e(t))
          })(t, function() {
            for (
              var e = 0,
                n = H(String(Kt)).split('.'),
                i = H(String(t)).split('.'),
                r = Math.max(n.length, i.length),
                o = 0;
              0 == e && o < r;
              o++
            ) {
              var a = n[o] || '',
                s = i[o] || ''
              do {
                if (
                  ((a = /(\d*)(\D*)(.*)/.exec(a) || ['', '', '', '']),
                  (s = /(\d*)(\D*)(.*)/.exec(s) || ['', '', '', '']),
                  0 == a[0].length && 0 == s[0].length)
                )
                  break
                ;(e =
                  Q(
                    0 == a[1].length ? 0 : parseInt(a[1], 10),
                    0 == s[1].length ? 0 : parseInt(s[1], 10),
                  ) ||
                  Q(0 == a[2].length, 0 == s[2].length) ||
                  Q(a[2], s[2])),
                  (a = a[3]),
                  (s = s[3])
              } while (0 == e)
            }
            return 0 <= e
          })
        }
        var ne = i.document
        Qt =
          ne && Wt
            ? Jt() || ('CSS1Compat' == ne.compatMode ? parseInt(Kt, 10) : 5)
            : void 0
        var ie =
            Object.freeze ||
            function(t) {
              return t
            },
          re = !Wt || 9 <= Number(Qt),
          oe = Wt && !ee('9'),
          ae = (function() {
            if (!i.addEventListener || !Object.defineProperty) return !1
            var t = !1,
              e = Object.defineProperty({}, 'passive', {
                get: function() {
                  t = !0
                },
              })
            return (
              i.addEventListener('test', a, e),
              i.removeEventListener('test', a, e),
              t
            )
          })()
        function se(t, e) {
          ;(this.type = t), (this.b = this.target = e), (this.Jb = !0)
        }
        function ue(t, e) {
          if (
            (se.call(this, t ? t.type : ''),
            (this.relatedTarget = this.b = this.target = null),
            (this.button = this.screenY = this.screenX = this.clientY = this.clientX = 0),
            (this.key = ''),
            (this.metaKey = this.shiftKey = this.altKey = this.ctrlKey = !1),
            (this.pointerId = 0),
            (this.pointerType = ''),
            (this.a = null),
            t)
          ) {
            var n = (this.type = t.type),
              i = t.changedTouches ? t.changedTouches[0] : null
            if (
              ((this.target = t.target || t.srcElement),
              (this.b = e),
              (e = t.relatedTarget))
            ) {
              if (Xt) {
                t: {
                  try {
                    Ft(e.nodeName)
                    var o = !0
                    break t
                  } catch (t) {}
                  o = !1
                }
                o || (e = null)
              }
            } else
              'mouseover' == n
                ? (e = t.fromElement)
                : 'mouseout' == n && (e = t.toElement)
            ;(this.relatedTarget = e),
              null === i
                ? ((this.clientX = void 0 !== t.clientX ? t.clientX : t.pageX),
                  (this.clientY = void 0 !== t.clientY ? t.clientY : t.pageY),
                  (this.screenX = t.screenX || 0),
                  (this.screenY = t.screenY || 0))
                : ((this.clientX = void 0 !== i.clientX ? i.clientX : i.pageX),
                  (this.clientY = void 0 !== i.clientY ? i.clientY : i.pageY),
                  (this.screenX = i.screenX || 0),
                  (this.screenY = i.screenY || 0)),
              (this.button = t.button),
              (this.key = t.key || ''),
              (this.ctrlKey = t.ctrlKey),
              (this.altKey = t.altKey),
              (this.shiftKey = t.shiftKey),
              (this.metaKey = t.metaKey),
              (this.pointerId = t.pointerId || 0),
              (this.pointerType = r(t.pointerType)
                ? t.pointerType
                : ce[t.pointerType] || ''),
              (this.a = t),
              t.defaultPrevented && this.preventDefault()
          }
        }
        ;(se.prototype.preventDefault = function() {
          this.Jb = !1
        }),
          w(ue, se)
        var ce = ie({ 2: 'touch', 3: 'pen', 4: 'mouse' })
        ;(ue.prototype.preventDefault = function() {
          ue.nb.preventDefault.call(this)
          var t = this.a
          if (t.preventDefault) t.preventDefault()
          else if (((t.returnValue = !1), oe))
            try {
              ;(t.ctrlKey || (112 <= t.keyCode && 123 >= t.keyCode)) &&
                (t.keyCode = -1)
            } catch (t) {}
        }),
          (ue.prototype.f = function() {
            return this.a
          })
        var he = 'closure_listenable_' + ((1e6 * Math.random()) | 0),
          fe = 0
        function le(t) {
          ;(t.na = !0),
            (t.listener = null),
            (t.proxy = null),
            (t.src = null),
            (t.La = null)
        }
        function de(t) {
          ;(this.src = t), (this.a = {}), (this.b = 0)
        }
        function pe(t, e) {
          var n = e.type
          n in t.a &&
            j(t.a[n], e) &&
            (le(e), 0 == t.a[n].length && (delete t.a[n], t.b--))
        }
        function ve(t, e, n, i) {
          for (var r = 0; r < t.length; ++r) {
            var o = t[r]
            if (!o.na && o.listener == e && o.capture == !!n && o.La == i)
              return r
          }
          return -1
        }
        de.prototype.add = function(t, e, n, i, r) {
          var o = t.toString()
          ;(t = this.a[o]) || ((t = this.a[o] = []), this.b++)
          var a = ve(t, e, i, r)
          return (
            -1 < a
              ? ((e = t[a]), n || (e.Ia = !1))
              : (((e = new function(t, e, n, i, r) {
                  ;(this.listener = t),
                    (this.proxy = null),
                    (this.src = e),
                    (this.type = n),
                    (this.capture = !!i),
                    (this.La = r),
                    (this.key = ++fe),
                    (this.na = this.Ia = !1)
                }(e, this.src, o, !!i, r)).Ia = n),
                t.push(e)),
            e
          )
        }
        var me = 'closure_lm_' + ((1e6 * Math.random()) | 0),
          ge = {}
        function be(t, e, n, i, r) {
          if (i && i.once) we(t, e, n, i, r)
          else if (c(e)) for (var o = 0; o < e.length; o++) be(t, e[o], n, i, r)
          else
            (n = Oe(n)),
              t && t[he]
                ? Re(t, e, n, l(i) ? !!i.capture : !!i, r)
                : ye(t, e, n, !1, i, r)
        }
        function ye(t, e, n, i, r, o) {
          if (!e) throw Error('Invalid event type')
          var a = l(r) ? !!r.capture : !!r,
            s = Ne(t)
          if ((s || (t[me] = s = new de(t)), !(n = s.add(e, n, i, a, o)).proxy))
            if (
              ((i = (function() {
                var t = Se,
                  e = re
                    ? function(n) {
                        return t.call(e.src, e.listener, n)
                      }
                    : function(n) {
                        if (!(n = t.call(e.src, e.listener, n))) return n
                      }
                return e
              })()),
              (n.proxy = i),
              (i.src = t),
              (i.listener = n),
              t.addEventListener)
            )
              ae || (r = a),
                void 0 === r && (r = !1),
                t.addEventListener(e.toString(), i, r)
            else if (t.attachEvent) t.attachEvent(Ee(e.toString()), i)
            else {
              if (!t.addListener || !t.removeListener)
                throw Error('addEventListener and attachEvent are unavailable.')
              t.addListener(i)
            }
        }
        function we(t, e, n, i, r) {
          if (c(e)) for (var o = 0; o < e.length; o++) we(t, e[o], n, i, r)
          else
            (n = Oe(n)),
              t && t[he]
                ? Ce(t, e, n, l(i) ? !!i.capture : !!i, r)
                : ye(t, e, n, !0, i, r)
        }
        function Ie(t, e, n, i, r) {
          if (c(e)) for (var o = 0; o < e.length; o++) Ie(t, e[o], n, i, r)
          else
            (i = l(i) ? !!i.capture : !!i),
              (n = Oe(n)),
              t && t[he]
                ? ((t = t.l),
                  (e = String(e).toString()) in t.a &&
                    (-1 < (n = ve((o = t.a[e]), n, i, r)) &&
                      (le(o[n]),
                      Array.prototype.splice.call(o, n, 1),
                      0 == o.length && (delete t.a[e], t.b--))))
                : t &&
                  (t = Ne(t)) &&
                  ((e = t.a[e.toString()]),
                  (t = -1),
                  e && (t = ve(e, n, i, r)),
                  (n = -1 < t ? e[t] : null) && Te(n))
        }
        function Te(t) {
          if ('number' != typeof t && t && !t.na) {
            var e = t.src
            if (e && e[he]) pe(e.l, t)
            else {
              var n = t.type,
                i = t.proxy
              e.removeEventListener
                ? e.removeEventListener(n, i, t.capture)
                : e.detachEvent
                  ? e.detachEvent(Ee(n), i)
                  : e.addListener && e.removeListener && e.removeListener(i),
                (n = Ne(e))
                  ? (pe(n, t), 0 == n.b && ((n.src = null), (e[me] = null)))
                  : le(t)
            }
          }
        }
        function Ee(t) {
          return t in ge ? ge[t] : (ge[t] = 'on' + t)
        }
        function Ae(t, e, n, i) {
          var r = !0
          if ((t = Ne(t)) && (e = t.a[e.toString()]))
            for (e = e.concat(), t = 0; t < e.length; t++) {
              var o = e[t]
              o &&
                o.capture == n &&
                !o.na &&
                ((o = ke(o, i)), (r = r && !1 !== o))
            }
          return r
        }
        function ke(t, e) {
          var n = t.listener,
            i = t.La || t.src
          return t.Ia && Te(t), n.call(i, e)
        }
        function Se(t, e) {
          if (t.na) return !0
          if (!re) {
            if (!e)
              t: {
                e = ['window', 'event']
                for (var n = i, r = 0; r < e.length; r++)
                  if (null == (n = n[e[r]])) {
                    e = null
                    break t
                  }
                e = n
              }
            if (
              ((e = new ue((r = e), this)),
              (n = !0),
              !(0 > r.keyCode || void 0 != r.returnValue))
            ) {
              t: {
                var o = !1
                if (0 == r.keyCode)
                  try {
                    r.keyCode = -1
                    break t
                  } catch (t) {
                    o = !0
                  }
                ;(o || void 0 == r.returnValue) && (r.returnValue = !0)
              }
              for (r = [], o = e.b; o; o = o.parentNode) r.push(o)
              for (t = t.type, o = r.length - 1; 0 <= o; o--) {
                e.b = r[o]
                var a = Ae(r[o], t, !0, e)
                n = n && a
              }
              for (o = 0; o < r.length; o++)
                (e.b = r[o]), (a = Ae(r[o], t, !1, e)), (n = n && a)
            }
            return n
          }
          return ke(t, new ue(e, this))
        }
        function Ne(t) {
          return (t = t[me]) instanceof de ? t : null
        }
        var _e = '__closure_events_fn_' + ((1e9 * Math.random()) >>> 0)
        function Oe(t) {
          return f(t)
            ? t
            : (t[_e] ||
                (t[_e] = function(e) {
                  return t.handleEvent(e)
                }),
              t[_e])
        }
        function Pe() {
          Mt.call(this),
            (this.l = new de(this)),
            (this.Qb = this),
            (this.Ua = null)
        }
        function Re(t, e, n, i, r) {
          t.l.add(String(e), n, !1, i, r)
        }
        function Ce(t, e, n, i, r) {
          t.l.add(String(e), n, !0, i, r)
        }
        function De(t, e, n, i) {
          if (!(e = t.l.a[String(e)])) return !0
          e = e.concat()
          for (var r = !0, o = 0; o < e.length; ++o) {
            var a = e[o]
            if (a && !a.na && a.capture == n) {
              var s = a.listener,
                u = a.La || a.src
              a.Ia && pe(t.l, a), (r = !1 !== s.call(u, i) && r)
            }
          }
          return r && 0 != i.Jb
        }
        function Le(t, e, n) {
          if (f(t)) n && (t = g(t, n))
          else {
            if (!t || 'function' != typeof t.handleEvent)
              throw Error('Invalid listener argument')
            t = g(t.handleEvent, t)
          }
          return 2147483647 < Number(e) ? -1 : i.setTimeout(t, e || 0)
        }
        function xe(t) {
          var e = null
          return new vt(function(n, i) {
            ;-1 ==
              (e = Le(function() {
                n(void 0)
              }, t)) && i(Error('Failed to schedule timer.'))
          }).s(function(t) {
            throw (i.clearTimeout(e), t)
          })
        }
        function Me(t) {
          if (t.S && 'function' == typeof t.S) return t.S()
          if (r(t)) return t.split('')
          if (h(t)) {
            for (var e = [], n = t.length, i = 0; i < n; i++) e.push(t[i])
            return e
          }
          for (i in ((e = []), (n = 0), t)) e[n++] = t[i]
          return e
        }
        function je(t) {
          if (t.U && 'function' == typeof t.U) return t.U()
          if (!t.S || 'function' != typeof t.S) {
            if (h(t) || r(t)) {
              var e = []
              t = t.length
              for (var n = 0; n < t; n++) e.push(n)
              return e
            }
            for (var i in ((e = []), (n = 0), t)) e[n++] = i
            return e
          }
        }
        function Ue(t, e) {
          ;(this.b = {}), (this.a = []), (this.c = 0)
          var n = arguments.length
          if (1 < n) {
            if (n % 2) throw Error('Uneven number of arguments')
            for (var i = 0; i < n; i += 2)
              this.set(arguments[i], arguments[i + 1])
          } else if (t)
            if (t instanceof Ue)
              for (n = t.U(), i = 0; i < n.length; i++)
                this.set(n[i], t.get(n[i]))
            else for (i in t) this.set(i, t[i])
        }
        function Ve(t) {
          if (t.c != t.a.length) {
            for (var e = 0, n = 0; e < t.a.length; ) {
              var i = t.a[e]
              Fe(t.b, i) && (t.a[n++] = i), e++
            }
            t.a.length = n
          }
          if (t.c != t.a.length) {
            var r = {}
            for (n = e = 0; e < t.a.length; )
              Fe(r, (i = t.a[e])) || ((t.a[n++] = i), (r[i] = 1)), e++
            t.a.length = n
          }
        }
        function Fe(t, e) {
          return Object.prototype.hasOwnProperty.call(t, e)
        }
        w(Pe, Mt),
          (Pe.prototype[he] = !0),
          (Pe.prototype.addEventListener = function(t, e, n, i) {
            be(this, t, e, n, i)
          }),
          (Pe.prototype.removeEventListener = function(t, e, n, i) {
            Ie(this, t, e, n, i)
          }),
          (Pe.prototype.dispatchEvent = function(t) {
            var e,
              n = this.Ua
            if (n) for (e = []; n; n = n.Ua) e.push(n)
            n = this.Qb
            var i = t.type || t
            if (r(t)) t = new se(t, n)
            else if (t instanceof se) t.target = t.target || n
            else {
              var o = t
              ct((t = new se(i, n)), o)
            }
            if (((o = !0), e))
              for (var a = e.length - 1; 0 <= a; a--) {
                var s = (t.b = e[a])
                o = De(s, i, !0, t) && o
              }
            if (
              ((o = De((s = t.b = n), i, !0, t) && o),
              (o = De(s, i, !1, t) && o),
              e)
            )
              for (a = 0; a < e.length; a++)
                o = De((s = t.b = e[a]), i, !1, t) && o
            return o
          }),
          (Pe.prototype.ua = function() {
            if ((Pe.nb.ua.call(this), this.l)) {
              var t,
                e = this.l
              for (t in e.a) {
                for (var n = e.a[t], i = 0; i < n.length; i++) le(n[i])
                delete e.a[t], e.b--
              }
            }
            this.Ua = null
          }),
          ((t = Ue.prototype).S = function() {
            Ve(this)
            for (var t = [], e = 0; e < this.a.length; e++)
              t.push(this.b[this.a[e]])
            return t
          }),
          (t.U = function() {
            return Ve(this), this.a.concat()
          }),
          (t.clear = function() {
            ;(this.b = {}), (this.c = this.a.length = 0)
          }),
          (t.get = function(t, e) {
            return Fe(this.b, t) ? this.b[t] : e
          }),
          (t.set = function(t, e) {
            Fe(this.b, t) || (this.c++, this.a.push(t)), (this.b[t] = e)
          }),
          (t.forEach = function(t, e) {
            for (var n = this.U(), i = 0; i < n.length; i++) {
              var r = n[i],
                o = this.get(r)
              t.call(e, o, r, this)
            }
          })
        var Ke = /^(?:([^:/?#.]+):)?(?:\/\/(?:([^/?#]*)@)?([^/#?]*?)(?::([0-9]+))?(?=[/#?]|$))?([^?#]+)?(?:\?([^#]*))?(?:#([\s\S]*))?$/
        function He(t, e) {
          if (
            ((this.b = this.l = this.c = ''),
            (this.i = null),
            (this.h = this.g = ''),
            (this.f = !1),
            t instanceof He)
          ) {
            ;(this.f = void 0 !== e ? e : t.f),
              qe(this, t.c),
              (this.l = t.l),
              (this.b = t.b),
              We(this, t.i),
              (this.g = t.g),
              (e = t.a)
            var n = new on()
            ;(n.c = e.c),
              e.a && ((n.a = new Ue(e.a)), (n.b = e.b)),
              Ge(this, n),
              (this.h = t.h)
          } else
            t && (n = String(t).match(Ke))
              ? ((this.f = !!e),
                qe(this, n[1] || '', !0),
                (this.l = Ye(n[2] || '')),
                (this.b = Ye(n[3] || '', !0)),
                We(this, n[4]),
                (this.g = Ye(n[5] || '', !0)),
                Ge(this, n[6] || '', !0),
                (this.h = Ye(n[7] || '')))
              : ((this.f = !!e), (this.a = new on(null, this.f)))
        }
        function qe(t, e, n) {
          ;(t.c = n ? Ye(e, !0) : e), t.c && (t.c = t.c.replace(/:$/, ''))
        }
        function We(t, e) {
          if (e) {
            if (((e = Number(e)), isNaN(e) || 0 > e))
              throw Error('Bad port number ' + e)
            t.i = e
          } else t.i = null
        }
        function Ge(t, e, n) {
          e instanceof on
            ? ((t.a = e),
              (function(t, e) {
                e &&
                  !t.f &&
                  (an(t),
                  (t.c = null),
                  t.a.forEach(function(t, e) {
                    var n = e.toLowerCase()
                    e != n && (un(this, e), hn(this, n, t))
                  }, t)),
                  (t.f = e)
              })(t.a, t.f))
            : (n || (e = $e(e, nn)), (t.a = new on(e, t.f)))
        }
        function Be(t, e, n) {
          t.a.set(e, n)
        }
        function Xe(t, e) {
          return t.a.get(e)
        }
        function ze(t) {
          return t instanceof He ? new He(t) : new He(t, void 0)
        }
        function Je(t, e) {
          var n = new He(null, void 0)
          return qe(n, 'https'), t && (n.b = t), e && (n.g = e), n
        }
        function Ye(t, e) {
          return t
            ? e
              ? decodeURI(t.replace(/%25/g, '%2525'))
              : decodeURIComponent(t)
            : ''
        }
        function $e(t, e, n) {
          return r(t)
            ? ((t = encodeURI(t).replace(e, Ze)),
              n && (t = t.replace(/%25([0-9a-fA-F]{2})/g, '%$1')),
              t)
            : null
        }
        function Ze(t) {
          return (
            '%' +
            (((t = t.charCodeAt(0)) >> 4) & 15).toString(16) +
            (15 & t).toString(16)
          )
        }
        He.prototype.toString = function() {
          var t = [],
            e = this.c
          e && t.push($e(e, Qe, !0), ':')
          var n = this.b
          return (
            (n || 'file' == e) &&
              (t.push('//'),
              (e = this.l) && t.push($e(e, Qe, !0), '@'),
              t.push(
                encodeURIComponent(String(n)).replace(
                  /%25([0-9a-fA-F]{2})/g,
                  '%$1',
                ),
              ),
              null != (n = this.i) && t.push(':', String(n))),
            (n = this.g) &&
              (this.b && '/' != n.charAt(0) && t.push('/'),
              t.push($e(n, '/' == n.charAt(0) ? en : tn, !0))),
            (n = this.a.toString()) && t.push('?', n),
            (n = this.h) && t.push('#', $e(n, rn)),
            t.join('')
          )
        }
        var Qe = /[#\/\?@]/g,
          tn = /[#\?:]/g,
          en = /[#\?]/g,
          nn = /[#\?@]/g,
          rn = /#/g
        function on(t, e) {
          ;(this.b = this.a = null), (this.c = t || null), (this.f = !!e)
        }
        function an(t) {
          t.a ||
            ((t.a = new Ue()),
            (t.b = 0),
            t.c &&
              (function(t, e) {
                if (t) {
                  t = t.split('&')
                  for (var n = 0; n < t.length; n++) {
                    var i = t[n].indexOf('='),
                      r = null
                    if (0 <= i) {
                      var o = t[n].substring(0, i)
                      r = t[n].substring(i + 1)
                    } else o = t[n]
                    e(o, r ? decodeURIComponent(r.replace(/\+/g, ' ')) : '')
                  }
                }
              })(t.c, function(e, n) {
                t.add(decodeURIComponent(e.replace(/\+/g, ' ')), n)
              }))
        }
        function sn(t) {
          var e = je(t)
          if (void 0 === e) throw Error('Keys are undefined')
          var n = new on(null, void 0)
          t = Me(t)
          for (var i = 0; i < e.length; i++) {
            var r = e[i],
              o = t[i]
            c(o) ? hn(n, r, o) : n.add(r, o)
          }
          return n
        }
        function un(t, e) {
          an(t),
            (e = fn(t, e)),
            Fe(t.a.b, e) &&
              ((t.c = null),
              (t.b -= t.a.get(e).length),
              Fe((t = t.a).b, e) &&
                (delete t.b[e], t.c--, t.a.length > 2 * t.c && Ve(t)))
        }
        function cn(t, e) {
          return an(t), (e = fn(t, e)), Fe(t.a.b, e)
        }
        function hn(t, e, n) {
          un(t, e),
            0 < n.length &&
              ((t.c = null), t.a.set(fn(t, e), F(n)), (t.b += n.length))
        }
        function fn(t, e) {
          return (e = String(e)), t.f && (e = e.toLowerCase()), e
        }
        ;((t = on.prototype).add = function(t, e) {
          an(this), (this.c = null), (t = fn(this, t))
          var n = this.a.get(t)
          return n || this.a.set(t, (n = [])), n.push(e), (this.b += 1), this
        }),
          (t.clear = function() {
            ;(this.a = this.c = null), (this.b = 0)
          }),
          (t.forEach = function(t, e) {
            an(this),
              this.a.forEach(function(n, i) {
                D(
                  n,
                  function(n) {
                    t.call(e, n, i, this)
                  },
                  this,
                )
              }, this)
          }),
          (t.U = function() {
            an(this)
            for (
              var t = this.a.S(), e = this.a.U(), n = [], i = 0;
              i < e.length;
              i++
            )
              for (var r = t[i], o = 0; o < r.length; o++) n.push(e[i])
            return n
          }),
          (t.S = function(t) {
            an(this)
            var e = []
            if (r(t)) cn(this, t) && (e = V(e, this.a.get(fn(this, t))))
            else {
              t = this.a.S()
              for (var n = 0; n < t.length; n++) e = V(e, t[n])
            }
            return e
          }),
          (t.set = function(t, e) {
            return (
              an(this),
              (this.c = null),
              cn(this, (t = fn(this, t))) && (this.b -= this.a.get(t).length),
              this.a.set(t, [e]),
              (this.b += 1),
              this
            )
          }),
          (t.get = function(t, e) {
            return 0 < (t = t ? this.S(t) : []).length ? String(t[0]) : e
          }),
          (t.toString = function() {
            if (this.c) return this.c
            if (!this.a) return ''
            for (var t = [], e = this.a.U(), n = 0; n < e.length; n++) {
              var i = e[n],
                r = encodeURIComponent(String(i))
              i = this.S(i)
              for (var o = 0; o < i.length; o++) {
                var a = r
                '' !== i[o] && (a += '=' + encodeURIComponent(String(i[o]))),
                  t.push(a)
              }
            }
            return (this.c = t.join('&'))
          })
        var ln = !Wt || 9 <= Number(Qt)
        function dn() {
          ;(this.a = ''), (this.b = vn)
        }
        function pn(t) {
          return t instanceof dn && t.constructor === dn && t.b === vn
            ? t.a
            : (k("expected object of type Const, got '" + t + "'"),
              'type_error:Const')
        }
        ;(dn.prototype.ma = !0),
          (dn.prototype.la = function() {
            return this.a
          }),
          (dn.prototype.toString = function() {
            return 'Const{' + this.a + '}'
          })
        var vn = {}
        function mn(t) {
          var e = new dn()
          return (e.a = t), e
        }
        function gn() {
          ;(this.a = ''), (this.b = Tn)
        }
        function bn(t) {
          return t instanceof gn && t.constructor === gn && t.b === Tn
            ? t.a
            : (k(
                "expected object of type TrustedResourceUrl, got '" +
                  t +
                  "' of type " +
                  s(t),
              ),
              'type_error:TrustedResourceUrl')
        }
        function yn(t, e) {
          var n = pn(t)
          if (!In.test(n))
            throw Error('Invalid TrustedResourceUrl format: ' + n)
          return (function(t) {
            var e = new gn()
            return (e.a = t), e
          })(
            (t = n.replace(wn, function(t, i) {
              if (!Object.prototype.hasOwnProperty.call(e, i))
                throw Error(
                  'Found marker, "' +
                    i +
                    '", in format string, "' +
                    n +
                    '", but no valid label mapping found in args: ' +
                    JSON.stringify(e),
                )
              return (t = e[i]) instanceof dn
                ? pn(t)
                : encodeURIComponent(String(t))
            })),
          )
        }
        mn(''),
          (gn.prototype.ma = !0),
          (gn.prototype.la = function() {
            return this.a
          }),
          (gn.prototype.toString = function() {
            return 'TrustedResourceUrl{' + this.a + '}'
          })
        var wn = /%{(\w+)}/g,
          In = /^(?:https:)?\/\/[0-9a-z.:[\]-]+\/|^\/[^\/\\]|^about:blank#/i,
          Tn = {}
        function En() {
          ;(this.a = ''), (this.b = Nn)
        }
        function An(t) {
          return t instanceof En && t.constructor === En && t.b === Nn
            ? t.a
            : (k(
                "expected object of type SafeUrl, got '" +
                  t +
                  "' of type " +
                  s(t),
              ),
              'type_error:SafeUrl')
        }
        ;(En.prototype.ma = !0),
          (En.prototype.la = function() {
            return this.a
          }),
          (En.prototype.toString = function() {
            return 'SafeUrl{' + this.a + '}'
          })
        var kn = /^(?:(?:https?|mailto|ftp):|[^:/?#]*(?:[/?#]|$))/i
        function Sn(t) {
          return t instanceof En
            ? t
            : ((t = t.ma ? t.la() : String(t)),
              kn.test(t) || (t = 'about:invalid#zClosurez'),
              _n(t))
        }
        var Nn = {}
        function _n(t) {
          var e = new En()
          return (e.a = t), e
        }
        function On() {
          ;(this.a = ''), (this.b = Pn)
        }
        _n('about:blank'),
          (On.prototype.ma = !0),
          (On.prototype.la = function() {
            return this.a
          }),
          (On.prototype.toString = function() {
            return 'SafeHtml{' + this.a + '}'
          })
        var Pn = {}
        function Rn(t) {
          var e = new On()
          return (e.a = t), e
        }
        function Cn(t) {
          var e = document
          return r(t) ? e.getElementById(t) : t
        }
        function Dn(t, e) {
          it(e, function(e, n) {
            e && e.ma && (e = e.la()),
              'style' == n
                ? (t.style.cssText = e)
                : 'class' == n
                  ? (t.className = e)
                  : 'for' == n
                    ? (t.htmlFor = e)
                    : Ln.hasOwnProperty(n)
                      ? t.setAttribute(Ln[n], e)
                      : 0 == n.lastIndexOf('aria-', 0) ||
                        0 == n.lastIndexOf('data-', 0)
                        ? t.setAttribute(n, e)
                        : (t[n] = e)
          })
        }
        Rn('<!DOCTYPE html>'), Rn(''), Rn('<br>')
        var Ln = {
          cellpadding: 'cellPadding',
          cellspacing: 'cellSpacing',
          colspan: 'colSpan',
          frameborder: 'frameBorder',
          height: 'height',
          maxlength: 'maxLength',
          nonce: 'nonce',
          role: 'role',
          rowspan: 'rowSpan',
          type: 'type',
          usemap: 'useMap',
          valign: 'vAlign',
          width: 'width',
        }
        function xn(t, e, n) {
          var i = arguments,
            o = document,
            a = String(i[0]),
            s = i[1]
          if (!ln && s && (s.name || s.type)) {
            if (
              ((a = ['<', a]),
              s.name && a.push(' name="', q(s.name), '"'),
              s.type)
            ) {
              a.push(' type="', q(s.type), '"')
              var u = {}
              ct(u, s), delete u.type, (s = u)
            }
            a.push('>'), (a = a.join(''))
          }
          return (
            (a = o.createElement(a)),
            s &&
              (r(s)
                ? (a.className = s)
                : c(s)
                  ? (a.className = s.join(' '))
                  : Dn(a, s)),
            2 < i.length &&
              (function(t, e, n) {
                function i(n) {
                  n && e.appendChild(r(n) ? t.createTextNode(n) : n)
                }
                for (var o = 2; o < n.length; o++) {
                  var a = n[o]
                  !h(a) || (l(a) && 0 < a.nodeType)
                    ? i(a)
                    : D(Mn(a) ? F(a) : a, i)
                }
              })(o, a, i),
            a
          )
        }
        function Mn(t) {
          if (t && 'number' == typeof t.length) {
            if (l(t))
              return 'function' == typeof t.item || 'string' == typeof t.item
            if (f(t)) return 'function' == typeof t.item
          }
          return !1
        }
        function jn(t) {
          var e = []
          return (
            (function t(e, n, i) {
              if (null == n) i.push('null')
              else {
                if ('object' == typeof n) {
                  if (c(n)) {
                    var r = n
                    ;(n = r.length), i.push('[')
                    for (var o = '', a = 0; a < n; a++)
                      i.push(o), t(e, r[a], i), (o = ',')
                    return void i.push(']')
                  }
                  if (
                    !(
                      n instanceof String ||
                      n instanceof Number ||
                      n instanceof Boolean
                    )
                  ) {
                    for (r in (i.push('{'), (o = ''), n))
                      Object.prototype.hasOwnProperty.call(n, r) &&
                        ('function' != typeof (a = n[r]) &&
                          (i.push(o),
                          Fn(r, i),
                          i.push(':'),
                          t(e, a, i),
                          (o = ',')))
                    return void i.push('}')
                  }
                  n = n.valueOf()
                }
                switch (typeof n) {
                  case 'string':
                    Fn(n, i)
                    break
                  case 'number':
                    i.push(isFinite(n) && !isNaN(n) ? String(n) : 'null')
                    break
                  case 'boolean':
                    i.push(String(n))
                    break
                  case 'function':
                    i.push('null')
                    break
                  default:
                    throw Error('Unknown type: ' + typeof n)
                }
              }
            })(new function() {}(), t, e),
            e.join('')
          )
        }
        var Un = {
            '"': '\\"',
            '\\': '\\\\',
            '/': '\\/',
            '\b': '\\b',
            '\f': '\\f',
            '\n': '\\n',
            '\r': '\\r',
            '\t': '\\t',
            '\v': '\\u000b',
          },
          Vn = /\uffff/.test('￿')
            ? /[\\"\x00-\x1f\x7f-\uffff]/g
            : /[\\"\x00-\x1f\x7f-\xff]/g
        function Fn(t, e) {
          e.push(
            '"',
            t.replace(Vn, function(t) {
              var e = Un[t]
              return (
                e ||
                  ((e =
                    '\\u' + (65536 | t.charCodeAt(0)).toString(16).substr(1)),
                  (Un[t] = e)),
                e
              )
            }),
            '"',
          )
        }
        function Kn() {
          var t = oi()
          return (Wt && !!Qt && 11 == Qt) || /Edge\/\d+/.test(t)
        }
        function Hn() {
          return (
            (i.window && i.window.location.href) ||
            (self && self.location && self.location.href) ||
            ''
          )
        }
        function qn(t, e) {
          e = e || i.window
          var n = 'about:blank'
          t && (n = An(Sn(t))), (e.location.href = n)
        }
        function Wn(t) {
          return !!(
            (t = (t || oi()).toLowerCase()).match(/android/) ||
            t.match(/webos/) ||
            t.match(/iphone|ipad|ipod/) ||
            t.match(/blackberry/) ||
            t.match(/windows phone/) ||
            t.match(/iemobile/)
          )
        }
        function Gn(t) {
          t = t || i.window
          try {
            t.close()
          } catch (t) {}
        }
        function Bn(t, e, n) {
          var i = Math.floor(1e9 * Math.random()).toString()
          ;(e = e || 500), (n = n || 600)
          var r = (window.screen.availHeight - n) / 2,
            o = (window.screen.availWidth - e) / 2
          for (a in ((e = {
            width: e,
            height: n,
            top: 0 < r ? r : 0,
            left: 0 < o ? o : 0,
            location: !0,
            resizable: !0,
            statusbar: !0,
            toolbar: !1,
          }),
          (n = oi().toLowerCase()),
          i && ((e.target = i), Z(n, 'crios/') && (e.target = '_blank')),
          ni(oi()) == ti &&
            ((t = t || 'http://localhost'), (e.scrollbars = !0)),
          (n = t || ''),
          (t = e) || (t = {}),
          (i = window),
          (e =
            n instanceof En ? n : Sn(void 0 !== n.href ? n.href : String(n))),
          (n = t.target || n.target),
          (r = []),
          t))
            switch (a) {
              case 'width':
              case 'height':
              case 'top':
              case 'left':
                r.push(a + '=' + t[a])
                break
              case 'target':
              case 'noopener':
              case 'noreferrer':
                break
              default:
                r.push(a + '=' + (t[a] ? 1 : 0))
            }
          var a = r.join(',')
          if (
            (((nt('iPhone') && !nt('iPod') && !nt('iPad')) ||
              nt('iPad') ||
              nt('iPod')) &&
            i.navigator &&
            i.navigator.standalone &&
            n &&
            '_self' != n
              ? ((a = i.document.createElement('A')),
                e instanceof En ||
                  e instanceof En ||
                  ((e = e.ma ? e.la() : String(e)),
                  kn.test(e) || (e = 'about:invalid#zClosurez'),
                  (e = _n(e))),
                (a.href = An(e)),
                a.setAttribute('target', n),
                t.noreferrer && a.setAttribute('rel', 'noreferrer'),
                (t = document.createEvent('MouseEvent')).initMouseEvent(
                  'click',
                  !0,
                  !0,
                  i,
                  1,
                ),
                a.dispatchEvent(t),
                (a = {}))
              : t.noreferrer
                ? ((a = i.open('', n, a)),
                  (t = An(e)),
                  a &&
                    (Bt &&
                      Z(t, ';') &&
                      (t = "'" + t.replace(/'/g, '%27') + "'"),
                    (a.opener = null),
                    mn('b/12014412, meta tag with sanitized URL'),
                    (t = Rn(
                      (t =
                        '<meta name="referrer" content="no-referrer"><meta http-equiv="refresh" content="0; url=' +
                        q(t) +
                        '">'),
                    )),
                    a.document.write(
                      (function(t) {
                        return t instanceof On &&
                          t.constructor === On &&
                          t.b === Pn
                          ? t.a
                          : (k(
                              "expected object of type SafeHtml, got '" +
                                t +
                                "' of type " +
                                s(t),
                            ),
                            'type_error:SafeHtml')
                      })(t),
                    ),
                    a.document.close()))
                : (a = i.open(An(e), n, a)) && t.noopener && (a.opener = null),
            a)
          )
            try {
              a.focus()
            } catch (t) {}
          return a
        }
        var Xn = /^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}$/
        function zn() {
          var t = null
          return new vt(function(e) {
            'complete' == i.document.readyState
              ? e()
              : ((t = function() {
                  e()
                }),
                we(window, 'load', t))
          }).s(function(e) {
            throw (Ie(window, 'load', t), e)
          })
        }
        function Jn(t) {
          return (
            (t = t || oi()),
            !(
              'file:' !== hi() ||
              !t.toLowerCase().match(/iphone|ipad|ipod|android/)
            )
          )
        }
        function Yn() {
          var t = i.window
          try {
            return !(!t || t == t.top)
          } catch (t) {
            return !1
          }
        }
        function $n() {
          return (
            'object' != typeof i.window && 'function' == typeof i.importScripts
          )
        }
        function Zn() {
          return e.INTERNAL.hasOwnProperty('reactNative')
            ? 'ReactNative'
            : e.INTERNAL.hasOwnProperty('node')
              ? 'Node'
              : $n()
                ? 'Worker'
                : 'Browser'
        }
        function Qn() {
          var t = Zn()
          return 'ReactNative' === t || 'Node' === t
        }
        var ti = 'Firefox',
          ei = 'Chrome'
        function ni(t) {
          var e = t.toLowerCase()
          return Z(e, 'opera/') || Z(e, 'opr/') || Z(e, 'opios/')
            ? 'Opera'
            : Z(e, 'iemobile')
              ? 'IEMobile'
              : Z(e, 'msie') || Z(e, 'trident/')
                ? 'IE'
                : Z(e, 'edge/')
                  ? 'Edge'
                  : Z(e, 'firefox/')
                    ? ti
                    : Z(e, 'silk/')
                      ? 'Silk'
                      : Z(e, 'blackberry')
                        ? 'Blackberry'
                        : Z(e, 'webos')
                          ? 'Webos'
                          : !Z(e, 'safari/') ||
                            Z(e, 'chrome/') ||
                            Z(e, 'crios/') ||
                            Z(e, 'android')
                            ? (!Z(e, 'chrome/') && !Z(e, 'crios/')) ||
                              Z(e, 'edge/')
                              ? Z(e, 'android')
                                ? 'Android'
                                : (t = t.match(
                                    /([a-zA-Z\d\.]+)\/[a-zA-Z\d\.]*$/,
                                  )) && 2 == t.length
                                  ? t[1]
                                  : 'Other'
                              : ei
                            : 'Safari'
        }
        var ii = { Tc: 'FirebaseCore-web', Vc: 'FirebaseUI-web' }
        function ri(t, e) {
          e = e || []
          var n,
            i = [],
            r = {}
          for (n in ii) r[ii[n]] = !0
          for (n = 0; n < e.length; n++)
            void 0 !== r[e[n]] && (delete r[e[n]], i.push(e[n]))
          return (
            i.sort(),
            (e = i).length || (e = ['FirebaseCore-web']),
            'Browser' === (i = Zn())
              ? (i = ni((r = oi())))
              : 'Worker' === i && (i = ni((r = oi())) + '-' + i),
            i + '/JsCore/' + t + '/' + e.join(',')
          )
        }
        function oi() {
          return (i.navigator && i.navigator.userAgent) || ''
        }
        function ai(t, e) {
          ;(t = t.split('.')), (e = e || i)
          for (
            var n = 0;
            n < t.length && 'object' == typeof e && null != e;
            n++
          )
            e = e[t[n]]
          return n != t.length && (e = void 0), e
        }
        function si() {
          try {
            var t = i.localStorage,
              e = vi()
            if (t)
              return t.setItem(e, '1'), t.removeItem(e), !Kn() || !!i.indexedDB
          } catch (t) {
            return $n() && !!i.indexedDB
          }
          return !1
        }
        function ui() {
          return (
            (ci() || 'chrome-extension:' === hi() || Jn()) &&
            !Qn() &&
            si() &&
            !$n()
          )
        }
        function ci() {
          return 'http:' === hi() || 'https:' === hi()
        }
        function hi() {
          return (i.location && i.location.protocol) || null
        }
        function fi(t) {
          return !Wn((t = t || oi())) && ni(t) != ti
        }
        function li(t) {
          return void 0 === t ? null : jn(t)
        }
        function di(t) {
          var e,
            n = {}
          for (e in t)
            t.hasOwnProperty(e) &&
              null !== t[e] &&
              void 0 !== t[e] &&
              (n[e] = t[e])
          return n
        }
        function pi(t) {
          if (null !== t) return JSON.parse(t)
        }
        function vi(t) {
          return t || Math.floor(1e9 * Math.random()).toString()
        }
        function mi(t) {
          return (
            'Safari' != ni((t = t || oi())) &&
            !t.toLowerCase().match(/iphone|ipad|ipod/)
          )
        }
        function gi() {
          var t = i.___jsl
          if (t && t.H)
            for (var e in t.H)
              if (
                ((t.H[e].r = t.H[e].r || []),
                (t.H[e].L = t.H[e].L || []),
                (t.H[e].r = t.H[e].L.concat()),
                t.CP)
              )
                for (var n = 0; n < t.CP.length; n++) t.CP[n] = null
        }
        function bi(t, e) {
          if (t > e) throw Error('Short delay should be less than long delay!')
          ;(this.a = t),
            (this.c = e),
            (t = oi()),
            (e = Zn()),
            (this.b = Wn(t) || 'ReactNative' === e)
        }
        function yi() {
          var t = i.document
          return (
            !t || void 0 === t.visibilityState || 'visible' == t.visibilityState
          )
        }
        function wi(t) {
          try {
            var e = new Date(parseInt(t, 10))
            if (!isNaN(e.getTime()) && !/[^0-9]/.test(t)) return e.toUTCString()
          } catch (t) {}
          return null
        }
        function Ii() {
          return !(!ai('fireauth.oauthhelper', i) && !ai('fireauth.iframe', i))
        }
        bi.prototype.get = function() {
          var t = i.navigator
          return !t ||
            'boolean' != typeof t.onLine ||
            (!ci() &&
              'chrome-extension:' !== hi() &&
              void 0 === t.connection) ||
            t.onLine
            ? this.b
              ? this.c
              : this.a
            : Math.min(5e3, this.a)
        }
        var Ti,
          Ei = {}
        function Ai(t) {
          Ei[t] ||
            ((Ei[t] = !0),
            'undefined' != typeof console &&
              'function' == typeof console.warn &&
              console.warn(t))
        }
        try {
          var ki = {}
          Object.defineProperty(ki, 'abcd', {
            configurable: !0,
            enumerable: !0,
            value: 1,
          }),
            Object.defineProperty(ki, 'abcd', {
              configurable: !0,
              enumerable: !0,
              value: 2,
            }),
            (Ti = 2 == ki.abcd)
        } catch (Ht) {
          Ti = !1
        }
        function Si(t, e, n) {
          Ti
            ? Object.defineProperty(t, e, {
                configurable: !0,
                enumerable: !0,
                value: n,
              })
            : (t[e] = n)
        }
        function Ni(t, e) {
          if (e) for (var n in e) e.hasOwnProperty(n) && Si(t, n, e[n])
        }
        function _i(t) {
          var e = {}
          return Ni(e, t), e
        }
        function Oi(t) {
          var e = t
          if ('object' == typeof t && null != t)
            for (var n in ((e = 'length' in t ? [] : {}), t)) Si(e, n, Oi(t[n]))
          return e
        }
        var Pi = 'EMAIL_SIGNIN',
          Ri = 'email',
          Ci = 'newEmail',
          Di = 'requestType',
          Li = 'email',
          xi = 'fromEmail',
          Mi = 'data',
          ji = 'operation'
        function Ui(t, e) {
          ;(this.code = Fi + t), (this.message = e || Ki[t] || '')
        }
        function Vi(t) {
          var e = t && t.code
          return e ? new Ui(e.substring(Fi.length), t.message) : null
        }
        w(Ui, Error),
          (Ui.prototype.D = function() {
            return { code: this.code, message: this.message }
          }),
          (Ui.prototype.toJSON = function() {
            return this.D()
          })
        var Fi = 'auth/',
          Ki = {
            'argument-error': '',
            'app-not-authorized':
              "This app, identified by the domain where it's hosted, is not authorized to use Firebase Authentication with the provided API key. Review your key configuration in the Google API console.",
            'app-not-installed':
              'The requested mobile application corresponding to the identifier (Android package name or iOS bundle ID) provided is not installed on this device.',
            'captcha-check-failed':
              'The reCAPTCHA response token provided is either invalid, expired, already used or the domain associated with it does not match the list of whitelisted domains.',
            'code-expired':
              'The SMS code has expired. Please re-send the verification code to try again.',
            'cordova-not-ready': 'Cordova framework is not ready.',
            'cors-unsupported': 'This browser is not supported.',
            'credential-already-in-use':
              'This credential is already associated with a different user account.',
            'custom-token-mismatch':
              'The custom token corresponds to a different audience.',
            'requires-recent-login':
              'This operation is sensitive and requires recent authentication. Log in again before retrying this request.',
            'dynamic-link-not-activated':
              'Please activate Dynamic Links in the Firebase Console and agree to the terms and conditions.',
            'email-already-in-use':
              'The email address is already in use by another account.',
            'expired-action-code': 'The action code has expired. ',
            'cancelled-popup-request':
              'This operation has been cancelled due to another conflicting popup being opened.',
            'internal-error': 'An internal error has occurred.',
            'invalid-app-credential':
              'The phone verification request contains an invalid application verifier. The reCAPTCHA token response is either invalid or expired.',
            'invalid-app-id':
              'The mobile app identifier is not registed for the current project.',
            'invalid-user-token':
              "This user's credential isn't valid for this project. This can happen if the user's token has been tampered with, or if the user isn't for the project associated with this API key.",
            'invalid-auth-event': 'An internal error has occurred.',
            'invalid-verification-code':
              'The SMS verification code used to create the phone auth credential is invalid. Please resend the verification code sms and be sure use the verification code provided by the user.',
            'invalid-continue-uri':
              'The continue URL provided in the request is invalid.',
            'invalid-cordova-configuration':
              'The following Cordova plugins must be installed to enable OAuth sign-in: cordova-plugin-buildinfo, cordova-universal-links-plugin, cordova-plugin-browsertab, cordova-plugin-inappbrowser and cordova-plugin-customurlscheme.',
            'invalid-custom-token':
              'The custom token format is incorrect. Please check the documentation.',
            'invalid-email': 'The email address is badly formatted.',
            'invalid-api-key':
              'Your API key is invalid, please check you have copied it correctly.',
            'invalid-cert-hash':
              'The SHA-1 certificate hash provided is invalid.',
            'invalid-credential':
              'The supplied auth credential is malformed or has expired.',
            'invalid-persistence-type':
              'The specified persistence type is invalid. It can only be local, session or none.',
            'invalid-message-payload':
              'The email template corresponding to this action contains invalid characters in its message. Please fix by going to the Auth email templates section in the Firebase Console.',
            'invalid-oauth-provider':
              'EmailAuthProvider is not supported for this operation. This operation only supports OAuth providers.',
            'invalid-oauth-client-id':
              'The OAuth client ID provided is either invalid or does not match the specified API key.',
            'unauthorized-domain':
              'This domain is not authorized for OAuth operations for your Firebase project. Edit the list of authorized domains from the Firebase console.',
            'invalid-action-code':
              'The action code is invalid. This can happen if the code is malformed, expired, or has already been used.',
            'wrong-password':
              'The password is invalid or the user does not have a password.',
            'invalid-phone-number':
              'The format of the phone number provided is incorrect. Please enter the phone number in a format that can be parsed into E.164 format. E.164 phone numbers are written in the format [+][country code][subscriber number including area code].',
            'invalid-provider-id': 'The specified provider ID is invalid.',
            'invalid-recipient-email':
              'The email corresponding to this action failed to send as the provided recipient email address is invalid.',
            'invalid-sender':
              'The email template corresponding to this action contains an invalid sender email or name. Please fix by going to the Auth email templates section in the Firebase Console.',
            'invalid-verification-id':
              'The verification ID used to create the phone auth credential is invalid.',
            'missing-android-pkg-name':
              'An Android Package Name must be provided if the Android App is required to be installed.',
            'auth-domain-config-required':
              'Be sure to include authDomain when calling firebase.initializeApp(), by following the instructions in the Firebase console.',
            'missing-app-credential':
              'The phone verification request is missing an application verifier assertion. A reCAPTCHA response token needs to be provided.',
            'missing-verification-code':
              'The phone auth credential was created with an empty SMS verification code.',
            'missing-continue-uri':
              'A continue URL must be provided in the request.',
            'missing-iframe-start': 'An internal error has occurred.',
            'missing-ios-bundle-id':
              'An iOS Bundle ID must be provided if an App Store ID is provided.',
            'missing-phone-number':
              'To send verification codes, provide a phone number for the recipient.',
            'missing-verification-id':
              'The phone auth credential was created with an empty verification ID.',
            'app-deleted': 'This instance of FirebaseApp has been deleted.',
            'account-exists-with-different-credential':
              'An account already exists with the same email address but different sign-in credentials. Sign in using a provider associated with this email address.',
            'network-request-failed':
              'A network error (such as timeout, interrupted connection or unreachable host) has occurred.',
            'no-auth-event': 'An internal error has occurred.',
            'no-such-provider':
              'User was not linked to an account with the given provider.',
            'null-user':
              'A null user object was provided as the argument for an operation which requires a non-null user object.',
            'operation-not-allowed':
              'The given sign-in provider is disabled for this Firebase project. Enable it in the Firebase console, under the sign-in method tab of the Auth section.',
            'operation-not-supported-in-this-environment':
              'This operation is not supported in the environment this application is running on. "location.protocol" must be http, https or chrome-extension and web storage must be enabled.',
            'popup-blocked':
              'Unable to establish a connection with the popup. It may have been blocked by the browser.',
            'popup-closed-by-user':
              'The popup has been closed by the user before finalizing the operation.',
            'provider-already-linked':
              'User can only be linked to one identity for the given provider.',
            'quota-exceeded':
              "The project's quota for this operation has been exceeded.",
            'redirect-cancelled-by-user':
              'The redirect operation has been cancelled by the user before finalizing.',
            'redirect-operation-pending':
              'A redirect sign-in operation is already pending.',
            'rejected-credential':
              'The request contains malformed or mismatching credentials.',
            timeout: 'The operation has timed out.',
            'user-token-expired':
              "The user's credential is no longer valid. The user must sign in again.",
            'too-many-requests':
              'We have blocked all requests from this device due to unusual activity. Try again later.',
            'unauthorized-continue-uri':
              'The domain of the continue URL is not whitelisted.  Please whitelist the domain in the Firebase console.',
            'unsupported-persistence-type':
              'The current environment does not support the specified persistence type.',
            'user-cancelled':
              'User did not grant your application the permissions it requested.',
            'user-not-found':
              'There is no user record corresponding to this identifier. The user may have been deleted.',
            'user-disabled':
              'The user account has been disabled by an administrator.',
            'user-mismatch':
              'The supplied credentials do not correspond to the previously signed in user.',
            'user-signed-out': '',
            'weak-password': 'The password must be 6 characters long or more.',
            'web-storage-unsupported':
              'This browser is not supported or 3rd party cookies and data may be disabled.',
          }
        function Hi(t) {
          var e = t[Bi]
          if (void 0 === e) throw new Ui('missing-continue-uri')
          if ('string' != typeof e || ('string' == typeof e && !e.length))
            throw new Ui('invalid-continue-uri')
          ;(this.h = e), (this.b = this.a = null), (this.g = !1)
          var n = t[qi]
          if (n && 'object' == typeof n) {
            e = n[Ji]
            var i = n[Xi]
            if (((n = n[zi]), 'string' == typeof e && e.length)) {
              if (((this.a = e), void 0 !== i && 'boolean' != typeof i))
                throw new Ui(
                  'argument-error',
                  Xi + ' property must be a boolean when specified.',
                )
              if (
                ((this.g = !!i),
                void 0 !== n &&
                  ('string' != typeof n || ('string' == typeof n && !n.length)))
              )
                throw new Ui(
                  'argument-error',
                  zi + ' property must be a non empty string when specified.',
                )
              this.b = n || null
            } else {
              if (void 0 !== e)
                throw new Ui(
                  'argument-error',
                  Ji + ' property must be a non empty string when specified.',
                )
              if (void 0 !== i || void 0 !== n)
                throw new Ui('missing-android-pkg-name')
            }
          } else if (void 0 !== n)
            throw new Ui(
              'argument-error',
              qi + ' property must be a non null object when specified.',
            )
          if (((this.f = null), (e = t[Gi]) && 'object' == typeof e)) {
            if ('string' == typeof (e = e[Yi]) && e.length) this.f = e
            else if (void 0 !== e)
              throw new Ui(
                'argument-error',
                Yi + ' property must be a non empty string when specified.',
              )
          } else if (void 0 !== e)
            throw new Ui(
              'argument-error',
              Gi + ' property must be a non null object when specified.',
            )
          if (void 0 !== (t = t[Wi]) && 'boolean' != typeof t)
            throw new Ui(
              'argument-error',
              Wi + ' property must be a boolean when specified.',
            )
          this.c = !!t
        }
        var qi = 'android',
          Wi = 'handleCodeInApp',
          Gi = 'iOS',
          Bi = 'url',
          Xi = 'installApp',
          zi = 'minimumVersion',
          Ji = 'packageName',
          Yi = 'bundleId'
        function $i(t) {
          var e = {}
          for (var n in ((e.continueUrl = t.h),
          (e.canHandleCodeInApp = t.c),
          (e.androidPackageName = t.a) &&
            ((e.androidMinimumVersion = t.b), (e.androidInstallApp = t.g)),
          (e.iOSBundleId = t.f),
          e))
            null === e[n] && delete e[n]
          return e
        }
        var Zi = null,
          Qi = null
        function tr(t) {
          var e = ''
          return (
            (function(t, e) {
              function n(e) {
                for (; i < t.length; ) {
                  var n = t.charAt(i++),
                    r = Qi[n]
                  if (null != r) return r
                  if (!/^[\s\xa0]*$/.test(n))
                    throw Error('Unknown base64 encoding at char: ' + n)
                }
                return e
              }
              !(function() {
                if (!Zi) {
                  ;(Zi = {}), (Qi = {})
                  for (var t = 0; 65 > t; t++)
                    (Zi[
                      t
                    ] = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='.charAt(
                      t,
                    )),
                      (Qi[Zi[t]] = t),
                      62 <= t &&
                        (Qi[
                          'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.'.charAt(
                            t,
                          )
                        ] = t)
                }
              })()
              for (var i = 0; ; ) {
                var r = n(-1),
                  o = n(0),
                  a = n(64),
                  s = n(64)
                if (64 === s && -1 === r) break
                e((r << 2) | (o >> 4)),
                  64 != a &&
                    (e(((o << 4) & 240) | (a >> 2)),
                    64 != s && e(((a << 6) & 192) | s))
              }
            })(t, function(t) {
              e += String.fromCharCode(t)
            }),
            e
          )
        }
        function er(t) {
          ;(this.c = t.sub),
            (this.a =
              t.provider_id ||
              (t.firebase && t.firebase.sign_in_provider) ||
              null),
            (this.b = !!t.is_anonymous || 'anonymous' == this.a)
        }
        function nr(t) {
          return (t = ir(t)) && t.sub && t.iss && t.aud && t.exp
            ? new er(t)
            : null
        }
        function ir(t) {
          if (!t) return null
          if (3 != (t = t.split('.')).length) return null
          for (var e = (4 - ((t = t[1]).length % 4)) % 4, n = 0; n < e; n++)
            t += '.'
          try {
            return JSON.parse(tr(t))
          } catch (t) {}
          return null
        }
        er.prototype.f = function() {
          return this.b
        }
        var rr = 'oauth_consumer_key oauth_nonce oauth_signature oauth_signature_method oauth_timestamp oauth_token oauth_version'.split(
            ' ',
          ),
          or = ['client_id', 'response_type', 'scope', 'redirect_uri', 'state'],
          ar = {
            Uc: { Ma: 'locale', Ba: 500, Aa: 600, Na: 'facebook.com', gb: or },
            Wc: { Ma: null, Ba: 500, Aa: 620, Na: 'github.com', gb: or },
            Xc: { Ma: 'hl', Ba: 515, Aa: 680, Na: 'google.com', gb: or },
            cd: { Ma: 'lang', Ba: 485, Aa: 705, Na: 'twitter.com', gb: rr },
          }
        function sr(t) {
          for (var e in ar) if (ar[e].Na == t) return ar[e]
          return null
        }
        function ur(t) {
          var e = {}
          ;(e['facebook.com'] = dr),
            (e['google.com'] = vr),
            (e['github.com'] = pr),
            (e['twitter.com'] = mr)
          var n = t && t[hr]
          try {
            if (n) return e[n] ? new e[n](t) : new lr(t)
            if (void 0 !== t[cr]) return new fr(t)
          } catch (t) {}
          return null
        }
        var cr = 'idToken',
          hr = 'providerId'
        function fr(t) {
          var e = t[hr]
          if (!e && t[cr]) {
            var n = nr(t[cr])
            n && n.a && (e = n.a)
          }
          if (!e) throw Error('Invalid additional user info!')
          ;('anonymous' != e && 'custom' != e) || (e = null),
            (n = !1),
            void 0 !== t.isNewUser
              ? (n = !!t.isNewUser)
              : 'identitytoolkit#SignupNewUserResponse' === t.kind && (n = !0),
            Si(this, 'providerId', e),
            Si(this, 'isNewUser', n)
        }
        function lr(t) {
          fr.call(this, t),
            Si(this, 'profile', Oi((t = pi(t.rawUserInfo || '{}')) || {}))
        }
        function dr(t) {
          if ((lr.call(this, t), 'facebook.com' != this.providerId))
            throw Error('Invalid provider ID!')
        }
        function pr(t) {
          if ((lr.call(this, t), 'github.com' != this.providerId))
            throw Error('Invalid provider ID!')
          Si(this, 'username', (this.profile && this.profile.login) || null)
        }
        function vr(t) {
          if ((lr.call(this, t), 'google.com' != this.providerId))
            throw Error('Invalid provider ID!')
        }
        function mr(t) {
          if ((lr.call(this, t), 'twitter.com' != this.providerId))
            throw Error('Invalid provider ID!')
          Si(this, 'username', t.screenName || null)
        }
        function gr(t) {
          var e = ze(t),
            n = Xe(e, 'link'),
            i = Xe(ze(n), 'link')
          return Xe(ze((e = Xe(e, 'deep_link_id'))), 'link') || e || i || n || t
        }
        function br(t, e) {
          return t
            .then(function(t) {
              if (t[Zo]) {
                var n = nr(t[Zo])
                if (!n || e != n.c) throw new Ui('user-mismatch')
                return t
              }
              throw new Ui('user-mismatch')
            })
            .s(function(t) {
              throw t && t.code && t.code == Fi + 'user-not-found'
                ? new Ui('user-mismatch')
                : t
            })
        }
        function yr(t, e, n) {
          if (e.idToken || e.accessToken)
            e.idToken && Si(this, 'idToken', e.idToken),
              e.accessToken && Si(this, 'accessToken', e.accessToken)
          else {
            if (!e.oauthToken || !e.oauthTokenSecret)
              throw new Ui('internal-error', 'failed to construct a credential')
            Si(this, 'accessToken', e.oauthToken),
              Si(this, 'secret', e.oauthTokenSecret)
          }
          Si(this, 'providerId', t), Si(this, 'signInMethod', n)
        }
        function wr(t) {
          var e = {}
          return (
            t.idToken && (e.id_token = t.idToken),
            t.accessToken && (e.access_token = t.accessToken),
            t.secret && (e.oauth_token_secret = t.secret),
            (e.providerId = t.providerId),
            { postBody: sn(e).toString(), requestUri: 'http://localhost' }
          )
        }
        function Ir(t, e) {
          ;(this.Cc = e || []),
            Ni(this, { providerId: t, isOAuthProvider: !0 }),
            (this.xb = {}),
            (this.bb = (sr(t) || {}).Ma || null),
            (this.Za = null)
        }
        function Tr(t) {
          Ir.call(this, t, or), (this.a = [])
        }
        function Er() {
          Tr.call(this, 'facebook.com')
        }
        function Ar(t) {
          if (!t)
            throw new Ui(
              'argument-error',
              'credential failed: expected 1 argument (the OAuth access token).',
            )
          var e = t
          return l(t) && (e = t.accessToken), new Er().credential(null, e)
        }
        function kr() {
          Tr.call(this, 'github.com')
        }
        function Sr(t) {
          if (!t)
            throw new Ui(
              'argument-error',
              'credential failed: expected 1 argument (the OAuth access token).',
            )
          var e = t
          return l(t) && (e = t.accessToken), new kr().credential(null, e)
        }
        function Nr() {
          Tr.call(this, 'google.com'), this.ta('profile')
        }
        function _r(t, e) {
          var n = t
          return (
            l(t) && ((n = t.idToken), (e = t.accessToken)),
            new Nr().credential(n, e)
          )
        }
        function Or() {
          Ir.call(this, 'twitter.com', rr)
        }
        function Pr(t, e) {
          var n = t
          if (
            (l(n) || (n = { oauthToken: t, oauthTokenSecret: e }),
            !n.oauthToken || !n.oauthTokenSecret)
          )
            throw new Ui(
              'argument-error',
              'credential failed: expected 2 arguments (the OAuth access token and secret).',
            )
          return new yr('twitter.com', n, 'twitter.com')
        }
        function Rr(t, e, n) {
          ;(this.a = t),
            (this.b = e),
            Si(this, 'providerId', 'password'),
            Si(
              this,
              'signInMethod',
              n === Cr.EMAIL_LINK_SIGN_IN_METHOD
                ? Cr.EMAIL_LINK_SIGN_IN_METHOD
                : Cr.EMAIL_PASSWORD_SIGN_IN_METHOD,
            )
        }
        function Cr() {
          Ni(this, { providerId: 'password', isOAuthProvider: !1 })
        }
        function Dr(t, e) {
          if (!(e = Lr(e)))
            throw new Ui('argument-error', 'Invalid email link!')
          return new Rr(t, e, Cr.EMAIL_LINK_SIGN_IN_METHOD)
        }
        function Lr(t) {
          var e =
            Xe(
              (t = new function(t) {
                this.a = ze(t)
              }((t = gr(t)))).a,
              'oobCode',
            ) || null
          return 'signIn' === (Xe(t.a, 'mode') || null) && e ? e : null
        }
        function xr(t) {
          if (!((t.Sa && t.Ra) || (t.Fa && t.$))) throw new Ui('internal-error')
          ;(this.a = t),
            Si(this, 'providerId', 'phone'),
            Si(this, 'signInMethod', 'phone')
        }
        function Mr(t) {
          return t.a.Fa && t.a.$
            ? { temporaryProof: t.a.Fa, phoneNumber: t.a.$ }
            : { sessionInfo: t.a.Sa, code: t.a.Ra }
        }
        function jr(t) {
          try {
            this.a = t || e.auth()
          } catch (t) {
            throw new Ui(
              'argument-error',
              'Either an instance of firebase.auth.Auth must be passed as an argument to the firebase.auth.PhoneAuthProvider constructor, or the default firebase App instance must be initialized via firebase.initializeApp().',
            )
          }
          Ni(this, { providerId: 'phone', isOAuthProvider: !1 })
        }
        function Ur(t, e) {
          if (!t) throw new Ui('missing-verification-id')
          if (!e) throw new Ui('missing-verification-code')
          return new xr({ Sa: t, Ra: e })
        }
        function Vr(t) {
          if (t.temporaryProof && t.phoneNumber)
            return new xr({ Fa: t.temporaryProof, $: t.phoneNumber })
          var e = t && t.providerId
          if (!e || 'password' === e) return null
          var n = t && t.oauthAccessToken,
            i = t && t.oauthTokenSecret
          t = t && t.oauthIdToken
          try {
            switch (e) {
              case 'google.com':
                return _r(t, n)
              case 'facebook.com':
                return Ar(n)
              case 'github.com':
                return Sr(n)
              case 'twitter.com':
                return Pr(n, i)
              default:
                return new Tr(e).credential(t, n)
            }
          } catch (t) {
            return null
          }
        }
        function Fr(t) {
          if (!t.isOAuthProvider) throw new Ui('invalid-oauth-provider')
        }
        function Kr(t, e, n, i, r) {
          if (
            ((this.b = t),
            (this.c = e || null),
            (this.f = n || null),
            (this.g = i || null),
            (this.a = r || null),
            !this.f && !this.a)
          )
            throw new Ui('invalid-auth-event')
          if (this.f && this.a) throw new Ui('invalid-auth-event')
          if (this.f && !this.g) throw new Ui('invalid-auth-event')
        }
        function Hr(t) {
          return (t = t || {}).type
            ? new Kr(
                t.type,
                t.eventId,
                t.urlResponse,
                t.sessionId,
                t.error && Vi(t.error),
              )
            : null
        }
        function qr() {
          ;(this.b = null), (this.a = [])
        }
        w(lr, fr),
          w(dr, lr),
          w(pr, lr),
          w(vr, lr),
          w(mr, lr),
          (yr.prototype.ya = function(t) {
            return va(t, wr(this))
          }),
          (yr.prototype.c = function(t, e) {
            var n = wr(this)
            return (n.idToken = e), ma(t, n)
          }),
          (yr.prototype.f = function(t, e) {
            return br(ga(t, wr(this)), e)
          }),
          (yr.prototype.D = function() {
            var t = {
              providerId: this.providerId,
              signInMethod: this.signInMethod,
            }
            return (
              this.idToken && (t.oauthIdToken = this.idToken),
              this.accessToken && (t.oauthAccessToken = this.accessToken),
              this.secret && (t.oauthTokenSecret = this.secret),
              t
            )
          }),
          (Ir.prototype.Da = function(t) {
            return (this.xb = ot(t)), this
          }),
          w(Tr, Ir),
          (Tr.prototype.ta = function(t) {
            return M(this.a, t) || this.a.push(t), this
          }),
          (Tr.prototype.Db = function() {
            return F(this.a)
          }),
          (Tr.prototype.credential = function(t, e) {
            if (!t && !e)
              throw new Ui(
                'argument-error',
                'credential failed: must provide the ID token and/or the access token.',
              )
            return new yr(
              this.providerId,
              { idToken: t || null, accessToken: e || null },
              this.providerId,
            )
          }),
          w(Er, Tr),
          Si(Er, 'PROVIDER_ID', 'facebook.com'),
          Si(Er, 'FACEBOOK_SIGN_IN_METHOD', 'facebook.com'),
          w(kr, Tr),
          Si(kr, 'PROVIDER_ID', 'github.com'),
          Si(kr, 'GITHUB_SIGN_IN_METHOD', 'github.com'),
          w(Nr, Tr),
          Si(Nr, 'PROVIDER_ID', 'google.com'),
          Si(Nr, 'GOOGLE_SIGN_IN_METHOD', 'google.com'),
          w(Or, Ir),
          Si(Or, 'PROVIDER_ID', 'twitter.com'),
          Si(Or, 'TWITTER_SIGN_IN_METHOD', 'twitter.com'),
          (Rr.prototype.ya = function(t) {
            return this.signInMethod == Cr.EMAIL_LINK_SIGN_IN_METHOD
              ? Ba(t, ka, { email: this.a, oobCode: this.b })
              : Ba(t, Ha, { email: this.a, password: this.b })
          }),
          (Rr.prototype.c = function(t, e) {
            return this.signInMethod == Cr.EMAIL_LINK_SIGN_IN_METHOD
              ? Ba(t, Sa, { idToken: e, email: this.a, oobCode: this.b })
              : Ba(t, Ma, { idToken: e, email: this.a, password: this.b })
          }),
          (Rr.prototype.f = function(t, e) {
            return br(this.ya(t), e)
          }),
          (Rr.prototype.D = function() {
            return {
              email: this.a,
              password: this.b,
              signInMethod: this.signInMethod,
            }
          }),
          Ni(Cr, { PROVIDER_ID: 'password' }),
          Ni(Cr, { EMAIL_LINK_SIGN_IN_METHOD: 'emailLink' }),
          Ni(Cr, { EMAIL_PASSWORD_SIGN_IN_METHOD: 'password' }),
          (xr.prototype.ya = function(t) {
            return t.Ta(Mr(this))
          }),
          (xr.prototype.c = function(t, e) {
            var n = Mr(this)
            return (n.idToken = e), Ba(t, Wa, n)
          }),
          (xr.prototype.f = function(t, e) {
            var n = Mr(this)
            return (n.operation = 'REAUTH'), br((t = Ba(t, Ga, n)), e)
          }),
          (xr.prototype.D = function() {
            var t = { providerId: 'phone' }
            return (
              this.a.Sa && (t.verificationId = this.a.Sa),
              this.a.Ra && (t.verificationCode = this.a.Ra),
              this.a.Fa && (t.temporaryProof = this.a.Fa),
              this.a.$ && (t.phoneNumber = this.a.$),
              t
            )
          }),
          (jr.prototype.Ta = function(t, e) {
            var n = this.a.b
            return Tt(e.verify()).then(function(i) {
              if (!r(i))
                throw new Ui(
                  'argument-error',
                  'An implementation of firebase.auth.ApplicationVerifier.prototype.verify() must return a firebase.Promise that resolves with a string.',
                )
              switch (e.type) {
                case 'recaptcha':
                  return (function(t, e) {
                    return Ba(t, La, e)
                  })(n, { phoneNumber: t, recaptchaToken: i }).then(
                    function(t) {
                      return 'function' == typeof e.reset && e.reset(), t
                    },
                    function(t) {
                      throw ('function' == typeof e.reset && e.reset(), t)
                    },
                  )
                default:
                  throw new Ui(
                    'argument-error',
                    'Only firebase.auth.ApplicationVerifiers with type="recaptcha" are currently supported.',
                  )
              }
            })
          }),
          Ni(jr, { PROVIDER_ID: 'phone' }),
          Ni(jr, { PHONE_SIGN_IN_METHOD: 'phone' }),
          (Kr.prototype.D = function() {
            return {
              type: this.b,
              eventId: this.c,
              urlResponse: this.f,
              sessionId: this.g,
              error: this.a && this.a.D(),
            }
          })
        var Wr = null
        function Gr(t) {
          var e = 'unauthorized-domain',
            n = void 0,
            i = ze(t)
          ;(t = i.b),
            'chrome-extension' == (i = i.c)
              ? (n = K(
                  'This chrome extension ID (chrome-extension://%s) is not authorized to run this operation. Add it to the OAuth redirect domains list in the Firebase console -> Auth section -> Sign in method tab.',
                  t,
                ))
              : 'http' == i || 'https' == i
                ? (n = K(
                    'This domain (%s) is not authorized to run this operation. Add it to the OAuth redirect domains list in the Firebase console -> Auth section -> Sign in method tab.',
                    t,
                  ))
                : (e = 'operation-not-supported-in-this-environment'),
            Ui.call(this, e, n)
        }
        function Br(t, e, n) {
          Ui.call(this, t, n),
            (t = e || {}).yb && Si(this, 'email', t.yb),
            t.$ && Si(this, 'phoneNumber', t.$),
            t.credential && Si(this, 'credential', t.credential)
        }
        function Xr(t) {
          if (t.code) {
            var e = t.code || ''
            0 == e.indexOf(Fi) && (e = e.substring(Fi.length))
            var n = { credential: Vr(t) }
            if (t.email) n.yb = t.email
            else if (t.phoneNumber) n.$ = t.phoneNumber
            else if (!n.credential) return new Ui(e, t.message || void 0)
            return new Br(e, n, t.message)
          }
          return null
        }
        ;(qr.prototype.subscribe = function(t) {
          var e = this
          this.a.push(t),
            this.b ||
              ((this.b = function(t) {
                for (var n = 0; n < e.a.length; n++) e.a[n](t)
              }),
              'function' == typeof (t = ai('universalLinks.subscribe', i)) &&
                t(null, this.b))
        }),
          (qr.prototype.unsubscribe = function(t) {
            U(this.a, function(e) {
              return e == t
            })
          }),
          w(Gr, Ui),
          w(Br, Ui),
          (Br.prototype.D = function() {
            var t = { code: this.code, message: this.message }
            this.email && (t.email = this.email),
              this.phoneNumber && (t.phoneNumber = this.phoneNumber)
            var e = this.credential && this.credential.D()
            return e && ct(t, e), t
          }),
          (Br.prototype.toJSON = function() {
            return this.D()
          })
        var zr,
          Jr = /^[+a-zA-Z0-9_.!#$%&'*\/=?^`{|}~-]+@([a-zA-Z0-9-]+\.)+[a-zA-Z0-9]{2,63}$/
        function Yr() {}
        function $r(t) {
          return t.c || (t.c = t.b())
        }
        function Zr() {}
        function Qr(t) {
          if (
            !t.f &&
            'undefined' == typeof XMLHttpRequest &&
            'undefined' != typeof ActiveXObject
          ) {
            for (
              var e = [
                  'MSXML2.XMLHTTP.6.0',
                  'MSXML2.XMLHTTP.3.0',
                  'MSXML2.XMLHTTP',
                  'Microsoft.XMLHTTP',
                ],
                n = 0;
              n < e.length;
              n++
            ) {
              var i = e[n]
              try {
                return new ActiveXObject(i), (t.f = i)
              } catch (t) {}
            }
            throw Error(
              'Could not create ActiveXObject. ActiveX might be disabled, or MSXML might not be installed',
            )
          }
          return t.f
        }
        function to() {}
        function eo() {
          ;(this.a = new XDomainRequest()),
            (this.readyState = 0),
            (this.onreadystatechange = null),
            (this.responseText = ''),
            (this.status = -1),
            (this.statusText = ''),
            (this.a.onload = g(this.dc, this)),
            (this.a.onerror = g(this.Eb, this)),
            (this.a.onprogress = g(this.ec, this)),
            (this.a.ontimeout = g(this.hc, this))
        }
        function no(t, e) {
          ;(t.readyState = e), t.onreadystatechange && t.onreadystatechange()
        }
        function io(t, e, n) {
          this.reset(t, e, n, void 0, void 0)
        }
        function ro(t) {
          ;(this.f = t), (this.b = this.c = this.a = null)
        }
        function oo(t, e) {
          ;(this.name = t), (this.value = e)
        }
        ;(Yr.prototype.c = null),
          w(Zr, Yr),
          (Zr.prototype.a = function() {
            var t = Qr(this)
            return t ? new ActiveXObject(t) : new XMLHttpRequest()
          }),
          (Zr.prototype.b = function() {
            var t = {}
            return Qr(this) && ((t[0] = !0), (t[1] = !0)), t
          }),
          (zr = new Zr()),
          w(to, Yr),
          (to.prototype.a = function() {
            var t = new XMLHttpRequest()
            if ('withCredentials' in t) return t
            if ('undefined' != typeof XDomainRequest) return new eo()
            throw Error('Unsupported browser')
          }),
          (to.prototype.b = function() {
            return {}
          }),
          ((t = eo.prototype).open = function(t, e, n) {
            if (null != n && !n)
              throw Error('Only async requests are supported.')
            this.a.open(t, e)
          }),
          (t.send = function(t) {
            if (t) {
              if ('string' != typeof t)
                throw Error('Only string data is supported')
              this.a.send(t)
            } else this.a.send()
          }),
          (t.abort = function() {
            this.a.abort()
          }),
          (t.setRequestHeader = function() {}),
          (t.getResponseHeader = function(t) {
            return 'content-type' == t.toLowerCase() ? this.a.contentType : ''
          }),
          (t.dc = function() {
            ;(this.status = 200),
              (this.responseText = this.a.responseText),
              no(this, 4)
          }),
          (t.Eb = function() {
            ;(this.status = 500), (this.responseText = ''), no(this, 4)
          }),
          (t.hc = function() {
            this.Eb()
          }),
          (t.ec = function() {
            ;(this.status = 200), no(this, 1)
          }),
          (t.getAllResponseHeaders = function() {
            return 'content-type: ' + this.a.contentType
          }),
          (io.prototype.a = null),
          (io.prototype.reset = function(t, e, n, i, r) {
            delete this.a
          }),
          (oo.prototype.toString = function() {
            return this.name
          })
        var ao = new oo('SEVERE', 1e3),
          so = new oo('WARNING', 900),
          uo = new oo('CONFIG', 700),
          co = new oo('FINE', 500)
        ro.prototype.log = function(t, e, n) {
          if (
            t.value >=
            (function t(e) {
              return e.c
                ? e.c
                : e.a
                  ? t(e.a)
                  : (k('Root logger has no level set.'), null)
            })(this).value
          )
            for (
              f(e) && (e = e()),
                t = new io(t, String(e), this.f),
                n && (t.a = n),
                n = this;
              n;

            )
              n = n.a
        }
        var ho = {},
          fo = null
        function lo(t) {
          var e
          if (
            (fo || ((fo = new ro('')), (ho[''] = fo), (fo.c = uo)),
            !(e = ho[t]))
          ) {
            e = new ro(t)
            var n = t.lastIndexOf('.'),
              i = t.substr(n + 1)
            ;(n = lo(t.substr(0, n))).b || (n.b = {}),
              (n.b[i] = e),
              (e.a = n),
              (ho[t] = e)
          }
          return e
        }
        function po(t, e) {
          t && t.log(co, e, void 0)
        }
        function vo(t) {
          this.f = t
        }
        function mo(t) {
          Pe.call(this),
            (this.i = t),
            (this.readyState = go),
            (this.status = 0),
            (this.responseText = this.statusText = ''),
            (this.onreadystatechange = null),
            (this.g = new Headers()),
            (this.b = null),
            (this.h = 'GET'),
            (this.c = ''),
            (this.a = !1),
            (this.f = lo('goog.net.FetchXmlHttp'))
        }
        w(vo, Yr),
          (vo.prototype.a = function() {
            return new mo(this.f)
          }),
          (vo.prototype.b = (function(t) {
            return function() {
              return t
            }
          })({})),
          w(mo, Pe)
        var go = 0
        function bo(t) {
          t.onreadystatechange && t.onreadystatechange.call(t)
        }
        function yo(t) {
          Pe.call(this),
            (this.headers = new Ue()),
            (this.C = t || null),
            (this.c = !1),
            (this.w = this.a = null),
            (this.h = this.N = this.m = ''),
            (this.f = this.I = this.i = this.G = !1),
            (this.g = 0),
            (this.u = null),
            (this.o = wo),
            (this.v = this.O = !1)
        }
        ;((t = mo.prototype).open = function(t, e) {
          if (this.readyState != go)
            throw (this.abort(), Error('Error reopening a connection'))
          ;(this.h = t), (this.c = e), (this.readyState = 1), bo(this)
        }),
          (t.send = function(t) {
            if (1 != this.readyState)
              throw (this.abort(), Error('need to call open() first. '))
            this.a = !0
            var e = {
              headers: this.g,
              method: this.h,
              credentials: void 0,
              cache: void 0,
            }
            t && (e.body = t),
              this.i
                .fetch(new Request(this.c, e))
                .then(this.gc.bind(this), this.Fb.bind(this))
          }),
          (t.abort = function() {
            ;(this.responseText = ''),
              (this.g = new Headers()),
              (this.status = 0),
              1 <= this.readyState &&
                this.a &&
                4 != this.readyState &&
                ((this.readyState = 4), (this.a = !1), bo(this)),
              (this.readyState = go)
          }),
          (t.gc = function(t) {
            this.a &&
              (this.b ||
                ((this.b = t.headers), (this.readyState = 2), bo(this)),
              this.a &&
                ((this.readyState = 3),
                bo(this),
                this.a &&
                  t.text().then(this.fc.bind(this, t), this.Fb.bind(this))))
          }),
          (t.fc = function(t, e) {
            this.a &&
              ((this.status = t.status),
              (this.statusText = t.statusText),
              (this.responseText = e),
              (this.readyState = 4),
              bo(this))
          }),
          (t.Fb = function(t) {
            var e = this.f
            e &&
              e.log(
                so,
                'Failed to fetch url ' + this.c,
                t instanceof Error ? t : Error(t),
              ),
              this.a && ((this.readyState = 4), bo(this))
          }),
          (t.setRequestHeader = function(t, e) {
            this.g.append(t, e)
          }),
          (t.getResponseHeader = function(t) {
            return this.b
              ? this.b.get(t.toLowerCase()) || ''
              : ((t = this.f) &&
                  t.log(
                    so,
                    'Attempting to get response header but no headers have been received for url: ' +
                      this.c,
                    void 0,
                  ),
                '')
          }),
          (t.getAllResponseHeaders = function() {
            if (!this.b) {
              var t = this.f
              return (
                t &&
                  t.log(
                    so,
                    'Attempting to get all response headers but no headers have been received for url: ' +
                      this.c,
                    void 0,
                  ),
                ''
              )
            }
            t = []
            for (var e = this.b.entries(), n = e.next(); !n.done; )
              (n = n.value), t.push(n[0] + ': ' + n[1]), (n = e.next())
            return t.join('\r\n')
          }),
          w(yo, Pe)
        var wo = ''
        yo.prototype.b = lo('goog.net.XhrIo')
        var Io = /^https?$/i,
          To = ['POST', 'PUT']
        function Eo(t, e, n, o, a) {
          if (t.a)
            throw Error(
              '[goog.net.XhrIo] Object is active with another request=' +
                t.m +
                '; newUri=' +
                e,
            )
          ;(n = n ? n.toUpperCase() : 'GET'),
            (t.m = e),
            (t.h = ''),
            (t.N = n),
            (t.G = !1),
            (t.c = !0),
            (t.a = t.C ? t.C.a() : zr.a()),
            (t.w = t.C ? $r(t.C) : $r(zr)),
            (t.a.onreadystatechange = g(t.Ib, t))
          try {
            po(t.b, Co(t, 'Opening Xhr')),
              (t.I = !0),
              t.a.open(n, String(e), !0),
              (t.I = !1)
          } catch (e) {
            return (
              po(t.b, Co(t, 'Error opening Xhr: ' + e.message)), void ko(t, e)
            )
          }
          e = o || ''
          var s = new Ue(t.headers)
          a &&
            (function(t, e) {
              if (t.forEach && 'function' == typeof t.forEach)
                t.forEach(e, void 0)
              else if (h(t) || r(t)) D(t, e, void 0)
              else
                for (var n = je(t), i = Me(t), o = i.length, a = 0; a < o; a++)
                  e.call(void 0, i[a], n && n[a], t)
            })(a, function(t, e) {
              s.set(e, t)
            }),
            (a = (function(t) {
              t: {
                for (
                  var e = Ao, n = t.length, i = r(t) ? t.split('') : t, o = 0;
                  o < n;
                  o++
                )
                  if (o in i && e.call(void 0, i[o], o, t)) {
                    e = o
                    break t
                  }
                e = -1
              }
              return 0 > e ? null : r(t) ? t.charAt(e) : t[e]
            })(s.U())),
            (o = i.FormData && e instanceof i.FormData),
            !M(To, n) ||
              a ||
              o ||
              s.set(
                'Content-Type',
                'application/x-www-form-urlencoded;charset=utf-8',
              ),
            s.forEach(function(t, e) {
              this.a.setRequestHeader(e, t)
            }, t),
            t.o && (t.a.responseType = t.o),
            'withCredentials' in t.a &&
              t.a.withCredentials !== t.O &&
              (t.a.withCredentials = t.O)
          try {
            Oo(t),
              0 < t.g &&
                ((t.v = (function(t) {
                  return (
                    Wt &&
                    ee(9) &&
                    'number' == typeof t.timeout &&
                    void 0 !== t.ontimeout
                  )
                })(t.a)),
                po(
                  t.b,
                  Co(
                    t,
                    'Will abort after ' + t.g + 'ms if incomplete, xhr2 ' + t.v,
                  ),
                ),
                t.v
                  ? ((t.a.timeout = t.g), (t.a.ontimeout = g(t.Ga, t)))
                  : (t.u = Le(t.Ga, t.g, t))),
              po(t.b, Co(t, 'Sending request')),
              (t.i = !0),
              t.a.send(e),
              (t.i = !1)
          } catch (e) {
            po(t.b, Co(t, 'Send error: ' + e.message)), ko(t, e)
          }
        }
        function Ao(t) {
          return 'content-type' == t.toLowerCase()
        }
        function ko(t, e) {
          ;(t.c = !1),
            t.a && ((t.f = !0), t.a.abort(), (t.f = !1)),
            (t.h = e),
            So(t),
            _o(t)
        }
        function So(t) {
          t.G ||
            ((t.G = !0), t.dispatchEvent('complete'), t.dispatchEvent('error'))
        }
        function No(t) {
          if (t.c && void 0 !== n)
            if (t.w[1] && 4 == Po(t) && 2 == Ro(t))
              po(t.b, Co(t, 'Local request error detected and ignored'))
            else if (t.i && 4 == Po(t)) Le(t.Ib, 0, t)
            else if ((t.dispatchEvent('readystatechange'), 4 == Po(t))) {
              po(t.b, Co(t, 'Request complete')), (t.c = !1)
              try {
                var e,
                  r = Ro(t)
                t: switch (r) {
                  case 200:
                  case 201:
                  case 202:
                  case 204:
                  case 206:
                  case 304:
                  case 1223:
                    var o = !0
                    break t
                  default:
                    o = !1
                }
                if (!(e = o)) {
                  var a
                  if ((a = 0 === r)) {
                    var s = String(t.m).match(Ke)[1] || null
                    if (!s && i.self && i.self.location) {
                      var u = i.self.location.protocol
                      s = u.substr(0, u.length - 1)
                    }
                    a = !Io.test(s ? s.toLowerCase() : '')
                  }
                  e = a
                }
                if (e) t.dispatchEvent('complete'), t.dispatchEvent('success')
                else {
                  try {
                    var c = 2 < Po(t) ? t.a.statusText : ''
                  } catch (e) {
                    po(t.b, 'Can not get status: ' + e.message), (c = '')
                  }
                  ;(t.h = c + ' [' + Ro(t) + ']'), So(t)
                }
              } finally {
                _o(t)
              }
            }
        }
        function _o(t, e) {
          if (t.a) {
            Oo(t)
            var n = t.a,
              i = t.w[0] ? a : null
            ;(t.a = null), (t.w = null), e || t.dispatchEvent('ready')
            try {
              n.onreadystatechange = i
            } catch (e) {
              ;(t = t.b) &&
                t.log(
                  ao,
                  'Problem encountered resetting onreadystatechange: ' +
                    e.message,
                  void 0,
                )
            }
          }
        }
        function Oo(t) {
          t.a && t.v && (t.a.ontimeout = null),
            t.u && (i.clearTimeout(t.u), (t.u = null))
        }
        function Po(t) {
          return t.a ? t.a.readyState : 0
        }
        function Ro(t) {
          try {
            return 2 < Po(t) ? t.a.status : -1
          } catch (t) {
            return -1
          }
        }
        function Co(t, e) {
          return e + ' [' + t.N + ' ' + t.m + ' ' + Ro(t) + ']'
        }
        function Do(t, e) {
          ;(this.g = []),
            (this.v = t),
            (this.u = e || null),
            (this.f = this.a = !1),
            (this.c = void 0),
            (this.m = this.w = this.i = !1),
            (this.h = 0),
            (this.b = null),
            (this.l = 0)
        }
        function Lo(t, e, n) {
          ;(t.a = !0), (t.c = n), (t.f = !e), Uo(t)
        }
        function xo(t) {
          if (t.a) {
            if (!t.m) throw new Vo(t)
            t.m = !1
          }
        }
        function Mo(t, e, n, i) {
          t.g.push([e, n, i]), t.a && Uo(t)
        }
        function jo(t) {
          return x(t.g, function(t) {
            return f(t[1])
          })
        }
        function Uo(t) {
          if (t.h && t.a && jo(t)) {
            var e = t.h,
              n = Ho[e]
            n && (i.clearTimeout(n.a), delete Ho[e]), (t.h = 0)
          }
          t.b && (t.b.l--, delete t.b), (e = t.c)
          for (var r = (n = !1); t.g.length && !t.i; ) {
            var o = t.g.shift(),
              a = o[0],
              s = o[1]
            if (((o = o[2]), (a = t.f ? s : a)))
              try {
                var u = a.call(o || t.u, e)
                void 0 !== u &&
                  ((t.f = t.f && (u == e || u instanceof Error)),
                  (t.c = e = u)),
                  (T(e) ||
                    ('function' == typeof i.Promise &&
                      e instanceof i.Promise)) &&
                    ((r = !0), (t.i = !0))
              } catch (i) {
                ;(e = i), (t.f = !0), jo(t) || (n = !0)
              }
          }
          ;(t.c = e),
            r &&
              ((u = g(t.o, t, !0)),
              (r = g(t.o, t, !1)),
              e instanceof Do ? (Mo(e, u, r), (e.w = !0)) : e.then(u, r)),
            n && ((e = new Ko(e)), (Ho[e.a] = e), (t.h = e.a))
        }
        function Vo() {
          E.call(this)
        }
        function Fo() {
          E.call(this)
        }
        function Ko(t) {
          ;(this.a = i.setTimeout(g(this.c, this), 0)), (this.b = t)
        }
        ;((t = yo.prototype).Ga = function() {
          void 0 !== n &&
            this.a &&
            ((this.h = 'Timed out after ' + this.g + 'ms, aborting'),
            po(this.b, Co(this, this.h)),
            this.dispatchEvent('timeout'),
            this.abort(8))
        }),
          (t.abort = function() {
            this.a &&
              this.c &&
              (po(this.b, Co(this, 'Aborting')),
              (this.c = !1),
              (this.f = !0),
              this.a.abort(),
              (this.f = !1),
              this.dispatchEvent('complete'),
              this.dispatchEvent('abort'),
              _o(this))
          }),
          (t.ua = function() {
            this.a &&
              (this.c &&
                ((this.c = !1), (this.f = !0), this.a.abort(), (this.f = !1)),
              _o(this, !0)),
              yo.nb.ua.call(this)
          }),
          (t.Ib = function() {
            this.pa || (this.I || this.i || this.f ? No(this) : this.vc())
          }),
          (t.vc = function() {
            No(this)
          }),
          (t.getResponse = function() {
            try {
              if (!this.a) return null
              if ('response' in this.a) return this.a.response
              switch (this.o) {
                case wo:
                case 'text':
                  return this.a.responseText
                case 'arraybuffer':
                  if ('mozResponseArrayBuffer' in this.a)
                    return this.a.mozResponseArrayBuffer
              }
              var t = this.b
              return (
                t &&
                  t.log(
                    ao,
                    'Response type ' +
                      this.o +
                      ' is not supported on this browser',
                    void 0,
                  ),
                null
              )
            } catch (t) {
              return po(this.b, 'Can not get response: ' + t.message), null
            }
          }),
          (Do.prototype.cancel = function(t) {
            if (this.a) this.c instanceof Do && this.c.cancel()
            else {
              if (this.b) {
                var e = this.b
                delete this.b, t ? e.cancel(t) : (e.l--, 0 >= e.l && e.cancel())
              }
              this.v ? this.v.call(this.u, this) : (this.m = !0),
                this.a || ((t = new Fo(this)), xo(this), Lo(this, !1, t))
            }
          }),
          (Do.prototype.o = function(t, e) {
            ;(this.i = !1), Lo(this, t, e)
          }),
          (Do.prototype.C = function() {
            xo(this), Lo(this, !0, null)
          }),
          (Do.prototype.then = function(t, e, n) {
            var i,
              r,
              o = new vt(function(t, e) {
                ;(i = t), (r = e)
              })
            return (
              Mo(this, i, function(t) {
                t instanceof Fo ? o.cancel() : r(t)
              }),
              o.then(t, e, n)
            )
          }),
          I(Do),
          w(Vo, E),
          (Vo.prototype.message = 'Deferred has already fired'),
          (Vo.prototype.name = 'AlreadyCalledError'),
          w(Fo, E),
          (Fo.prototype.message = 'Deferred was canceled'),
          (Fo.prototype.name = 'CanceledError'),
          (Ko.prototype.c = function() {
            throw (delete Ho[this.a], this.b)
          })
        var Ho = {}
        function qo(t) {
          var e = {},
            n = e.document || document,
            i = bn(t),
            r = document.createElement('SCRIPT'),
            o = { Kb: r, Ga: void 0 },
            a = new Do(Wo, o),
            s = null,
            u = null != e.timeout ? e.timeout : 5e3
          return (
            0 < u &&
              ((s = window.setTimeout(function() {
                Go(r, !0)
                var t = new zo(Xo, 'Timeout reached for loading script ' + i)
                xo(a), Lo(a, !1, t)
              }, u)),
              (o.Ga = s)),
            (r.onload = r.onreadystatechange = function() {
              ;(r.readyState &&
                'loaded' != r.readyState &&
                'complete' != r.readyState) ||
                (Go(r, e.ed || !1, s), a.C())
            }),
            (r.onerror = function() {
              Go(r, !0, s)
              var t = new zo(Bo, 'Error while loading script ' + i)
              xo(a), Lo(a, !1, t)
            }),
            ct((o = e.attributes || {}), {
              type: 'text/javascript',
              charset: 'UTF-8',
            }),
            Dn(r, o),
            (r.src = bn(t)),
            (function(t) {
              var e
              return (e = (t || document).getElementsByTagName('HEAD')) &&
                0 != e.length
                ? e[0]
                : t.documentElement
            })(n).appendChild(r),
            a
          )
        }
        function Wo() {
          if (this && this.Kb) {
            var t = this.Kb
            t && 'SCRIPT' == t.tagName && Go(t, !0, this.Ga)
          }
        }
        function Go(t, e, n) {
          null != n && i.clearTimeout(n),
            (t.onload = a),
            (t.onerror = a),
            (t.onreadystatechange = a),
            e &&
              window.setTimeout(function() {
                t && t.parentNode && t.parentNode.removeChild(t)
              }, 0)
        }
        var Bo = 0,
          Xo = 1
        function zo(t, e) {
          var n = 'Jsloader error (code #' + t + ')'
          e && (n += ': ' + e), E.call(this, n), (this.code = t)
        }
        function Jo(t) {
          this.f = t
        }
        function Yo(t, n, r) {
          if (
            ((this.b = t),
            (t = n || {}),
            (this.i =
              t.secureTokenEndpoint ||
              'https://securetoken.googleapis.com/v1/token'),
            (this.l = t.secureTokenTimeout || Qo),
            (this.f = ot(t.secureTokenHeaders || ta)),
            (this.g =
              t.firebaseEndpoint ||
              'https://www.googleapis.com/identitytoolkit/v3/relyingparty/'),
            (this.h = t.firebaseTimeout || ea),
            (this.a = ot(t.firebaseHeaders || na)),
            r &&
              ((this.a['X-Client-Version'] = r),
              (this.f['X-Client-Version'] = r)),
            (r = 'Node' == Zn()),
            !(r =
              i.XMLHttpRequest ||
              (r && e.INTERNAL.node && e.INTERNAL.node.XMLHttpRequest)) &&
              !$n())
          )
            throw new Ui(
              'internal-error',
              'The XMLHttpRequest compatibility library was not found.',
            )
          ;(this.c = void 0),
            $n()
              ? (this.c = new vo(self))
              : Qn()
                ? (this.c = new Jo(r))
                : (this.c = new to())
        }
        w(zo, E),
          w(Jo, Yr),
          (Jo.prototype.a = function() {
            return new this.f()
          }),
          (Jo.prototype.b = function() {
            return {}
          })
        var $o,
          Zo = 'idToken',
          Qo = new bi(3e4, 6e4),
          ta = { 'Content-Type': 'application/x-www-form-urlencoded' },
          ea = new bi(3e4, 6e4),
          na = { 'Content-Type': 'application/json' }
        function ia(t, e) {
          e ? (t.a['X-Firebase-Locale'] = e) : delete t.a['X-Firebase-Locale']
        }
        function ra(t, e) {
          e
            ? ((t.a['X-Client-Version'] = e), (t.f['X-Client-Version'] = e))
            : (delete t.a['X-Client-Version'], delete t.f['X-Client-Version'])
        }
        function oa(t, e, n, r, o, a, s) {
          ;(function() {
            var t = oi()
            return !(
              ((t =
                ni(t) != ei
                  ? null
                  : (t = t.match(/\sChrome\/(\d+)/i)) && 2 == t.length
                    ? parseInt(t[1], 10)
                    : null) &&
                30 > t) ||
              (Wt && Qt && !(9 < Qt))
            )
          })() || $n()
            ? (t = g(t.o, t))
            : ($o ||
                ($o = new vt(function(t, e) {
                  !(function(t, e) {
                    if (((window.gapi || {}).client || {}).request) t()
                    else {
                      i[sa] = function() {
                        ;((window.gapi || {}).client || {}).request
                          ? t()
                          : e(Error('CORS_UNSUPPORTED'))
                      }
                      var n = yn(aa, { onload: sa })
                      !(function(t, e) {
                        Mo(t, null, e, void 0)
                      })(qo(n), function() {
                        e(Error('CORS_UNSUPPORTED'))
                      })
                    }
                  })(t, e)
                })),
              (t = g(t.m, t))),
            t(e, n, r, o, a, s)
        }
        Yo.prototype.o = function(t, e, n, r, o, a) {
          if (
            $n() &&
            (void 0 === i.fetch || void 0 === i.Headers || void 0 === i.Request)
          )
            throw new Ui(
              'operation-not-supported-in-this-environment',
              'fetch, Headers and Request native APIs or equivalent Polyfills must be available to support HTTP requests from a Worker environment.',
            )
          var s = new yo(this.c)
          if (a) {
            s.g = Math.max(0, a)
            var u = setTimeout(function() {
              s.dispatchEvent('timeout')
            }, a)
          }
          Re(s, 'complete', function() {
            u && clearTimeout(u)
            var t = null
            try {
              t =
                JSON.parse(
                  (function(t) {
                    try {
                      return t.a ? t.a.responseText : ''
                    } catch (e) {
                      return (
                        po(t.b, 'Can not get responseText: ' + e.message), ''
                      )
                    }
                  })(this),
                ) || null
            } catch (e) {
              t = null
            }
            e && e(t)
          }),
            Ce(s, 'ready', function() {
              u && clearTimeout(u), Vt(this)
            }),
            Ce(s, 'timeout', function() {
              u && clearTimeout(u), Vt(this), e && e(null)
            }),
            Eo(s, t, n, r, o)
        }
        var aa = mn('https://apis.google.com/js/client.js?onload=%{onload}'),
          sa = '__fcb' + Math.floor(1e6 * Math.random()).toString()
        function ua(t) {
          if (!Jr.test(t.email)) throw new Ui('invalid-email')
        }
        function ca(t) {
          'email' in t && ua(t)
        }
        function ha(t) {
          if (!t[Zo]) throw new Ui('internal-error')
        }
        function fa(t) {
          if (t.phoneNumber || t.temporaryProof) {
            if (!t.phoneNumber || !t.temporaryProof)
              throw new Ui('internal-error')
          } else {
            if (!t.sessionInfo) throw new Ui('missing-verification-id')
            if (!t.code) throw new Ui('missing-verification-code')
          }
        }
        ;(Yo.prototype.m = function(t, e, n, i, r) {
          var o = this
          $o.then(function() {
            window.gapi.client.setApiKey(o.b)
            var a = window.gapi.auth.getToken()
            window.gapi.auth.setToken(null),
              window.gapi.client.request({
                path: t,
                method: n,
                body: i,
                headers: r,
                authType: 'none',
                callback: function(t) {
                  window.gapi.auth.setToken(a), e && e(t)
                },
              })
          }).s(function(t) {
            e &&
              e({ error: { message: (t && t.message) || 'CORS_UNSUPPORTED' } })
          })
        }),
          (Yo.prototype.Pa = function() {
            return Ba(this, ja, {})
          }),
          (Yo.prototype.ob = function(t, e) {
            return Ba(this, xa, { idToken: t, email: e })
          }),
          (Yo.prototype.pb = function(t, e) {
            return Ba(this, Ma, { idToken: t, password: e })
          })
        var la = { displayName: 'DISPLAY_NAME', photoUrl: 'PHOTO_URL' }
        function da(t) {
          if (!t.requestUri || (!t.sessionId && !t.postBody))
            throw new Ui('internal-error')
        }
        function pa(t) {
          var e = null
          if (
            (t.needConfirmation
              ? ((t.code = 'account-exists-with-different-credential'),
                (e = Xr(t)))
              : 'FEDERATED_USER_ID_ALREADY_LINKED' == t.errorMessage
                ? ((t.code = 'credential-already-in-use'), (e = Xr(t)))
                : 'EMAIL_EXISTS' == t.errorMessage
                  ? ((t.code = 'email-already-in-use'), (e = Xr(t)))
                  : t.errorMessage && (e = Xa(t.errorMessage)),
            e)
          )
            throw e
          if (!t[Zo]) throw new Ui('internal-error')
        }
        function va(t, e) {
          return (e.returnIdpCredential = !0), Ba(t, Ua, e)
        }
        function ma(t, e) {
          return (e.returnIdpCredential = !0), Ba(t, Fa, e)
        }
        function ga(t, e) {
          return (e.returnIdpCredential = !0), (e.autoCreate = !1), Ba(t, Va, e)
        }
        function ba(t) {
          if (!t.oobCode) throw new Ui('invalid-action-code')
        }
        ;((t = Yo.prototype).qb = function(t, e) {
          var n = { idToken: t },
            i = []
          return (
            it(la, function(t, r) {
              var o = e[r]
              null === o ? i.push(t) : r in e && (n[r] = o)
            }),
            i.length && (n.deleteAttribute = i),
            Ba(this, xa, n)
          )
        }),
          (t.jb = function(t, e) {
            return (
              ct((t = { requestType: 'PASSWORD_RESET', email: t }), e),
              Ba(this, Pa, t)
            )
          }),
          (t.kb = function(t, e) {
            return (
              ct((t = { requestType: 'EMAIL_SIGNIN', email: t }), e),
              Ba(this, _a, t)
            )
          }),
          (t.ib = function(t, e) {
            return (
              ct((t = { requestType: 'VERIFY_EMAIL', idToken: t }), e),
              Ba(this, Oa, t)
            )
          }),
          (t.Ta = function(t) {
            return Ba(this, qa, t)
          }),
          (t.Ya = function(t, e) {
            return Ba(this, Da, { oobCode: t, newPassword: e })
          }),
          (t.Ja = function(t) {
            return Ba(this, wa, { oobCode: t })
          }),
          (t.Va = function(t) {
            return Ba(this, ya, { oobCode: t })
          })
        var ya = { endpoint: 'setAccountInfo', B: ba, da: 'email' },
          wa = {
            endpoint: 'resetPassword',
            B: ba,
            J: function(t) {
              var e = t.requestType
              if (!e || (!t.email && 'EMAIL_SIGNIN' != e))
                throw new Ui('internal-error')
            },
          },
          Ia = {
            endpoint: 'signupNewUser',
            B: function(t) {
              if ((ua(t), !t.password)) throw new Ui('weak-password')
            },
            J: ha,
            R: !0,
          },
          Ta = { endpoint: 'createAuthUri' },
          Ea = { endpoint: 'deleteAccount', T: ['idToken'] },
          Aa = {
            endpoint: 'setAccountInfo',
            T: ['idToken', 'deleteProvider'],
            B: function(t) {
              if (!c(t.deleteProvider)) throw new Ui('internal-error')
            },
          },
          ka = {
            endpoint: 'emailLinkSignin',
            T: ['email', 'oobCode'],
            B: ua,
            J: ha,
            R: !0,
          },
          Sa = {
            endpoint: 'emailLinkSignin',
            T: ['idToken', 'email', 'oobCode'],
            B: ua,
            J: ha,
            R: !0,
          },
          Na = { endpoint: 'getAccountInfo' },
          _a = {
            endpoint: 'getOobConfirmationCode',
            T: ['requestType'],
            B: function(t) {
              if ('EMAIL_SIGNIN' != t.requestType)
                throw new Ui('internal-error')
              ua(t)
            },
            da: 'email',
          },
          Oa = {
            endpoint: 'getOobConfirmationCode',
            T: ['idToken', 'requestType'],
            B: function(t) {
              if ('VERIFY_EMAIL' != t.requestType)
                throw new Ui('internal-error')
            },
            da: 'email',
          },
          Pa = {
            endpoint: 'getOobConfirmationCode',
            T: ['requestType'],
            B: function(t) {
              if ('PASSWORD_RESET' != t.requestType)
                throw new Ui('internal-error')
              ua(t)
            },
            da: 'email',
          },
          Ra = { tb: !0, endpoint: 'getProjectConfig', Hb: 'GET' },
          Ca = {
            tb: !0,
            endpoint: 'getRecaptchaParam',
            Hb: 'GET',
            J: function(t) {
              if (!t.recaptchaSiteKey) throw new Ui('internal-error')
            },
          },
          Da = { endpoint: 'resetPassword', B: ba, da: 'email' },
          La = {
            endpoint: 'sendVerificationCode',
            T: ['phoneNumber', 'recaptchaToken'],
            da: 'sessionInfo',
          },
          xa = { endpoint: 'setAccountInfo', T: ['idToken'], B: ca, R: !0 },
          Ma = {
            endpoint: 'setAccountInfo',
            T: ['idToken'],
            B: function(t) {
              if ((ca(t), !t.password)) throw new Ui('weak-password')
            },
            J: ha,
            R: !0,
          },
          ja = { endpoint: 'signupNewUser', J: ha, R: !0 },
          Ua = { endpoint: 'verifyAssertion', B: da, J: pa, R: !0 },
          Va = {
            endpoint: 'verifyAssertion',
            B: da,
            J: function(t) {
              if (t.errorMessage && 'USER_NOT_FOUND' == t.errorMessage)
                throw new Ui('user-not-found')
              if (t.errorMessage) throw Xa(t.errorMessage)
              if (!t[Zo]) throw new Ui('internal-error')
            },
            R: !0,
          },
          Fa = {
            endpoint: 'verifyAssertion',
            B: function(t) {
              if ((da(t), !t.idToken)) throw new Ui('internal-error')
            },
            J: pa,
            R: !0,
          },
          Ka = {
            endpoint: 'verifyCustomToken',
            B: function(t) {
              if (!t.token) throw new Ui('invalid-custom-token')
            },
            J: ha,
            R: !0,
          },
          Ha = {
            endpoint: 'verifyPassword',
            B: function(t) {
              if ((ua(t), !t.password)) throw new Ui('wrong-password')
            },
            J: ha,
            R: !0,
          },
          qa = { endpoint: 'verifyPhoneNumber', B: fa, J: ha },
          Wa = {
            endpoint: 'verifyPhoneNumber',
            B: function(t) {
              if (!t.idToken) throw new Ui('internal-error')
              fa(t)
            },
            J: function(t) {
              if (t.temporaryProof)
                throw ((t.code = 'credential-already-in-use'), Xr(t))
              ha(t)
            },
          },
          Ga = {
            Wb: { USER_NOT_FOUND: 'user-not-found' },
            endpoint: 'verifyPhoneNumber',
            B: fa,
            J: ha,
          }
        function Ba(t, e, n) {
          if (
            !(function(t, e) {
              if (!e || !e.length) return !0
              if (!t) return !1
              for (var n = 0; n < e.length; n++) {
                var i = t[e[n]]
                if (void 0 === i || null === i || '' === i) return !1
              }
              return !0
            })(n, e.T)
          )
            return Et(new Ui('internal-error'))
          var i,
            r = e.Hb || 'POST'
          return Tt(n)
            .then(e.B)
            .then(function() {
              return (
                e.R && (n.returnSecureToken = !0),
                (function(t, e, n, i, r, o) {
                  var a = ze(t.g + e)
                  Be(a, 'key', t.b), o && Be(a, 'cb', y().toString())
                  var s = 'GET' == n
                  if (s) for (var u in i) i.hasOwnProperty(u) && Be(a, u, i[u])
                  return new vt(function(e, o) {
                    oa(
                      t,
                      a.toString(),
                      function(t) {
                        t
                          ? t.error
                            ? o(za(t, r || {}))
                            : e(t)
                          : o(new Ui('network-request-failed'))
                      },
                      n,
                      s ? void 0 : jn(di(i)),
                      t.a,
                      t.h.get(),
                    )
                  })
                })(t, e.endpoint, r, n, e.Wb, e.tb || !1)
              )
            })
            .then(function(t) {
              return (i = t)
            })
            .then(e.J)
            .then(function() {
              if (!e.da) return i
              if (!(e.da in i)) throw new Ui('internal-error')
              return i[e.da]
            })
        }
        function Xa(t) {
          return za({
            error: { errors: [{ message: t }], code: 400, message: t },
          })
        }
        function za(t, e) {
          var n =
              ((t.error && t.error.errors && t.error.errors[0]) || {}).reason ||
              '',
            i = {
              keyInvalid: 'invalid-api-key',
              ipRefererBlocked: 'app-not-authorized',
            }
          if ((n = i[n] ? new Ui(i[n]) : null)) return n
          for (var r in ((n = (t.error && t.error.message) || ''),
          ct(
            (i = {
              INVALID_CUSTOM_TOKEN: 'invalid-custom-token',
              CREDENTIAL_MISMATCH: 'custom-token-mismatch',
              MISSING_CUSTOM_TOKEN: 'internal-error',
              INVALID_IDENTIFIER: 'invalid-email',
              MISSING_CONTINUE_URI: 'internal-error',
              INVALID_EMAIL: 'invalid-email',
              INVALID_PASSWORD: 'wrong-password',
              USER_DISABLED: 'user-disabled',
              MISSING_PASSWORD: 'internal-error',
              EMAIL_EXISTS: 'email-already-in-use',
              PASSWORD_LOGIN_DISABLED: 'operation-not-allowed',
              INVALID_IDP_RESPONSE: 'invalid-credential',
              FEDERATED_USER_ID_ALREADY_LINKED: 'credential-already-in-use',
              INVALID_MESSAGE_PAYLOAD: 'invalid-message-payload',
              INVALID_RECIPIENT_EMAIL: 'invalid-recipient-email',
              INVALID_SENDER: 'invalid-sender',
              EMAIL_NOT_FOUND: 'user-not-found',
              RESET_PASSWORD_EXCEED_LIMIT: 'too-many-requests',
              EXPIRED_OOB_CODE: 'expired-action-code',
              INVALID_OOB_CODE: 'invalid-action-code',
              MISSING_OOB_CODE: 'internal-error',
              INVALID_PROVIDER_ID: 'invalid-provider-id',
              CREDENTIAL_TOO_OLD_LOGIN_AGAIN: 'requires-recent-login',
              INVALID_ID_TOKEN: 'invalid-user-token',
              TOKEN_EXPIRED: 'user-token-expired',
              USER_NOT_FOUND: 'user-token-expired',
              CORS_UNSUPPORTED: 'cors-unsupported',
              DYNAMIC_LINK_NOT_ACTIVATED: 'dynamic-link-not-activated',
              INVALID_APP_ID: 'invalid-app-id',
              TOO_MANY_ATTEMPTS_TRY_LATER: 'too-many-requests',
              WEAK_PASSWORD: 'weak-password',
              OPERATION_NOT_ALLOWED: 'operation-not-allowed',
              USER_CANCELLED: 'user-cancelled',
              CAPTCHA_CHECK_FAILED: 'captcha-check-failed',
              INVALID_APP_CREDENTIAL: 'invalid-app-credential',
              INVALID_CODE: 'invalid-verification-code',
              INVALID_PHONE_NUMBER: 'invalid-phone-number',
              INVALID_SESSION_INFO: 'invalid-verification-id',
              INVALID_TEMPORARY_PROOF: 'invalid-credential',
              MISSING_APP_CREDENTIAL: 'missing-app-credential',
              MISSING_CODE: 'missing-verification-code',
              MISSING_PHONE_NUMBER: 'missing-phone-number',
              MISSING_SESSION_INFO: 'missing-verification-id',
              QUOTA_EXCEEDED: 'quota-exceeded',
              SESSION_EXPIRED: 'code-expired',
              REJECTED_CREDENTIAL: 'rejected-credential',
              INVALID_CONTINUE_URI: 'invalid-continue-uri',
              MISSING_ANDROID_PACKAGE_NAME: 'missing-android-pkg-name',
              MISSING_IOS_BUNDLE_ID: 'missing-ios-bundle-id',
              UNAUTHORIZED_DOMAIN: 'unauthorized-continue-uri',
              INVALID_OAUTH_CLIENT_ID: 'invalid-oauth-client-id',
              INVALID_CERT_HASH: 'invalid-cert-hash',
            }),
            e || {},
          ),
          (e =
            (e = n.match(/^[^\s]+\s*:\s*(.*)$/)) && 1 < e.length
              ? e[1]
              : void 0),
          i))
            if (0 === n.indexOf(r)) return new Ui(i[r], e)
          return !e && t && (e = li(t)), new Ui('internal-error', e)
        }
        var Ja,
          Ya = {
            Zc: {
              $a: 'https://www.googleapis.com/identitytoolkit/v3/relyingparty/',
              hb: 'https://securetoken.googleapis.com/v1/token',
              id: 'p',
            },
            ad: {
              $a:
                'https://staging-www.sandbox.googleapis.com/identitytoolkit/v3/relyingparty/',
              hb: 'https://staging-securetoken.sandbox.googleapis.com/v1/token',
              id: 's',
            },
            bd: {
              $a:
                'https://www-googleapis-test.sandbox.google.com/identitytoolkit/v3/relyingparty/',
              hb: 'https://test-securetoken.sandbox.googleapis.com/v1/token',
              id: 't',
            },
          }
        function $a(t) {
          for (var e in Ya)
            if (Ya[e].id === t)
              return {
                firebaseEndpoint: (t = Ya[e]).$a,
                secureTokenEndpoint: t.hb,
              }
          return null
        }
        function Za(t) {
          ;(this.b = t),
            (this.a = null),
            (this.eb = (function(t) {
              return (
                ns ||
                (ns = new vt(function(t, e) {
                  function n() {
                    gi(),
                      ai('gapi.load')('gapi.iframes', {
                        callback: t,
                        ontimeout: function() {
                          gi(), e(Error('Network Error'))
                        },
                        timeout: ts.get(),
                      })
                  }
                  if (ai('gapi.iframes.Iframe')) t()
                  else if (ai('gapi.load')) n()
                  else {
                    var r =
                      '__iframefcb' + Math.floor(1e6 * Math.random()).toString()
                    ;(i[r] = function() {
                      ai('gapi.load') ? n() : e(Error('Network Error'))
                    }),
                      Tt(qo((r = yn(Qa, { onload: r })))).s(function() {
                        e(Error('Network Error'))
                      })
                  }
                }).s(function(t) {
                  throw ((ns = null), t)
                }))
              ).then(function() {
                return new vt(function(e, n) {
                  ai('gapi.iframes.getContext')().open(
                    {
                      where: document.body,
                      url: t.b,
                      messageHandlersFilter: ai(
                        'gapi.iframes.CROSS_ORIGIN_IFRAMES_FILTER',
                      ),
                      attributes: {
                        style: {
                          position: 'absolute',
                          top: '-100px',
                          width: '1px',
                          height: '1px',
                        },
                      },
                      dontclear: !0,
                    },
                    function(i) {
                      function r() {
                        clearTimeout(o), e()
                      }
                      ;(t.a = i), t.a.restyle({ setHideOnLeave: !1 })
                      var o = setTimeout(function() {
                        n(Error('Network Error'))
                      }, es.get())
                      i.ping(r).then(r, function() {
                        n(Error('Network Error'))
                      })
                    },
                  )
                })
              })
            })(this))
        }
        Ja = $a('__EID__') ? '__EID__' : void 0
        var Qa = mn('https://apis.google.com/js/api.js?onload=%{onload}'),
          ts = new bi(3e4, 6e4),
          es = new bi(5e3, 15e3),
          ns = null
        function is(t, e, n) {
          ;(this.i = t),
            (this.g = e),
            (this.h = n),
            (this.f = null),
            (this.a = Je(this.i, '/__/auth/iframe')),
            Be(this.a, 'apiKey', this.g),
            Be(this.a, 'appName', this.h),
            (this.b = null),
            (this.c = [])
        }
        function rs(t, e, n, i, r) {
          ;(this.o = t),
            (this.m = e),
            (this.c = n),
            (this.l = i),
            (this.h = this.g = this.i = null),
            (this.a = r),
            (this.f = null)
        }
        function os(t) {
          try {
            return e
              .app(t)
              .auth()
              .xa()
          } catch (t) {
            return []
          }
        }
        function as(t, e, n, i, r) {
          ;(this.m = t),
            (this.f = e),
            (this.b = n),
            (this.c = i || null),
            (this.h = r || null),
            (this.o = this.u = this.v = null),
            (this.g = []),
            (this.l = this.a = null)
        }
        function ss(t) {
          var e = Hn()
          return (function(t) {
            return Ba(t, Ra, {}).then(function(t) {
              return t.authorizedDomains || []
            })
          })(t).then(function(t) {
            t: {
              var n = ze(e),
                i = n.c
              n = n.b
              for (var r = 0; r < t.length; r++) {
                var o = t[r],
                  a = n,
                  s = i
                if (
                  (0 == o.indexOf('chrome-extension://')
                    ? (a = ze(o).b == a && 'chrome-extension' == s)
                    : 'http' != s && 'https' != s
                      ? (a = !1)
                      : Xn.test(o)
                        ? (a = a == o)
                        : ((o = o.split('.').join('\\.')),
                          (a = new RegExp(
                            '^(.+\\.' + o + '|' + o + ')$',
                            'i',
                          ).test(a))),
                  a)
                ) {
                  t = !0
                  break t
                }
              }
              t = !1
            }
            if (!t) throw new Gr(Hn())
          })
        }
        function us(t) {
          return t.l
            ? t.l
            : ((t.l = zn().then(function() {
                if (!t.u) {
                  var e = t.c,
                    n = t.h,
                    i = os(t.b),
                    r = new is(t.m, t.f, t.b)
                  ;(r.f = e),
                    (r.b = n),
                    (r.c = F(i || [])),
                    (t.u = r.toString())
                }
                ;(t.i = new Za(t.u)),
                  (function(t) {
                    if (!t.i) throw Error('IfcHandler must be initialized!')
                    !(function(t, e) {
                      t.eb.then(function() {
                        t.a.register(
                          'authEvent',
                          e,
                          ai('gapi.iframes.CROSS_ORIGIN_IFRAMES_FILTER'),
                        )
                      })
                    })(t.i, function(e) {
                      var n = {}
                      if (e && e.authEvent) {
                        var i = !1
                        for (e = Hr(e.authEvent), n = 0; n < t.g.length; n++)
                          i = t.g[n](e) || i
                        return ((n = {}).status = i ? 'ACK' : 'ERROR'), Tt(n)
                      }
                      return (n.status = 'ERROR'), Tt(n)
                    })
                  })(t)
              })),
              t.l)
        }
        function cs(t) {
          return (
            t.o ||
              ((t.v = t.c ? ri(t.c, os(t.b)) : null),
              (t.o = new Yo(t.f, $a(t.h), t.v))),
            t.o
          )
        }
        function hs(t, e, n, i, r, o, a, s, u, c) {
          return (
            ((t = new rs(t, e, n, i, r)).i = o),
            (t.g = a),
            (t.h = s),
            (t.b = ot(u || null)),
            (t.f = c),
            t.toString()
          )
        }
        function fs(t) {
          if (
            ((this.a =
              t ||
              (e.INTERNAL.reactNative && e.INTERNAL.reactNative.AsyncStorage)),
            !this.a)
          )
            throw new Ui(
              'internal-error',
              'The React Native compatibility library was not found.',
            )
          this.type = 'asyncStorage'
        }
        function ls(t) {
          ;(this.b = t), (this.a = {}), (this.c = g(this.f, this))
        }
        ;(is.prototype.toString = function() {
          return (
            this.f ? Be(this.a, 'v', this.f) : un(this.a.a, 'v'),
            this.b ? Be(this.a, 'eid', this.b) : un(this.a.a, 'eid'),
            this.c.length
              ? Be(this.a, 'fw', this.c.join(','))
              : un(this.a.a, 'fw'),
            this.a.toString()
          )
        }),
          (rs.prototype.toString = function() {
            var t = Je(this.o, '/__/auth/handler')
            if (
              (Be(t, 'apiKey', this.m),
              Be(t, 'appName', this.c),
              Be(t, 'authType', this.l),
              this.a.isOAuthProvider)
            ) {
              var n = this.a
              try {
                var i = e
                  .app(this.c)
                  .auth()
                  .ea()
              } catch (t) {
                i = null
              }
              for (var r in ((n.Za = i),
              Be(t, 'providerId', this.a.providerId),
              (i = di((n = this.a).xb))))
                i[r] = i[r].toString()
              ;(r = n.Cc), (i = ot(i))
              for (var o = 0; o < r.length; o++) {
                var a = r[o]
                a in i && delete i[a]
              }
              n.bb && n.Za && !i[n.bb] && (i[n.bb] = n.Za),
                rt(i) || Be(t, 'customParameters', li(i))
            }
            if (
              ('function' == typeof this.a.Db &&
                ((n = this.a.Db()).length && Be(t, 'scopes', n.join(','))),
              this.i ? Be(t, 'redirectUrl', this.i) : un(t.a, 'redirectUrl'),
              this.g ? Be(t, 'eventId', this.g) : un(t.a, 'eventId'),
              this.h ? Be(t, 'v', this.h) : un(t.a, 'v'),
              this.b)
            )
              for (var s in this.b)
                this.b.hasOwnProperty(s) && !Xe(t, s) && Be(t, s, this.b[s])
            return (
              this.f ? Be(t, 'eid', this.f) : un(t.a, 'eid'),
              (s = os(this.c)).length && Be(t, 'fw', s.join(',')),
              t.toString()
            )
          }),
          ((t = as.prototype).Ea = function(t, e, n) {
            var i = new Ui('popup-closed-by-user'),
              r = new Ui('web-storage-unsupported'),
              o = this,
              a = !1
            return this.ga()
              .then(function() {
                ;(function(t) {
                  var e = { type: 'webStorageSupport' }
                  return us(t)
                    .then(function() {
                      return (function(t, e) {
                        return t.eb.then(function() {
                          return new vt(function(n) {
                            t.a.send(
                              e.type,
                              e,
                              n,
                              ai('gapi.iframes.CROSS_ORIGIN_IFRAMES_FILTER'),
                            )
                          })
                        })
                      })(t.i, e)
                    })
                    .then(function(t) {
                      if (t && t.length && void 0 !== t[0].webStorageSupport)
                        return t[0].webStorageSupport
                      throw Error()
                    })
                })(o).then(function(n) {
                  n || (t && Gn(t), e(r), (a = !0))
                })
              })
              .s(function() {})
              .then(function() {
                if (!a)
                  return (function(t) {
                    return new vt(function(e) {
                      return (function n() {
                        xe(2e3).then(function() {
                          if (t && !t.closed) return n()
                          e()
                        })
                      })()
                    })
                  })(t)
              })
              .then(function() {
                if (!a)
                  return xe(n).then(function() {
                    e(i)
                  })
              })
          }),
          (t.Lb = function() {
            var t = oi()
            return !fi(t) && !mi(t)
          }),
          (t.Gb = function() {
            return !1
          }),
          (t.Bb = function(t, e, n, i, r, o, a) {
            if (!t) return Et(new Ui('popup-blocked'))
            if (a && !fi())
              return (
                this.ga().s(function(e) {
                  Gn(t), r(e)
                }),
                i(),
                Tt()
              )
            this.a || (this.a = ss(cs(this)))
            var s = this
            return this.a
              .then(function() {
                var e = s.ga().s(function(e) {
                  throw (Gn(t), r(e), e)
                })
                return i(), e
              })
              .then(function() {
                ;(Fr(n), a) ||
                  qn(hs(s.m, s.f, s.b, e, n, null, o, s.c, void 0, s.h), t)
              })
              .s(function(t) {
                throw ('auth/network-request-failed' == t.code && (s.a = null),
                t)
              })
          }),
          (t.Ca = function(t, e, n) {
            this.a || (this.a = ss(cs(this)))
            var i = this
            return this.a
              .then(function() {
                Fr(e), qn(hs(i.m, i.f, i.b, t, e, Hn(), n, i.c, void 0, i.h))
              })
              .s(function(t) {
                throw ('auth/network-request-failed' == t.code && (i.a = null),
                t)
              })
          }),
          (t.ga = function() {
            var t = this
            return us(this)
              .then(function() {
                return t.i.eb
              })
              .s(function() {
                throw ((t.a = null), new Ui('network-request-failed'))
              })
          }),
          (t.Pb = function() {
            return !0
          }),
          (t.va = function(t) {
            this.g.push(t)
          }),
          (t.Ka = function(t) {
            U(this.g, function(e) {
              return e == t
            })
          }),
          ((t = fs.prototype).get = function(t) {
            return Tt(this.a.getItem(t)).then(function(t) {
              return t && pi(t)
            })
          }),
          (t.set = function(t, e) {
            return Tt(this.a.setItem(t, li(e)))
          }),
          (t.P = function(t) {
            return Tt(this.a.removeItem(t))
          }),
          (t.Y = function() {}),
          (t.ca = function() {})
        var ds,
          ps = []
        function vs(t) {
          this.a = t
        }
        function ms(t) {
          ;(this.c = t), (this.b = !1), (this.a = [])
        }
        function gs(t, e, n, i) {
          var r,
            o,
            a,
            s,
            u = n || {},
            c = null
          if (t.b) return Et(Error('connection_unavailable'))
          var h = i ? 800 : 50,
            f =
              'undefined' != typeof MessageChannel ? new MessageChannel() : null
          return new vt(function(n, i) {
            f
              ? ((r = Math.floor(Math.random() * Math.pow(10, 20)).toString()),
                f.port1.start(),
                (a = setTimeout(function() {
                  i(Error('unsupported_event'))
                }, h)),
                (c = {
                  messageChannel: f,
                  onMessage: (o = function(t) {
                    t.data.eventId === r &&
                      ('ack' === t.data.status
                        ? (clearTimeout(a),
                          (s = setTimeout(function() {
                            i(Error('timeout'))
                          }, 3e3)))
                        : 'done' === t.data.status
                          ? (clearTimeout(s),
                            void 0 !== t.data.response
                              ? n(t.data.response)
                              : i(Error('unknown_error')))
                          : (clearTimeout(a),
                            clearTimeout(s),
                            i(Error('invalid_response'))))
                  }),
                }),
                t.a.push(c),
                f.port1.addEventListener('message', o),
                t.c.postMessage({ eventType: e, eventId: r, data: u }, [
                  f.port2,
                ]))
              : i(Error('connection_unavailable'))
          })
            .then(function(e) {
              return bs(t, c), e
            })
            .s(function(e) {
              throw (bs(t, c), e)
            })
        }
        function bs(t, e) {
          if (e) {
            var n = e.messageChannel,
              i = e.onMessage
            n && (n.port1.removeEventListener('message', i), n.port1.close()),
              U(t.a, function(t) {
                return t == e
              })
          }
        }
        function ys() {
          if (!Ts()) throw new Ui('web-storage-unsupported')
          ;(this.c = {}),
            (this.a = []),
            (this.b = 0),
            (this.m = i.indexedDB),
            (this.type = 'indexedDB'),
            (this.g = this.l = this.f = this.i = null),
            (this.u = !1),
            (this.h = null)
          var t = this
          $n() && self
            ? ((this.l = (function() {
                var t = $n() ? self : null
                if (
                  (D(ps, function(n) {
                    n.b == t && (e = n)
                  }),
                  !e)
                ) {
                  var e = new ls(t)
                  ps.push(e)
                }
                return e
              })()),
              this.l.subscribe('keyChanged', function(e, n) {
                return Ns(t).then(function(e) {
                  return (
                    0 < e.length &&
                      D(t.a, function(t) {
                        t(e)
                      }),
                    { keyProcessed: M(e, n.key) }
                  )
                })
              }),
              this.l.subscribe('ping', function() {
                return Tt(['keyChanged'])
              }))
            : (function() {
                var t = i.navigator
                return t && t.serviceWorker
                  ? Tt()
                      .then(function() {
                        return t.serviceWorker.ready
                      })
                      .then(function(t) {
                        return t.active || null
                      })
                      .s(function() {
                        return null
                      })
                  : Tt(null)
              })().then(function(e) {
                ;(t.h = e) &&
                  ((t.g = new ms(new vs(e))),
                  gs(t.g, 'ping', null, !0)
                    .then(function(e) {
                      e[0].fulfilled &&
                        M(e[0].value, 'keyChanged') &&
                        (t.u = !0)
                    })
                    .s(function() {}))
              })
        }
        function ws(t) {
          return new vt(function(e, n) {
            var i = t.m.open('firebaseLocalStorageDb', 1)
            ;(i.onerror = function(t) {
              try {
                t.preventDefault()
              } catch (t) {}
              n(Error(t.target.error))
            }),
              (i.onupgradeneeded = function(t) {
                t = t.target.result
                try {
                  t.createObjectStore('firebaseLocalStorage', {
                    keyPath: 'fbase_key',
                  })
                } catch (t) {
                  n(t)
                }
              }),
              (i.onsuccess = function(i) {
                ;(i = i.target.result).objectStoreNames.contains(
                  'firebaseLocalStorage',
                )
                  ? e(i)
                  : (function(t) {
                      return new vt(function(e, n) {
                        var i = t.m.deleteDatabase('firebaseLocalStorageDb')
                        ;(i.onsuccess = function() {
                          e()
                        }),
                          (i.onerror = function(t) {
                            n(Error(t.target.error))
                          })
                      })
                    })(t)
                      .then(function() {
                        return ws(t)
                      })
                      .then(function(t) {
                        e(t)
                      })
                      .s(function(t) {
                        n(t)
                      })
              })
          })
        }
        function Is(t) {
          return t.o || (t.o = ws(t)), t.o
        }
        function Ts() {
          try {
            return !!i.indexedDB
          } catch (t) {
            return !1
          }
        }
        function Es(t) {
          return t.objectStore('firebaseLocalStorage')
        }
        function As(t, e) {
          return t.transaction(
            ['firebaseLocalStorage'],
            e ? 'readwrite' : 'readonly',
          )
        }
        function ks(t) {
          return new vt(function(e, n) {
            ;(t.onsuccess = function(t) {
              t && t.target ? e(t.target.result) : e()
            }),
              (t.onerror = function(t) {
                n(Error(t.target.errorCode))
              })
          })
        }
        function Ss(t, e) {
          return t.g &&
            t.h &&
            (function() {
              var t = i.navigator
              return (
                (t && t.serviceWorker && t.serviceWorker.controller) || null
              )
            })() === t.h
            ? gs(t.g, 'keyChanged', { key: e }, t.u)
                .then(function() {})
                .s(function() {})
            : Tt()
        }
        function Ns(t) {
          return Is(t)
            .then(function(t) {
              var e = Es(As(t, !1))
              return e.getAll
                ? ks(e.getAll())
                : new vt(function(t, n) {
                    var i = [],
                      r = e.openCursor()
                    ;(r.onsuccess = function(e) {
                      ;(e = e.target.result)
                        ? (i.push(e.value), e.continue())
                        : t(i)
                    }),
                      (r.onerror = function(t) {
                        n(Error(t.target.errorCode))
                      })
                  })
            })
            .then(function(e) {
              var n = {},
                i = []
              if (0 == t.b) {
                for (i = 0; i < e.length; i++) n[e[i].fbase_key] = e[i].value
                ;(i = (function t(e, n) {
                  var i,
                    r = []
                  for (i in e)
                    i in n
                      ? typeof e[i] != typeof n[i]
                        ? r.push(i)
                        : 'object' == typeof e[i] &&
                          null != e[i] &&
                          null != n[i]
                          ? 0 < t(e[i], n[i]).length && r.push(i)
                          : e[i] !== n[i] && r.push(i)
                      : r.push(i)
                  for (i in n) i in e || r.push(i)
                  return r
                })(t.c, n)),
                  (t.c = n)
              }
              return i
            })
        }
        function _s(t) {
          t.i && t.i.cancel('STOP_EVENT'),
            t.f && (clearTimeout(t.f), (t.f = null))
        }
        function Os(t) {
          var e = this,
            n = null
          ;(this.a = []),
            (this.type = 'indexedDB'),
            (this.c = t),
            (this.b = Tt()
              .then(function() {
                if (Ts()) {
                  var t = vi(),
                    i = '__sak' + t
                  return (
                    ds || (ds = new ys()),
                    (n = ds)
                      .set(i, t)
                      .then(function() {
                        return n.get(i)
                      })
                      .then(function(e) {
                        if (e !== t) throw Error('indexedDB not supported!')
                        return n.P(i)
                      })
                      .then(function() {
                        return n
                      })
                      .s(function() {
                        return e.c
                      })
                  )
                }
                return e.c
              })
              .then(function(t) {
                return (
                  (e.type = t.type),
                  t.Y(function(t) {
                    D(e.a, function(e) {
                      e(t)
                    })
                  }),
                  t
                )
              }))
        }
        function Ps() {
          ;(this.a = {}), (this.type = 'inMemory')
        }
        function Rs() {
          if (
            !(function() {
              var t = 'Node' == Zn()
              if (
                !(t =
                  Cs() ||
                  (t && e.INTERNAL.node && e.INTERNAL.node.localStorage))
              )
                return !1
              try {
                return t.setItem('__sak', '1'), t.removeItem('__sak'), !0
              } catch (t) {
                return !1
              }
            })()
          ) {
            if ('Node' == Zn())
              throw new Ui(
                'internal-error',
                'The LocalStorage compatibility library was not found.',
              )
            throw new Ui('web-storage-unsupported')
          }
          ;(this.a = Cs() || e.INTERNAL.node.localStorage),
            (this.type = 'localStorage')
        }
        function Cs() {
          try {
            var t = i.localStorage,
              e = vi()
            return t && (t.setItem(e, '1'), t.removeItem(e)), t
          } catch (t) {
            return null
          }
        }
        function Ds() {
          this.type = 'nullStorage'
        }
        function Ls() {
          if (
            !(function() {
              var t = 'Node' == Zn()
              if (
                !(t =
                  xs() ||
                  (t && e.INTERNAL.node && e.INTERNAL.node.sessionStorage))
              )
                return !1
              try {
                return t.setItem('__sak', '1'), t.removeItem('__sak'), !0
              } catch (t) {
                return !1
              }
            })()
          ) {
            if ('Node' == Zn())
              throw new Ui(
                'internal-error',
                'The SessionStorage compatibility library was not found.',
              )
            throw new Ui('web-storage-unsupported')
          }
          ;(this.a = xs() || e.INTERNAL.node.sessionStorage),
            (this.type = 'sessionStorage')
        }
        function xs() {
          try {
            var t = i.sessionStorage,
              e = vi()
            return t && (t.setItem(e, '1'), t.removeItem(e)), t
          } catch (t) {
            return null
          }
        }
        ;(ls.prototype.f = function(t) {
          var e = t.data.eventType,
            n = t.data.eventId,
            i = this.a[e]
          if (i && 0 < i.length) {
            t.ports[0].postMessage({
              status: 'ack',
              eventId: n,
              eventType: e,
              response: null,
            })
            var r = []
            D(i, function(e) {
              r.push(
                Tt().then(function() {
                  return e(t.origin, t.data.data)
                }),
              )
            }),
              kt(r).then(function(i) {
                var r = []
                D(i, function(t) {
                  r.push({
                    fulfilled: t.Cb,
                    value: t.value,
                    reason: t.reason ? t.reason.message : void 0,
                  })
                }),
                  D(r, function(t) {
                    for (var e in t) void 0 === t[e] && delete t[e]
                  }),
                  t.ports[0].postMessage({
                    status: 'done',
                    eventId: n,
                    eventType: e,
                    response: r,
                  })
              })
          }
        }),
          (ls.prototype.subscribe = function(t, e) {
            rt(this.a) && this.b.addEventListener('message', this.c),
              void 0 === this.a[t] && (this.a[t] = []),
              this.a[t].push(e)
          }),
          (ls.prototype.unsubscribe = function(t, e) {
            void 0 !== this.a[t] && e
              ? (U(this.a[t], function(t) {
                  return t == e
                }),
                0 == this.a[t].length && delete this.a[t])
              : e || delete this.a[t],
              rt(this.a) && this.b.removeEventListener('message', this.c)
          }),
          (vs.prototype.postMessage = function(t, e) {
            this.a.postMessage(t, e)
          }),
          (ms.prototype.close = function() {
            for (; 0 < this.a.length; ) bs(this, this.a[0])
            this.b = !0
          }),
          ((t = ys.prototype).set = function(t, e) {
            var n,
              i = !1,
              r = this
            return Is(this)
              .then(function(e) {
                return ks((e = Es(As((n = e), !0))).get(t))
              })
              .then(function(o) {
                var a = Es(As(n, !0))
                return o
                  ? ((o.value = e), ks(a.put(o)))
                  : (r.b++,
                    (i = !0),
                    ((o = {}).fbase_key = t),
                    (o.value = e),
                    ks(a.add(o)))
              })
              .then(function() {
                return (r.c[t] = e), Ss(r, t)
              })
              .ia(function() {
                i && r.b--
              })
          }),
          (t.get = function(t) {
            return Is(this)
              .then(function(e) {
                return ks(Es(As(e, !1)).get(t))
              })
              .then(function(t) {
                return t && t.value
              })
          }),
          (t.P = function(t) {
            var e = !1,
              n = this
            return Is(this)
              .then(function(i) {
                return (e = !0), n.b++, ks(Es(As(i, !0)).delete(t))
              })
              .then(function() {
                return delete n.c[t], Ss(n, t)
              })
              .ia(function() {
                e && n.b--
              })
          }),
          (t.Y = function(t) {
            0 == this.a.length &&
              (function(t) {
                _s(t),
                  (function e() {
                    t.f = setTimeout(function() {
                      t.i = Ns(t)
                        .then(function(e) {
                          0 < e.length &&
                            D(t.a, function(t) {
                              t(e)
                            })
                        })
                        .then(function() {
                          e()
                        })
                        .s(function(t) {
                          'STOP_EVENT' != t.message && e()
                        })
                    }, 800)
                  })()
              })(this),
              this.a.push(t)
          }),
          (t.ca = function(t) {
            U(this.a, function(e) {
              return e == t
            }),
              0 == this.a.length && _s(this)
          }),
          ((t = Os.prototype).get = function(t) {
            return this.b.then(function(e) {
              return e.get(t)
            })
          }),
          (t.set = function(t, e) {
            return this.b.then(function(n) {
              return n.set(t, e)
            })
          }),
          (t.P = function(t) {
            return this.b.then(function(e) {
              return e.P(t)
            })
          }),
          (t.Y = function(t) {
            this.a.push(t)
          }),
          (t.ca = function(t) {
            U(this.a, function(e) {
              return e == t
            })
          }),
          ((t = Ps.prototype).get = function(t) {
            return Tt(this.a[t])
          }),
          (t.set = function(t, e) {
            return (this.a[t] = e), Tt()
          }),
          (t.P = function(t) {
            return delete this.a[t], Tt()
          }),
          (t.Y = function() {}),
          (t.ca = function() {}),
          ((t = Rs.prototype).get = function(t) {
            var e = this
            return Tt().then(function() {
              return pi(e.a.getItem(t))
            })
          }),
          (t.set = function(t, e) {
            var n = this
            return Tt().then(function() {
              var i = li(e)
              null === i ? n.P(t) : n.a.setItem(t, i)
            })
          }),
          (t.P = function(t) {
            var e = this
            return Tt().then(function() {
              e.a.removeItem(t)
            })
          }),
          (t.Y = function(t) {
            i.window && be(i.window, 'storage', t)
          }),
          (t.ca = function(t) {
            i.window && Ie(i.window, 'storage', t)
          }),
          ((t = Ds.prototype).get = function() {
            return Tt(null)
          }),
          (t.set = function() {
            return Tt()
          }),
          (t.P = function() {
            return Tt()
          }),
          (t.Y = function() {}),
          (t.ca = function() {}),
          ((t = Ls.prototype).get = function(t) {
            var e = this
            return Tt().then(function() {
              return pi(e.a.getItem(t))
            })
          }),
          (t.set = function(t, e) {
            var n = this
            return Tt().then(function() {
              var i = li(e)
              null === i ? n.P(t) : n.a.setItem(t, i)
            })
          }),
          (t.P = function(t) {
            var e = this
            return Tt().then(function() {
              e.a.removeItem(t)
            })
          }),
          (t.Y = function() {}),
          (t.ca = function() {})
        var Ms,
          js,
          Us = { A: Rs, Qa: Ls },
          Vs = { A: Rs, Qa: Ls },
          Fs = { A: fs, Qa: Ds },
          Ks = { A: Rs, Qa: Ds },
          Hs = { Yc: 'local', NONE: 'none', $c: 'session' }
        function qs() {
          var t = !(mi(oi()) || !Yn()),
            e = fi(),
            n = si()
          ;(this.o = t),
            (this.h = e),
            (this.l = n),
            (this.a = {}),
            Ms ||
              (Ms = new function() {
                var t = {}
                ;(t.Browser = Us),
                  (t.Node = Vs),
                  (t.ReactNative = Fs),
                  (t.Worker = Ks),
                  (this.a = t[Zn()])
              }()),
            (t = Ms)
          try {
            this.g =
              (!Kn() && Ii()) || !i.indexedDB
                ? new t.a.A()
                : new Os($n() ? new Ps() : new t.a.A())
          } catch (t) {
            ;(this.g = new Ps()), (this.h = !0)
          }
          try {
            this.i = new t.a.Qa()
          } catch (t) {
            this.i = new Ps()
          }
          ;(this.m = new Ps()), (this.f = g(this.Ob, this)), (this.b = {})
        }
        function Ws() {
          return js || (js = new qs()), js
        }
        function Gs(t, e) {
          switch (e) {
            case 'session':
              return t.i
            case 'none':
              return t.m
            default:
              return t.g
          }
        }
        function Bs(t, e) {
          return 'firebase:' + t.name + (e ? ':' + e : '')
        }
        function Xs(t, e, n) {
          return (
            (n = Bs(e, n)), 'local' == e.A && (t.b[n] = null), Gs(t, e.A).P(n)
          )
        }
        function zs(t) {
          t.c && (clearInterval(t.c), (t.c = null))
        }
        ;((t = qs.prototype).get = function(t, e) {
          return Gs(this, t.A).get(Bs(t, e))
        }),
          (t.set = function(t, e, n) {
            var i = Bs(t, n),
              r = this,
              o = Gs(this, t.A)
            return o
              .set(i, e)
              .then(function() {
                return o.get(i)
              })
              .then(function(e) {
                'local' == t.A && (r.b[i] = e)
              })
          }),
          (t.addListener = function(t, e, n) {
            ;(t = Bs(t, e)),
              this.l && (this.b[t] = i.localStorage.getItem(t)),
              rt(this.a) &&
                (Gs(this, 'local').Y(this.f),
                this.h ||
                  ((Kn() || !Ii()) && i.indexedDB) ||
                  !this.l ||
                  (function(t) {
                    zs(t),
                      (t.c = setInterval(function() {
                        for (var e in t.a) {
                          var n = i.localStorage.getItem(e),
                            r = t.b[e]
                          n != r &&
                            ((t.b[e] = n),
                            (n = new ue({
                              type: 'storage',
                              key: e,
                              target: window,
                              oldValue: r,
                              newValue: n,
                              a: !0,
                            })),
                            t.Ob(n))
                        }
                      }, 1e3))
                  })(this)),
              this.a[t] || (this.a[t] = []),
              this.a[t].push(n)
          }),
          (t.removeListener = function(t, e, n) {
            ;(t = Bs(t, e)),
              this.a[t] &&
                (U(this.a[t], function(t) {
                  return t == n
                }),
                0 == this.a[t].length && delete this.a[t]),
              rt(this.a) && (Gs(this, 'local').ca(this.f), zs(this))
          }),
          (t.Ob = function(t) {
            if (t && t.f) {
              var e = t.a.key
              if (null == e)
                for (var n in this.a) {
                  var r = this.b[n]
                  void 0 === r && (r = null)
                  var o = i.localStorage.getItem(n)
                  o !== r && ((this.b[n] = o), this.Wa(n))
                }
              else if (0 == e.indexOf('firebase:') && this.a[e]) {
                if (
                  (void 0 !== t.a.a ? Gs(this, 'local').ca(this.f) : zs(this),
                  this.o)
                )
                  if (
                    ((n = i.localStorage.getItem(e)), (r = t.a.newValue) !== n)
                  )
                    null !== r
                      ? i.localStorage.setItem(e, r)
                      : i.localStorage.removeItem(e)
                  else if (this.b[e] === r && void 0 === t.a.a) return
                var a = this
                ;(n = function() {
                  ;(void 0 === t.a.a && a.b[e] === i.localStorage.getItem(e)) ||
                    ((a.b[e] = i.localStorage.getItem(e)), a.Wa(e))
                }),
                  Wt &&
                  Qt &&
                  10 == Qt &&
                  i.localStorage.getItem(e) !== t.a.newValue &&
                  t.a.newValue !== t.a.oldValue
                    ? setTimeout(n, 10)
                    : n()
              }
            } else D(t, g(this.Wa, this))
          }),
          (t.Wa = function(t) {
            this.a[t] &&
              D(this.a[t], function(t) {
                t()
              })
          })
        var Js,
          Ys = { name: 'authEvent', A: 'local' }
        function $s(t, e) {
          ;(this.b = Zs),
            (this.f = i.Uint8Array ? new Uint8Array(this.b) : Array(this.b)),
            (this.g = this.c = 0),
            (this.a = []),
            (this.i = t),
            (this.h = e),
            (this.l = i.Int32Array ? new Int32Array(64) : Array(64)),
            void 0 !== Js || (Js = i.Int32Array ? new Int32Array(ou) : ou),
            this.reset()
        }
        w($s, function() {
          this.b = -1
        })
        for (var Zs = 64, Qs = Zs - 1, tu = [], eu = 0; eu < Qs; eu++)
          tu[eu] = 0
        var nu = V(128, tu)
        function iu(t) {
          for (var e = t.f, n = t.l, i = 0, r = 0; r < e.length; )
            (n[i++] =
              (e[r] << 24) | (e[r + 1] << 16) | (e[r + 2] << 8) | e[r + 3]),
              (r = 4 * i)
          for (e = 16; 64 > e; e++) {
            ;(r = 0 | n[e - 15]), (i = 0 | n[e - 2])
            var o =
                ((0 | n[e - 16]) +
                  (((r >>> 7) | (r << 25)) ^
                    ((r >>> 18) | (r << 14)) ^
                    (r >>> 3))) |
                0,
              a =
                ((0 | n[e - 7]) +
                  (((i >>> 17) | (i << 15)) ^
                    ((i >>> 19) | (i << 13)) ^
                    (i >>> 10))) |
                0
            n[e] = (o + a) | 0
          }
          ;(i = 0 | t.a[0]), (r = 0 | t.a[1])
          var s = 0 | t.a[2],
            u = 0 | t.a[3],
            c = 0 | t.a[4],
            h = 0 | t.a[5],
            f = 0 | t.a[6]
          for (o = 0 | t.a[7], e = 0; 64 > e; e++) {
            var l =
              ((((i >>> 2) | (i << 30)) ^
                ((i >>> 13) | (i << 19)) ^
                ((i >>> 22) | (i << 10))) +
                ((i & r) ^ (i & s) ^ (r & s))) |
              0
            ;(a =
              ((o =
                (o +
                  (((c >>> 6) | (c << 26)) ^
                    ((c >>> 11) | (c << 21)) ^
                    ((c >>> 25) | (c << 7)))) |
                0) +
                (((a = ((a = (c & h) ^ (~c & f)) + (0 | Js[e])) | 0) +
                  (0 | n[e])) |
                  0)) |
              0),
              (o = f),
              (f = h),
              (h = c),
              (c = (u + a) | 0),
              (u = s),
              (s = r),
              (r = i),
              (i = (a + l) | 0)
          }
          ;(t.a[0] = (t.a[0] + i) | 0),
            (t.a[1] = (t.a[1] + r) | 0),
            (t.a[2] = (t.a[2] + s) | 0),
            (t.a[3] = (t.a[3] + u) | 0),
            (t.a[4] = (t.a[4] + c) | 0),
            (t.a[5] = (t.a[5] + h) | 0),
            (t.a[6] = (t.a[6] + f) | 0),
            (t.a[7] = (t.a[7] + o) | 0)
        }
        function ru(t, e, n) {
          void 0 === n && (n = e.length)
          var i = 0,
            o = t.c
          if (r(e))
            for (; i < n; )
              (t.f[o++] = e.charCodeAt(i++)), o == t.b && (iu(t), (o = 0))
          else {
            if (!h(e)) throw Error('message must be string or array')
            for (; i < n; ) {
              var a = e[i++]
              if (!('number' == typeof a && 0 <= a && 255 >= a && a == (0 | a)))
                throw Error('message must be a byte array')
              ;(t.f[o++] = a), o == t.b && (iu(t), (o = 0))
            }
          }
          ;(t.c = o), (t.g += n)
        }
        $s.prototype.reset = function() {
          ;(this.g = this.c = 0),
            (this.a = i.Int32Array ? new Int32Array(this.h) : F(this.h))
        }
        var ou = [
          1116352408,
          1899447441,
          3049323471,
          3921009573,
          961987163,
          1508970993,
          2453635748,
          2870763221,
          3624381080,
          310598401,
          607225278,
          1426881987,
          1925078388,
          2162078206,
          2614888103,
          3248222580,
          3835390401,
          4022224774,
          264347078,
          604807628,
          770255983,
          1249150122,
          1555081692,
          1996064986,
          2554220882,
          2821834349,
          2952996808,
          3210313671,
          3336571891,
          3584528711,
          113926993,
          338241895,
          666307205,
          773529912,
          1294757372,
          1396182291,
          1695183700,
          1986661051,
          2177026350,
          2456956037,
          2730485921,
          2820302411,
          3259730800,
          3345764771,
          3516065817,
          3600352804,
          4094571909,
          275423344,
          430227734,
          506948616,
          659060556,
          883997877,
          958139571,
          1322822218,
          1537002063,
          1747873779,
          1955562222,
          2024104815,
          2227730452,
          2361852424,
          2428436474,
          2756734187,
          3204031479,
          3329325298,
        ]
        function au() {
          $s.call(this, 8, su)
        }
        w(au, $s)
        var su = [
          1779033703,
          3144134277,
          1013904242,
          2773480762,
          1359893119,
          2600822924,
          528734635,
          1541459225,
        ]
        function uu(t, e, n, i, r) {
          ;(this.m = t),
            (this.i = e),
            (this.l = n),
            (this.o = i || null),
            (this.u = r || null),
            (this.h = e + ':' + n),
            (this.v = new function() {
              this.a = Ws()
            }()),
            (this.g = new function(t) {
              ;(this.a = t), (this.b = Ws())
            }(this.h)),
            (this.f = null),
            (this.b = []),
            (this.a = this.c = null)
        }
        function cu(t) {
          return new Ui('invalid-cordova-configuration', t)
        }
        function hu(t) {
          var e = new au()
          ru(e, t), (t = [])
          var n = 8 * e.g
          56 > e.c ? ru(e, nu, 56 - e.c) : ru(e, nu, e.b - (e.c - 56))
          for (var i = 63; 56 <= i; i--) (e.f[i] = 255 & n), (n /= 256)
          for (iu(e), i = n = 0; i < e.i; i++)
            for (var r = 24; 0 <= r; r -= 8) t[n++] = (e.a[i] >> r) & 255
          return (function(t) {
            return L(t, function(t) {
              return 1 < (t = t.toString(16)).length ? t : '0' + t
            }).join('')
          })(t)
        }
        function fu(t, e) {
          for (var n = 0; n < t.b.length; n++)
            try {
              t.b[n](e)
            } catch (t) {}
        }
        function lu(t) {
          return (
            t.f ||
              (t.f = t.ga().then(function() {
                return new vt(function(e) {
                  t.va(function n(i) {
                    return e(i), t.Ka(n), !1
                  }),
                    (function(t) {
                      function e(e) {
                        ;(r = !0),
                          o && o.cancel(),
                          du(t).then(function(i) {
                            var r = n
                            if (i && e && e.url) {
                              var o = null
                              ;-1 !=
                                (r = gr(e.url)).indexOf('/__/auth/callback') &&
                                (o = (o =
                                  'object' ==
                                  typeof (o = pi(
                                    Xe((o = ze(r)), 'firebaseError') || null,
                                  ))
                                    ? Vi(o)
                                    : null)
                                  ? new Kr(i.b, i.c, null, null, o)
                                  : new Kr(i.b, i.c, r, i.g)),
                                (r = o || n)
                            }
                            fu(t, r)
                          })
                      }
                      var n = new Kr(
                          'unknown',
                          null,
                          null,
                          null,
                          new Ui('no-auth-event'),
                        ),
                        r = !1,
                        o = xe(500).then(function() {
                          return du(t).then(function() {
                            r || fu(t, n)
                          })
                        }),
                        a = i.handleOpenURL
                      ;(i.handleOpenURL = function(t) {
                        if (
                          (0 ==
                            t
                              .toLowerCase()
                              .indexOf(
                                ai('BuildInfo.packageName', i).toLowerCase() +
                                  '://',
                              ) && e({ url: t }),
                          'function' == typeof a)
                        )
                          try {
                            a(t)
                          } catch (t) {
                            console.error(t)
                          }
                      }),
                        Wr || (Wr = new qr()),
                        Wr.subscribe(e)
                    })(t)
                })
              })),
            t.f
          )
        }
        function du(t) {
          var e = null
          return (function(t) {
            return t.b.get(Ys, t.a).then(function(t) {
              return Hr(t)
            })
          })(t.g)
            .then(function(n) {
              return (e = n), Xs((n = t.g).b, Ys, n.a)
            })
            .then(function() {
              return e
            })
        }
        ;((t = uu.prototype).ga = function() {
          return this.za
            ? this.za
            : (this.za = (Jn(void 0)
                ? zn().then(function() {
                    return new vt(function(t, e) {
                      var n = i.document,
                        r = setTimeout(function() {
                          e(Error('Cordova framework is not ready.'))
                        }, 1e3)
                      n.addEventListener(
                        'deviceready',
                        function() {
                          clearTimeout(r), t()
                        },
                        !1,
                      )
                    })
                  })
                : Et(
                    Error('Cordova must run in an Android or iOS file scheme.'),
                  )
              ).then(
                function() {
                  if ('function' != typeof ai('universalLinks.subscribe', i))
                    throw cu(
                      'cordova-universal-links-plugin-fix is not installed',
                    )
                  if (void 0 === ai('BuildInfo.packageName', i))
                    throw cu('cordova-plugin-buildinfo is not installed')
                  if (
                    'function' !=
                    typeof ai('cordova.plugins.browsertab.openUrl', i)
                  )
                    throw cu('cordova-plugin-browsertab is not installed')
                  if ('function' != typeof ai('cordova.InAppBrowser.open', i))
                    throw cu('cordova-plugin-inappbrowser is not installed')
                },
                function() {
                  throw new Ui('cordova-not-ready')
                },
              ))
        }),
          (t.Ea = function(t, e) {
            return (
              e(new Ui('operation-not-supported-in-this-environment')), Tt()
            )
          }),
          (t.Bb = function() {
            return Et(new Ui('operation-not-supported-in-this-environment'))
          }),
          (t.Pb = function() {
            return !1
          }),
          (t.Lb = function() {
            return !0
          }),
          (t.Gb = function() {
            return !0
          }),
          (t.Ca = function(t, e, n) {
            if (this.c) return Et(new Ui('redirect-operation-pending'))
            var r = this,
              o = i.document,
              a = null,
              s = null,
              u = null,
              c = null
            return (this.c = Tt()
              .then(function() {
                return Fr(e), lu(r)
              })
              .then(function() {
                return (function(t, e, n, r) {
                  var o = (function() {
                      for (var t = 20, e = []; 0 < t; )
                        e.push(
                          '1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'.charAt(
                            Math.floor(62 * Math.random()),
                          ),
                        ),
                          t--
                      return e.join('')
                    })(),
                    a = new Kr(e, r, null, o, new Ui('no-auth-event')),
                    s = ai('BuildInfo.packageName', i)
                  if ('string' != typeof s)
                    throw new Ui('invalid-cordova-configuration')
                  var u = ai('BuildInfo.displayName', i),
                    c = {}
                  if (
                    oi()
                      .toLowerCase()
                      .match(/iphone|ipad|ipod/)
                  )
                    c.ibi = s
                  else {
                    if (
                      !oi()
                        .toLowerCase()
                        .match(/android/)
                    )
                      return Et(
                        new Ui('operation-not-supported-in-this-environment'),
                      )
                    c.apn = s
                  }
                  u && (c.appDisplayName = u), (o = hu(o)), (c.sessionId = o)
                  var h = hs(t.m, t.i, t.l, e, n, null, r, t.o, c, t.u)
                  return t
                    .ga()
                    .then(function() {
                      var e = t.h
                      return t.v.a.set(Ys, a.D(), e)
                    })
                    .then(function() {
                      var e = ai('cordova.plugins.browsertab.isAvailable', i)
                      if ('function' != typeof e)
                        throw new Ui('invalid-cordova-configuration')
                      var n = null
                      e(function(e) {
                        if (e) {
                          if (
                            'function' !=
                            typeof (n = ai(
                              'cordova.plugins.browsertab.openUrl',
                              i,
                            ))
                          )
                            throw new Ui('invalid-cordova-configuration')
                          n(h)
                        } else {
                          if (
                            'function' !=
                            typeof (n = ai('cordova.InAppBrowser.open', i))
                          )
                            throw new Ui('invalid-cordova-configuration')
                          ;(e = !(
                            !(e = oi()).match(/(iPad|iPhone|iPod).*OS 7_\d/i) &&
                            !e.match(/(iPad|iPhone|iPod).*OS 8_\d/i)
                          )),
                            (t.a = n(
                              h,
                              e ? '_blank' : '_system',
                              'location=yes',
                            ))
                        }
                      })
                    })
                })(r, t, e, n)
              })
              .then(function() {
                return new vt(function(t, e) {
                  ;(s = function() {
                    var e = ai('cordova.plugins.browsertab.close', i)
                    return (
                      t(),
                      'function' == typeof e && e(),
                      r.a &&
                        'function' == typeof r.a.close &&
                        (r.a.close(), (r.a = null)),
                      !1
                    )
                  }),
                    r.va(s),
                    (u = function() {
                      a ||
                        (a = xe(2e3).then(function() {
                          e(new Ui('redirect-cancelled-by-user'))
                        }))
                    }),
                    (c = function() {
                      yi() && u()
                    }),
                    o.addEventListener('resume', u, !1),
                    oi()
                      .toLowerCase()
                      .match(/android/) ||
                      o.addEventListener('visibilitychange', c, !1)
                }).s(function(t) {
                  return du(r).then(function() {
                    throw t
                  })
                })
              })
              .ia(function() {
                u && o.removeEventListener('resume', u, !1),
                  c && o.removeEventListener('visibilitychange', c, !1),
                  a && a.cancel(),
                  s && r.Ka(s),
                  (r.c = null)
              }))
          }),
          (t.va = function(t) {
            this.b.push(t),
              lu(this).s(function(e) {
                'auth/invalid-cordova-configuration' === e.code &&
                  ((e = new Kr(
                    'unknown',
                    null,
                    null,
                    null,
                    new Ui('no-auth-event'),
                  )),
                  t(e))
              })
          }),
          (t.Ka = function(t) {
            U(this.b, function(e) {
              return e == t
            })
          })
        var pu = { name: 'pendingRedirect', A: 'session' }
        function vu(t) {
          return Xs(t.b, pu, t.a)
        }
        function mu(t, e, n) {
          ;(this.u = t),
            (this.l = e),
            (this.m = n),
            (this.h = []),
            (this.f = !1),
            (this.i = g(this.ab, this)),
            (this.b = new Su()),
            (this.o = new Pu()),
            (this.g = new function(t) {
              ;(this.a = t), (this.b = Ws())
            }(this.l + ':' + this.m)),
            (this.c = {}),
            (this.c.unknown = this.b),
            (this.c.signInViaRedirect = this.b),
            (this.c.linkViaRedirect = this.b),
            (this.c.reauthViaRedirect = this.b),
            (this.c.signInViaPopup = this.o),
            (this.c.linkViaPopup = this.o),
            (this.c.reauthViaPopup = this.o),
            (this.a = gu(this.u, this.l, this.m, Ja))
        }
        function gu(t, n, i, r) {
          var o = e.SDK_VERSION || null
          return Jn() ? new uu(t, n, i, o, r) : new as(t, n, i, o, r)
        }
        function bu(t) {
          t.f || ((t.f = !0), t.a.va(t.i))
          var e = t.a
          return t.a.ga().s(function(n) {
            throw (t.a == e && t.reset(), n)
          })
        }
        function yu(t) {
          t.a.Lb() &&
            bu(t).s(function(e) {
              var n = new Kr(
                'unknown',
                null,
                null,
                null,
                new Ui('operation-not-supported-in-this-environment'),
              )
              Eu(e) && t.ab(n)
            }),
            t.a.Gb() || Nu(t.b)
        }
        ;((t = mu.prototype).reset = function() {
          ;(this.f = !1),
            this.a.Ka(this.i),
            (this.a = gu(this.u, this.l, this.m))
        }),
          (t.Xa = function() {
            this.b.Xa()
          }),
          (t.subscribe = function(t) {
            if ((M(this.h, t) || this.h.push(t), !this.f)) {
              var e = this
              ;(function(t) {
                return t.b.get(pu, t.a).then(function(t) {
                  return 'pending' == t
                })
              })(this.g)
                .then(function(t) {
                  t
                    ? vu(e.g).then(function() {
                        bu(e).s(function(t) {
                          var n = new Kr(
                            'unknown',
                            null,
                            null,
                            null,
                            new Ui(
                              'operation-not-supported-in-this-environment',
                            ),
                          )
                          Eu(t) && e.ab(n)
                        })
                      })
                    : yu(e)
                })
                .s(function() {
                  yu(e)
                })
            }
          }),
          (t.unsubscribe = function(t) {
            U(this.h, function(e) {
              return e == t
            })
          }),
          (t.ab = function(t) {
            if (!t) throw new Ui('invalid-auth-event')
            for (var e = !1, n = 0; n < this.h.length; n++) {
              var i = this.h[n]
              if (i.ub(t.b, t.c)) {
                ;(e = this.c[t.b]) && e.h(t, i), (e = !0)
                break
              }
            }
            return Nu(this.b), e
          })
        var wu = new bi(2e3, 1e4),
          Iu = new bi(3e4, 6e4)
        function Tu(t, e, n, i, r, o) {
          return t.a.Bb(
            e,
            n,
            i,
            function() {
              t.f || ((t.f = !0), t.a.va(t.i))
            },
            function() {
              t.reset()
            },
            r,
            o,
          )
        }
        function Eu(t) {
          return !(!t || 'auth/cordova-not-ready' != t.code)
        }
        ;(mu.prototype.fa = function() {
          return this.b.fa()
        }),
          (mu.prototype.Ca = function(t, e, n) {
            var i,
              r = this
            return (function(t) {
              return t.b.set(pu, 'pending', t.a)
            })(this.g).then(function() {
              return r.a
                .Ca(t, e, n)
                .s(function(t) {
                  if (Eu(t))
                    throw new Ui('operation-not-supported-in-this-environment')
                  return (
                    (i = t),
                    vu(r.g).then(function() {
                      throw i
                    })
                  )
                })
                .then(function() {
                  return r.a.Pb()
                    ? new vt(function() {})
                    : vu(r.g)
                        .then(function() {
                          return r.fa()
                        })
                        .then(function() {})
                        .s(function() {})
                })
            })
          }),
          (mu.prototype.Ea = function(t, e, n, i) {
            return this.a.Ea(
              n,
              function(n) {
                t.ha(e, null, n, i)
              },
              wu.get(),
            )
          })
        var Au = {}
        function ku(t, e, n) {
          var i = e + ':' + n
          return Au[i] || (Au[i] = new mu(t, e, n)), Au[i]
        }
        function Su() {
          ;(this.b = null),
            (this.f = []),
            (this.c = []),
            (this.a = null),
            (this.i = this.g = !1)
        }
        function Nu(t) {
          t.g || ((t.g = !0), Ou(t, !1, null, null))
        }
        function _u(t, e) {
          if (
            ((t.b = function() {
              return Tt(e)
            }),
            t.f.length)
          )
            for (var n = 0; n < t.f.length; n++) t.f[n](e)
        }
        function Ou(t, e, n, i) {
          e
            ? i
              ? (function(t, e) {
                  if (
                    ((t.b = function() {
                      return Et(e)
                    }),
                    t.c.length)
                  )
                    for (var n = 0; n < t.c.length; n++) t.c[n](e)
                })(t, i)
              : _u(t, n)
            : _u(t, { user: null }),
            (t.f = []),
            (t.c = [])
        }
        function Pu() {}
        function Ru() {
          ;(this.rb = !1),
            Object.defineProperty(this, 'appVerificationDisabled', {
              get: function() {
                return this.rb
              },
              set: function(t) {
                this.rb = t
              },
              enumerable: !1,
            })
        }
        function Cu(t, e) {
          ;(this.a = e), Si(this, 'verificationId', t)
        }
        function Du(t, e, n, i) {
          return new jr(t).Ta(e, n).then(function(t) {
            return new Cu(t, i)
          })
        }
        function Lu(t, e, n) {
          if (
            ((this.h = t),
            (this.i = e),
            (this.g = n),
            (this.c = 3e4),
            (this.f = 96e4),
            (this.b = null),
            (this.a = this.c),
            this.f < this.c)
          )
            throw Error(
              'Proactive refresh lower bound greater than upper bound!',
            )
        }
        function xu(t) {
          ;(this.f = t), (this.b = this.a = null), (this.c = 0)
        }
        function Mu(t, e) {
          var n = e[Zo],
            i = e.refreshToken
          ;(e = ju(e.expiresIn)), (t.b = n), (t.c = e), (t.a = i)
        }
        function ju(t) {
          return y() + 1e3 * parseInt(t, 10)
        }
        function Uu(t, e) {
          return (function(t, e) {
            return new vt(function(n, i) {
              ;('refresh_token' == e.grant_type && e.refresh_token) ||
              ('authorization_code' == e.grant_type && e.code)
                ? oa(
                    t,
                    t.i + '?key=' + encodeURIComponent(t.b),
                    function(t) {
                      t
                        ? t.error
                          ? i(za(t))
                          : t.access_token && t.refresh_token
                            ? n(t)
                            : i(new Ui('internal-error'))
                        : i(new Ui('network-request-failed'))
                    },
                    'POST',
                    sn(e).toString(),
                    t.f,
                    t.l.get(),
                  )
                : i(new Ui('internal-error'))
            })
          })(t.f, e)
            .then(function(e) {
              return (
                (t.b = e.access_token),
                (t.c = ju(e.expires_in)),
                (t.a = e.refresh_token),
                { accessToken: t.b, expirationTime: t.c, refreshToken: t.a }
              )
            })
            .s(function(e) {
              throw ('auth/user-token-expired' == e.code && (t.a = null), e)
            })
        }
        function Vu(t, e) {
          ;(this.a = t || null),
            (this.b = e || null),
            Ni(this, {
              lastSignInTime: wi(e || null),
              creationTime: wi(t || null),
            })
        }
        function Fu(t, e) {
          for (var n in (se.call(this, t), e)) this[n] = e[n]
        }
        function Ku(t, n, i) {
          ;(this.G = []),
            (this.m = t.apiKey),
            (this.o = t.appName),
            (this.u = t.authDomain || null),
            (t = e.SDK_VERSION ? ri(e.SDK_VERSION) : null),
            (this.b = new Yo(this.m, $a(Ja), t)),
            (this.h = new xu(this.b)),
            zu(this, n[Zo]),
            Mu(this.h, n),
            Si(this, 'refreshToken', this.h.a),
            $u(this, i || {}),
            Pe.call(this),
            (this.I = !1),
            this.u && ui() && (this.a = ku(this.u, this.m, this.o)),
            (this.N = []),
            (this.i = null),
            (this.w = (function(t) {
              return new Lu(
                function() {
                  return t.F(!0)
                },
                function(t) {
                  return !(!t || 'auth/network-request-failed' != t.code)
                },
                function() {
                  var e = t.h.c - y() - 3e5
                  return 0 < e ? e : 0
                },
              )
            })(this)),
            (this.V = g(this.Ha, this))
          var r = this
          ;(this.ka = null),
            (this.sa = function(t) {
              r.oa(t.g)
            }),
            (this.X = null),
            (this.O = []),
            (this.ra = function(t) {
              qu(r, t.c)
            }),
            (this.W = null)
        }
        function Hu(t, e) {
          t.X && Ie(t.X, 'languageCodeChanged', t.sa),
            (t.X = e) && be(e, 'languageCodeChanged', t.sa)
        }
        function qu(t, n) {
          ;(t.O = n), ra(t.b, e.SDK_VERSION ? ri(e.SDK_VERSION, t.O) : null)
        }
        function Wu(t, e) {
          t.W && Ie(t.W, 'frameworkChanged', t.ra),
            (t.W = e) && be(e, 'frameworkChanged', t.ra)
        }
        function Gu(t) {
          try {
            return e.app(t.o).auth()
          } catch (e) {
            throw new Ui(
              'internal-error',
              "No firebase.auth.Auth instance is available for the Firebase App '" +
                t.o +
                "'!",
            )
          }
        }
        function Bu(t) {
          t.C ||
            t.w.b ||
            (t.w.start(),
            Ie(t, 'tokenChanged', t.V),
            be(t, 'tokenChanged', t.V))
        }
        function Xu(t) {
          Ie(t, 'tokenChanged', t.V), t.w.stop()
        }
        function zu(t, e) {
          ;(t.qa = e), Si(t, '_lat', e)
        }
        function Ju(t) {
          for (var e = [], n = 0; n < t.N.length; n++) e.push(t.N[n](t))
          return kt(e).then(function() {
            return t
          })
        }
        function Yu(t) {
          t.a && !t.I && ((t.I = !0), t.a.subscribe(t))
        }
        function $u(t, e) {
          Ni(t, {
            uid: e.uid,
            displayName: e.displayName || null,
            photoURL: e.photoURL || null,
            email: e.email || null,
            emailVerified: e.emailVerified || !1,
            phoneNumber: e.phoneNumber || null,
            isAnonymous: e.isAnonymous || !1,
            metadata: new Vu(e.createdAt, e.lastLoginAt),
            providerData: [],
          })
        }
        function Zu() {}
        function Qu(t) {
          return Tt().then(function() {
            if (t.C) throw new Ui('app-deleted')
          })
        }
        function tc(t) {
          return L(t.providerData, function(t) {
            return t.providerId
          })
        }
        function ec(t, e) {
          e && (nc(t, e.providerId), t.providerData.push(e))
        }
        function nc(t, e) {
          U(t.providerData, function(t) {
            return t.providerId == e
          })
        }
        function ic(t, e, n) {
          ;('uid' != e || n) && t.hasOwnProperty(e) && Si(t, e, n)
        }
        function rc(t, e) {
          t != e &&
            (Ni(t, {
              uid: e.uid,
              displayName: e.displayName,
              photoURL: e.photoURL,
              email: e.email,
              emailVerified: e.emailVerified,
              phoneNumber: e.phoneNumber,
              isAnonymous: e.isAnonymous,
              providerData: [],
            }),
            e.metadata
              ? Si(
                  t,
                  'metadata',
                  (function(t) {
                    return new Vu(t.a, t.b)
                  })(e.metadata),
                )
              : Si(t, 'metadata', new Vu()),
            D(e.providerData, function(e) {
              ec(t, e)
            }),
            (function(t, e) {
              ;(t.b = e.b), (t.a = e.a), (t.c = e.c)
            })(t.h, e.h),
            Si(t, 'refreshToken', t.h.a))
        }
        function oc(t) {
          return t.F().then(function(e) {
            var n = t.isAnonymous
            return (function(t, e) {
              return Ba(t.b, Na, { idToken: e }).then(g(t.wc, t))
            })(t, e).then(function() {
              return n || ic(t, 'isAnonymous', !1), e
            })
          })
        }
        function ac(t, e) {
          e[Zo] &&
            t.qa != e[Zo] &&
            (Mu(t.h, e),
            t.dispatchEvent(new Fu('tokenChanged')),
            zu(t, e[Zo]),
            ic(t, 'refreshToken', t.h.a))
        }
        function sc(t, e) {
          return oc(t).then(function() {
            if (M(tc(t), e))
              return Ju(t).then(function() {
                throw new Ui('provider-already-linked')
              })
          })
        }
        function uc(t, e, n) {
          return _i({
            user: t,
            credential: Vr(e),
            additionalUserInfo: (e = ur(e)),
            operationType: n,
          })
        }
        function cc(t, e) {
          return (
            ac(t, e),
            t.reload().then(function() {
              return t
            })
          )
        }
        function hc(t, n, i, r, o) {
          if (!ui())
            return Et(new Ui('operation-not-supported-in-this-environment'))
          if (t.i && !o) return Et(t.i)
          var a = sr(i.providerId),
            s = vi(t.uid + ':::'),
            u = null
          ;(!fi() || Yn()) &&
            t.u &&
            i.isOAuthProvider &&
            (u = hs(t.u, t.m, t.o, n, i, null, s, e.SDK_VERSION || null))
          var c = Bn(u, a && a.Ba, a && a.Aa)
          return (
            (r = r()
              .then(function() {
                if ((lc(t), !o)) return t.F().then(function() {})
              })
              .then(function() {
                return Tu(t.a, c, n, i, s, !!u)
              })
              .then(function() {
                return new vt(function(e, i) {
                  t.ha(n, null, new Ui('cancelled-popup-request'), t.g || null),
                    (t.f = e),
                    (t.v = i),
                    (t.g = s),
                    (t.c = t.a.Ea(t, n, c, s))
                })
              })
              .then(function(t) {
                return c && Gn(c), t ? _i(t) : null
              })
              .s(function(t) {
                throw (c && Gn(c), t)
              })),
            dc(t, r, o)
          )
        }
        function fc(t, e, n, i, r) {
          if (!ui())
            return Et(new Ui('operation-not-supported-in-this-environment'))
          if (t.i && !r) return Et(t.i)
          var o = null,
            a = vi(t.uid + ':::')
          return (
            (i = i()
              .then(function() {
                if ((lc(t), !r)) return t.F().then(function() {})
              })
              .then(function() {
                return (t.aa = a), Ju(t)
              })
              .then(function(e) {
                return t.ba && (e = (e = t.ba).b.set(vc, t.D(), e.a)), e
              })
              .then(function() {
                return t.a.Ca(e, n, a)
              })
              .s(function(e) {
                if (((o = e), t.ba)) return mc(t.ba)
                throw o
              })
              .then(function() {
                if (o) throw o
              })),
            dc(t, i, r)
          )
        }
        function lc(t) {
          if (!t.a || !t.I) {
            if (t.a && !t.I) throw new Ui('internal-error')
            throw new Ui('auth-domain-config-required')
          }
        }
        function dc(t, e, n) {
          var i = (function(t, e, n) {
            return t.i && !n
              ? (e.cancel(), Et(t.i))
              : e.s(function(e) {
                  throw (!e ||
                    ('auth/user-disabled' != e.code &&
                      'auth/user-token-expired' != e.code) ||
                    (t.i || t.dispatchEvent(new Fu('userInvalidated')),
                    (t.i = e)),
                  e)
                })
          })(t, e, n)
          return (
            t.G.push(i),
            i.ia(function() {
              j(t.G, i)
            }),
            i
          )
        }
        function pc(t) {
          if (!t.apiKey) return null
          var e = {
              apiKey: t.apiKey,
              authDomain: t.authDomain,
              appName: t.appName,
            },
            n = {}
          if (
            !(
              t.stsTokenManager &&
              t.stsTokenManager.accessToken &&
              t.stsTokenManager.expirationTime
            )
          )
            return null
          ;(n[Zo] = t.stsTokenManager.accessToken),
            (n.refreshToken = t.stsTokenManager.refreshToken || null),
            (n.expiresIn = (t.stsTokenManager.expirationTime - y()) / 1e3)
          var i = new Ku(e, n, t)
          return (
            t.providerData &&
              D(t.providerData, function(t) {
                t && ec(i, _i(t))
              }),
            t.redirectEventId && (i.aa = t.redirectEventId),
            i
          )
        }
        ;(Su.prototype.reset = function() {
          ;(this.b = null), this.a && (this.a.cancel(), (this.a = null))
        }),
          (Su.prototype.h = function(t, e) {
            if (t) {
              this.reset(), (this.g = !0)
              var n = t.b,
                i = t.c,
                r = t.a && 'auth/web-storage-unsupported' == t.a.code,
                o =
                  t.a &&
                  'auth/operation-not-supported-in-this-environment' == t.a.code
              ;(this.i = !(!r && !o)),
                'unknown' != n || r || o
                  ? t.a
                    ? (Ou(this, !0, null, t.a), Tt())
                    : e.wa(n, i)
                      ? (function(t, e, n) {
                          n = n.wa(e.b, e.c)
                          var i = e.f,
                            r = e.g,
                            o = !!e.b.match(/Redirect$/)
                          n(i, r)
                            .then(function(e) {
                              Ou(t, o, e, null)
                            })
                            .s(function(e) {
                              Ou(t, o, null, e)
                            })
                        })(this, t, e)
                      : Et(new Ui('invalid-auth-event'))
                  : (Ou(this, !1, null, null), Tt())
            } else Et(new Ui('invalid-auth-event'))
          }),
          (Su.prototype.Xa = function() {
            this.g && !this.i && Ou(this, !1, null, null)
          }),
          (Su.prototype.fa = function() {
            var t = this
            return new vt(function(e, n) {
              t.b
                ? t.b().then(e, n)
                : (t.f.push(e),
                  t.c.push(n),
                  (function(t) {
                    var e = new Ui('timeout')
                    t.a && t.a.cancel(),
                      (t.a = xe(Iu.get()).then(function() {
                        t.b || ((t.g = !0), Ou(t, !0, null, e))
                      }))
                  })(t))
            })
          }),
          (Pu.prototype.h = function(t, e) {
            if (t) {
              var n = t.b,
                i = t.c
              t.a
                ? (e.ha(t.b, null, t.a, t.c), Tt())
                : e.wa(n, i)
                  ? (function(t, e) {
                      var n = t.c,
                        i = t.b
                      e.wa(i, n)(t.f, t.g)
                        .then(function(t) {
                          e.ha(i, t, null, n)
                        })
                        .s(function(t) {
                          e.ha(i, null, t, n)
                        })
                    })(t, e)
                  : Et(new Ui('invalid-auth-event'))
            } else Et(new Ui('invalid-auth-event'))
          }),
          (Cu.prototype.confirm = function(t) {
            return (t = Ur(this.verificationId, t)), this.a(t)
          }),
          (Lu.prototype.start = function() {
            ;(this.a = this.c),
              (function t(e, n) {
                e.stop()
                e.b = xe(
                  (function(t, e) {
                    return e
                      ? ((t.a = t.c), t.g())
                      : ((e = t.a), (t.a *= 2), t.a > t.f && (t.a = t.f), e)
                  })(e, n),
                )
                  .then(function() {
                    return (
                      (t = i.document),
                      (e = null),
                      yi() || !t
                        ? Tt()
                        : new vt(function(n) {
                            ;(e = function() {
                              yi() &&
                                (t.removeEventListener(
                                  'visibilitychange',
                                  e,
                                  !1,
                                ),
                                n())
                            }),
                              t.addEventListener('visibilitychange', e, !1)
                          }).s(function(n) {
                            throw (t.removeEventListener(
                              'visibilitychange',
                              e,
                              !1,
                            ),
                            n)
                          })
                    )
                    var t, e
                  })
                  .then(function() {
                    return e.h()
                  })
                  .then(function() {
                    t(e, !0)
                  })
                  .s(function(n) {
                    e.i(n) && t(e, !1)
                  })
              })(this, !0)
          }),
          (Lu.prototype.stop = function() {
            this.b && (this.b.cancel(), (this.b = null))
          }),
          (xu.prototype.D = function() {
            return {
              apiKey: this.f.b,
              refreshToken: this.a,
              accessToken: this.b,
              expirationTime: this.c,
            }
          }),
          (xu.prototype.getToken = function(t) {
            return (
              (t = !!t),
              this.b && !this.a
                ? Et(new Ui('user-token-expired'))
                : t || !this.b || y() > this.c - 3e4
                  ? this.a
                    ? Uu(this, {
                        grant_type: 'refresh_token',
                        refresh_token: this.a,
                      })
                    : Tt(null)
                  : Tt({
                      accessToken: this.b,
                      expirationTime: this.c,
                      refreshToken: this.a,
                    })
            )
          }),
          (Vu.prototype.D = function() {
            return { lastLoginAt: this.b, createdAt: this.a }
          }),
          w(Fu, se),
          w(Ku, Pe),
          (Ku.prototype.oa = function(t) {
            ;(this.ka = t), ia(this.b, t)
          }),
          (Ku.prototype.ea = function() {
            return this.ka
          }),
          (Ku.prototype.xa = function() {
            return F(this.O)
          }),
          (Ku.prototype.Ha = function() {
            this.w.b && (this.w.stop(), this.w.start())
          }),
          Si(Ku.prototype, 'providerId', 'firebase'),
          ((t = Ku.prototype).reload = function() {
            var t = this
            return dc(
              this,
              Qu(this).then(function() {
                return oc(t)
                  .then(function() {
                    return Ju(t)
                  })
                  .then(Zu)
              }),
            )
          }),
          (t.cc = function(t) {
            return this.F(t).then(function(t) {
              return new function(t) {
                var e = ir(t)
                if (!(e && e.exp && e.auth_time && e.iat))
                  throw new Ui(
                    'internal-error',
                    'An internal error occurred. The token obtained by Firebase appears to be malformed. Please retry the operation.',
                  )
                Ni(this, {
                  token: t,
                  expirationTime: wi(1e3 * e.exp),
                  authTime: wi(1e3 * e.auth_time),
                  issuedAtTime: wi(1e3 * e.iat),
                  signInProvider:
                    e.firebase && e.firebase.sign_in_provider
                      ? e.firebase.sign_in_provider
                      : null,
                  claims: e,
                })
              }(t)
            })
          }),
          (t.F = function(t) {
            var e = this
            return dc(
              this,
              Qu(this)
                .then(function() {
                  return e.h.getToken(t)
                })
                .then(function(t) {
                  if (!t) throw new Ui('internal-error')
                  return (
                    t.accessToken != e.qa &&
                      (zu(e, t.accessToken),
                      e.dispatchEvent(new Fu('tokenChanged'))),
                    ic(e, 'refreshToken', t.refreshToken),
                    t.accessToken
                  )
                }),
            )
          }),
          (t.wc = function(t) {
            if (!(t = t.users) || !t.length) throw new Ui('internal-error')
            $u(this, {
              uid: (t = t[0]).localId,
              displayName: t.displayName,
              photoURL: t.photoUrl,
              email: t.email,
              emailVerified: !!t.emailVerified,
              phoneNumber: t.phoneNumber,
              lastLoginAt: t.lastLoginAt,
              createdAt: t.createdAt,
            })
            for (
              var e = (function(t) {
                  return (t = t.providerUserInfo) && t.length
                    ? L(t, function(t) {
                        return new function(t, e, n, i, r, o) {
                          Ni(this, {
                            uid: t,
                            displayName: i || null,
                            photoURL: r || null,
                            email: n || null,
                            phoneNumber: o || null,
                            providerId: e,
                          })
                        }(
                          t.rawId,
                          t.providerId,
                          t.email,
                          t.displayName,
                          t.photoUrl,
                          t.phoneNumber,
                        )
                      })
                    : []
                })(t),
                n = 0;
              n < e.length;
              n++
            )
              ec(this, e[n])
            ic(
              this,
              'isAnonymous',
              !(
                (this.email && t.passwordHash) ||
                (this.providerData && this.providerData.length)
              ),
            )
          }),
          (t.fb = function(t) {
            var e = this,
              n = null
            return dc(
              this,
              t
                .f(this.b, this.uid)
                .then(function(t) {
                  return (
                    ac(e, t),
                    (n = uc(e, t, 'reauthenticate')),
                    (e.i = null),
                    e.reload()
                  )
                })
                .then(function() {
                  return n
                }),
              !0,
            )
          }),
          (t.xc = function(t) {
            return (
              Ai(
                'firebase.User.prototype.reauthenticateWithCredential is deprecated. Please use firebase.User.prototype.reauthenticateAndRetrieveDataWithCredential instead.',
              ),
              this.fb(t).then(function() {})
            )
          }),
          (t.cb = function(t) {
            var e = this,
              n = null
            return dc(
              this,
              sc(this, t.providerId)
                .then(function() {
                  return e.F()
                })
                .then(function(n) {
                  return t.c(e.b, n)
                })
                .then(function(t) {
                  return (n = uc(e, t, 'link')), cc(e, t)
                })
                .then(function() {
                  return n
                }),
            )
          }),
          (t.oc = function(t) {
            return (
              Ai(
                'firebase.User.prototype.linkWithCredential is deprecated. Please use firebase.User.prototype.linkAndRetrieveDataWithCredential instead.',
              ),
              this.cb(t).then(function(t) {
                return t.user
              })
            )
          }),
          (t.pc = function(t, e) {
            var n = this
            return dc(
              this,
              sc(this, 'phone').then(function() {
                return Du(Gu(n), t, e, g(n.cb, n))
              }),
            )
          }),
          (t.yc = function(t, e) {
            var n = this
            return dc(
              this,
              Tt().then(function() {
                return Du(Gu(n), t, e, g(n.fb, n))
              }),
              !0,
            )
          }),
          (t.ob = function(t) {
            var e = this
            return dc(
              this,
              this.F()
                .then(function(n) {
                  return e.b.ob(n, t)
                })
                .then(function(t) {
                  return ac(e, t), e.reload()
                }),
            )
          }),
          (t.Qc = function(t) {
            var e = this
            return dc(
              this,
              this.F()
                .then(function(n) {
                  return t.c(e.b, n)
                })
                .then(function(t) {
                  return ac(e, t), e.reload()
                }),
            )
          }),
          (t.pb = function(t) {
            var e = this
            return dc(
              this,
              this.F()
                .then(function(n) {
                  return e.b.pb(n, t)
                })
                .then(function(t) {
                  return ac(e, t), e.reload()
                }),
            )
          }),
          (t.qb = function(t) {
            if (void 0 === t.displayName && void 0 === t.photoURL)
              return Qu(this)
            var e = this
            return dc(
              this,
              this.F()
                .then(function(n) {
                  return e.b.qb(n, {
                    displayName: t.displayName,
                    photoUrl: t.photoURL,
                  })
                })
                .then(function(t) {
                  return (
                    ac(e, t),
                    ic(e, 'displayName', t.displayName || null),
                    ic(e, 'photoURL', t.photoUrl || null),
                    D(e.providerData, function(t) {
                      'password' === t.providerId &&
                        (Si(t, 'displayName', e.displayName),
                        Si(t, 'photoURL', e.photoURL))
                    }),
                    Ju(e)
                  )
                })
                .then(Zu),
            )
          }),
          (t.Oc = function(t) {
            var e = this
            return dc(
              this,
              oc(this).then(function(n) {
                return M(tc(e), t)
                  ? (function(t, e, n) {
                      return Ba(t, Aa, { idToken: e, deleteProvider: n })
                    })(e.b, n, [t]).then(function(t) {
                      var n = {}
                      return (
                        D(t.providerUserInfo || [], function(t) {
                          n[t.providerId] = !0
                        }),
                        D(tc(e), function(t) {
                          n[t] || nc(e, t)
                        }),
                        n[jr.PROVIDER_ID] || Si(e, 'phoneNumber', null),
                        Ju(e)
                      )
                    })
                  : Ju(e).then(function() {
                      throw new Ui('no-such-provider')
                    })
              }),
            )
          }),
          (t.delete = function() {
            var t = this
            return dc(
              this,
              this.F()
                .then(function(e) {
                  return Ba(t.b, Ea, { idToken: e })
                })
                .then(function() {
                  t.dispatchEvent(new Fu('userDeleted'))
                }),
            ).then(function() {
              for (var e = 0; e < t.G.length; e++) t.G[e].cancel('app-deleted')
              Hu(t, null),
                Wu(t, null),
                (t.G = []),
                (t.C = !0),
                Xu(t),
                Si(t, 'refreshToken', null),
                t.a && t.a.unsubscribe(t)
            })
          }),
          (t.ub = function(t, e) {
            return !!(
              ('linkViaPopup' == t && (this.g || null) == e && this.f) ||
              ('reauthViaPopup' == t && (this.g || null) == e && this.f) ||
              ('linkViaRedirect' == t && (this.aa || null) == e) ||
              ('reauthViaRedirect' == t && (this.aa || null) == e)
            )
          }),
          (t.ha = function(t, e, n, i) {
            ;('linkViaPopup' != t && 'reauthViaPopup' != t) ||
              i != (this.g || null) ||
              (n && this.v ? this.v(n) : e && !n && this.f && this.f(e),
              this.c && (this.c.cancel(), (this.c = null)),
              delete this.f,
              delete this.v)
          }),
          (t.wa = function(t, e) {
            return 'linkViaPopup' == t && e == (this.g || null)
              ? g(this.zb, this)
              : 'reauthViaPopup' == t && e == (this.g || null)
                ? g(this.Ab, this)
                : 'linkViaRedirect' == t && (this.aa || null) == e
                  ? g(this.zb, this)
                  : 'reauthViaRedirect' == t && (this.aa || null) == e
                    ? g(this.Ab, this)
                    : null
          }),
          (t.qc = function(t) {
            var e = this
            return hc(
              this,
              'linkViaPopup',
              t,
              function() {
                return sc(e, t.providerId).then(function() {
                  return Ju(e)
                })
              },
              !1,
            )
          }),
          (t.zc = function(t) {
            return hc(
              this,
              'reauthViaPopup',
              t,
              function() {
                return Tt()
              },
              !0,
            )
          }),
          (t.rc = function(t) {
            var e = this
            return fc(
              this,
              'linkViaRedirect',
              t,
              function() {
                return sc(e, t.providerId)
              },
              !1,
            )
          }),
          (t.Ac = function(t) {
            return fc(
              this,
              'reauthViaRedirect',
              t,
              function() {
                return Tt()
              },
              !0,
            )
          }),
          (t.zb = function(t, e) {
            var n = this
            this.c && (this.c.cancel(), (this.c = null))
            var i = null
            return dc(
              this,
              this.F()
                .then(function(i) {
                  return ma(n.b, { requestUri: t, sessionId: e, idToken: i })
                })
                .then(function(t) {
                  return (i = uc(n, t, 'link')), cc(n, t)
                })
                .then(function() {
                  return i
                }),
            )
          }),
          (t.Ab = function(t, e) {
            var n = this
            this.c && (this.c.cancel(), (this.c = null))
            var i = null
            return dc(
              this,
              Tt()
                .then(function() {
                  return br(ga(n.b, { requestUri: t, sessionId: e }), n.uid)
                })
                .then(function(t) {
                  return (
                    (i = uc(n, t, 'reauthenticate')),
                    ac(n, t),
                    (n.i = null),
                    n.reload()
                  )
                })
                .then(function() {
                  return i
                }),
              !0,
            )
          }),
          (t.ib = function(t) {
            var e = this,
              n = null
            return dc(
              this,
              this.F()
                .then(function(e) {
                  return (n = e), void 0 === t || rt(t) ? {} : $i(new Hi(t))
                })
                .then(function(t) {
                  return e.b.ib(n, t)
                })
                .then(function(t) {
                  if (e.email != t) return e.reload()
                })
                .then(function() {}),
            )
          }),
          (t.toJSON = function() {
            return this.D()
          }),
          (t.D = function() {
            var t = {
              uid: this.uid,
              displayName: this.displayName,
              photoURL: this.photoURL,
              email: this.email,
              emailVerified: this.emailVerified,
              phoneNumber: this.phoneNumber,
              isAnonymous: this.isAnonymous,
              providerData: [],
              apiKey: this.m,
              appName: this.o,
              authDomain: this.u,
              stsTokenManager: this.h.D(),
              redirectEventId: this.aa || null,
            }
            return (
              this.metadata && ct(t, this.metadata.D()),
              D(this.providerData, function(e) {
                t.providerData.push(
                  (function(t) {
                    var e,
                      n = {}
                    for (e in t) t.hasOwnProperty(e) && (n[e] = t[e])
                    return n
                  })(e),
                )
              }),
              t
            )
          })
        var vc = { name: 'redirectUser', A: 'session' }
        function mc(t) {
          return Xs(t.b, vc, t.a)
        }
        function gc(t) {
          ;(this.a = t),
            (this.b = Ws()),
            (this.c = null),
            (this.f = (function(t) {
              var e = wc('local'),
                n = wc('session'),
                r = wc('none')
              return (function(t, e, n) {
                var r = Bs(e, n),
                  o = Gs(t, e.A)
                return t.get(e, n).then(function(a) {
                  var s = null
                  try {
                    s = pi(i.localStorage.getItem(r))
                  } catch (t) {}
                  if (s && !a)
                    return i.localStorage.removeItem(r), t.set(e, s, n)
                  s &&
                    a &&
                    'localStorage' != o.type &&
                    i.localStorage.removeItem(r)
                })
              })(t.b, e, t.a)
                .then(function() {
                  return t.b.get(n, t.a)
                })
                .then(function(i) {
                  return i
                    ? n
                    : t.b.get(r, t.a).then(function(n) {
                        return n
                          ? r
                          : t.b.get(e, t.a).then(function(n) {
                              return n
                                ? e
                                : t.b.get(yc, t.a).then(function(t) {
                                    return t ? wc(t) : e
                                  })
                            })
                      })
                })
                .then(function(e) {
                  return (t.c = e), bc(t, e.A)
                })
                .s(function() {
                  t.c || (t.c = e)
                })
            })(this)),
            this.b.addListener(wc('local'), this.a, g(this.g, this))
        }
        function bc(t, e) {
          var n,
            i = []
          for (n in Hs) Hs[n] !== e && i.push(Xs(t.b, wc(Hs[n]), t.a))
          return (
            i.push(Xs(t.b, yc, t.a)),
            (function(t) {
              return new vt(function(e, n) {
                var i = t.length,
                  r = []
                if (i)
                  for (
                    var o = function(t, n) {
                        i--, (r[t] = n), 0 == i && e(r)
                      },
                      a = function(t) {
                        n(t)
                      },
                      s = 0;
                    s < t.length;
                    s++
                  )
                    At(t[s], b(o, s), a)
                else e(r)
              })
            })(i)
          )
        }
        gc.prototype.g = function() {
          var t = this,
            e = wc('local')
          Ac(this, function() {
            return Tt()
              .then(function() {
                return t.c && 'local' != t.c.A ? t.b.get(e, t.a) : null
              })
              .then(function(n) {
                if (n)
                  return bc(t, 'local').then(function() {
                    t.c = e
                  })
              })
          })
        }
        var yc = { name: 'persistence', A: 'session' }
        function wc(t) {
          return { name: 'authUser', A: t }
        }
        function Ic(t, e) {
          return Ac(t, function() {
            return t.b.set(t.c, e.D(), t.a)
          })
        }
        function Tc(t) {
          return Ac(t, function() {
            return Xs(t.b, t.c, t.a)
          })
        }
        function Ec(t, e) {
          return Ac(t, function() {
            return t.b.get(t.c, t.a).then(function(t) {
              return t && e && (t.authDomain = e), pc(t || {})
            })
          })
        }
        function Ac(t, e) {
          return (t.f = t.f.then(e, e)), t.f
        }
        function kc(t) {
          if (
            ((this.m = !1),
            Si(this, 'settings', new Ru()),
            Si(this, 'app', t),
            !Cc(this).options || !Cc(this).options.apiKey)
          )
            throw new Ui('invalid-api-key')
          ;(t = e.SDK_VERSION ? ri(e.SDK_VERSION) : null),
            (this.b = new Yo(
              Cc(this).options && Cc(this).options.apiKey,
              $a(Ja),
              t,
            )),
            (this.N = []),
            (this.o = []),
            (this.I = []),
            (this.Sb = e.INTERNAL.createSubscribe(g(this.kc, this))),
            (this.O = void 0),
            (this.Tb = e.INTERNAL.createSubscribe(g(this.mc, this))),
            Pc(this, null),
            (this.h = new gc(Cc(this).options.apiKey + ':' + Cc(this).name)),
            (this.w = new function(t) {
              ;(this.a = t), (this.b = Ws())
            }(Cc(this).options.apiKey + ':' + Cc(this).name)),
            (this.V = Mc(
              this,
              (function(t) {
                var e = Cc(t).options.authDomain,
                  n = (function(t) {
                    var e = (function(t, e) {
                      return t.b.get(vc, t.a).then(function(t) {
                        return t && e && (t.authDomain = e), pc(t || {})
                      })
                    })(t.w, Cc(t).options.authDomain).then(function(e) {
                      return (t.C = e) && (e.ba = t.w), mc(t.w)
                    })
                    return Mc(t, e)
                  })(t)
                    .then(function() {
                      return Ec(t.h, e)
                    })
                    .then(function(e) {
                      return e
                        ? ((e.ba = t.w),
                          t.C && (t.C.aa || null) == (e.aa || null)
                            ? e
                            : e
                                .reload()
                                .then(function() {
                                  return Ic(t.h, e).then(function() {
                                    return e
                                  })
                                })
                                .s(function(n) {
                                  return 'auth/network-request-failed' == n.code
                                    ? e
                                    : Tc(t.h)
                                }))
                        : null
                    })
                    .then(function(e) {
                      Pc(t, e || null)
                    })
                return Mc(t, n)
              })(this),
            )),
            (this.i = Mc(
              this,
              (function(t) {
                return t.V.then(function() {
                  return t.fa()
                })
                  .s(function() {})
                  .then(function() {
                    if (!t.m) return t.ka()
                  })
                  .s(function() {})
                  .then(function() {
                    if (!t.m) {
                      t.X = !0
                      var e = t.h
                      e.b.addListener(wc('local'), e.a, t.ka)
                    }
                  })
              })(this),
            )),
            (this.X = !1),
            (this.ka = g(this.Lc, this)),
            (this.Ha = g(this.Z, this)),
            (this.qa = g(this.ac, this)),
            (this.ra = g(this.ic, this)),
            (this.sa = g(this.jc, this)),
            (function(t) {
              var e = Cc(t).options.authDomain,
                n = Cc(t).options.apiKey
              e &&
                ui() &&
                (t.Rb = t.V.then(function() {
                  if (!t.m) {
                    if (
                      ((t.a = ku(e, n, Cc(t).name)),
                      t.a.subscribe(t),
                      Dc(t) && Yu(Dc(t)),
                      t.C)
                    ) {
                      Yu(t.C)
                      var i = t.C
                      i.oa(t.ea()),
                        Hu(i, t),
                        qu((i = t.C), t.G),
                        Wu(i, t),
                        (t.C = null)
                    }
                    return t.a
                  }
                }))
            })(this),
            (this.INTERNAL = {}),
            (this.INTERNAL.delete = g(this.delete, this)),
            (this.INTERNAL.logFramework = g(this.sc, this)),
            (this.u = 0),
            Pe.call(this),
            (function(t) {
              Object.defineProperty(t, 'lc', {
                get: function() {
                  return this.ea()
                },
                set: function(t) {
                  this.oa(t)
                },
                enumerable: !1,
              }),
                (t.W = null)
            })(this),
            (this.G = [])
        }
        function Sc(t) {
          se.call(this, 'languageCodeChanged'), (this.g = t)
        }
        function Nc(t) {
          se.call(this, 'frameworkChanged'), (this.c = t)
        }
        function _c(t) {
          return t.Rb || Et(new Ui('auth-domain-config-required'))
        }
        function Oc(t, e) {
          var n = {}
          return (
            (n.apiKey = Cc(t).options.apiKey),
            (n.authDomain = Cc(t).options.authDomain),
            (n.appName = Cc(t).name),
            t.V.then(function() {
              return (function(t, e, n, i) {
                var r = new Ku(t, e)
                return (
                  n && (r.ba = n),
                  i && qu(r, i),
                  r.reload().then(function() {
                    return r
                  })
                )
              })(n, e, t.w, t.xa())
            })
              .then(function(e) {
                return Dc(t) && e.uid == Dc(t).uid
                  ? (rc(Dc(t), e), t.Z(e))
                  : (Pc(t, e), Yu(e), t.Z(e))
              })
              .then(function() {
                xc(t)
              })
          )
        }
        function Pc(t, e) {
          Dc(t) &&
            ((function(t, e) {
              U(t.N, function(t) {
                return t == e
              })
            })(Dc(t), t.Ha),
            Ie(Dc(t), 'tokenChanged', t.qa),
            Ie(Dc(t), 'userDeleted', t.ra),
            Ie(Dc(t), 'userInvalidated', t.sa),
            Xu(Dc(t))),
            e &&
              (e.N.push(t.Ha),
              be(e, 'tokenChanged', t.qa),
              be(e, 'userDeleted', t.ra),
              be(e, 'userInvalidated', t.sa),
              0 < t.u && Bu(e)),
            Si(t, 'currentUser', e),
            e && (e.oa(t.ea()), Hu(e, t), qu(e, t.G), Wu(e, t))
        }
        function Rc(t, e) {
          var n = null,
            i = null
          return Mc(
            t,
            e
              .then(function(e) {
                return (n = Vr(e)), (i = ur(e)), Oc(t, e)
              })
              .then(function() {
                return _i({
                  user: Dc(t),
                  credential: n,
                  additionalUserInfo: i,
                  operationType: 'signIn',
                })
              }),
          )
        }
        function Cc(t) {
          return t.app
        }
        function Dc(t) {
          return t.currentUser
        }
        function Lc(t) {
          return (Dc(t) && Dc(t)._lat) || null
        }
        function xc(t) {
          if (t.X) {
            for (var e = 0; e < t.o.length; e++) t.o[e] && t.o[e](Lc(t))
            if (t.O !== t.getUid() && t.I.length)
              for (t.O = t.getUid(), e = 0; e < t.I.length; e++)
                t.I[e] && t.I[e](Lc(t))
          }
        }
        function Mc(t, e) {
          return (
            t.N.push(e),
            e.ia(function() {
              j(t.N, e)
            }),
            e
          )
        }
        function jc() {}
        function Uc() {
          ;(this.a = {}), (this.b = 1e12)
        }
        ;(gc.prototype.lb = function(t) {
          var e = null,
            n = this
          return (
            (function(t) {
              var e = new Ui('invalid-persistence-type'),
                n = new Ui('unsupported-persistence-type')
              t: {
                for (i in Hs)
                  if (Hs[i] == t) {
                    var i = !0
                    break t
                  }
                i = !1
              }
              if (!i || 'string' != typeof t) throw e
              switch (Zn()) {
                case 'ReactNative':
                  if ('session' === t) throw n
                  break
                case 'Node':
                  if ('none' !== t) throw n
                  break
                default:
                  if (!si() && 'none' !== t) throw n
              }
            })(t),
            Ac(this, function() {
              return t != n.c.A
                ? n.b
                    .get(n.c, n.a)
                    .then(function(i) {
                      return (e = i), bc(n, t)
                    })
                    .then(function() {
                      if (((n.c = wc(t)), e)) return n.b.set(n.c, e, n.a)
                    })
                : Tt()
            })
          )
        }),
          w(kc, Pe),
          w(Sc, se),
          w(Nc, se),
          ((t = kc.prototype).lb = function(t) {
            return Mc(this, (t = this.h.lb(t)))
          }),
          (t.oa = function(t) {
            this.W === t ||
              this.m ||
              ((this.W = t),
              ia(this.b, this.W),
              this.dispatchEvent(new Sc(this.ea())))
          }),
          (t.ea = function() {
            return this.W
          }),
          (t.Rc = function() {
            var t = i.navigator
            this.oa(
              (t &&
                ((t.languages && t.languages[0]) ||
                  t.language ||
                  t.userLanguage)) ||
                null,
            )
          }),
          (t.sc = function(t) {
            this.G.push(t),
              ra(this.b, e.SDK_VERSION ? ri(e.SDK_VERSION, this.G) : null),
              this.dispatchEvent(new Nc(this.G))
          }),
          (t.xa = function() {
            return F(this.G)
          }),
          (t.toJSON = function() {
            return {
              apiKey: Cc(this).options.apiKey,
              authDomain: Cc(this).options.authDomain,
              appName: Cc(this).name,
              currentUser: Dc(this) && Dc(this).D(),
            }
          }),
          (t.ub = function(t, e) {
            switch (t) {
              case 'unknown':
              case 'signInViaRedirect':
                return !0
              case 'signInViaPopup':
                return this.g == e && !!this.f
              default:
                return !1
            }
          }),
          (t.ha = function(t, e, n, i) {
            'signInViaPopup' == t &&
              this.g == i &&
              (n && this.v ? this.v(n) : e && !n && this.f && this.f(e),
              this.c && (this.c.cancel(), (this.c = null)),
              delete this.f,
              delete this.v)
          }),
          (t.wa = function(t, e) {
            return 'signInViaRedirect' == t ||
              ('signInViaPopup' == t && this.g == e && this.f)
              ? g(this.$b, this)
              : null
          }),
          (t.$b = function(t, e) {
            var n = this
            ;(t = { requestUri: t, sessionId: e }),
              this.c && (this.c.cancel(), (this.c = null))
            var i = null,
              r = null,
              o = va(n.b, t).then(function(t) {
                return (i = Vr(t)), (r = ur(t)), t
              })
            return Mc(
              this,
              (t = n.V.then(function() {
                return o
              })
                .then(function(t) {
                  return Oc(n, t)
                })
                .then(function() {
                  return _i({
                    user: Dc(n),
                    credential: i,
                    additionalUserInfo: r,
                    operationType: 'signIn',
                  })
                })),
            )
          }),
          (t.Jc = function(t) {
            if (!ui())
              return Et(new Ui('operation-not-supported-in-this-environment'))
            var n = this,
              i = sr(t.providerId),
              r = vi(),
              o = null
            ;(!fi() || Yn()) &&
              Cc(this).options.authDomain &&
              t.isOAuthProvider &&
              (o = hs(
                Cc(this).options.authDomain,
                Cc(this).options.apiKey,
                Cc(this).name,
                'signInViaPopup',
                t,
                null,
                r,
                e.SDK_VERSION || null,
              ))
            var a = Bn(o, i && i.Ba, i && i.Aa)
            return Mc(
              this,
              (i = _c(this)
                .then(function(e) {
                  return Tu(e, a, 'signInViaPopup', t, r, !!o)
                })
                .then(function() {
                  return new vt(function(t, e) {
                    n.ha(
                      'signInViaPopup',
                      null,
                      new Ui('cancelled-popup-request'),
                      n.g,
                    ),
                      (n.f = t),
                      (n.v = e),
                      (n.g = r),
                      (n.c = n.a.Ea(n, 'signInViaPopup', a, r))
                  })
                })
                .then(function(t) {
                  return a && Gn(a), t ? _i(t) : null
                })
                .s(function(t) {
                  throw (a && Gn(a), t)
                })),
            )
          }),
          (t.Kc = function(t) {
            if (!ui())
              return Et(new Ui('operation-not-supported-in-this-environment'))
            var e = this
            return Mc(
              this,
              _c(this)
                .then(function() {
                  return Ac((t = e.h), function() {
                    return t.b.set(yc, t.c.A, t.a)
                  })
                  var t
                })
                .then(function() {
                  return e.a.Ca('signInViaRedirect', t)
                }),
            )
          }),
          (t.fa = function() {
            if (!ui())
              return Et(new Ui('operation-not-supported-in-this-environment'))
            var t = this
            return Mc(
              this,
              _c(this)
                .then(function() {
                  return t.a.fa()
                })
                .then(function(t) {
                  return t ? _i(t) : null
                }),
            )
          }),
          (t.Pc = function(t) {
            if (!t) return Et(new Ui('null-user'))
            var e = this,
              n = {}
            ;(n.apiKey = Cc(this).options.apiKey),
              (n.authDomain = Cc(this).options.authDomain),
              (n.appName = Cc(this).name)
            var i = (function(t, e, n, i) {
              e = e || { apiKey: t.m, authDomain: t.u, appName: t.o }
              var r = t.h,
                o = {}
              return (
                (o[Zo] = r.b),
                (o.refreshToken = r.a),
                (o.expiresIn = (r.c - y()) / 1e3),
                (e = new Ku(e, o)),
                n && (e.ba = n),
                i && qu(e, i),
                rc(e, t),
                e
              )
            })(t, n, e.w, e.xa())
            return Mc(
              this,
              this.i
                .then(function() {
                  if (Cc(e).options.apiKey != t.m) return i.reload()
                })
                .then(function() {
                  return Dc(e) && t.uid == Dc(e).uid
                    ? (rc(Dc(e), t), e.Z(t))
                    : (Pc(e, i), Yu(i), e.Z(i))
                })
                .then(function() {
                  xc(e)
                }),
            )
          }),
          (t.mb = function() {
            var t = this
            return Mc(
              this,
              this.i.then(function() {
                return Dc(t)
                  ? (Pc(t, null),
                    Tc(t.h).then(function() {
                      xc(t)
                    }))
                  : Tt()
              }),
            )
          }),
          (t.Lc = function() {
            var t = this
            return Ec(this.h, Cc(this).options.authDomain).then(function(e) {
              if (!t.m) {
                var n
                if ((n = Dc(t) && e)) {
                  n = Dc(t).uid
                  var i = e.uid
                  n =
                    void 0 !== n &&
                    null !== n &&
                    '' !== n &&
                    void 0 !== i &&
                    null !== i &&
                    '' !== i &&
                    n == i
                }
                if (n) return rc(Dc(t), e), Dc(t).F()
                ;(Dc(t) || e) &&
                  (Pc(t, e),
                  e && (Yu(e), (e.ba = t.w)),
                  t.a && t.a.subscribe(t),
                  xc(t))
              }
            })
          }),
          (t.Z = function(t) {
            return Ic(this.h, t)
          }),
          (t.ac = function() {
            xc(this), this.Z(Dc(this))
          }),
          (t.ic = function() {
            this.mb()
          }),
          (t.jc = function() {
            this.mb()
          }),
          (t.kc = function(t) {
            var e = this
            this.addAuthTokenListener(function() {
              t.next(Dc(e))
            })
          }),
          (t.mc = function(t) {
            var e = this
            !(function(t, e) {
              t.I.push(e),
                Mc(
                  t,
                  t.i.then(function() {
                    !t.m &&
                      M(t.I, e) &&
                      t.O !== t.getUid() &&
                      ((t.O = t.getUid()), e(Lc(t)))
                  }),
                )
            })(this, function() {
              t.next(Dc(e))
            })
          }),
          (t.uc = function(t, n, i) {
            var r = this
            return (
              this.X &&
                e.Promise.resolve().then(function() {
                  f(t) ? t(Dc(r)) : f(t.next) && t.next(Dc(r))
                }),
              this.Sb(t, n, i)
            )
          }),
          (t.tc = function(t, n, i) {
            var r = this
            return (
              this.X &&
                e.Promise.resolve().then(function() {
                  ;(r.O = r.getUid()),
                    f(t) ? t(Dc(r)) : f(t.next) && t.next(Dc(r))
                }),
              this.Tb(t, n, i)
            )
          }),
          (t.bc = function(t) {
            var e = this
            return Mc(
              this,
              this.i.then(function() {
                return Dc(e)
                  ? Dc(e)
                      .F(t)
                      .then(function(t) {
                        return { accessToken: t }
                      })
                  : null
              }),
            )
          }),
          (t.Mb = function(t) {
            var e = this
            return this.i
              .then(function() {
                return Rc(e, Ba(e.b, Ka, { token: t }))
              })
              .then(function(t) {
                var n = t.user
                return ic(n, 'isAnonymous', !1), e.Z(n), t
              })
          }),
          (t.Dc = function(t) {
            return (
              Ai(
                'firebase.auth.Auth.prototype.signInAndRetrieveDataWithCustomToken is deprecated. Please use firebase.auth.Auth.prototype.signInWithCustomToken instead.',
              ),
              this.Mb(t)
            )
          }),
          (t.Ec = function(t, e) {
            return (
              Ai(
                'firebase.auth.Auth.prototype.signInAndRetrieveDataWithEmailAndPassword is deprecated. Please use firebase.auth.Auth.prototype.signInWithEmailAndPassword instead.',
              ),
              this.Nb(t, e)
            )
          }),
          (t.Nb = function(t, e) {
            var n = this
            return this.i.then(function() {
              return Rc(n, Ba(n.b, Ha, { email: t, password: e }))
            })
          }),
          (t.wb = function(t, e) {
            var n = this
            return this.i.then(function() {
              return Rc(n, Ba(n.b, Ia, { email: t, password: e }))
            })
          }),
          (t.Vb = function(t, e) {
            return (
              Ai(
                'firebase.auth.Auth.prototype.createUserAndRetrieveDataWithEmailAndPassword is deprecated. Please use firebase.auth.Auth.prototype.createUserWithEmailAndPassword instead.',
              ),
              this.wb(t, e)
            )
          }),
          (t.Gc = function(t) {
            return (
              Ai(
                'firebase.auth.Auth.prototype.signInWithCredential is deprecated. Please use firebase.auth.Auth.prototype.signInAndRetrieveDataWithCredential instead.',
              ),
              this.Oa(t).then(function(t) {
                return t.user
              })
            )
          }),
          (t.Oa = function(t) {
            var e = this
            return this.i.then(function() {
              return Rc(e, t.ya(e.b))
            })
          }),
          (t.Pa = function() {
            var t = this
            return this.i.then(function() {
              var e = Dc(t)
              return e && e.isAnonymous
                ? _i({
                    user: e,
                    credential: null,
                    additionalUserInfo: _i({ providerId: null, isNewUser: !1 }),
                    operationType: 'signIn',
                  })
                : Rc(t, t.b.Pa()).then(function(e) {
                    var n = e.user
                    return ic(n, 'isAnonymous', !0), t.Z(n), e
                  })
            })
          }),
          (t.Fc = function() {
            return (
              Ai(
                'firebase.auth.Auth.prototype.signInAnonymouslyAndRetrieveData is deprecated. Please use firebase.auth.Auth.prototype.signInAnonymously instead.',
              ),
              this.Pa()
            )
          }),
          (t.getUid = function() {
            return (Dc(this) && Dc(this).uid) || null
          }),
          (t.Ub = function(t) {
            this.addAuthTokenListener(t),
              this.u++,
              0 < this.u && Dc(this) && Bu(Dc(this))
          }),
          (t.Bc = function(t) {
            var e = this
            D(this.o, function(n) {
              n == t && e.u--
            }),
              0 > this.u && (this.u = 0),
              0 == this.u && Dc(this) && Xu(Dc(this)),
              this.removeAuthTokenListener(t)
          }),
          (t.addAuthTokenListener = function(t) {
            var e = this
            this.o.push(t),
              Mc(
                this,
                this.i.then(function() {
                  e.m || (M(e.o, t) && t(Lc(e)))
                }),
              )
          }),
          (t.removeAuthTokenListener = function(t) {
            U(this.o, function(e) {
              return e == t
            })
          }),
          (t.delete = function() {
            this.m = !0
            for (var t = 0; t < this.N.length; t++)
              this.N[t].cancel('app-deleted')
            return (
              (this.N = []),
              this.h &&
                (t = this.h).b.removeListener(wc('local'), t.a, this.ka),
              this.a && (this.a.unsubscribe(this), this.a.Xa()),
              e.Promise.resolve()
            )
          }),
          (t.Yb = function(t) {
            return (
              Ai(
                'firebase.auth.Auth.prototype.fetchProvidersForEmail is deprecated. Please use firebase.auth.Auth.prototype.fetchSignInMethodsForEmail instead.',
              ),
              Mc(
                this,
                (function(t, e) {
                  return Ba(t, Ta, {
                    identifier: e,
                    continueUri: ci() ? Hn() : 'http://localhost',
                  }).then(function(t) {
                    return t.allProviders || []
                  })
                })(this.b, t),
              )
            )
          }),
          (t.Zb = function(t) {
            return Mc(
              this,
              (function(t, e) {
                return Ba(t, Ta, {
                  identifier: e,
                  continueUri: ci() ? Hn() : 'http://localhost',
                }).then(function(t) {
                  return t.signinMethods || []
                })
              })(this.b, t),
            )
          }),
          (t.nc = function(t) {
            return !!Lr(t)
          }),
          (t.kb = function(t, e) {
            var n = this
            return Mc(
              this,
              Tt()
                .then(function() {
                  var t = new Hi(e)
                  if (!t.c)
                    throw new Ui(
                      'argument-error',
                      Wi + ' must be true when sending sign in link to email',
                    )
                  return $i(t)
                })
                .then(function(e) {
                  return n.b.kb(t, e)
                })
                .then(function() {}),
            )
          }),
          (t.Sc = function(t) {
            return this.Ja(t).then(function(t) {
              return t.data.email
            })
          }),
          (t.Ya = function(t, e) {
            return Mc(this, this.b.Ya(t, e).then(function() {}))
          }),
          (t.Ja = function(t) {
            return Mc(
              this,
              this.b.Ja(t).then(function(t) {
                return new function(t) {
                  var e = {},
                    n = t[Ri],
                    i = t[Ci]
                  if (!(t = t[Di]) || (t != Pi && !n))
                    throw Error('Invalid provider user info!')
                  ;(e[xi] = i || null),
                    (e[Li] = n || null),
                    Si(this, ji, t),
                    Si(this, Mi, Oi(e))
                }(t)
              }),
            )
          }),
          (t.Va = function(t) {
            return Mc(this, this.b.Va(t).then(function() {}))
          }),
          (t.jb = function(t, e) {
            var n = this
            return Mc(
              this,
              Tt()
                .then(function() {
                  return void 0 === e || rt(e) ? {} : $i(new Hi(e))
                })
                .then(function(e) {
                  return n.b.jb(t, e)
                })
                .then(function() {}),
            )
          }),
          (t.Ic = function(t, e) {
            return Mc(this, Du(this, t, e, g(this.Oa, this)))
          }),
          (t.Hc = function(t, e) {
            var n = this
            return Mc(
              this,
              Tt().then(function() {
                var i = Dr(t, e || Hn())
                return n.Oa(i)
              }),
            )
          }),
          (jc.prototype.render = function() {}),
          (jc.prototype.reset = function() {}),
          (jc.prototype.getResponse = function() {}),
          (jc.prototype.execute = function() {})
        var Vc = null
        function Fc(t, e) {
          return ((e = Kc(e)) && t.a[e]) || null
        }
        function Kc(t) {
          return (t = void 0 === t ? 1e12 : t) ? t.toString() : null
        }
        function Hc(t, e) {
          ;(this.g = !1),
            (this.c = e),
            (this.a = this.b = null),
            (this.h = 'invisible' !== this.c.size),
            (this.f = Cn(t))
          var n = this
          ;(this.i = function() {
            n.execute()
          }),
            this.h ? this.execute() : be(this.f, 'click', this.i)
        }
        function qc(t) {
          if (t.g) throw Error('reCAPTCHA mock was already deleted!')
        }
        function Wc() {}
        ;(Uc.prototype.render = function(t, e) {
          return (this.a[this.b.toString()] = new Hc(t, e)), this.b++
        }),
          (Uc.prototype.reset = function(t) {
            var e = Fc(this, t)
            ;(t = Kc(t)), e && t && (e.delete(), delete this.a[t])
          }),
          (Uc.prototype.getResponse = function(t) {
            return (t = Fc(this, t)) ? t.getResponse() : null
          }),
          (Uc.prototype.execute = function(t) {
            ;(t = Fc(this, t)) && t.execute()
          }),
          (Hc.prototype.getResponse = function() {
            return qc(this), this.b
          }),
          (Hc.prototype.execute = function() {
            qc(this)
            var t = this
            this.a ||
              (this.a = setTimeout(function() {
                t.b = (function() {
                  for (var t = 50, e = []; 0 < t; )
                    e.push(
                      '1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'.charAt(
                        Math.floor(62 * Math.random()),
                      ),
                    ),
                      t--
                  return e.join('')
                })()
                var e = t.c.callback,
                  n = t.c['expired-callback']
                if (e)
                  try {
                    e(t.b)
                  } catch (t) {}
                t.a = setTimeout(function() {
                  if (((t.a = null), (t.b = null), n))
                    try {
                      n()
                    } catch (t) {}
                  t.h && t.execute()
                }, 6e4)
              }, 500))
          }),
          (Hc.prototype.delete = function() {
            qc(this),
              (this.g = !0),
              clearTimeout(this.a),
              (this.a = null),
              Ie(this.f, 'click', this.i)
          }),
          (Wc.prototype.g = function() {
            return Vc || (Vc = new Uc()), Tt(Vc)
          }),
          (Wc.prototype.c = function() {})
        var Gc = null
        function Bc() {
          ;(this.b = i.grecaptcha ? 1 / 0 : 0),
            (this.f = null),
            (this.a = '__rcb' + Math.floor(1e6 * Math.random()).toString())
        }
        var Xc = mn(
            'https://www.google.com/recaptcha/api.js?onload=%{onload}&render=explicit&hl=%{hl}',
          ),
          zc = new bi(3e4, 6e4)
        ;(Bc.prototype.g = function(t) {
          var e = this
          return new vt(function(n, r) {
            var o = setTimeout(function() {
              r(new Ui('network-request-failed'))
            }, zc.get())
            !i.grecaptcha || (t !== e.f && !e.b)
              ? ((i[e.a] = function() {
                  if (i.grecaptcha) {
                    e.f = t
                    var a = i.grecaptcha.render
                    ;(i.grecaptcha.render = function(t, n) {
                      return (t = a(t, n)), e.b++, t
                    }),
                      clearTimeout(o),
                      n(i.grecaptcha)
                  } else clearTimeout(o), r(new Ui('internal-error'))
                  delete i[e.a]
                }),
                Tt(qo(yn(Xc, { onload: e.a, hl: t || '' }))).s(function() {
                  clearTimeout(o),
                    r(
                      new Ui(
                        'internal-error',
                        'Unable to load external reCAPTCHA dependencies!',
                      ),
                    )
                }))
              : (clearTimeout(o), n(i.grecaptcha))
          })
        }),
          (Bc.prototype.c = function() {
            this.b--
          })
        var Jc = null
        function Yc(t, e, n, r, o, a, s) {
          if (
            (Si(this, 'type', 'recaptcha'),
            (this.c = this.f = null),
            (this.C = !1),
            (this.m = e),
            (this.g = null),
            s
              ? (Gc || (Gc = new Wc()), (s = Gc))
              : (Jc || (Jc = new Bc()), (s = Jc)),
            (this.o = s),
            (this.a = n || { theme: 'light', type: 'image' }),
            (this.h = []),
            this.a[Qc])
          )
            throw new Ui(
              'argument-error',
              'sitekey should not be provided for reCAPTCHA as one is automatically provisioned for the current project.',
            )
          if (((this.i = 'invisible' === this.a[th]), !i.document))
            throw new Ui(
              'operation-not-supported-in-this-environment',
              'RecaptchaVerifier is only supported in a browser HTTP/HTTPS environment with DOM support.',
            )
          if (!Cn(e) || (!this.i && Cn(e).hasChildNodes()))
            throw new Ui(
              'argument-error',
              'reCAPTCHA container is either not found or already contains inner elements!',
            )
          ;(this.u = new Yo(t, a || null, o || null)),
            (this.v =
              r ||
              function() {
                return null
              })
          var u = this
          this.l = []
          var c = this.a[$c]
          this.a[$c] = function(t) {
            if ((eh(u, t), 'function' == typeof c)) c(t)
            else if ('string' == typeof c) {
              var e = ai(c, i)
              'function' == typeof e && e(t)
            }
          }
          var h = this.a[Zc]
          this.a[Zc] = function() {
            if ((eh(u, null), 'function' == typeof h)) h()
            else if ('string' == typeof h) {
              var t = ai(h, i)
              'function' == typeof t && t()
            }
          }
        }
        var $c = 'callback',
          Zc = 'expired-callback',
          Qc = 'sitekey',
          th = 'size'
        function eh(t, e) {
          for (var n = 0; n < t.l.length; n++)
            try {
              t.l[n](e)
            } catch (t) {}
        }
        function nh(t, e) {
          return (
            t.h.push(e),
            e.ia(function() {
              j(t.h, e)
            }),
            e
          )
        }
        function ih(t) {
          if (t.C)
            throw new Ui(
              'internal-error',
              'RecaptchaVerifier instance has been destroyed.',
            )
        }
        function rh(t, n, i) {
          var r = !1
          try {
            this.b = i || e.app()
          } catch (t) {
            throw new Ui(
              'argument-error',
              'No firebase.app.App instance is currently initialized.',
            )
          }
          if (!this.b.options || !this.b.options.apiKey)
            throw new Ui('invalid-api-key')
          i = this.b.options.apiKey
          var o = this,
            a = null
          try {
            a = this.b.auth().xa()
          } catch (t) {}
          try {
            r = this.b.auth().settings.appVerificationDisabledForTesting
          } catch (t) {}
          ;(a = e.SDK_VERSION ? ri(e.SDK_VERSION, a) : null),
            Yc.call(
              this,
              i,
              t,
              n,
              function() {
                try {
                  var t = o.b.auth().ea()
                } catch (e) {
                  t = null
                }
                return t
              },
              a,
              $a(Ja),
              r,
            )
        }
        function oh(t, e, n, i) {
          t: {
            n = Array.prototype.slice.call(n)
            for (var r = 0, o = !1, a = 0; a < e.length; a++)
              if (e[a].optional) o = !0
              else {
                if (o)
                  throw new Ui(
                    'internal-error',
                    'Argument validator encountered a required argument after an optional argument.',
                  )
                r++
              }
            if (((o = e.length), n.length < r || o < n.length))
              i =
                'Expected ' +
                (r == o
                  ? 1 == r
                    ? '1 argument'
                    : r + ' arguments'
                  : r + '-' + o + ' arguments') +
                ' but got ' +
                n.length +
                '.'
            else {
              for (r = 0; r < n.length; r++)
                if (
                  ((o = e[r].optional && void 0 === n[r]), !e[r].M(n[r]) && !o)
                ) {
                  if (((e = e[r]), 0 > r || r >= ah.length))
                    throw new Ui(
                      'internal-error',
                      'Argument validator received an unsupported number of arguments.',
                    )
                  ;(n = ah[r]),
                    (i =
                      (i ? '' : n + ' argument ') +
                      (e.name ? '"' + e.name + '" ' : '') +
                      'must be ' +
                      e.K +
                      '.')
                  break t
                }
              i = null
            }
          }
          if (i) throw new Ui('argument-error', t + ' failed: ' + i)
        }
        ;((t = Yc.prototype).za = function() {
          var t = this
          return this.f
            ? this.f
            : (this.f = nh(
                this,
                Tt()
                  .then(function() {
                    if (ci() && !$n()) return zn()
                    throw new Ui(
                      'operation-not-supported-in-this-environment',
                      'RecaptchaVerifier is only supported in a browser HTTP/HTTPS environment.',
                    )
                  })
                  .then(function() {
                    return t.o.g(t.v())
                  })
                  .then(function(e) {
                    return (t.g = e), Ba(t.u, Ca, {})
                  })
                  .then(function(e) {
                    t.a[Qc] = e.recaptchaSiteKey
                  })
                  .s(function(e) {
                    throw ((t.f = null), e)
                  }),
              ))
        }),
          (t.render = function() {
            ih(this)
            var t = this
            return nh(
              this,
              this.za().then(function() {
                if (null === t.c) {
                  var e = t.m
                  if (!t.i) {
                    var n = Cn(e)
                    ;(e = xn('DIV')), n.appendChild(e)
                  }
                  t.c = t.g.render(e, t.a)
                }
                return t.c
              }),
            )
          }),
          (t.verify = function() {
            ih(this)
            var t = this
            return nh(
              this,
              this.render().then(function(e) {
                return new vt(function(n) {
                  var i = t.g.getResponse(e)
                  if (i) n(i)
                  else {
                    var r = function(e) {
                      e &&
                        ((function(t, e) {
                          U(t.l, function(t) {
                            return t == e
                          })
                        })(t, r),
                        n(e))
                    }
                    t.l.push(r), t.i && t.g.execute(t.c)
                  }
                })
              }),
            )
          }),
          (t.reset = function() {
            ih(this), null !== this.c && this.g.reset(this.c)
          }),
          (t.clear = function() {
            ih(this), (this.C = !0), this.o.c()
            for (var t = 0; t < this.h.length; t++)
              this.h[t].cancel('RecaptchaVerifier instance has been destroyed.')
            if (!this.i) {
              t = Cn(this.m)
              for (var e; (e = t.firstChild); ) t.removeChild(e)
            }
          }),
          w(rh, Yc)
        var ah = 'First Second Third Fourth Fifth Sixth Seventh Eighth Ninth'.split(
          ' ',
        )
        function sh(t, e) {
          return { name: t || '', K: 'a valid string', optional: !!e, M: r }
        }
        function uh(t, e) {
          return { name: t || '', K: 'a boolean', optional: !!e, M: o }
        }
        function ch(t, e) {
          return { name: t || '', K: 'a valid object', optional: !!e, M: l }
        }
        function hh(t, e) {
          return { name: t || '', K: 'a function', optional: !!e, M: f }
        }
        function fh(t, e) {
          return { name: t || '', K: 'null', optional: !!e, M: u }
        }
        function lh(t) {
          return {
            name: t ? t + 'Credential' : 'credential',
            K: t ? 'a valid ' + t + ' credential' : 'a valid credential',
            optional: !1,
            M: function(e) {
              if (!e) return !1
              var n = !t || e.providerId === t
              return !(!e.ya || !n)
            },
          }
        }
        function dh() {
          return {
            name: 'applicationVerifier',
            K: 'an implementation of firebase.auth.ApplicationVerifier',
            optional: !1,
            M: function(t) {
              return !!(t && r(t.type) && f(t.verify))
            },
          }
        }
        function ph(t, e, n, i) {
          return {
            name: n || '',
            K: t.K + ' or ' + e.K,
            optional: !!i,
            M: function(n) {
              return t.M(n) || e.M(n)
            },
          }
        }
        function vh(t, e) {
          for (var n in e) {
            var i = e[n].name
            t[i] = bh(i, t[n], e[n].j)
          }
        }
        function mh(t, e) {
          for (var n in e) {
            var i = e[n].name
            i !== n &&
              Object.defineProperty(t, i, {
                get: b(function(t) {
                  return this[t]
                }, n),
                set: b(
                  function(t, e, n, i) {
                    oh(t, [n], [i], !0), (this[e] = i)
                  },
                  i,
                  n,
                  e[n].sb,
                ),
                enumerable: !0,
              })
          }
        }
        function gh(t, e, n, i) {
          t[e] = bh(e, n, i)
        }
        function bh(t, e, n) {
          function i() {
            var t = Array.prototype.slice.call(arguments)
            return oh(o, n, t), e.apply(this, t)
          }
          if (!n) return e
          var r,
            o = (function(t) {
              return (t = t.split('.'))[t.length - 1]
            })(t)
          for (r in e) i[r] = e[r]
          for (r in e.prototype) i.prototype[r] = e.prototype[r]
          return i
        }
        vh(kc.prototype, {
          Va: { name: 'applyActionCode', j: [sh('code')] },
          Ja: { name: 'checkActionCode', j: [sh('code')] },
          Ya: {
            name: 'confirmPasswordReset',
            j: [sh('code'), sh('newPassword')],
          },
          wb: {
            name: 'createUserWithEmailAndPassword',
            j: [sh('email'), sh('password')],
          },
          Vb: {
            name: 'createUserAndRetrieveDataWithEmailAndPassword',
            j: [sh('email'), sh('password')],
          },
          Yb: { name: 'fetchProvidersForEmail', j: [sh('email')] },
          Zb: { name: 'fetchSignInMethodsForEmail', j: [sh('email')] },
          fa: { name: 'getRedirectResult', j: [] },
          nc: { name: 'isSignInWithEmailLink', j: [sh('emailLink')] },
          tc: {
            name: 'onAuthStateChanged',
            j: [
              ph(ch(), hh(), 'nextOrObserver'),
              hh('opt_error', !0),
              hh('opt_completed', !0),
            ],
          },
          uc: {
            name: 'onIdTokenChanged',
            j: [
              ph(ch(), hh(), 'nextOrObserver'),
              hh('opt_error', !0),
              hh('opt_completed', !0),
            ],
          },
          jb: {
            name: 'sendPasswordResetEmail',
            j: [
              sh('email'),
              ph(
                ch('opt_actionCodeSettings', !0),
                fh(null, !0),
                'opt_actionCodeSettings',
                !0,
              ),
            ],
          },
          kb: {
            name: 'sendSignInLinkToEmail',
            j: [sh('email'), ch('actionCodeSettings')],
          },
          lb: { name: 'setPersistence', j: [sh('persistence')] },
          Oa: { name: 'signInAndRetrieveDataWithCredential', j: [lh()] },
          Pa: { name: 'signInAnonymously', j: [] },
          Fc: { name: 'signInAnonymouslyAndRetrieveData', j: [] },
          Gc: { name: 'signInWithCredential', j: [lh()] },
          Mb: { name: 'signInWithCustomToken', j: [sh('token')] },
          Dc: {
            name: 'signInAndRetrieveDataWithCustomToken',
            j: [sh('token')],
          },
          Nb: {
            name: 'signInWithEmailAndPassword',
            j: [sh('email'), sh('password')],
          },
          Hc: {
            name: 'signInWithEmailLink',
            j: [sh('email'), sh('emailLink', !0)],
          },
          Ec: {
            name: 'signInAndRetrieveDataWithEmailAndPassword',
            j: [sh('email'), sh('password')],
          },
          Ic: { name: 'signInWithPhoneNumber', j: [sh('phoneNumber'), dh()] },
          Jc: {
            name: 'signInWithPopup',
            j: [
              {
                name: 'authProvider',
                K: 'a valid Auth provider',
                optional: !1,
                M: function(t) {
                  return !!(
                    t &&
                    t.providerId &&
                    t.hasOwnProperty &&
                    t.hasOwnProperty('isOAuthProvider')
                  )
                },
              },
            ],
          },
          Kc: {
            name: 'signInWithRedirect',
            j: [
              {
                name: 'authProvider',
                K: 'a valid Auth provider',
                optional: !1,
                M: function(t) {
                  return !!(
                    t &&
                    t.providerId &&
                    t.hasOwnProperty &&
                    t.hasOwnProperty('isOAuthProvider')
                  )
                },
              },
            ],
          },
          Pc: {
            name: 'updateCurrentUser',
            j: [
              ph(
                {
                  name: 'user',
                  K: 'an instance of Firebase User',
                  optional: !1,
                  M: function(t) {
                    return !!(t && t instanceof Ku)
                  },
                },
                fh(),
                'user',
              ),
            ],
          },
          mb: { name: 'signOut', j: [] },
          toJSON: { name: 'toJSON', j: [sh(null, !0)] },
          Rc: { name: 'useDeviceLanguage', j: [] },
          Sc: { name: 'verifyPasswordResetCode', j: [sh('code')] },
        }),
          mh(kc.prototype, {
            lc: { name: 'languageCode', sb: ph(sh(), fh(), 'languageCode') },
          }),
          (kc.Persistence = Hs),
          (kc.Persistence.LOCAL = 'local'),
          (kc.Persistence.SESSION = 'session'),
          (kc.Persistence.NONE = 'none'),
          vh(Ku.prototype, {
            delete: { name: 'delete', j: [] },
            cc: { name: 'getIdTokenResult', j: [uh('opt_forceRefresh', !0)] },
            F: { name: 'getIdToken', j: [uh('opt_forceRefresh', !0)] },
            cb: { name: 'linkAndRetrieveDataWithCredential', j: [lh()] },
            oc: { name: 'linkWithCredential', j: [lh()] },
            pc: { name: 'linkWithPhoneNumber', j: [sh('phoneNumber'), dh()] },
            qc: {
              name: 'linkWithPopup',
              j: [
                {
                  name: 'authProvider',
                  K: 'a valid Auth provider',
                  optional: !1,
                  M: function(t) {
                    return !!(
                      t &&
                      t.providerId &&
                      t.hasOwnProperty &&
                      t.hasOwnProperty('isOAuthProvider')
                    )
                  },
                },
              ],
            },
            rc: {
              name: 'linkWithRedirect',
              j: [
                {
                  name: 'authProvider',
                  K: 'a valid Auth provider',
                  optional: !1,
                  M: function(t) {
                    return !!(
                      t &&
                      t.providerId &&
                      t.hasOwnProperty &&
                      t.hasOwnProperty('isOAuthProvider')
                    )
                  },
                },
              ],
            },
            fb: {
              name: 'reauthenticateAndRetrieveDataWithCredential',
              j: [lh()],
            },
            xc: { name: 'reauthenticateWithCredential', j: [lh()] },
            yc: {
              name: 'reauthenticateWithPhoneNumber',
              j: [sh('phoneNumber'), dh()],
            },
            zc: {
              name: 'reauthenticateWithPopup',
              j: [
                {
                  name: 'authProvider',
                  K: 'a valid Auth provider',
                  optional: !1,
                  M: function(t) {
                    return !!(
                      t &&
                      t.providerId &&
                      t.hasOwnProperty &&
                      t.hasOwnProperty('isOAuthProvider')
                    )
                  },
                },
              ],
            },
            Ac: {
              name: 'reauthenticateWithRedirect',
              j: [
                {
                  name: 'authProvider',
                  K: 'a valid Auth provider',
                  optional: !1,
                  M: function(t) {
                    return !!(
                      t &&
                      t.providerId &&
                      t.hasOwnProperty &&
                      t.hasOwnProperty('isOAuthProvider')
                    )
                  },
                },
              ],
            },
            reload: { name: 'reload', j: [] },
            ib: {
              name: 'sendEmailVerification',
              j: [
                ph(
                  ch('opt_actionCodeSettings', !0),
                  fh(null, !0),
                  'opt_actionCodeSettings',
                  !0,
                ),
              ],
            },
            toJSON: { name: 'toJSON', j: [sh(null, !0)] },
            Oc: { name: 'unlink', j: [sh('provider')] },
            ob: { name: 'updateEmail', j: [sh('email')] },
            pb: { name: 'updatePassword', j: [sh('password')] },
            Qc: { name: 'updatePhoneNumber', j: [lh('phone')] },
            qb: { name: 'updateProfile', j: [ch('profile')] },
          }),
          vh(Uc.prototype, {
            execute: { name: 'execute' },
            render: { name: 'render' },
            reset: { name: 'reset' },
            getResponse: { name: 'getResponse' },
          }),
          vh(jc.prototype, {
            execute: { name: 'execute' },
            render: { name: 'render' },
            reset: { name: 'reset' },
            getResponse: { name: 'getResponse' },
          }),
          vh(vt.prototype, {
            ia: { name: 'finally' },
            s: { name: 'catch' },
            then: { name: 'then' },
          }),
          mh(Ru.prototype, {
            appVerificationDisabled: {
              name: 'appVerificationDisabledForTesting',
              sb: uh('appVerificationDisabledForTesting'),
            },
          }),
          vh(Cu.prototype, {
            confirm: { name: 'confirm', j: [sh('verificationCode')] },
          }),
          gh(
            Cr,
            'credential',
            function(t, e) {
              return new Rr(t, e)
            },
            [sh('email'), sh('password')],
          ),
          vh(Er.prototype, {
            ta: { name: 'addScope', j: [sh('scope')] },
            Da: {
              name: 'setCustomParameters',
              j: [ch('customOAuthParameters')],
            },
          }),
          gh(Er, 'credential', Ar, [ph(sh(), ch(), 'token')]),
          gh(Cr, 'credentialWithLink', Dr, [sh('email'), sh('emailLink')]),
          vh(kr.prototype, {
            ta: { name: 'addScope', j: [sh('scope')] },
            Da: {
              name: 'setCustomParameters',
              j: [ch('customOAuthParameters')],
            },
          }),
          gh(kr, 'credential', Sr, [ph(sh(), ch(), 'token')]),
          vh(Nr.prototype, {
            ta: { name: 'addScope', j: [sh('scope')] },
            Da: {
              name: 'setCustomParameters',
              j: [ch('customOAuthParameters')],
            },
          }),
          gh(Nr, 'credential', _r, [
            ph(sh(), ph(ch(), fh()), 'idToken'),
            ph(sh(), fh(), 'accessToken', !0),
          ]),
          vh(Or.prototype, {
            Da: {
              name: 'setCustomParameters',
              j: [ch('customOAuthParameters')],
            },
          }),
          gh(Or, 'credential', Pr, [ph(sh(), ch(), 'token'), sh('secret', !0)]),
          vh(Tr.prototype, {
            ta: { name: 'addScope', j: [sh('scope')] },
            credential: {
              name: 'credential',
              j: [
                ph(sh(), fh(), 'idToken', !0),
                ph(sh(), fh(), 'accessToken', !0),
              ],
            },
            Da: {
              name: 'setCustomParameters',
              j: [ch('customOAuthParameters')],
            },
          }),
          gh(jr, 'credential', Ur, [
            sh('verificationId'),
            sh('verificationCode'),
          ]),
          vh(jr.prototype, {
            Ta: { name: 'verifyPhoneNumber', j: [sh('phoneNumber'), dh()] },
          }),
          vh(Ui.prototype, { toJSON: { name: 'toJSON', j: [sh(null, !0)] } }),
          vh(Br.prototype, { toJSON: { name: 'toJSON', j: [sh(null, !0)] } }),
          vh(Gr.prototype, { toJSON: { name: 'toJSON', j: [sh(null, !0)] } }),
          vh(rh.prototype, {
            clear: { name: 'clear', j: [] },
            render: { name: 'render', j: [] },
            verify: { name: 'verify', j: [] },
          }),
          (function() {
            if (void 0 === e || !e.INTERNAL || !e.INTERNAL.registerService)
              throw Error(
                'Cannot find the firebase namespace; be sure to include firebase-app.js before this library.',
              )
            var t = { Auth: kc, Error: Ui }
            gh(t, 'EmailAuthProvider', Cr, []),
              gh(t, 'FacebookAuthProvider', Er, []),
              gh(t, 'GithubAuthProvider', kr, []),
              gh(t, 'GoogleAuthProvider', Nr, []),
              gh(t, 'TwitterAuthProvider', Or, []),
              gh(t, 'OAuthProvider', Tr, [sh('providerId')]),
              gh(t, 'PhoneAuthProvider', jr, [
                {
                  name: 'auth',
                  K: 'an instance of Firebase Auth',
                  optional: !0,
                  M: function(t) {
                    return !!(t && t instanceof kc)
                  },
                },
              ]),
              gh(t, 'RecaptchaVerifier', rh, [
                ph(
                  sh(),
                  {
                    name: '',
                    K: 'an HTML element',
                    optional: !1,
                    M: function(t) {
                      return !!(t && t instanceof Element)
                    },
                  },
                  'recaptchaContainer',
                ),
                ch('recaptchaParameters', !0),
                {
                  name: 'app',
                  K: 'an instance of Firebase App',
                  optional: !0,
                  M: function(t) {
                    return !!(t && t instanceof e.app.App)
                  },
                },
              ]),
              e.INTERNAL.registerService(
                'auth',
                function(t, e) {
                  return (
                    e({
                      INTERNAL: {
                        getUid: g((t = new kc(t)).getUid, t),
                        getToken: g(t.bc, t),
                        addAuthTokenListener: g(t.Ub, t),
                        removeAuthTokenListener: g(t.Bc, t),
                      },
                    }),
                    t
                  )
                },
                t,
                function(t, e) {
                  if ('create' === t)
                    try {
                      e.auth()
                    } catch (t) {}
                },
              ),
              e.INTERNAL.extendNamespace({ User: Ku })
          })()
      }.apply(
        'undefined' != typeof global
          ? global
          : 'undefined' != typeof self
            ? self
            : 'undefined' != typeof window
              ? window
              : {},
      )
  })((this.firebase = this.firebase || {}), firebase)
} catch (t) {
  throw (console.error(t),
  new Error(
    'Cannot instantiate firebase-auth - be sure to load firebase-app.js first.',
  ))
}
//# sourceMappingURL=firebase-auth.js.map
