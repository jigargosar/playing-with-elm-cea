try {
  !(function(t, e) {
    'use strict'
    var n
    ;(e = e && e.hasOwnProperty('default') ? e.default : e),
      (function(t) {
        ;(t[(t.DEBUG = 0)] = 'DEBUG'),
          (t[(t.VERBOSE = 1)] = 'VERBOSE'),
          (t[(t.INFO = 2)] = 'INFO'),
          (t[(t.WARN = 3)] = 'WARN'),
          (t[(t.ERROR = 4)] = 'ERROR'),
          (t[(t.SILENT = 5)] = 'SILENT')
      })(n || (n = {}))
    var r = n.INFO,
      i = function(t, e) {
        for (var r = [], i = 2; i < arguments.length; i++)
          r[i - 2] = arguments[i]
        if (!(e < t.logLevel)) {
          var o = new Date().toISOString()
          switch (e) {
            case n.DEBUG:
            case n.VERBOSE:
              console.log.apply(
                console,
                ['[' + o + ']  ' + t.name + ':'].concat(r),
              )
              break
            case n.INFO:
              console.info.apply(
                console,
                ['[' + o + ']  ' + t.name + ':'].concat(r),
              )
              break
            case n.WARN:
              console.warn.apply(
                console,
                ['[' + o + ']  ' + t.name + ':'].concat(r),
              )
              break
            case n.ERROR:
              console.error.apply(
                console,
                ['[' + o + ']  ' + t.name + ':'].concat(r),
              )
              break
            default:
              throw new Error(
                'Attempted to log a message with an invalid logType (value: ' +
                  e +
                  ')',
              )
          }
        }
      },
      o = (function() {
        function t(t) {
          ;(this.name = t), (this._logLevel = r), (this._logHandler = i)
        }
        return (
          Object.defineProperty(t.prototype, 'logLevel', {
            get: function() {
              return this._logLevel
            },
            set: function(t) {
              if (!(t in n))
                throw new TypeError('Invalid value assigned to `logLevel`')
              this._logLevel = t
            },
            enumerable: !0,
            configurable: !0,
          }),
          Object.defineProperty(t.prototype, 'logHandler', {
            get: function() {
              return this._logHandler
            },
            set: function(t) {
              if ('function' != typeof t)
                throw new TypeError(
                  'Value assigned to `logHandler` must be a function',
                )
              this._logHandler = t
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.debug = function() {
            for (var t = [], e = 0; e < arguments.length; e++)
              t[e] = arguments[e]
            this._logHandler.apply(this, [this, n.DEBUG].concat(t))
          }),
          (t.prototype.log = function() {
            for (var t = [], e = 0; e < arguments.length; e++)
              t[e] = arguments[e]
            this._logHandler.apply(this, [this, n.VERBOSE].concat(t))
          }),
          (t.prototype.info = function() {
            for (var t = [], e = 0; e < arguments.length; e++)
              t[e] = arguments[e]
            this._logHandler.apply(this, [this, n.INFO].concat(t))
          }),
          (t.prototype.warn = function() {
            for (var t = [], e = 0; e < arguments.length; e++)
              t[e] = arguments[e]
            this._logHandler.apply(this, [this, n.WARN].concat(t))
          }),
          (t.prototype.error = function() {
            for (var t = [], e = 0; e < arguments.length; e++)
              t[e] = arguments[e]
            this._logHandler.apply(this, [this, n.ERROR].concat(t))
          }),
          t
        )
      })(),
      s =
        Object.setPrototypeOf ||
        ({ __proto__: [] } instanceof Array &&
          function(t, e) {
            t.__proto__ = e
          }) ||
        function(t, e) {
          for (var n in e) e.hasOwnProperty(n) && (t[n] = e[n])
        }
    function a(t, e) {
      function n() {
        this.constructor = t
      }
      s(t, e),
        (t.prototype =
          null === e
            ? Object.create(e)
            : ((n.prototype = e.prototype), new n()))
    }
    function u(t, e, n, r) {
      return new (n || (n = Promise))(function(i, o) {
        function s(t) {
          try {
            u(r.next(t))
          } catch (t) {
            o(t)
          }
        }
        function a(t) {
          try {
            u(r.throw(t))
          } catch (t) {
            o(t)
          }
        }
        function u(t) {
          t.done
            ? i(t.value)
            : new n(function(e) {
                e(t.value)
              }).then(s, a)
        }
        u((r = r.apply(t, e || [])).next())
      })
    }
    function c(t, e) {
      var n,
        r,
        i,
        o,
        s = {
          label: 0,
          sent: function() {
            if (1 & i[0]) throw i[1]
            return i[1]
          },
          trys: [],
          ops: [],
        }
      return (
        (o = { next: a(0), throw: a(1), return: a(2) }),
        'function' == typeof Symbol &&
          (o[Symbol.iterator] = function() {
            return this
          }),
        o
      )
      function a(o) {
        return function(a) {
          return (function(o) {
            if (n) throw new TypeError('Generator is already executing.')
            for (; s; )
              try {
                if (
                  ((n = 1),
                  r &&
                    (i = r[2 & o[0] ? 'return' : o[0] ? 'throw' : 'next']) &&
                    !(i = i.call(r, o[1])).done)
                )
                  return i
                switch (((r = 0), i && (o = [0, i.value]), o[0])) {
                  case 0:
                  case 1:
                    i = o
                    break
                  case 4:
                    return s.label++, { value: o[1], done: !1 }
                  case 5:
                    s.label++, (r = o[1]), (o = [0])
                    continue
                  case 7:
                    ;(o = s.ops.pop()), s.trys.pop()
                    continue
                  default:
                    if (
                      !(i = (i = s.trys).length > 0 && i[i.length - 1]) &&
                      (6 === o[0] || 2 === o[0])
                    ) {
                      s = 0
                      continue
                    }
                    if (3 === o[0] && (!i || (o[1] > i[0] && o[1] < i[3]))) {
                      s.label = o[1]
                      break
                    }
                    if (6 === o[0] && s.label < i[1]) {
                      ;(s.label = i[1]), (i = o)
                      break
                    }
                    if (i && s.label < i[2]) {
                      ;(s.label = i[2]), s.ops.push(o)
                      break
                    }
                    i[2] && s.ops.pop(), s.trys.pop()
                    continue
                }
                o = e.call(t, s)
              } catch (t) {
                ;(o = [6, t]), (r = 0)
              } finally {
                n = i = 0
              }
            if (5 & o[0]) throw o[1]
            return { value: o[0] ? o[1] : void 0, done: !0 }
          })([o, a])
        }
      }
    }
    var h,
      l =
        'undefined' != typeof window
          ? window
          : 'undefined' != typeof global
            ? global
            : 'undefined' != typeof self
              ? self
              : {},
      f = f || {},
      d = l
    function p(t) {
      return 'string' == typeof t
    }
    function m(t, e) {
      ;(t = t.split('.')), (e = e || d)
      for (var n = 0; n < t.length; n++) if (null == (e = e[t[n]])) return null
      return e
    }
    function y() {}
    function g(t) {
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
    function v(t) {
      return 'array' == g(t)
    }
    function b(t) {
      var e = g(t)
      return 'array' == e || ('object' == e && 'number' == typeof t.length)
    }
    function w(t) {
      return 'function' == g(t)
    }
    function T(t) {
      var e = typeof t
      return ('object' == e && null != t) || 'function' == e
    }
    var S = 'closure_uid_' + ((1e9 * Math.random()) >>> 0),
      E = 0
    function I(t, e, n) {
      return t.call.apply(t.bind, arguments)
    }
    function C(t, e, n) {
      if (!t) throw Error()
      if (2 < arguments.length) {
        var r = Array.prototype.slice.call(arguments, 2)
        return function() {
          var n = Array.prototype.slice.call(arguments)
          return Array.prototype.unshift.apply(n, r), t.apply(e, n)
        }
      }
      return function() {
        return t.apply(e, arguments)
      }
    }
    function D(t, e, n) {
      return (D =
        Function.prototype.bind &&
        -1 != Function.prototype.bind.toString().indexOf('native code')
          ? I
          : C).apply(null, arguments)
    }
    function N(t, e) {
      var n = Array.prototype.slice.call(arguments, 1)
      return function() {
        var e = n.slice()
        return e.push.apply(e, arguments), t.apply(this, e)
      }
    }
    var A =
      Date.now ||
      function() {
        return +new Date()
      }
    function k(t, e) {
      function n() {}
      ;(n.prototype = e.prototype),
        (t.L = e.prototype),
        (t.prototype = new n()),
        (t.prototype.constructor = t),
        (t.mh = function(t, n, r) {
          for (
            var i = Array(arguments.length - 2), o = 2;
            o < arguments.length;
            o++
          )
            i[o - 2] = arguments[o]
          return e.prototype[n].apply(t, i)
        })
    }
    function R(t) {
      if (Error.captureStackTrace) Error.captureStackTrace(this, R)
      else {
        var e = Error().stack
        e && (this.stack = e)
      }
      t && (this.message = String(t))
    }
    function M(t, e) {
      for (var n = '', r = (t = t.split('%s')).length - 1, i = 0; i < r; i++)
        n += t[i] + (i < e.length ? e[i] : '%s')
      R.call(this, n + t[r])
    }
    function O() {
      0 != P && (_[this[S] || (this[S] = ++E)] = this),
        (this.Ka = this.Ka),
        (this.Qa = this.Qa)
    }
    k(R, Error),
      (R.prototype.name = 'CustomError'),
      k(M, R),
      (M.prototype.name = 'AssertionError')
    var P = 0,
      _ = {}
    ;(O.prototype.Ka = !1),
      (O.prototype.bb = function() {
        if (!this.Ka && ((this.Ka = !0), this.F(), 0 != P)) {
          var t = this[S] || (this[S] = ++E)
          if (0 != P && this.Qa && 0 < this.Qa.length)
            throw Error(
              this +
                " did not empty its onDisposeCallbacks queue. This probably means it overrode dispose() or disposeInternal() without calling the superclass' method.",
            )
          delete _[t]
        }
      }),
      (O.prototype.F = function() {
        if (this.Qa) for (; this.Qa.length; ) this.Qa.shift()()
      })
    var L = Array.prototype.indexOf
        ? function(t, e) {
            return Array.prototype.indexOf.call(t, e, void 0)
          }
        : function(t, e) {
            if (p(t)) return p(e) && 1 == e.length ? t.indexOf(e, 0) : -1
            for (var n = 0; n < t.length; n++)
              if (n in t && t[n] === e) return n
            return -1
          },
      x = Array.prototype.lastIndexOf
        ? function(t, e) {
            return Array.prototype.lastIndexOf.call(t, e, t.length - 1)
          }
        : function(t, e) {
            var n = t.length - 1
            if ((0 > n && (n = Math.max(0, t.length + n)), p(t)))
              return p(e) && 1 == e.length ? t.lastIndexOf(e, n) : -1
            for (; 0 <= n; n--) if (n in t && t[n] === e) return n
            return -1
          },
      q = Array.prototype.forEach
        ? function(t, e, n) {
            Array.prototype.forEach.call(t, e, n)
          }
        : function(t, e, n) {
            for (
              var r = t.length, i = p(t) ? t.split('') : t, o = 0;
              o < r;
              o++
            )
              o in i && e.call(n, i[o], o, t)
          },
      F = Array.prototype.some
        ? function(t, e) {
            return Array.prototype.some.call(t, e, void 0)
          }
        : function(t, e) {
            for (
              var n = t.length, r = p(t) ? t.split('') : t, i = 0;
              i < n;
              i++
            )
              if (i in r && e.call(void 0, r[i], i, t)) return !0
            return !1
          }
    function V(t) {
      if (!v(t)) for (var e = t.length - 1; 0 <= e; e--) delete t[e]
      t.length = 0
    }
    function B(t, e) {
      var n
      return (n = 0 <= (e = L(t, e))) && Array.prototype.splice.call(t, e, 1), n
    }
    function U(t) {
      return Array.prototype.concat.apply([], arguments)
    }
    function Q(t) {
      var e = t.length
      if (0 < e) {
        for (var n = Array(e), r = 0; r < e; r++) n[r] = t[r]
        return n
      }
      return []
    }
    function K(t) {
      return /^[\s\xa0]*$/.test(t)
    }
    var W,
      j = String.prototype.trim
        ? function(t) {
            return t.trim()
          }
        : function(t) {
            return /^[\s\xa0]*([\s\S]*?)[\s\xa0]*$/.exec(t)[1]
          }
    function G(t, e) {
      return t < e ? -1 : t > e ? 1 : 0
    }
    t: {
      var z = d.navigator
      if (z) {
        var H = z.userAgent
        if (H) {
          W = H
          break t
        }
      }
      W = ''
    }
    function X(t) {
      return -1 != W.indexOf(t)
    }
    function Y(t, e, n) {
      for (var r in t) e.call(n, t[r], r, t)
    }
    function J(t) {
      var e,
        n = [],
        r = 0
      for (e in t) n[r++] = t[e]
      return n
    }
    function $(t) {
      var e,
        n = [],
        r = 0
      for (e in t) n[r++] = e
      return n
    }
    function Z(t) {
      var e,
        n = {}
      for (e in t) n[e] = t[e]
      return n
    }
    var tt = 'constructor hasOwnProperty isPrototypeOf propertyIsEnumerable toLocaleString toString valueOf'.split(
      ' ',
    )
    function et(t, e) {
      for (var n, r, i = 1; i < arguments.length; i++) {
        for (n in (r = arguments[i])) t[n] = r[n]
        for (var o = 0; o < tt.length; o++)
          (n = tt[o]),
            Object.prototype.hasOwnProperty.call(r, n) && (t[n] = r[n])
      }
    }
    function nt(t) {
      return nt[' '](t), t
    }
    nt[' '] = y
    var rt,
      it,
      ot = X('Opera'),
      st = X('Trident') || X('MSIE'),
      at = X('Edge'),
      ut = at || st,
      ct =
        X('Gecko') &&
        !(-1 != W.toLowerCase().indexOf('webkit') && !X('Edge')) &&
        !(X('Trident') || X('MSIE')) &&
        !X('Edge'),
      ht = -1 != W.toLowerCase().indexOf('webkit') && !X('Edge')
    function lt() {
      var t = d.document
      return t ? t.documentMode : void 0
    }
    t: {
      var ft = '',
        dt = ((it = W),
        ct
          ? /rv:([^\);]+)(\)|;)/.exec(it)
          : at
            ? /Edge\/([\d\.]+)/.exec(it)
            : st
              ? /\b(?:MSIE|rv)[: ]([^\);]+)(\)|;)/.exec(it)
              : ht
                ? /WebKit\/(\S+)/.exec(it)
                : ot
                  ? /(?:Version)[ \/]?(\S+)/.exec(it)
                  : void 0)
      if ((dt && (ft = dt ? dt[1] : ''), st)) {
        var pt = lt()
        if (null != pt && pt > parseFloat(ft)) {
          rt = String(pt)
          break t
        }
      }
      rt = ft
    }
    var mt,
      yt = {}
    function gt(t) {
      return (function(t, e) {
        var n = yt
        return Object.prototype.hasOwnProperty.call(n, t) ? n[t] : (n[t] = e(t))
      })(t, function() {
        for (
          var e = 0,
            n = j(String(rt)).split('.'),
            r = j(String(t)).split('.'),
            i = Math.max(n.length, r.length),
            o = 0;
          0 == e && o < i;
          o++
        ) {
          var s = n[o] || '',
            a = r[o] || ''
          do {
            if (
              ((s = /(\d*)(\D*)(.*)/.exec(s) || ['', '', '', '']),
              (a = /(\d*)(\D*)(.*)/.exec(a) || ['', '', '', '']),
              0 == s[0].length && 0 == a[0].length)
            )
              break
            ;(e =
              G(
                0 == s[1].length ? 0 : parseInt(s[1], 10),
                0 == a[1].length ? 0 : parseInt(a[1], 10),
              ) ||
              G(0 == s[2].length, 0 == a[2].length) ||
              G(s[2], a[2])),
              (s = s[3]),
              (a = a[3])
          } while (0 == e)
        }
        return 0 <= e
      })
    }
    var vt = d.document
    mt =
      vt && st
        ? lt() || ('CSS1Compat' == vt.compatMode ? parseInt(rt, 10) : 5)
        : void 0
    var bt =
        Object.freeze ||
        function(t) {
          return t
        },
      wt = !st || 9 <= Number(mt),
      Tt = st && !gt('9'),
      St = (function() {
        if (!d.addEventListener || !Object.defineProperty) return !1
        var t = !1,
          e = Object.defineProperty({}, 'passive', {
            get: function() {
              t = !0
            },
          })
        try {
          d.addEventListener('test', y, e), d.removeEventListener('test', y, e)
        } catch (t) {}
        return t
      })()
    function Et(t, e) {
      ;(this.type = t),
        (this.currentTarget = this.target = e),
        (this.defaultPrevented = this.Ea = !1),
        (this.Be = !0)
    }
    function It(t, e) {
      Et.call(this, t ? t.type : ''),
        (this.relatedTarget = this.currentTarget = this.target = null),
        (this.button = this.screenY = this.screenX = this.clientY = this.clientX = this.offsetY = this.offsetX = 0),
        (this.key = ''),
        (this.charCode = this.keyCode = 0),
        (this.metaKey = this.shiftKey = this.altKey = this.ctrlKey = !1),
        (this.state = null),
        (this.pointerId = 0),
        (this.pointerType = ''),
        (this.fb = null),
        t && this.Kf(t, e)
    }
    ;(Et.prototype.stopPropagation = function() {
      this.Ea = !0
    }),
      (Et.prototype.preventDefault = function() {
        ;(this.defaultPrevented = !0), (this.Be = !1)
      }),
      k(It, Et)
    var Ct = bt({ 2: 'touch', 3: 'pen', 4: 'mouse' })
    ;(It.prototype.Kf = function(t, e) {
      var n = (this.type = t.type),
        r = t.changedTouches ? t.changedTouches[0] : null
      if (
        ((this.target = t.target || t.srcElement),
        (this.currentTarget = e),
        (e = t.relatedTarget))
      ) {
        if (ct) {
          t: {
            try {
              nt(e.nodeName)
              var i = !0
              break t
            } catch (t) {}
            i = !1
          }
          i || (e = null)
        }
      } else
        'mouseover' == n
          ? (e = t.fromElement)
          : 'mouseout' == n && (e = t.toElement)
      ;(this.relatedTarget = e),
        null === r
          ? ((this.offsetX = ht || void 0 !== t.offsetX ? t.offsetX : t.layerX),
            (this.offsetY = ht || void 0 !== t.offsetY ? t.offsetY : t.layerY),
            (this.clientX = void 0 !== t.clientX ? t.clientX : t.pageX),
            (this.clientY = void 0 !== t.clientY ? t.clientY : t.pageY),
            (this.screenX = t.screenX || 0),
            (this.screenY = t.screenY || 0))
          : ((this.clientX = void 0 !== r.clientX ? r.clientX : r.pageX),
            (this.clientY = void 0 !== r.clientY ? r.clientY : r.pageY),
            (this.screenX = r.screenX || 0),
            (this.screenY = r.screenY || 0)),
        (this.button = t.button),
        (this.keyCode = t.keyCode || 0),
        (this.key = t.key || ''),
        (this.charCode = t.charCode || ('keypress' == n ? t.keyCode : 0)),
        (this.ctrlKey = t.ctrlKey),
        (this.altKey = t.altKey),
        (this.shiftKey = t.shiftKey),
        (this.metaKey = t.metaKey),
        (this.pointerId = t.pointerId || 0),
        (this.pointerType = p(t.pointerType)
          ? t.pointerType
          : Ct[t.pointerType] || ''),
        (this.state = t.state),
        (this.fb = t),
        t.defaultPrevented && this.preventDefault()
    }),
      (It.prototype.stopPropagation = function() {
        It.L.stopPropagation.call(this),
          this.fb.stopPropagation
            ? this.fb.stopPropagation()
            : (this.fb.cancelBubble = !0)
      }),
      (It.prototype.preventDefault = function() {
        It.L.preventDefault.call(this)
        var t = this.fb
        if (t.preventDefault) t.preventDefault()
        else if (((t.returnValue = !1), Tt))
          try {
            ;(t.ctrlKey || (112 <= t.keyCode && 123 >= t.keyCode)) &&
              (t.keyCode = -1)
          } catch (t) {}
      })
    var Dt = 'closure_listenable_' + ((1e6 * Math.random()) | 0)
    function Nt(t) {
      return !(!t || !t[Dt])
    }
    var At = 0
    function kt(t, e, n, r, i) {
      ;(this.listener = t),
        (this.proxy = null),
        (this.src = e),
        (this.type = n),
        (this.capture = !!r),
        (this.Ob = i),
        (this.key = ++At),
        (this.Sa = this.Eb = !1)
    }
    function Rt(t) {
      ;(this.src = t), (this.J = {}), (this.xb = 0)
    }
    function Mt(t, e, n, r) {
      for (var i = 0; i < t.length; ++i) {
        var o = t[i]
        if (!o.Sa && o.listener == e && o.capture == !!n && o.Ob == r) return i
      }
      return -1
    }
    ;(kt.prototype.Vb = function() {
      ;(this.Sa = !0), (this.Ob = this.src = this.proxy = this.listener = null)
    }),
      ((h = Rt.prototype).add = function(t, e, n, r, i) {
        var o = t.toString()
        ;(t = this.J[o]) || ((t = this.J[o] = []), this.xb++)
        var s = Mt(t, e, r, i)
        return (
          -1 < s
            ? ((e = t[s]), n || (e.Eb = !1))
            : (((e = new kt(e, this.src, o, !!r, i)).Eb = n), t.push(e)),
          e
        )
      }),
      (h.remove = function(t, e, n, r) {
        if (!((t = t.toString()) in this.J)) return !1
        var i = this.J[t]
        return (
          -1 < (e = Mt(i, e, n, r)) &&
          (i[e].Vb(),
          Array.prototype.splice.call(i, e, 1),
          0 == i.length && (delete this.J[t], this.xb--),
          !0)
        )
      }),
      (h.ye = function(t) {
        var e = t.type
        e in this.J &&
          B(this.J[e], t) &&
          (t.Vb(), 0 == this.J[e].length && (delete this.J[e], this.xb--))
      }),
      (h.pb = function(t) {
        var e
        for (e in ((t = t && t.toString()), this.J))
          if (!t || e == t) {
            for (var n = this.J[e], r = 0; r < n.length; r++) n[r].Vb()
            delete this.J[e], this.xb--
          }
      }),
      (h.jb = function(t, e, n, r) {
        var i = -1
        return (
          (t = this.J[t.toString()]) && (i = Mt(t, e, n, r)),
          -1 < i ? t[i] : null
        )
      })
    var Ot = 'closure_lm_' + ((1e6 * Math.random()) | 0),
      Pt = {}
    function _t(t, e, n, r, i) {
      if (r && r.once) return xt(t, e, n, r, i)
      if (v(e)) {
        for (var o = 0; o < e.length; o++) _t(t, e[o], n, r, i)
        return null
      }
      return (
        (n = Wt(n)),
        Nt(t) ? t.nb(e, n, T(r) ? !!r.capture : !!r, i) : Lt(t, e, n, !1, r, i)
      )
    }
    function Lt(t, e, n, r, i, o) {
      if (!e) throw Error('Invalid event type')
      var s = T(i) ? !!i.capture : !!i,
        a = Qt(t)
      if ((a || (t[Ot] = a = new Rt(t)), (n = a.add(e, n, r, s, o)).proxy))
        return n
      if (
        ((r = (function() {
          var t = Ut,
            e = wt
              ? function(n) {
                  return t.call(e.src, e.listener, n)
                }
              : function(n) {
                  if (!(n = t.call(e.src, e.listener, n))) return n
                }
          return e
        })()),
        (n.proxy = r),
        (r.src = t),
        (r.listener = n),
        t.addEventListener)
      )
        St || (i = s),
          void 0 === i && (i = !1),
          t.addEventListener(e.toString(), r, i)
      else if (t.attachEvent) t.attachEvent(Ft(e.toString()), r)
      else {
        if (!t.addListener || !t.removeListener)
          throw Error('addEventListener and attachEvent are unavailable.')
        t.addListener(r)
      }
      return n
    }
    function xt(t, e, n, r, i) {
      if (v(e)) {
        for (var o = 0; o < e.length; o++) xt(t, e[o], n, r, i)
        return null
      }
      return (
        (n = Wt(n)),
        Nt(t) ? t.Oc(e, n, T(r) ? !!r.capture : !!r, i) : Lt(t, e, n, !0, r, i)
      )
    }
    function qt(t) {
      if ('number' != typeof t && t && !t.Sa) {
        var e = t.src
        if (Nt(e)) e.Le(t)
        else {
          var n = t.type,
            r = t.proxy
          e.removeEventListener
            ? e.removeEventListener(n, r, t.capture)
            : e.detachEvent
              ? e.detachEvent(Ft(n), r)
              : e.addListener && e.removeListener && e.removeListener(r),
            (n = Qt(e))
              ? (n.ye(t), 0 == n.xb && ((n.src = null), (e[Ot] = null)))
              : t.Vb()
        }
      }
    }
    function Ft(t) {
      return t in Pt ? Pt[t] : (Pt[t] = 'on' + t)
    }
    function Vt(t, e, n, r) {
      var i = !0
      if ((t = Qt(t)) && (e = t.J[e.toString()]))
        for (e = e.concat(), t = 0; t < e.length; t++) {
          var o = e[t]
          o && o.capture == n && !o.Sa && ((o = Bt(o, r)), (i = i && !1 !== o))
        }
      return i
    }
    function Bt(t, e) {
      var n = t.listener,
        r = t.Ob || t.src
      return t.Eb && qt(t), n.call(r, e)
    }
    function Ut(t, e) {
      if (t.Sa) return !0
      if (!wt) {
        var n = e || m('window.event')
        e = new It(n, this)
        var r = !0
        if (!(0 > n.keyCode || void 0 != n.returnValue)) {
          t: {
            var i = !1
            if (0 == n.keyCode)
              try {
                n.keyCode = -1
                break t
              } catch (t) {
                i = !0
              }
            ;(i || void 0 == n.returnValue) && (n.returnValue = !0)
          }
          for (n = [], i = e.currentTarget; i; i = i.parentNode) n.push(i)
          for (t = t.type, i = n.length - 1; !e.Ea && 0 <= i; i--) {
            e.currentTarget = n[i]
            var o = Vt(n[i], t, !0, e)
            r = r && o
          }
          for (i = 0; !e.Ea && i < n.length; i++)
            (e.currentTarget = n[i]), (o = Vt(n[i], t, !1, e)), (r = r && o)
        }
        return r
      }
      return Bt(t, new It(e, this))
    }
    function Qt(t) {
      return (t = t[Ot]) instanceof Rt ? t : null
    }
    var Kt = '__closure_events_fn_' + ((1e9 * Math.random()) >>> 0)
    function Wt(t) {
      return w(t)
        ? t
        : (t[Kt] ||
            (t[Kt] = function(e) {
              return t.handleEvent(e)
            }),
          t[Kt])
    }
    function jt() {
      O.call(this), (this.ka = new Rt(this)), (this.Pe = this), (this.Uc = null)
    }
    k(jt, O),
      (jt.prototype[Dt] = !0),
      ((h = jt.prototype).addEventListener = function(t, e, n, r) {
        _t(this, t, e, n, r)
      }),
      (h.removeEventListener = function(t, e, n, r) {
        !(function t(e, n, r, i, o) {
          if (v(n)) for (var s = 0; s < n.length; s++) t(e, n[s], r, i, o)
          else
            (i = T(i) ? !!i.capture : !!i),
              (r = Wt(r)),
              Nt(e)
                ? e.ed(n, r, i, o)
                : e && (e = Qt(e)) && (n = e.jb(n, r, i, o)) && qt(n)
        })(this, t, e, n, r)
      }),
      (h.dispatchEvent = function(t) {
        var e,
          n = this.Uc
        if (n) for (e = []; n; n = n.Uc) e.push(n)
        n = this.Pe
        var r = t.type || t
        if (p(t)) t = new Et(t, n)
        else if (t instanceof Et) t.target = t.target || n
        else {
          var i = t
          et((t = new Et(r, n)), i)
        }
        if (((i = !0), e))
          for (var o = e.length - 1; !t.Ea && 0 <= o; o--) {
            var s = (t.currentTarget = e[o])
            i = s.Lb(r, !0, t) && i
          }
        if (
          (t.Ea ||
            ((i = (s = t.currentTarget = n).Lb(r, !0, t) && i),
            t.Ea || (i = s.Lb(r, !1, t) && i)),
          e)
        )
          for (o = 0; !t.Ea && o < e.length; o++)
            i = (s = t.currentTarget = e[o]).Lb(r, !1, t) && i
        return i
      }),
      (h.F = function() {
        jt.L.F.call(this), this.pg(), (this.Uc = null)
      }),
      (h.nb = function(t, e, n, r) {
        return this.ka.add(String(t), e, !1, n, r)
      }),
      (h.Oc = function(t, e, n, r) {
        return this.ka.add(String(t), e, !0, n, r)
      }),
      (h.ed = function(t, e, n, r) {
        this.ka.remove(String(t), e, n, r)
      }),
      (h.Le = function(t) {
        this.ka.ye(t)
      }),
      (h.pg = function() {
        this.ka && this.ka.pb(void 0)
      }),
      (h.Lb = function(t, e, n) {
        if (!(t = this.ka.J[String(t)])) return !0
        t = t.concat()
        for (var r = !0, i = 0; i < t.length; ++i) {
          var o = t[i]
          if (o && !o.Sa && o.capture == e) {
            var s = o.listener,
              a = o.Ob || o.src
            o.Eb && this.Le(o), (r = !1 !== s.call(a, n) && r)
          }
        }
        return r && 0 != n.Be
      }),
      (h.jb = function(t, e, n, r) {
        return this.ka.jb(String(t), e, n, r)
      })
    var Gt = d.JSON.stringify
    function zt(t, e) {
      ;(this.Sf = 100),
        (this.ef = t),
        (this.ug = e),
        (this.Zb = 0),
        (this.Pb = null)
    }
    function Ht() {
      this.lc = this.Va = null
    }
    ;(zt.prototype.get = function() {
      if (0 < this.Zb) {
        this.Zb--
        var t = this.Pb
        ;(this.Pb = t.next), (t.next = null)
      } else t = this.ef()
      return t
    }),
      (zt.prototype.put = function(t) {
        this.ug(t),
          this.Zb < this.Sf && (this.Zb++, (t.next = this.Pb), (this.Pb = t))
      })
    var Xt,
      Yt,
      Jt = new zt(
        function() {
          return new $t()
        },
        function(t) {
          t.reset()
        },
      )
    function $t() {
      this.next = this.scope = this.Gc = null
    }
    function Zt(t) {
      d.setTimeout(function() {
        throw t
      }, 0)
    }
    function te() {
      if (d.Promise && d.Promise.resolve) {
        var t = d.Promise.resolve(void 0)
        Yt = function() {
          t.then(re)
        }
      } else
        Yt = function() {
          var t = re
          !w(d.setImmediate) ||
          (d.Window &&
            d.Window.prototype &&
            !X('Edge') &&
            d.Window.prototype.setImmediate == d.setImmediate)
            ? (Xt ||
                (Xt = (function() {
                  var t = d.MessageChannel
                  if (
                    (void 0 === t &&
                      'undefined' != typeof window &&
                      window.postMessage &&
                      window.addEventListener &&
                      !X('Presto') &&
                      (t = function() {
                        var t = document.createElement('IFRAME')
                        ;(t.style.display = 'none'),
                          (t.src = ''),
                          document.documentElement.appendChild(t)
                        var e = t.contentWindow
                        ;(t = e.document).open(), t.write(''), t.close()
                        var n = 'callImmediate' + Math.random(),
                          r =
                            'file:' == e.location.protocol
                              ? '*'
                              : e.location.protocol + '//' + e.location.host
                        ;(t = D(function(t) {
                          ;('*' != r && t.origin != r) ||
                            t.data != n ||
                            this.port1.onmessage()
                        }, this)),
                          e.addEventListener('message', t, !1),
                          (this.port1 = {}),
                          (this.port2 = {
                            postMessage: function() {
                              e.postMessage(n, r)
                            },
                          })
                      }),
                    void 0 !== t && !X('Trident') && !X('MSIE'))
                  ) {
                    var e = new t(),
                      n = {},
                      r = n
                    return (
                      (e.port1.onmessage = function() {
                        if (void 0 !== n.next) {
                          var t = (n = n.next).rd
                          ;(n.rd = null), t()
                        }
                      }),
                      function(t) {
                        ;(r.next = { rd: t }),
                          (r = r.next),
                          e.port2.postMessage(0)
                      }
                    )
                  }
                  return 'undefined' != typeof document &&
                    'onreadystatechange' in document.createElement('SCRIPT')
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
                        d.setTimeout(t, 0)
                      }
                })()),
              Xt(t))
            : d.setImmediate(t)
        }
    }
    ;(Ht.prototype.add = function(t, e) {
      var n = this.Af()
      n.set(t, e), this.lc ? (this.lc.next = n) : (this.Va = n), (this.lc = n)
    }),
      (Ht.prototype.remove = function() {
        var t = null
        return (
          this.Va &&
            ((t = this.Va),
            (this.Va = this.Va.next),
            this.Va || (this.lc = null),
            (t.next = null)),
          t
        )
      }),
      (Ht.prototype.wg = function(t) {
        Jt.put(t)
      }),
      (Ht.prototype.Af = function() {
        return Jt.get()
      }),
      ($t.prototype.set = function(t, e) {
        ;(this.Gc = t), (this.scope = e), (this.next = null)
      }),
      ($t.prototype.reset = function() {
        this.next = this.scope = this.Gc = null
      })
    var ee = !1,
      ne = new Ht()
    function re() {
      for (var t; (t = ne.remove()); ) {
        try {
          t.Gc.call(t.scope)
        } catch (t) {
          Zt(t)
        }
        ne.wg(t)
      }
      ee = !1
    }
    function ie(t, e) {
      jt.call(this),
        (this.Na = t || 1),
        (this.wb = e || d),
        (this.nd = D(this.Rg, this)),
        (this.ie = A())
    }
    function oe(t, e, n) {
      if (w(t)) n && (t = D(t, n))
      else {
        if (!t || 'function' != typeof t.handleEvent)
          throw Error('Invalid listener argument')
        t = D(t.handleEvent, t)
      }
      return 2147483647 < Number(e) ? -1 : d.setTimeout(t, e || 0)
    }
    function se(t, e, n) {
      O.call(this),
        (this.Uf = null != n ? D(t, n) : t),
        (this.Na = e),
        (this.Xe = D(this.fg, this)),
        (this.qc = [])
    }
    function ae(t) {
      O.call(this), (this.i = t), (this.o = {})
    }
    k(ie, jt),
      ((h = ie.prototype).enabled = !1),
      (h.B = null),
      (h.setInterval = function(t) {
        ;(this.Na = t),
          this.B && this.enabled
            ? (this.stop(), this.start())
            : this.B && this.stop()
      }),
      (h.Rg = function() {
        if (this.enabled) {
          var t = A() - this.ie
          0 < t && t < 0.8 * this.Na
            ? (this.B = this.wb.setTimeout(this.nd, this.Na - t))
            : (this.B && (this.wb.clearTimeout(this.B), (this.B = null)),
              this.ff(),
              this.enabled && (this.stop(), this.start()))
        }
      }),
      (h.ff = function() {
        this.dispatchEvent('tick')
      }),
      (h.start = function() {
        ;(this.enabled = !0),
          this.B ||
            ((this.B = this.wb.setTimeout(this.nd, this.Na)), (this.ie = A()))
      }),
      (h.stop = function() {
        ;(this.enabled = !1),
          this.B && (this.wb.clearTimeout(this.B), (this.B = null))
      }),
      (h.F = function() {
        ie.L.F.call(this), this.stop(), delete this.wb
      }),
      k(se, O),
      ((h = se.prototype).Ta = !1),
      (h.ob = 0),
      (h.B = null),
      (h.mf = function(t) {
        ;(this.qc = arguments), this.B || this.ob ? (this.Ta = !0) : this.Cc()
      }),
      (h.stop = function() {
        this.B &&
          (d.clearTimeout(this.B),
          (this.B = null),
          (this.Ta = !1),
          (this.qc = []))
      }),
      (h.pause = function() {
        this.ob++
      }),
      (h.resume = function() {
        this.ob--, this.ob || !this.Ta || this.B || ((this.Ta = !1), this.Cc())
      }),
      (h.F = function() {
        se.L.F.call(this), this.stop()
      }),
      (h.fg = function() {
        ;(this.B = null), this.Ta && !this.ob && ((this.Ta = !1), this.Cc())
      }),
      (h.Cc = function() {
        ;(this.B = oe(this.Xe, this.Na)), this.Uf.apply(null, this.qc)
      }),
      k(ae, O)
    var ue = []
    function ce(t, e, n) {
      this.reset(t, e, n, void 0, void 0)
    }
    function he(t) {
      ;(this.pe = t), (this.Zd = this.uc = this.mb = this.$b = null)
    }
    function le(t, e) {
      ;(this.name = t), (this.value = e)
    }
    ;((h = ae.prototype).nb = function(t, e, n, r) {
      return this.Tf(t, e, n, r)
    }),
      (h.Tf = function(t, e, n, r) {
        v(e) || (e && (ue[0] = e.toString()), (e = ue))
        for (var i = 0; i < e.length; i++) {
          var o = _t(t, e[i], n || this.handleEvent, r || !1, this.i || this)
          if (!o) break
          this.o[o.key] = o
        }
        return this
      }),
      (h.Oc = function(t, e, n, r) {
        return this.je(t, e, n, r)
      }),
      (h.je = function(t, e, n, r, i) {
        if (v(e)) for (var o = 0; o < e.length; o++) this.je(t, e[o], n, r, i)
        else {
          if (!(t = xt(t, e, n || this.handleEvent, r, i || this.i || this)))
            return this
          this.o[t.key] = t
        }
        return this
      }),
      (h.ed = function(t, e, n, r, i) {
        if (v(e)) for (var o = 0; o < e.length; o++) this.ed(t, e[o], n, r, i)
        else
          (n = n || this.handleEvent),
            (r = T(r) ? !!r.capture : !!r),
            (i = i || this.i || this),
            (n = Wt(n)),
            (r = !!r),
            (e = Nt(t)
              ? t.jb(e, n, r, i)
              : t && (t = Qt(t))
                ? t.jb(e, n, r, i)
                : null) && (qt(e), delete this.o[e.key])
      }),
      (h.pb = function() {
        Y(
          this.o,
          function(t, e) {
            this.o.hasOwnProperty(e) && qt(t)
          },
          this,
        ),
          (this.o = {})
      }),
      (h.F = function() {
        ae.L.F.call(this), this.pb()
      }),
      (h.handleEvent = function() {
        throw Error('EventHandler.handleEvent not implemented')
      }),
      (ce.prototype.Md = null),
      (ce.prototype.reset = function(t, e, n, r, i) {
        ;(this.mb = t), delete this.Md
      }),
      (ce.prototype.Bg = function(t) {
        this.Md = t
      }),
      (ce.prototype.Ge = function(t) {
        this.mb = t
      }),
      (le.prototype.toString = function() {
        return this.name
      })
    var fe = new le('SEVERE', 1e3),
      de = new le('WARNING', 900),
      pe = new le('INFO', 800),
      me = new le('CONFIG', 700),
      ye = new le('FINE', 500)
    ;((h = he.prototype).getName = function() {
      return this.pe
    }),
      (h.getParent = function() {
        return this.$b
      }),
      (h.pf = function() {
        return this.uc || (this.uc = {}), this.uc
      }),
      (h.Ge = function(t) {
        this.mb = t
      }),
      (h.Qd = function() {
        return this.mb
          ? this.mb
          : this.$b
            ? this.$b.Qd()
            : ((function(t, e) {
                throw new M(
                  'Failure' + (t ? ': ' + t : ''),
                  Array.prototype.slice.call(arguments, 1),
                )
              })('Root logger has no level set.'),
              null)
      }),
      (h.Pf = function(t) {
        return t.value >= this.Qd().value
      }),
      (h.log = function(t, e, n) {
        this.Pf(t) && (w(e) && (e = e()), this.gf(this.uf(t, e, n)))
      }),
      (h.uf = function(t, e, n) {
        return (t = new ce(t, String(e), this.pe)), n && t.Bg(n), t
      }),
      (h.ca = function(t, e) {
        this.log(fe, t, e)
      }),
      (h.T = function(t, e) {
        this.log(de, t, e)
      }),
      (h.info = function(t, e) {
        this.log(pe, t, e)
      }),
      (h.lf = function(t) {
        this.log(ye, t, void 0)
      }),
      (h.gf = function(t) {
        for (var e = this; e; ) e.We(t), (e = e.getParent())
      }),
      (h.We = function(t) {
        if (this.Zd) for (var e, n = 0; (e = this.Zd[n]); n++) e(t)
      }),
      (h.Fg = function(t) {
        this.$b = t
      }),
      (h.Qe = function(t, e) {
        this.pf()[t] = e
      })
    var ge = {},
      ve = null
    function be(t) {
      var e
      if ((ve || ((ve = new he('')), (ge[''] = ve), ve.Ge(me)), !(e = ge[t]))) {
        e = new he(t)
        var n = t.lastIndexOf('.'),
          r = t.substr(n + 1)
        ;(n = be(t.substr(0, n))).Qe(r, e), e.Fg(n), (ge[t] = e)
      }
      return e
    }
    function we(t, e) {
      t && t.info(e, void 0)
    }
    function Te(t, e) {
      t && t.lf(e)
    }
    function Se() {
      ;(this.s = be('goog.labs.net.webChannel.WebChannelDebug')), (this.Wc = !0)
    }
    ;((h = Se.prototype).Id = function() {
      this.Wc = !1
    }),
      (h.Tg = function(t, e, n, r, i) {
        var o = this
        this.info(function() {
          return (
            'XMLHTTP REQ (' +
            n +
            ') [attempt ' +
            r +
            ']: ' +
            t +
            '\n' +
            e +
            '\n' +
            o.Xf(i)
          )
        })
      }),
      (h.Ug = function(t, e, n, r, i, o) {
        this.info(function() {
          return (
            'XMLHTTP RESP (' +
            n +
            ') [ attempt ' +
            r +
            ']: ' +
            t +
            '\n' +
            e +
            '\n' +
            i +
            ' ' +
            o
          )
        })
      }),
      (h.Wa = function(t, e, n) {
        var r = this
        this.info(function() {
          return 'XMLHTTP TEXT (' + t + '): ' + r.ng(e) + (n ? ' ' + n : '')
        })
      }),
      (h.Sg = function(t) {
        this.info(function() {
          return 'TIMEOUT: ' + t
        })
      }),
      (h.debug = function(t) {
        Te(this.s, t)
      }),
      (h.cb = function(t, e) {
        var n = this.s
        n && n.ca(e || 'Exception', t)
      }),
      (h.info = function(t) {
        we(this.s, t)
      }),
      (h.T = function(t) {
        var e = this.s
        e && e.T(t, void 0)
      }),
      (h.ca = function(t) {
        var e = this.s
        e && e.ca(t, void 0)
      }),
      (h.ng = function(t) {
        if (!this.Wc) return t
        if (!t) return null
        try {
          var e = JSON.parse(t)
          if (e) for (var n = 0; n < e.length; n++) v(e[n]) && this.Wf(e[n])
          return Gt(e)
        } catch (e) {
          return (
            this.debug(
              'Exception parsing expected JS array - probably was not JS',
            ),
            t
          )
        }
      }),
      (h.Wf = function(t) {
        if (!(2 > t.length || ((t = t[1]), !v(t) || 1 > t.length))) {
          var e = t[0]
          if ('noop' != e && 'stop' != e && 'close' != e)
            for (e = 1; e < t.length; e++) t[e] = ''
        }
      }),
      (h.Xf = function(t) {
        if (!this.Wc) return t
        if (!t) return null
        var e = ''
        t = t.split('&')
        for (var n = 0; n < t.length; n++) {
          var r = t[n].split('=')
          if (1 < r.length) {
            var i = r[0]
            r = r[1]
            var o = i.split('_')
            e =
              2 <= o.length && 'type' == o[1]
                ? e + (i + '=') + r + '&'
                : e + (i + '=redacted&')
          }
        }
        return e
      })
    var Ee = new jt()
    function Ie(t) {
      Et.call(this, 'serverreachability', t)
    }
    function Ce(t) {
      Ee.dispatchEvent(new Ie(Ee, t))
    }
    function De(t, e) {
      Et.call(this, 'statevent', t), (this.stat = e)
    }
    function Ne(t) {
      Ee.dispatchEvent(new De(Ee, t))
    }
    function Ae(t, e, n) {
      Et.call(this, 'timingevent', t), (this.size = e), (this.rtt = n)
    }
    function ke(t, e) {
      if (!w(t)) throw Error('Fn must not be null and must be a function')
      return d.setTimeout(function() {
        t()
      }, e)
    }
    k(Ie, Et), k(De, Et), k(Ae, Et)
    var Re = {
        NO_ERROR: 0,
        Vg: 1,
        bh: 2,
        ah: 3,
        Yg: 4,
        $g: 5,
        dh: 6,
        Ne: 7,
        TIMEOUT: 8,
        gh: 9,
      },
      Me = {
        Xg: 'complete',
        kh: 'success',
        Oe: 'error',
        Ne: 'abort',
        ih: 'ready',
        jh: 'readystatechange',
        TIMEOUT: 'timeout',
        eh: 'incrementaldata',
        hh: 'progress',
        Zg: 'downloadprogress',
        lh: 'uploadprogress',
      }
    function Oe() {}
    function Pe() {}
    ;(Oe.prototype.pd = null),
      (Oe.prototype.Vd = function() {
        return this.pd || (this.pd = this.Mf())
      })
    var _e,
      Le = { OPEN: 'a', Wg: 'b', Oe: 'c', fh: 'd' }
    function xe() {
      Et.call(this, 'd')
    }
    function qe() {
      Et.call(this, 'c')
    }
    function Fe() {}
    function Ve(t, e, n, r, i) {
      ;(this.b = t),
        (this.a = e),
        (this.ra = n),
        (this.R = r),
        (this.Xc = i || 1),
        (this.Fc = new ae(this)),
        (this.Ua = Be),
        (t = ut ? 125 : void 0),
        (this.Vc = new ie(t)),
        (this.A = null),
        (this.S = !1),
        (this.Da = this.pa = this.ua = this.ic = this.qb = this.hd = this.Ga = null),
        (this.ba = []),
        (this.h = null),
        (this.Bb = 0),
        (this.I = this.Fa = null),
        (this.w = -1),
        (this.Za = !1),
        (this.Ra = 0),
        (this.ac = null),
        (this.lb = this.Ed = this.yc = !1)
    }
    k(xe, Et),
      k(qe, Et),
      k(Fe, Oe),
      (Fe.prototype.Dd = function() {
        var t = this.Wd()
        return t ? new ActiveXObject(t) : new XMLHttpRequest()
      }),
      (Fe.prototype.Mf = function() {
        var t = {}
        return this.Wd() && ((t[0] = !0), (t[1] = !0)), t
      }),
      (Fe.prototype.Wd = function() {
        if (
          !this.be &&
          'undefined' == typeof XMLHttpRequest &&
          'undefined' != typeof ActiveXObject
        ) {
          for (
            var t = [
                'MSXML2.XMLHTTP.6.0',
                'MSXML2.XMLHTTP.3.0',
                'MSXML2.XMLHTTP',
                'Microsoft.XMLHTTP',
              ],
              e = 0;
            e < t.length;
            e++
          ) {
            var n = t[e]
            try {
              return new ActiveXObject(n), (this.be = n)
            } catch (t) {}
          }
          throw Error(
            'Could not create ActiveXObject. ActiveX might be disabled, or MSXML might not be installed',
          )
        }
        return this.be
      }),
      (_e = new Fe())
    var Be = 45e3
    var Ue = {},
      Qe = {}
    function Ke(t) {
      if (t.H && 'function' == typeof t.H) return t.H()
      if (p(t)) return t.split('')
      if (b(t)) {
        for (var e = [], n = t.length, r = 0; r < n; r++) e.push(t[r])
        return e
      }
      return J(t)
    }
    function We(t, e, n) {
      if (t.forEach && 'function' == typeof t.forEach) t.forEach(e, n)
      else if (b(t) || p(t)) q(t, e, n)
      else {
        if (t.W && 'function' == typeof t.W) var r = t.W()
        else if (t.H && 'function' == typeof t.H) r = void 0
        else if (b(t) || p(t)) {
          r = []
          for (var i = t.length, o = 0; o < i; o++) r.push(o)
        } else r = $(t)
        o = (i = Ke(t)).length
        for (var s = 0; s < o; s++) e.call(n, i[s], r && r[s], t)
      }
    }
    function je(t, e) {
      ;(this.D = {}), (this.o = []), (this.j = 0)
      var n = arguments.length
      if (1 < n) {
        if (n % 2) throw Error('Uneven number of arguments')
        for (var r = 0; r < n; r += 2) this.set(arguments[r], arguments[r + 1])
      } else t && this.addAll(t)
    }
    function Ge(t, e) {
      return Object.prototype.hasOwnProperty.call(t, e)
    }
    ;((h = Ve.prototype).ga = function(t) {
      this.A = t
    }),
      (h.setTimeout = function(t) {
        this.Ua = t
      }),
      (h.He = function(t) {
        this.Ra = t
      }),
      (h.Gg = function(t) {
        this.ba = t
      }),
      (h.la = function() {
        return this.ba
      }),
      (h.kd = function(t, e) {
        ;(this.ic = 1),
          (this.ua = t.clone().Ub()),
          (this.Da = e),
          (this.yc = !0),
          this.Ce(null)
      }),
      (h.jd = function(t, e, n) {
        ;(this.ic = 1),
          (this.ua = t.clone().Ub()),
          (this.Da = null),
          (this.yc = e),
          this.Ce(n)
      }),
      (h.Ce = function(t) {
        ;(this.qb = A()),
          this.eb(),
          (this.pa = this.ua.clone()),
          this.pa.dc('t', this.Xc),
          (this.Bb = 0),
          (this.h = this.b.Jb(this.b.fc() ? t : null)),
          0 < this.Ra && (this.ac = new se(D(this.Me, this, this.h), this.Ra)),
          this.Fc.nb(this.h, 'readystatechange', this.mg),
          (t = this.A ? Z(this.A) : {}),
          this.Da
            ? (this.Fa || (this.Fa = 'POST'),
              (t['Content-Type'] = 'application/x-www-form-urlencoded'),
              this.h.send(this.pa, this.Fa, this.Da, t))
            : ((this.Fa = 'GET'), this.h.send(this.pa, this.Fa, null, t)),
          Ce(1),
          this.a.Tg(this.Fa, this.pa, this.R, this.Xc, this.Da)
      }),
      (h.mg = function(t) {
        t = t.target
        var e = this.ac
        e && 3 == t.ma()
          ? (this.a.debug('Throttling readystatechange.'), e.mf())
          : this.Me(t)
      }),
      (h.Me = function(t) {
        try {
          t == this.h
            ? this.hg()
            : this.a.T('Called back with an unexpected xmlhttp')
        } catch (t) {
          if (
            (this.a.debug('Failed call to OnXmlHttpReadyStateChanged_'),
            this.h && this.h.ya())
          ) {
            var e = this
            this.a.cb(t, function() {
              return 'ResponseText: ' + e.h.ya()
            })
          } else this.a.cb(t, 'No response text')
        }
      }),
      (h.hg = function() {
        var t = this.h.ma(),
          e = this.h.Ud(),
          n = this.h.za()
        if (!(3 > t || (3 == t && !ut && !this.h.ya()))) {
          this.Za || 4 != t || 7 == e || Ce(8 == e || 0 >= n ? 3 : 2), this.Fb()
          var r = this.h.za()
          if (((this.w = r), !(e = this.h.ya()))) {
            var i = this
            this.a.debug(function() {
              return 'No response text for uri ' + i.pa + ' status ' + r
            })
          }
          if (
            ((this.S = 200 == r),
            this.a.Ug(this.Fa, this.pa, this.R, this.Xc, t, r),
            this.S)
          ) {
            if (this.Ig()) {
              if (!(n = this.sf()))
                return (
                  (this.S = !1),
                  (this.I = 3),
                  Ne(12),
                  this.a.T(
                    'XMLHTTP Missing X_HTTP_INITIAL_RESPONSE (' + this.R + ')',
                  ),
                  this.Ia(),
                  void this.Kb()
                )
              this.a.Wa(
                this.R,
                n,
                'Initial handshake response via X-HTTP-Initial-Response',
              ),
                (this.lb = !0),
                this.Yc(n)
            }
            this.yc
              ? (this.Fd(t, e), ut && this.S && 3 == t && this.Ng())
              : (this.a.Wa(this.R, e, null), this.Yc(e)),
              4 == t && this.Ia(),
              this.S &&
                !this.Za &&
                (4 == t ? this.b.Tc(this) : ((this.S = !1), this.eb()))
          } else
            400 == r && 0 < e.indexOf('Unknown SID')
              ? ((this.I = 3),
                Ne(12),
                this.a.T('XMLHTTP Unknown SID (' + this.R + ')'))
              : ((this.I = 0),
                Ne(13),
                this.a.T('XMLHTTP Bad status ' + r + ' (' + this.R + ')')),
              this.Ia(),
              this.Kb()
        }
      }),
      (h.Ig = function() {
        return this.Ed && !this.lb
      }),
      (h.sf = function() {
        if (this.h) {
          var t = this.h.kb('X-HTTP-Initial-Response')
          if (t && !K(t)) return t
        }
        return null
      }),
      (h.Ag = function() {
        this.Ed = !0
      }),
      (h.Fd = function(t, e) {
        for (var n = !0; !this.Za && this.Bb < e.length; ) {
          var r = this.vf(e)
          if (r == Qe) {
            4 == t && ((this.I = 4), Ne(14), (n = !1)),
              this.a.Wa(this.R, null, '[Incomplete Response]')
            break
          }
          if (r == Ue) {
            ;(this.I = 4),
              Ne(15),
              this.a.Wa(this.R, e, '[Invalid Chunk]'),
              (n = !1)
            break
          }
          this.a.Wa(this.R, r, null), this.Yc(r)
        }
        4 == t && 0 == e.length && ((this.I = 1), Ne(16), (n = !1)),
          (this.S = this.S && n),
          n ||
            (this.a.Wa(this.R, e, '[Invalid Chunked Response]'),
            this.Ia(),
            this.Kb())
      }),
      (h.kg = function() {
        if (this.h) {
          var t = this.h.ma(),
            e = this.h.ya()
          this.Bb < e.length &&
            (this.Fb(), this.Fd(t, e), this.S && 4 != t && this.eb())
        }
      }),
      (h.Ng = function() {
        this.Fc.nb(this.Vc, 'tick', this.kg), this.Vc.start()
      }),
      (h.vf = function(t) {
        var e = this.Bb,
          n = t.indexOf('\n', e)
        return -1 == n
          ? Qe
          : ((e = Number(t.substring(e, n))),
            isNaN(e)
              ? Ue
              : (n += 1) + e > t.length
                ? Qe
                : ((t = t.substr(n, e)), (this.Bb = n + e), t))
      }),
      (h.yg = function(t) {
        ;(this.ic = 2),
          (this.ua = t.clone().Ub()),
          (t = !1),
          d.navigator &&
            d.navigator.sendBeacon &&
            (t = d.navigator.sendBeacon(this.ua.toString(), '')),
          !t && d.Image && ((new Image().src = this.ua), (t = !0)),
          t || ((this.h = this.b.Jb(null)), this.h.send(this.ua)),
          (this.qb = A()),
          this.eb()
      }),
      (h.cancel = function() {
        ;(this.Za = !0), this.Ia()
      }),
      (h.tg = function(t) {
        t && this.setTimeout(t), this.Ga && (this.Fb(), this.eb())
      }),
      (h.eb = function() {
        ;(this.hd = A() + this.Ua), this.Ke(this.Ua)
      }),
      (h.Ke = function(t) {
        if (null != this.Ga) throw Error('WatchDog timer not null')
        this.Ga = ke(D(this.gg, this), t)
      }),
      (h.Fb = function() {
        this.Ga && (d.clearTimeout(this.Ga), (this.Ga = null))
      }),
      (h.gg = function() {
        this.Ga = null
        var t = A()
        0 <= t - this.hd
          ? this.Df()
          : (this.a.T('WatchDog timer called too early'), this.Ke(this.hd - t))
      }),
      (h.Df = function() {
        this.S &&
          this.a.ca(
            'Received watchdog timeout even though request loaded successfully',
          ),
          this.a.Sg(this.pa),
          2 != this.ic && (Ce(3), Ne(17)),
          this.Ia(),
          (this.I = 2),
          this.Kb()
      }),
      (h.Kb = function() {
        this.b.de() || this.Za || this.b.Tc(this)
      }),
      (h.Ia = function() {
        this.Fb()
        var t = this.ac
        t && 'function' == typeof t.bb && t.bb(),
          (this.ac = null),
          this.Vc.stop(),
          this.Fc.pb(),
          this.h && ((t = this.h), (this.h = null), t.abort(), t.bb())
      }),
      (h.Hc = function() {
        return this.I
      }),
      (h.Yc = function(t) {
        try {
          this.b.ue(this, t), Ce(4)
        } catch (t) {
          this.a.cb(t, 'Error in httprequest callback')
        }
      }),
      ((h = je.prototype).C = function() {
        return this.j
      }),
      (h.H = function() {
        this.wc()
        for (var t = [], e = 0; e < this.o.length; e++)
          t.push(this.D[this.o[e]])
        return t
      }),
      (h.W = function() {
        return this.wc(), this.o.concat()
      }),
      (h.va = function(t) {
        return Ge(this.D, t)
      }),
      (h.X = function() {
        return 0 == this.j
      }),
      (h.clear = function() {
        ;(this.D = {}), (this.j = this.o.length = 0)
      }),
      (h.remove = function(t) {
        return (
          !!Ge(this.D, t) &&
          (delete this.D[t],
          this.j--,
          this.o.length > 2 * this.j && this.wc(),
          !0)
        )
      }),
      (h.wc = function() {
        if (this.j != this.o.length) {
          for (var t = 0, e = 0; t < this.o.length; ) {
            var n = this.o[t]
            Ge(this.D, n) && (this.o[e++] = n), t++
          }
          this.o.length = e
        }
        if (this.j != this.o.length) {
          var r = {}
          for (e = t = 0; t < this.o.length; )
            Ge(r, (n = this.o[t])) || ((this.o[e++] = n), (r[n] = 1)), t++
          this.o.length = e
        }
      }),
      (h.get = function(t, e) {
        return Ge(this.D, t) ? this.D[t] : e
      }),
      (h.set = function(t, e) {
        Ge(this.D, t) || (this.j++, this.o.push(t)), (this.D[t] = e)
      }),
      (h.addAll = function(t) {
        if (t instanceof je)
          for (var e = t.W(), n = 0; n < e.length; n++)
            this.set(e[n], t.get(e[n]))
        else for (e in t) this.set(e, t[e])
      }),
      (h.forEach = function(t, e) {
        for (var n = this.W(), r = 0; r < n.length; r++) {
          var i = n[r],
            o = this.get(i)
          t.call(e, o, i, this)
        }
      }),
      (h.clone = function() {
        return new je(this)
      })
    var ze = /^(?:([^:/?#.]+):)?(?:\/\/(?:([^/?#]*)@)?([^/#?]*?)(?::([0-9]+))?(?=[/#?]|$))?([^?#]+)?(?:\?([^#]*))?(?:#([\s\S]*))?$/
    function He(t, e) {
      var n
      ;(this.xa = this.zb = this.qa = ''),
        (this.Ca = null),
        (this.ib = this.K = ''),
        (this.O = this.Qf = !1),
        t instanceof He
          ? ((this.O = void 0 !== e ? e : t.O),
            this.tb(t.qa),
            this.cd(t.zb),
            this.rb(t.xa),
            this.sb(t.Ca),
            this.ec(t.K),
            this.bd(t.P.clone()),
            this.$c(t.ib))
          : t && (n = String(t).match(ze))
            ? ((this.O = !!e),
              this.tb(n[1] || '', !0),
              this.cd(n[2] || '', !0),
              this.rb(n[3] || '', !0),
              this.sb(n[4]),
              this.ec(n[5] || '', !0),
              this.bd(n[6] || '', !0),
              this.$c(n[7] || '', !0))
            : ((this.O = !!e), (this.P = new rn(null, this.O)))
    }
    function Xe(t, e) {
      return t
        ? e
          ? decodeURI(t.replace(/%25/g, '%2525'))
          : decodeURIComponent(t)
        : ''
    }
    function Ye(t, e, n) {
      return p(t)
        ? ((t = encodeURI(t).replace(e, Je)),
          n && (t = t.replace(/%25([0-9a-fA-F]{2})/g, '%$1')),
          t)
        : null
    }
    function Je(t) {
      return (
        '%' +
        (((t = t.charCodeAt(0)) >> 4) & 15).toString(16) +
        (15 & t).toString(16)
      )
    }
    ;((h = He.prototype).toString = function() {
      var t = [],
        e = this.qa
      e && t.push(Ye(e, $e, !0), ':')
      var n = this.xa
      return (
        (n || 'file' == e) &&
          (t.push('//'),
          (e = this.zb) && t.push(Ye(e, $e, !0), '@'),
          t.push(
            encodeURIComponent(String(n)).replace(
              /%25([0-9a-fA-F]{2})/g,
              '%$1',
            ),
          ),
          null != (n = this.Ca) && t.push(':', String(n))),
        (n = this.K) &&
          (this.Ic() && '/' != n.charAt(0) && t.push('/'),
          t.push(Ye(n, '/' == n.charAt(0) ? tn : Ze, !0))),
        (n = this.Rd()) && t.push('?', n),
        (n = this.ib) && t.push('#', Ye(n, nn)),
        t.join('')
      )
    }),
      (h.resolve = function(t) {
        var e = this.clone(),
          n = t.Hf()
        n ? e.tb(t.qa) : (n = t.If()),
          n ? e.cd(t.zb) : (n = t.Ic()),
          n ? e.rb(t.xa) : (n = t.Ff())
        var r = t.K
        if (n) e.sb(t.Ca)
        else if ((n = t.ae())) {
          if ('/' != r.charAt(0))
            if (this.Ic() && !this.ae()) r = '/' + r
            else {
              var i = e.K.lastIndexOf('/')
              ;-1 != i && (r = e.K.substr(0, i + 1) + r)
            }
          if ('..' == (i = r) || '.' == i) r = ''
          else if (-1 != i.indexOf('./') || -1 != i.indexOf('/.')) {
            ;(r = 0 == i.lastIndexOf('/', 0)), (i = i.split('/'))
            for (var o = [], s = 0; s < i.length; ) {
              var a = i[s++]
              '.' == a
                ? r && s == i.length && o.push('')
                : '..' == a
                  ? ((1 < o.length || (1 == o.length && '' != o[0])) && o.pop(),
                    r && s == i.length && o.push(''))
                  : (o.push(a), (r = !0))
            }
            r = o.join('/')
          } else r = i
        }
        return (
          n ? e.ec(r) : (n = t.Gf()),
          n ? e.bd(t.P.clone()) : (n = t.Ef()),
          n && e.$c(t.ib),
          e
        )
      }),
      (h.clone = function() {
        return new He(this)
      }),
      (h.tb = function(t, e) {
        this.U(),
          (this.qa = e ? Xe(t, !0) : t) && (this.qa = this.qa.replace(/:$/, ''))
      }),
      (h.Hf = function() {
        return !!this.qa
      }),
      (h.cd = function(t, e) {
        this.U(), (this.zb = e ? Xe(t) : t)
      }),
      (h.If = function() {
        return !!this.zb
      }),
      (h.rb = function(t, e) {
        this.U(), (this.xa = e ? Xe(t, !0) : t)
      }),
      (h.Ic = function() {
        return !!this.xa
      }),
      (h.sb = function(t) {
        if ((this.U(), t)) {
          if (((t = Number(t)), isNaN(t) || 0 > t))
            throw Error('Bad port number ' + t)
          this.Ca = t
        } else this.Ca = null
      }),
      (h.Ff = function() {
        return null != this.Ca
      }),
      (h.ec = function(t, e) {
        this.U(), (this.K = e ? Xe(t, !0) : t)
      }),
      (h.ae = function() {
        return !!this.K
      }),
      (h.Gf = function() {
        return '' !== this.P.toString()
      }),
      (h.bd = function(t, e) {
        this.U(),
          t instanceof rn
            ? ((this.P = t), this.P.ad(this.O))
            : (e || (t = Ye(t, en)), (this.P = new rn(t, this.O)))
      }),
      (h.Rd = function() {
        return this.P.toString()
      }),
      (h.getQuery = function() {
        return this.Rd()
      }),
      (h.l = function(t, e) {
        this.U(), this.P.set(t, e)
      }),
      (h.dc = function(t, e) {
        this.U(), v(e) || (e = [String(e)]), this.P.Ie(t, e)
      }),
      (h.$c = function(t, e) {
        this.U(), (this.ib = e ? Xe(t) : t)
      }),
      (h.Ef = function() {
        return !!this.ib
      }),
      (h.Ub = function() {
        return (
          this.U(),
          this.l(
            'zx',
            Math.floor(2147483648 * Math.random()).toString(36) +
              Math.abs(Math.floor(2147483648 * Math.random()) ^ A()).toString(
                36,
              ),
          ),
          this
        )
      }),
      (h.removeParameter = function(t) {
        return this.U(), this.P.remove(t), this
      }),
      (h.U = function() {
        if (this.Qf) throw Error('Tried to modify a read-only Uri')
      }),
      (h.ad = function(t) {
        ;(this.O = t), this.P && this.P.ad(t)
      })
    var $e = /[#\/\?@]/g,
      Ze = /[#\?:]/g,
      tn = /[#\?]/g,
      en = /[#\?@]/g,
      nn = /#/g
    function rn(t, e) {
      ;(this.j = this.m = null), (this.ja = t || null), (this.O = !!e)
    }
    function on(t, e) {
      ;(this.b = t),
        (this.a = e),
        (this.f = this.A = null),
        (this.bc = !1),
        (this.K = null),
        (this.w = -1),
        (this.Ad = this.na = null)
    }
    function sn(t) {
      ;(this.D = new je()), t && this.addAll(t)
    }
    function an(t) {
      var e = typeof t
      return ('object' == e && t) || 'function' == e
        ? 'o' + (t[S] || (t[S] = ++E))
        : e.substr(0, 1) + t
    }
    function un(t) {
      ;(this.me = t || cn),
        d.PerformanceNavigationTiming
          ? (t =
              0 < (t = d.performance.getEntriesByType('navigation')).length &&
              ('hq' == t[0].nextHopProtocol || 'h2' == t[0].nextHopProtocol))
          : (t = !!(d.vc && d.vc.ke && d.vc.ke() && d.vc.ke().nh)),
        (this.Xb = t ? this.me : 1),
        (this.v = null),
        1 < this.Xb && (this.v = new sn()),
        (this.f = null),
        (this.ba = [])
    }
    ;((h = rn.prototype).$ = function() {
      if (!this.m && ((this.m = new je()), (this.j = 0), this.ja)) {
        var t = this
        !(function(t, e) {
          if (t) {
            t = t.split('&')
            for (var n = 0; n < t.length; n++) {
              var r = t[n].indexOf('='),
                i = null
              if (0 <= r) {
                var o = t[n].substring(0, r)
                i = t[n].substring(r + 1)
              } else o = t[n]
              e(o, i ? decodeURIComponent(i.replace(/\+/g, ' ')) : '')
            }
          }
        })(this.ja, function(e, n) {
          t.add(decodeURIComponent(e.replace(/\+/g, ' ')), n)
        })
      }
    }),
      (h.C = function() {
        return this.$(), this.j
      }),
      (h.add = function(t, e) {
        this.$(), this.Oa(), (t = this.Ma(t))
        var n = this.m.get(t)
        return n || this.m.set(t, (n = [])), n.push(e), (this.j += 1), this
      }),
      (h.remove = function(t) {
        return (
          this.$(),
          (t = this.Ma(t)),
          !!this.m.va(t) &&
            (this.Oa(), (this.j -= this.m.get(t).length), this.m.remove(t))
        )
      }),
      (h.clear = function() {
        this.Oa(), (this.m = null), (this.j = 0)
      }),
      (h.X = function() {
        return this.$(), 0 == this.j
      }),
      (h.va = function(t) {
        return this.$(), (t = this.Ma(t)), this.m.va(t)
      }),
      (h.forEach = function(t, e) {
        this.$(),
          this.m.forEach(function(n, r) {
            q(
              n,
              function(n) {
                t.call(e, n, r, this)
              },
              this,
            )
          }, this)
      }),
      (h.W = function() {
        this.$()
        for (
          var t = this.m.H(), e = this.m.W(), n = [], r = 0;
          r < e.length;
          r++
        )
          for (var i = t[r], o = 0; o < i.length; o++) n.push(e[r])
        return n
      }),
      (h.H = function(t) {
        this.$()
        var e = []
        if (p(t)) this.va(t) && (e = U(e, this.m.get(this.Ma(t))))
        else {
          t = this.m.H()
          for (var n = 0; n < t.length; n++) e = U(e, t[n])
        }
        return e
      }),
      (h.set = function(t, e) {
        return (
          this.$(),
          this.Oa(),
          (t = this.Ma(t)),
          this.va(t) && (this.j -= this.m.get(t).length),
          this.m.set(t, [e]),
          (this.j += 1),
          this
        )
      }),
      (h.get = function(t, e) {
        return t && 0 < (t = this.H(t)).length ? String(t[0]) : e
      }),
      (h.Ie = function(t, e) {
        this.remove(t),
          0 < e.length &&
            (this.Oa(), this.m.set(this.Ma(t), Q(e)), (this.j += e.length))
      }),
      (h.toString = function() {
        if (this.ja) return this.ja
        if (!this.m) return ''
        for (var t = [], e = this.m.W(), n = 0; n < e.length; n++) {
          var r = e[n],
            i = encodeURIComponent(String(r))
          r = this.H(r)
          for (var o = 0; o < r.length; o++) {
            var s = i
            '' !== r[o] && (s += '=' + encodeURIComponent(String(r[o]))),
              t.push(s)
          }
        }
        return (this.ja = t.join('&'))
      }),
      (h.Oa = function() {
        this.ja = null
      }),
      (h.clone = function() {
        var t = new rn()
        return (
          (t.ja = this.ja),
          this.m && ((t.m = this.m.clone()), (t.j = this.j)),
          t
        )
      }),
      (h.Ma = function(t) {
        return (t = String(t)), this.O && (t = t.toLowerCase()), t
      }),
      (h.ad = function(t) {
        t &&
          !this.O &&
          (this.$(),
          this.Oa(),
          this.m.forEach(function(t, e) {
            var n = e.toLowerCase()
            e != n && (this.remove(e), this.Ie(n, t))
          }, this)),
          (this.O = t)
      }),
      (h.extend = function(t) {
        for (var e = 0; e < arguments.length; e++)
          We(
            arguments[e],
            function(t, e) {
              this.add(e, t)
            },
            this,
          )
      }),
      k(function() {}, function() {}),
      ((h = on.prototype).g = null),
      (h.ga = function(t) {
        this.A = t
      }),
      (h.connect = function(t) {
        ;(this.K = t), (t = this.b.Sd(this.K)), Ne(3)
        var e = this.b.Ib.$d
        null != e
          ? ((this.na = this.b.$a(e[0])), (this.g = 1), this.xd())
          : (t.dc('MODE', 'init'),
            !this.b.ta && this.b.aa && t.dc('X-HTTP-Session-Id', this.b.aa),
            (this.f = new Ve(this, this.a, void 0, void 0, void 0)),
            this.f.ga(this.A),
            this.f.jd(t, !1, null),
            (this.g = 0))
      }),
      (h.xd = function() {
        this.a.debug('TestConnection: starting stage 2')
        var t = this.b.Ib.od
        if (null != t)
          this.a.debug(function() {
            return 'Buffered'
          }),
            Ne(4),
            t ? (Ne(10), this.b.ub(this, !1)) : (Ne(11), this.b.ub(this, !0))
        else {
          ;(this.f = new Ve(this, this.a, void 0, void 0, void 0)),
            this.f.ga(this.A)
          var e = this.b.Pd(this.na, this.K)
          Ne(4), e.dc('TYPE', 'xmlhttp')
          var n = this.b.aa,
            r = this.b.Kc
          n && r && e.l(n, r), this.f.jd(e, !1, this.na)
        }
      }),
      (h.Jb = function(t) {
        return this.b.Jb(t)
      }),
      (h.abort = function() {
        this.f && (this.f.cancel(), (this.f = null)), (this.w = -1)
      }),
      (h.de = function() {
        return !1
      }),
      (h.ue = function(t, e) {
        if (((this.w = t.w), 0 == this.g))
          if (
            (this.a.debug('TestConnection: Got data for stage 1'),
            this.pc(t),
            e)
          ) {
            try {
              var n = this.b.kc.zc(e)
            } catch (t) {
              return this.a.cb(t), void this.b.dd(this)
            }
            this.na = this.b.$a(n[0])
          } else
            this.a.debug('TestConnection: Null responseText'), this.b.dd(this)
        else
          1 == this.g &&
            (this.bc
              ? Ne(6)
              : '11111' == e
                ? (Ne(5),
                  (this.bc = !0),
                  this.Ze() &&
                    ((this.w = 200),
                    this.f.cancel(),
                    this.a.debug(
                      'Test connection succeeded; using streaming connection',
                    ),
                    Ne(11),
                    this.b.ub(this, !0)))
                : (Ne(7), (this.bc = !1)))
      }),
      (h.Tc = function() {
        ;(this.w = this.f.w),
          this.f.S
            ? 0 == this.g
              ? ((this.g = 1),
                this.a.debug(
                  'TestConnection: request complete for initial check',
                ),
                this.xd())
              : 1 == this.g &&
                (this.a.debug('TestConnection: request complete for stage 2'),
                this.bc
                  ? (this.a.debug(
                      'Test connection succeeded; using streaming connection',
                    ),
                    Ne(11),
                    this.b.ub(this, !0))
                  : (this.a.debug(
                      'Test connection failed; not using streaming',
                    ),
                    Ne(10),
                    this.b.ub(this, !1)))
            : (this.a.debug(
                'TestConnection: request failed, in state ' + this.g,
              ),
              0 == this.g ? Ne(8) : 1 == this.g && Ne(9),
              this.b.dd(this))
      }),
      (h.pc = function(t) {
        if (!this.b.ta && (t = t.h)) {
          var e = t.kb('X-Client-Wire-Protocol')
          ;(this.Ad = e || null),
            this.b.aa &&
              ((t = t.kb('X-HTTP-Session-Id'))
                ? this.b.Fe(t)
                : this.a.T(
                    'Missing X_HTTP_SESSION_ID in the handshake response',
                  ))
        }
      }),
      (h.fc = function() {
        return this.b.fc()
      }),
      (h.Ba = function() {
        return this.b.Ba()
      }),
      (h.Ze = function() {
        return !st || 10 <= Number(mt)
      }),
      ((h = sn.prototype).C = function() {
        return this.D.C()
      }),
      (h.add = function(t) {
        this.D.set(an(t), t)
      }),
      (h.addAll = function(t) {
        for (var e = (t = Ke(t)).length, n = 0; n < e; n++) this.add(t[n])
      }),
      (h.pb = function(t) {
        for (var e = (t = Ke(t)).length, n = 0; n < e; n++) this.remove(t[n])
      }),
      (h.remove = function(t) {
        return this.D.remove(an(t))
      }),
      (h.clear = function() {
        this.D.clear()
      }),
      (h.X = function() {
        return this.D.X()
      }),
      (h.contains = function(t) {
        return this.D.va(an(t))
      }),
      (h.H = function() {
        return this.D.H()
      }),
      (h.clone = function() {
        return new sn(this)
      })
    var cn = 10
    function hn() {
      this.xg = this.rg = void 0
    }
    function ln() {
      this.jg = new hn()
    }
    function fn(t, e, n, r, i) {
      try {
        t.debug(n),
          (e.onload = null),
          (e.onerror = null),
          (e.onabort = null),
          (e.ontimeout = null),
          i(r)
      } catch (e) {
        t.cb(e)
      }
    }
    ;((h = un.prototype).ld = function(t) {
      this.v ||
        (-1 == t.indexOf('spdy') &&
          -1 == t.indexOf('quic') &&
          -1 == t.indexOf('h2')) ||
        ((this.Xb = this.me),
        (this.v = new sn()),
        this.f && (this.oc(this.f), (this.f = null)))
    }),
      (h.ee = function() {
        return !!this.f || (!!this.v && this.v.C() >= this.Xb)
      }),
      (h.xf = function() {
        return this.f ? 1 : this.v ? this.v.C() : 0
      }),
      (h.Jc = function(t) {
        return this.f ? this.f == t : !!this.v && this.v.contains(t)
      }),
      (h.oc = function(t) {
        this.v ? this.v.add(t) : (this.f = t)
      }),
      (h.ze = function(t) {
        this.f && this.f == t
          ? (this.f = null)
          : this.v && this.v.contains(t) && this.v.remove(t)
      }),
      (h.cancel = function() {
        ;(this.ba = this.la()),
          this.f
            ? (this.f.cancel(), (this.f = null))
            : this.v &&
              !this.v.X() &&
              (q(this.v.H(), function(t) {
                t.cancel()
              }),
              this.v.clear())
      }),
      (h.la = function() {
        if (null != this.f) return this.ba.concat(this.f.la())
        if (null != this.v && !this.v.X()) {
          var t = this.ba
          return (
            q(this.v.H(), function(e) {
              t = t.concat(e.la())
            }),
            t
          )
        }
        return Q(this.ba)
      }),
      (h.Re = function(t) {
        this.ba = this.ba.concat(t)
      }),
      (h.$e = function() {
        this.ba.length = 0
      }),
      (hn.prototype.stringify = function(t) {
        return d.JSON.stringify(t, this.rg)
      }),
      (hn.prototype.parse = function(t) {
        return d.JSON.parse(t, this.xg)
      }),
      (ln.prototype.hf = function(t, e, n) {
        var r = n || ''
        try {
          We(t, function(t, n) {
            var i = t
            T(t) && (i = Gt(t)), e.push(r + n + '=' + encodeURIComponent(i))
          })
        } catch (t) {
          throw (e.push(r + 'type=' + encodeURIComponent('_badmap')), t)
        }
      }),
      (ln.prototype.jf = function(t, e, n) {
        for (var r = -1; ; ) {
          var i = ['count=' + e]
          ;-1 == r
            ? 0 < e
              ? ((r = t[0].Pc), i.push('ofs=' + r))
              : (r = 0)
            : i.push('ofs=' + r)
          for (var o = !0, s = 0; s < e; s++) {
            var a = t[s].Pc,
              u = t[s].map
            if (0 > (a -= r)) (r = Math.max(0, t[s].Pc - 100)), (o = !1)
            else
              try {
                this.hf(u, i, 'req' + a + '_')
              } catch (t) {
                n && n(u)
              }
          }
          if (o) return i.join('&')
        }
      }),
      (ln.prototype.zc = function(t) {
        return this.jg.parse(t)
      })
    var dn = d.JSON.parse
    function pn(t) {
      jt.call(this),
        (this.headers = new je()),
        (this.Xa = t || null),
        (this.ha = !1),
        (this.mc = this.c = null),
        (this.ge = this.Tb = ''),
        (this.Pa = 0),
        (this.I = ''),
        (this.Aa = this.Lc = this.Qb = this.Ec = !1),
        (this.vb = 0),
        (this.hc = null),
        (this.Ae = mn),
        (this.jc = this.lg = this.Ab = !1)
    }
    k(pn, jt)
    var mn = ''
    pn.prototype.s = be('goog.net.XhrIo')
    var yn = /^https?$/i,
      gn = ['POST', 'PUT']
    function vn(t) {
      return 'content-type' == t.toLowerCase()
    }
    function bn(t, e) {
      return {
        type: e,
        lengthComputable: t.lengthComputable,
        loaded: t.loaded,
        total: t.total,
      }
    }
    function wn(t, e, n) {
      t: {
        for (r in n) {
          var r = !1
          break t
        }
        r = !0
      }
      if (r) return t
      if (
        ((n = (function(t) {
          var e = ''
          return (
            Y(t, function(t, n) {
              ;(e += n), (e += ':'), (e += t), (e += '\r\n')
            }),
            e
          )
        })(n)),
        p(t))
      ) {
        if (
          ((e = encodeURIComponent(String(e))),
          (e += n = null != n ? '=' + encodeURIComponent(String(n)) : ''))
        ) {
          if (
            (0 > (n = t.indexOf('#')) && (n = t.length),
            0 > (r = t.indexOf('?')) || r > n)
          ) {
            r = n
            var i = ''
          } else i = t.substring(r + 1, n)
          ;(n = (t = [t.substr(0, r), i, t.substr(n)])[1]),
            (t[1] = e ? (n ? n + '&' + e : e) : n),
            (t = t[0] + (t[1] ? '?' + t[1] : '') + t[2])
        }
        return t
      }
      return t.l(e, n), t
    }
    function Tn(t) {
      ;(this.Bd = 22),
        (this.De = 0),
        (this.M = []),
        (this.a = new Se()),
        (this.Ib = new function() {
          this.od = this.$d = null
        }()),
        (this.na = this.md = this.hb = this.K = this.u = this.Kc = this.aa = this.gb = this.N = this.Rb = this.A = null),
        (this.Te = !0),
        (this.ag = this.Yb = 0),
        (this.kf = !!m('internalChannelParams.failFast', t)),
        (this.fd = this.Ja = this.wa = this.ia = this.ea = this.i = null),
        (this.Se = !0),
        (this.w = this.he = this.Sb = -1),
        (this.rc = this.Ha = this.La = 0),
        (this.Ve = m('internalChannelParams.baseRetryDelayMs', t) || 5e3),
        (this.vg = m('internalChannelParams.retryDelaySeedMs', t) || 1e4),
        (this.nf = m('internalChannelParams.forwardChannelMaxRetries', t) || 2),
        (this.Od =
          m('internalChannelParams.forwardChannelRequestTimeoutMs', t) || 2e4),
        (this.Xa = (t && t.oh) || void 0),
        (this.Db = void 0),
        (this.Ra = 0),
        (this.gc = (t && t.supportsCrossDomainXhr) || !1),
        (this.ra = ''),
        (this.G = new un(t && t.concurrentRequestLimit)),
        (this.kc = new ln()),
        (this.ta =
          !t || void 0 === t.backgroundChannelTest || t.backgroundChannelTest),
        (this.Nd = (t && t.fastHandshake) || !1) &&
          !this.ta &&
          (this.a.T(
            'Force backgroundChannelTest when fastHandshake is enabled.',
          ),
          (this.ta = !0)),
        t && t.Id && this.a.Id()
    }
    function Sn() {}
    function En() {
      if (st && !(10 <= Number(mt)))
        throw Error('Environmental error: no available transport.')
    }
    function In(t, e) {
      jt.call(this),
        (this.b = new Tn(e)),
        (this.yb = t),
        (this.Qg =
          e && e.testUrl
            ? e.testUrl
            : (function(t) {
                for (var e = arguments[0], n = 1; n < arguments.length; n++) {
                  var r,
                    i = arguments[n]
                  0 == i.lastIndexOf('/', 0)
                    ? (e = i)
                    : ((r = '' == e) ||
                        (r = 0 <= (r = e.length - 1) && e.indexOf('/', r) == r),
                      (e = r ? e + i : e + '/' + i))
                }
                return e
              })(this.yb, 'test')),
        (this.s = be('goog.labs.net.webChannel.WebChannelBaseTransport')),
        (this.Rc = (e && e.messageUrlParams) || null),
        (t = (e && e.messageHeaders) || null),
        e &&
          e.clientProtocolHeaderRequired &&
          (t
            ? (t['X-Client-Protocol'] = 'webchannel')
            : (t = { 'X-Client-Protocol': 'webchannel' })),
        this.b.ga(t),
        (t = (e && e.initMessageHeaders) || null),
        e &&
          e.messageContentType &&
          (t
            ? (t['X-WebChannel-Content-Type'] = e.messageContentType)
            : (t = { 'X-WebChannel-Content-Type': e.messageContentType })),
        e &&
          e.zd &&
          (t
            ? (t['X-WebChannel-Client-Profile'] = e.zd)
            : (t = { 'X-WebChannel-Client-Profile': e.zd })),
        this.b.Eg(t),
        (t = e && e.httpHeadersOverwriteParam) && !K(t) && this.b.Cg(t),
        (this.Og = (e && e.supportsCrossDomainXhr) || !1),
        (this.zg = (e && e.sendRawJson) || !1),
        (e = e && e.httpSessionIdParam) &&
          !K(e) &&
          (this.b.Dg(e),
          null !== (t = this.Rc) &&
            e in t &&
            (e in (t = this.Rc) && delete t[e],
            (t = this.s) &&
              t.T(
                'Ignore httpSessionIdParam also specified with messageUrlParams: ' +
                  e,
                void 0,
              ))),
        (this.vd = new Nn(this))
    }
    function Cn(t) {
      xe.call(this)
      var e = t.__sm__
      if (e) {
        t: {
          for (var n in e) {
            t = n
            break t
          }
          t = void 0
        }
        ;(this.ne = t) &&
          ((t = this.ne), (e = null !== e && t in e ? e[t] : void 0)),
          (this.data = e)
      } else this.data = t
    }
    function Dn(t) {
      qe.call(this), (this.status = 1), (this.errorCode = t)
    }
    function Nn(t) {
      this.b = t
    }
    ;((h = pn.prototype).Je = function(t) {
      this.Ab = t
    }),
      (h.send = function(t, e, n, r) {
        if (this.c)
          throw Error(
            '[goog.net.XhrIo] Object is active with another request=' +
              this.Tb +
              '; newUri=' +
              t,
          )
        ;(e = e ? e.toUpperCase() : 'GET'),
          (this.Tb = t),
          (this.I = ''),
          (this.Pa = 0),
          (this.ge = e),
          (this.Ec = !1),
          (this.ha = !0),
          (this.c = this.df()),
          (this.mc = this.Xa ? this.Xa.Vd() : _e.Vd()),
          (this.c.onreadystatechange = D(this.te, this)),
          this.lg &&
            'onprogress' in this.c &&
            ((this.c.onprogress = D(function(t) {
              this.re(t, !0)
            }, this)),
            this.c.upload && (this.c.upload.onprogress = D(this.re, this)))
        try {
          Te(this.s, this.da('Opening Xhr')),
            (this.Lc = !0),
            this.c.open(e, String(t), !0),
            (this.Lc = !1)
        } catch (t) {
          return (
            Te(this.s, this.da('Error opening Xhr: ' + t.message)),
            void this.Ld(t)
          )
        }
        t = n || ''
        var i = this.headers.clone()
        r &&
          We(r, function(t, e) {
            i.set(e, t)
          }),
          (r = (function(t) {
            t: {
              for (
                var e = vn, n = t.length, r = p(t) ? t.split('') : t, i = 0;
                i < n;
                i++
              )
                if (i in r && e.call(void 0, r[i], i, t)) {
                  e = i
                  break t
                }
              e = -1
            }
            return 0 > e ? null : p(t) ? t.charAt(e) : t[e]
          })(i.W())),
          (n = d.FormData && t instanceof d.FormData),
          !(0 <= L(gn, e)) ||
            r ||
            n ||
            i.set(
              'Content-Type',
              'application/x-www-form-urlencoded;charset=utf-8',
            ),
          i.forEach(function(t, e) {
            this.c.setRequestHeader(e, t)
          }, this),
          this.Ae && (this.c.responseType = this.Ae),
          'withCredentials' in this.c &&
            this.c.withCredentials !== this.Ab &&
            (this.c.withCredentials = this.Ab)
        try {
          this.yd(),
            0 < this.vb &&
              ((this.jc = (function(t) {
                return (
                  st &&
                  gt(9) &&
                  'number' == typeof t.timeout &&
                  void 0 !== t.ontimeout
                )
              })(this.c)),
              Te(
                this.s,
                this.da(
                  'Will abort after ' +
                    this.vb +
                    'ms if incomplete, xhr2 ' +
                    this.jc,
                ),
              ),
              this.jc
                ? ((this.c.timeout = this.vb),
                  (this.c.ontimeout = D(this.Ua, this)))
                : (this.hc = oe(this.Ua, this.vb, this))),
            Te(this.s, this.da('Sending request')),
            (this.Qb = !0),
            this.c.send(t),
            (this.Qb = !1)
        } catch (t) {
          Te(this.s, this.da('Send error: ' + t.message)), this.Ld(t)
        }
      }),
      (h.df = function() {
        return this.Xa ? this.Xa.Dd() : _e.Dd()
      }),
      (h.Ua = function() {
        void 0 !== f &&
          this.c &&
          ((this.I = 'Timed out after ' + this.vb + 'ms, aborting'),
          (this.Pa = 8),
          Te(this.s, this.da(this.I)),
          this.dispatchEvent('timeout'),
          this.abort(8))
      }),
      (h.Ld = function(t) {
        ;(this.ha = !1),
          this.c && ((this.Aa = !0), this.c.abort(), (this.Aa = !1)),
          (this.I = t),
          (this.Pa = 5),
          this.Jd(),
          this.Gb()
      }),
      (h.Jd = function() {
        this.Ec ||
          ((this.Ec = !0),
          this.dispatchEvent('complete'),
          this.dispatchEvent('error'))
      }),
      (h.abort = function(t) {
        this.c &&
          this.ha &&
          (Te(this.s, this.da('Aborting')),
          (this.ha = !1),
          (this.Aa = !0),
          this.c.abort(),
          (this.Aa = !1),
          (this.Pa = t || 7),
          this.dispatchEvent('complete'),
          this.dispatchEvent('abort'),
          this.Gb())
      }),
      (h.F = function() {
        this.c &&
          (this.ha &&
            ((this.ha = !1), (this.Aa = !0), this.c.abort(), (this.Aa = !1)),
          this.Gb(!0)),
          pn.L.F.call(this)
      }),
      (h.te = function() {
        this.Ka || (this.Lc || this.Qb || this.Aa ? this.se() : this.eg())
      }),
      (h.eg = function() {
        this.se()
      }),
      (h.se = function() {
        if (this.ha && void 0 !== f)
          if (this.mc[1] && 4 == this.ma() && 2 == this.za())
            Te(this.s, this.da('Local request error detected and ignored'))
          else if (this.Qb && 4 == this.ma()) oe(this.te, 0, this)
          else if ((this.dispatchEvent('readystatechange'), this.Mc())) {
            Te(this.s, this.da('Request complete')), (this.ha = !1)
            try {
              this.Rf()
                ? (this.dispatchEvent('complete'),
                  this.dispatchEvent('success'))
                : ((this.Pa = 6),
                  (this.I = this.Yd() + ' [' + this.za() + ']'),
                  this.Jd())
            } finally {
              this.Gb()
            }
          }
      }),
      (h.re = function(t, e) {
        this.dispatchEvent(bn(t, 'progress')),
          this.dispatchEvent(bn(t, e ? 'downloadprogress' : 'uploadprogress'))
      }),
      (h.Gb = function(t) {
        if (this.c) {
          this.yd()
          var e = this.c,
            n = this.mc[0] ? y : null
          ;(this.mc = this.c = null), t || this.dispatchEvent('ready')
          try {
            e.onreadystatechange = n
          } catch (e) {
            ;(t = this.s) &&
              t.ca(
                'Problem encountered resetting onreadystatechange: ' +
                  e.message,
                void 0,
              )
          }
        }
      }),
      (h.yd = function() {
        this.c && this.jc && (this.c.ontimeout = null),
          this.hc && (d.clearTimeout(this.hc), (this.hc = null))
      }),
      (h.Ba = function() {
        return !!this.c
      }),
      (h.Mc = function() {
        return 4 == this.ma()
      }),
      (h.Rf = function() {
        var t = this.za()
        t: switch (t) {
          case 200:
          case 201:
          case 202:
          case 204:
          case 206:
          case 304:
          case 1223:
            var e = !0
            break t
          default:
            e = !1
        }
        return e || (0 === t && !this.Of())
      }),
      (h.Of = function() {
        var t = String(this.Tb).match(ze)[1] || null
        return (
          !t &&
            d.self &&
            d.self.location &&
            (t = (t = d.self.location.protocol).substr(0, t.length - 1)),
          yn.test(t ? t.toLowerCase() : '')
        )
      }),
      (h.ma = function() {
        return this.c ? this.c.readyState : 0
      }),
      (h.za = function() {
        try {
          return 2 < this.ma() ? this.c.status : -1
        } catch (t) {
          return -1
        }
      }),
      (h.Yd = function() {
        try {
          return 2 < this.ma() ? this.c.statusText : ''
        } catch (t) {
          return Te(this.s, 'Can not get status: ' + t.message), ''
        }
      }),
      (h.ya = function() {
        try {
          return this.c ? this.c.responseText : ''
        } catch (t) {
          return Te(this.s, 'Can not get responseText: ' + t.message), ''
        }
      }),
      (h.yf = function(t) {
        if (this.c) {
          var e = this.c.responseText
          return t && 0 == e.indexOf(t) && (e = e.substring(t.length)), dn(e)
        }
      }),
      (h.getResponseHeader = function(t) {
        if (this.c && this.Mc())
          return null === (t = this.c.getResponseHeader(t)) ? void 0 : t
      }),
      (h.getAllResponseHeaders = function() {
        return (this.c && this.Mc() && this.c.getAllResponseHeaders()) || ''
      }),
      (h.kb = function(t) {
        return this.c ? this.c.getResponseHeader(t) : null
      }),
      (h.Ud = function() {
        return this.Pa
      }),
      (h.Hc = function() {
        return p(this.I) ? this.I : String(this.I)
      }),
      (h.da = function(t) {
        return t + ' [' + this.ge + ' ' + this.Tb + ' ' + this.za() + ']'
      }),
      ((h = Tn.prototype).tc = 8),
      (h.g = 1),
      (h.connect = function(t, e, n, r, i) {
        this.a.debug('connect()'),
          Ne(0),
          (this.K = e),
          (this.gb = n || {}),
          r && void 0 !== i && ((this.gb.OSID = r), (this.gb.OAID = i)),
          this.ta &&
            (this.a.debug('connect() bypassed channel-test.'),
            (this.Ib.$d = []),
            (this.Ib.od = !1)),
          this.bf(t)
      }),
      (h.disconnect = function() {
        if ((this.a.debug('disconnect()'), this.qd(), 3 == this.g)) {
          var t = this.Yb++,
            e = this.hb.clone()
          e.l('SID', this.ra),
            e.l('RID', t),
            e.l('TYPE', 'terminate'),
            this.Ya(e),
            new Ve(this, this.a, this.ra, t, void 0).yg(e)
        }
        this.qe()
      }),
      (h.bf = function(t) {
        this.a.debug('connectTest_()'),
          (this.Ja = new on(this, this.a)),
          null === this.N && this.Ja.ga(this.A)
        var e = t
        this.N && this.A && (e = wn(t, this.N, this.A)), this.Ja.connect(e)
      }),
      (h.af = function() {
        this.a.debug('connectChannel_()'),
          (this.hb = this.Sd(this.K)),
          this.Dc()
      }),
      (h.qd = function() {
        this.Ja && (this.Ja.abort(), (this.Ja = null)),
          this.u && (this.u.cancel(), (this.u = null)),
          this.ia && (d.clearTimeout(this.ia), (this.ia = null)),
          this.Hb(),
          this.G.cancel(),
          this.ea && (d.clearTimeout(this.ea), (this.ea = null))
      }),
      (h.ga = function(t) {
        this.A = t
      }),
      (h.Eg = function(t) {
        this.Rb = t
      }),
      (h.Cg = function(t) {
        this.N = t
      }),
      (h.Dg = function(t) {
        this.aa = t
      }),
      (h.Fe = function(t) {
        this.Kc = t
      }),
      (h.He = function(t) {
        this.Ra = t
      }),
      (h.Hg = function() {
        this.gc = !0
      }),
      (h.Ee = function(t) {
        this.i = t
      }),
      (h.Nf = function() {
        return !this.fd
      }),
      (h.Zc = function(t) {
        1e3 == this.M.length &&
          this.a.ca(function() {
            return 'Already have 1000 queued maps upon queueing ' + Gt(t)
          }),
          this.M.push(
            new function(t, e) {
              ;(this.Pc = t), (this.map = e), (this.context = null)
            }(this.ag++, t),
          ),
          3 == this.g && this.Dc()
      }),
      (h.qf = function() {
        return this.kf ? 0 : this.nf
      }),
      (h.de = function() {
        return 0 == this.g
      }),
      (h.getState = function() {
        return this.g
      }),
      (h.Dc = function() {
        this.G.ee() ||
          this.ea ||
          ((this.ea = ke(D(this.we, this), 0)), (this.La = 0))
      }),
      (h.Yf = function(t) {
        return this.G.xf() >= this.G.Xb - (this.ea ? 1 : 0)
          ? (this.a.ca('Unexpected retry request is scheduled.'), !1)
          : this.ea
            ? (this.a.debug('Use the retry request that is already scheduled.'),
              (this.M = t.la().concat(this.M)),
              !0)
            : !(1 == this.g || 2 == this.g || this.La >= this.qf()) &&
              (this.a.debug('Going to retry POST'),
              (this.ea = ke(D(this.we, this, t), this.Xd(this.La))),
              this.La++,
              !0)
      }),
      (h.we = function(t) {
        ;(this.ea = null), this.Mg(t)
      }),
      (h.Mg = function(t) {
        this.a.debug('startForwardChannel_'),
          1 == this.g
            ? t
              ? this.a.ca('Not supposed to retry the open')
              : (this.ig(), (this.g = 2))
            : 3 == this.g &&
              (t
                ? this.le(t)
                : 0 == this.M.length
                  ? this.a.debug(
                      'startForwardChannel_ returned: nothing to send',
                    )
                  : this.G.ee()
                    ? this.a.ca(
                        'startForwardChannel_ returned: connection already in progress',
                      )
                    : (this.le(),
                      this.a.debug(
                        'startForwardChannel_ finished, sent request',
                      )))
      }),
      (h.ig = function() {
        this.a.debug('open_()'), (this.Yb = Math.floor(1e5 * Math.random()))
        var t = this.Yb++,
          e = new Ve(this, this.a, '', t, void 0),
          n = this.A
        this.Rb && (n ? et((n = Z(n)), this.Rb) : (n = this.Rb)),
          null === this.N && e.ga(n)
        var r = this.Hd(e),
          i = this.hb.clone()
        i.l('RID', t),
          0 < this.Bd && i.l('CVER', this.Bd),
          this.ta && this.aa && i.l('X-HTTP-Session-Id', this.aa),
          this.Ya(i),
          this.N && n && wn(i, this.N, n),
          this.G.oc(e),
          this.Nd
            ? (i.l('$req', r), i.l('SID', 'null'), e.Ag(), e.kd(i, null))
            : e.kd(i, r)
      }),
      (h.le = function(t) {
        var e = t ? t.R : this.Yb++,
          n = this.hb.clone()
        n.l('SID', this.ra),
          n.l('RID', e),
          n.l('AID', this.Sb),
          this.Ya(n),
          this.N && this.A && wn(n, this.N, this.A),
          (e = new Ve(this, this.a, this.ra, e, this.La + 1)),
          null === this.N && e.ga(this.A),
          t && this.sg(t),
          (t = this.Hd(e)),
          e.setTimeout(
            Math.round(0.5 * this.Od) +
              Math.round(0.5 * this.Od * Math.random()),
          ),
          this.G.oc(e),
          e.kd(n, t)
      }),
      (h.Ya = function(t) {
        this.i &&
          We({}, function(e, n) {
            t.l(n, e)
          })
      }),
      (h.Hd = function(t) {
        var e = Math.min(this.M.length, 1e3),
          n = this.i ? D(this.i.Ue, this.i, this) : null
        return (n = this.kc.jf(this.M, e, n)), t.Gg(this.M.splice(0, e)), n
      }),
      (h.sg = function(t) {
        this.M = t.la().concat(this.M)
      }),
      (h.Kd = function() {
        if (!this.u && !this.ia) {
          this.rc = 1
          var t = this.ve
          Yt || te(), ee || (Yt(), (ee = !0)), ne.add(t, this), (this.Ha = 0)
        }
      }),
      (h.Qc = function() {
        return this.u || this.ia
          ? (this.a.ca('Request already in progress'), !1)
          : !(3 <= this.Ha) &&
              (this.a.debug('Going to retry GET'),
              this.rc++,
              (this.ia = ke(D(this.ve, this), this.Xd(this.Ha))),
              this.Ha++,
              !0)
      }),
      (h.ve = function() {
        ;(this.ia = null), this.Kg()
      }),
      (h.Kg = function() {
        this.a.debug('Creating new HttpRequest'),
          (this.u = new Ve(this, this.a, this.ra, 'rpc', this.rc)),
          null === this.N && this.u.ga(this.A),
          this.u.He(this.Ra)
        var t = this.md.clone()
        t.l('RID', 'rpc'),
          t.l('SID', this.ra),
          t.l('CI', this.fd ? '0' : '1'),
          t.l('AID', this.Sb),
          this.Ya(t),
          t.l('TYPE', 'xmlhttp'),
          this.N && this.A && wn(t, this.N, this.A),
          this.Db && this.u.setTimeout(this.Db),
          this.u.jd(t, !0, this.na),
          this.a.debug('New Request created')
      }),
      (h.ub = function(t, e) {
        this.a.debug('Test Connection Finished')
        var n = t.Ad
        n && this.G.ld(n), (this.fd = this.Se && e), (this.w = t.w), this.af()
      }),
      (h.dd = function(t) {
        this.a.debug('Test Connection Failed'), (this.w = t.w), this.sa(2)
      }),
      (h.ue = function(t, e) {
        if (0 != this.g && (this.u == t || this.G.Jc(t)))
          if (((this.w = t.w), !t.lb && this.G.Jc(t) && 3 == this.g)) {
            try {
              var n = this.kc.zc(e)
            } catch (t) {
              n = null
            }
            v(n) && 3 == n.length
              ? this.Cf(n, t)
              : (this.a.debug('Bad POST response data returned'), this.sa(11))
          } else
            (t.lb || this.u == t) && this.Hb(),
              K(e) || ((n = this.kc.zc(e)), this.dg(n, t))
      }),
      (h.Cf = function(t, e) {
        0 == t[0]
          ? this.Bf(e)
          : ((this.he = t[1]),
            0 < (e = this.he - this.Sb) &&
              ((t = t[2]),
              this.a.debug(
                t +
                  ' bytes (in ' +
                  e +
                  ' arrays) are outstanding on the BackChannel',
              ),
              this.Jg(t) && !this.wa && (this.wa = ke(D(this.bg, this), 6e3))))
      }),
      (h.Bf = function(t) {
        if (
          (this.a.debug('Server claims our backchannel is missing.'), this.ia)
        )
          this.a.debug('But we are currently starting the request.')
        else {
          if (this.u) {
            if (!(this.u.qb + 3e3 < t.qb)) return
            this.Hb(), this.u.cancel(), (this.u = null)
          } else this.a.T('We do not have a BackChannel established')
          this.Qc(), Ne(18)
        }
      }),
      (h.Jg = function(t) {
        return 37500 > t && !this.Nf() && 0 == this.Ha
      }),
      (h.$a = function(t) {
        return this.Te ? (this.i ? this.i.$a(t) : t) : null
      }),
      (h.bg = function() {
        null != this.wa &&
          ((this.wa = null),
          this.u.cancel(),
          (this.u = null),
          this.Qc(),
          Ne(19))
      }),
      (h.Hb = function() {
        null != this.wa && (d.clearTimeout(this.wa), (this.wa = null))
      }),
      (h.Tc = function(t) {
        this.a.debug('Request complete')
        var e = null
        if (this.u == t) {
          this.Hb(), (this.u = null)
          var n = 2
        } else {
          if (!this.G.Jc(t)) return
          ;(e = t.la()), this.G.ze(t), (n = 1)
        }
        if (((this.w = t.w), 0 != this.g))
          if (t.S)
            1 == n
              ? ((function(t, e, n) {
                  Ee.dispatchEvent(new Ae(Ee, t, e, n))
                })(t.Da ? t.Da.length : 0, A() - t.qb, this.La),
                this.Dc())
              : this.Kd()
          else {
            var r = t.Hc()
            if (3 == r || (0 == r && 0 < this.w))
              this.a.debug('Not retrying due to error type')
            else {
              var i = this
              if (
                (this.a.debug(function() {
                  return (
                    'Maybe retrying, last error: ' +
                    (function(t, e) {
                      switch (t) {
                        case 0:
                          return 'Non-200 return code (' + e + ')'
                        case 1:
                          return 'XMLHTTP failure (no data)'
                        case 2:
                          return 'HttpConnection timeout'
                        default:
                          return 'Unknown error'
                      }
                    })(r, i.w)
                  )
                }),
                (1 == n && this.Yf(t)) || (2 == n && this.Qc()))
              )
                return
              this.a.debug('Exceeded max number of retries')
            }
            switch (
              (e && 0 < e.length && this.G.Re(e),
              this.a.debug('Error: HTTP request failed'),
              r)
            ) {
              case 1:
                this.sa(5)
                break
              case 4:
                this.sa(10)
                break
              case 3:
                this.sa(6)
                break
              default:
                this.sa(2)
            }
          }
      }),
      (h.Xd = function(t) {
        var e = this.Ve + Math.floor(Math.random() * this.vg)
        return this.Ba() || (this.a.debug('Inactive channel'), (e *= 2)), e * t
      }),
      (h.pc = function(t) {
        if (this.ta && (t = t.h)) {
          var e = t.kb('X-Client-Wire-Protocol')
          e && this.G.ld(e),
            this.aa &&
              ((t = t.kb('X-HTTP-Session-Id'))
                ? (this.Fe(t), this.hb.l(this.aa, t))
                : this.a.T(
                    'Missing X_HTTP_SESSION_ID in the handshake response',
                  ))
        }
      }),
      (h.dg = function(t, e) {
        for (
          var n = this.i && this.i.sc ? [] : null, r = 0;
          r < t.length;
          r++
        ) {
          var i = t[r]
          if (((this.Sb = i[0]), (i = i[1]), 2 == this.g))
            if ('c' == i[0]) {
              ;(this.ra = i[1]), (this.na = this.$a(i[2]))
              var o = i[3]
              null != o && ((this.tc = o), this.a.info('VER=' + this.tc)),
                null != (o = i[4]) &&
                  ((this.De = o), this.a.info('SVER=' + this.De)),
                null != (i = i[5]) &&
                  'number' == typeof i &&
                  0 < i &&
                  ((this.Db = i *= 1.5),
                  this.a.info('backChannelRequestTimeoutMs_=' + i)),
                this.pc(e),
                (this.g = 3),
                this.i && this.i.wd(),
                this.Lg(e)
            } else ('stop' != i[0] && 'close' != i[0]) || this.sa(7)
          else
            3 == this.g &&
              ('stop' == i[0] || 'close' == i[0]
                ? (n && 0 != n.length && (this.i.sc(this, n), (n.length = 0)),
                  'stop' == i[0] ? this.sa(7) : this.disconnect())
                : 'noop' != i[0] && (n ? n.push(i) : this.i && this.i.ud(i)),
              (this.Ha = 0))
        }
        n && 0 != n.length && this.i.sc(this, n)
      }),
      (h.Lg = function(t) {
        ;(this.md = this.Pd(this.na, this.K)),
          t.lb
            ? (this.a.debug('Upgrade the handshake request to a backchannel.'),
              this.G.ze(t),
              t.tg(this.Db),
              (this.u = t))
            : this.Kd()
      }),
      (h.sa = function(t) {
        if ((this.a.info('Error code ' + t), 2 == t)) {
          var e = null
          this.i && (e = null)
          var n = D(this.Pg, this)
          e ||
            ((e = new He('//www.google.com/images/cleardot.gif')),
            (d.location && 'http' == d.location.protocol) || e.tb('https'),
            e.Ub()),
            (function(t, e) {
              var n = new Se()
              n.debug('TestLoadImage: loading ' + t)
              var r = new Image()
              ;(r.onload = N(fn, n, r, 'TestLoadImage: loaded', !0, e)),
                (r.onerror = N(fn, n, r, 'TestLoadImage: error', !1, e)),
                (r.onabort = N(fn, n, r, 'TestLoadImage: abort', !1, e)),
                (r.ontimeout = N(fn, n, r, 'TestLoadImage: timeout', !1, e)),
                d.setTimeout(function() {
                  r.ontimeout && r.ontimeout()
                }, 1e4),
                (r.src = t)
            })(e.toString(), n)
        } else Ne(2)
        this.cg(t)
      }),
      (h.Pg = function(t) {
        t
          ? (this.a.info('Successfully pinged google.com'), Ne(2))
          : (this.a.info('Failed to ping google.com'), Ne(1))
      }),
      (h.cg = function(t) {
        this.a.debug('HttpChannel: error - ' + t),
          (this.g = 0),
          this.i && this.i.td(t),
          this.qe(),
          this.qd()
      }),
      (h.qe = function() {
        if (((this.g = 0), (this.w = -1), this.i)) {
          var t = this.G.la()
          if (0 != t.length || 0 != this.M.length) {
            var e = this
            this.a.debug(function() {
              return (
                'Number of undelivered maps, pending: ' +
                t.length +
                ', outgoing: ' +
                e.M.length
              )
            }),
              this.G.$e(),
              Q(this.M),
              (this.M.length = 0)
          }
          this.i.sd()
        }
      }),
      (h.Sd = function(t) {
        return (
          (t = this.Cd(null, t)), this.a.debug('GetForwardChannelUri: ' + t), t
        )
      }),
      (h.Pd = function(t, e) {
        return (
          (t = this.Cd(this.fc() ? t : null, e)),
          this.a.debug('GetBackChannelUri: ' + t),
          t
        )
      }),
      (h.Cd = function(t, e) {
        var n = (function(t) {
          return t instanceof He ? t.clone() : new He(t, void 0)
        })(e)
        if ('' != n.xa) t && n.rb(t + '.' + n.xa), n.sb(n.Ca)
        else {
          var r = d.location
          n = (function(t, e, n, r) {
            var i = new He(null, void 0)
            return t && i.tb(t), e && i.rb(e), n && i.sb(n), r && i.ec(r), i
          })(r.protocol, t ? t + '.' + r.hostname : r.hostname, +r.port, e)
        }
        return (
          this.gb &&
            Y(this.gb, function(t, e) {
              n.l(e, t)
            }),
          (t = this.aa),
          (e = this.Kc),
          t && e && n.l(t, e),
          n.l('VER', this.tc),
          this.Ya(n),
          n
        )
      }),
      (h.Jb = function(t) {
        if (t && !this.gc)
          throw Error("Can't create secondary domain capable XhrIo object.")
        return (t = new pn(this.Xa)).Je(this.gc), t
      }),
      (h.Ba = function() {
        return !!this.i && this.i.Ba()
      }),
      (h.fc = function() {
        return this.gc
      }),
      ((h = Sn.prototype).sc = null),
      (h.wd = function() {}),
      (h.ud = function() {}),
      (h.td = function() {}),
      (h.sd = function() {}),
      (h.Ba = function() {
        return !0
      }),
      (h.Ue = function() {}),
      (h.$a = function(t) {
        return t
      }),
      (En.prototype.cf = function(t, e) {
        return new In(t, e)
      }),
      k(In, jt),
      ((h = In.prototype).addEventListener = function(t, e, n, r) {
        In.L.addEventListener.call(this, t, e, n, r)
      }),
      (h.removeEventListener = function(t, e, n, r) {
        In.L.removeEventListener.call(this, t, e, n, r)
      }),
      (h.open = function() {
        this.b.Ee(this.vd),
          this.Og && this.b.Hg(),
          this.b.connect(
            this.Qg,
            this.yb,
            this.Rc || void 0,
          )
      }),
      (h.close = function() {
        this.b.disconnect()
      }),
      (h.send = function(t) {
        if (p(t)) {
          var e = {}
          ;(e.__data__ = t), this.b.Zc(e)
        } else
          this.zg ? (((e = {}).__data__ = Gt(t)), this.b.Zc(e)) : this.b.Zc(t)
      }),
      (h.F = function() {
        this.b.Ee(null),
          delete this.vd,
          this.b.disconnect(),
          delete this.b,
          In.L.F.call(this)
      }),
      k(Cn, xe),
      k(Dn, qe),
      k(Nn, Sn),
      (Nn.prototype.wd = function() {
        we(this.b.s, 'WebChannel opened on ' + this.b.yb),
          this.b.dispatchEvent('a')
      }),
      (Nn.prototype.ud = function(t) {
        this.b.dispatchEvent(new Cn(t))
      }),
      (Nn.prototype.td = function(t) {
        we(
          this.b.s,
          'WebChannel aborted on ' + this.b.yb + ' due to channel error: ' + t,
        ),
          this.b.dispatchEvent(new Dn(t))
      }),
      (Nn.prototype.sd = function() {
        we(this.b.s, 'WebChannel closed on ' + this.b.yb),
          this.b.dispatchEvent('b')
      })
    var An = N(function(t, e) {
      function n() {}
      n.prototype = t.prototype
      var r = new n()
      return t.apply(r, Array.prototype.slice.call(arguments, 1)), r
    }, En)
    function kn() {
      ;(this.V = []), (this.Z = [])
    }
    function Rn(t, e) {
      if (
        (O.call(this),
        (this.oe = t || 0),
        (this.Wb = e || 10),
        this.oe > this.Wb)
      )
        throw Error(Mn)
      ;(this.fa = new kn()),
        (this.oa = new sn()),
        (this.Ac = 0),
        (this.Nc = null),
        this.Cb()
    }
    ;((h = kn.prototype).Vf = function() {
      0 == this.V.length && ((this.V = this.Z), this.V.reverse(), (this.Z = []))
    }),
      (h.enqueue = function(t) {
        this.Z.push(t)
      }),
      (h.ab = function() {
        return this.Vf(), this.V.pop()
      }),
      (h.C = function() {
        return this.V.length + this.Z.length
      }),
      (h.X = function() {
        return 0 == this.V.length && 0 == this.Z.length
      }),
      (h.clear = function() {
        ;(this.V = []), (this.Z = [])
      }),
      (h.contains = function(t) {
        return 0 <= L(this.V, t) || 0 <= L(this.Z, t)
      }),
      (h.remove = function(t) {
        var e = this.V,
          n = x(e, t)
        return (
          0 <= n ? (Array.prototype.splice.call(e, n, 1), (e = !0)) : (e = !1),
          e || B(this.Z, t)
        )
      }),
      (h.H = function() {
        for (var t = [], e = this.V.length - 1; 0 <= e; --e) t.push(this.V[e])
        var n = this.Z.length
        for (e = 0; e < n; ++e) t.push(this.Z[e])
        return t
      }),
      k(Rn, O)
    var Mn = '[goog.structs.Pool] Min can not be greater than max'
    function On(t, e) {
      ;(this.fe = t), (this.gd = e)
    }
    function Pn(t) {
      ;(this.Y = []), t && this.Lf(t)
    }
    function _n() {
      Pn.call(this)
    }
    function Ln(t, e) {
      ;(this.Gd = void 0), (this.cc = new _n()), Rn.call(this, t, e)
    }
    function xn(t, e, n, r) {
      ;(this.Jf = t), (this.Ab = !!r), Ln.call(this, e, n)
    }
    ;((h = Rn.prototype).Mb = function() {
      var t = A()
      if (!(null != this.Nc && t - this.Nc < this.Ac)) {
        var e = this.qg()
        return e && ((this.Nc = t), this.oa.add(e)), e
      }
    }),
      (h.og = function(t) {
        return !!this.oa.remove(t) && (this.nc(t), !0)
      }),
      (h.qg = function() {
        for (var t; 0 < this.Td() && ((t = this.fa.ab()), !this.Sc(t)); )
          this.Cb()
        return !t && this.C() < this.Wb && (t = this.xc()), t
      }),
      (h.nc = function(t) {
        this.oa.remove(t),
          this.Sc(t) && this.C() < this.Wb ? this.fa.enqueue(t) : this.Bc(t)
      }),
      (h.Cb = function() {
        for (var t = this.fa; this.C() < this.oe; ) t.enqueue(this.xc())
        for (; this.C() > this.Wb && 0 < this.Td(); ) this.Bc(t.ab())
      }),
      (h.xc = function() {
        return {}
      }),
      (h.Bc = function(t) {
        if ('function' == typeof t.bb) t.bb()
        else for (var e in t) t[e] = null
      }),
      (h.Sc = function(t) {
        return 'function' != typeof t.Ye || t.Ye()
      }),
      (h.contains = function(t) {
        return this.fa.contains(t) || this.oa.contains(t)
      }),
      (h.C = function() {
        return this.fa.C() + this.oa.C()
      }),
      (h.rf = function() {
        return this.oa.C()
      }),
      (h.Td = function() {
        return this.fa.C()
      }),
      (h.X = function() {
        return this.fa.X() && this.oa.X()
      }),
      (h.F = function() {
        if ((Rn.L.F.call(this), 0 < this.rf()))
          throw Error('[goog.structs.Pool] Objects not released')
        delete this.oa
        for (var t = this.fa; !t.X(); ) this.Bc(t.ab())
        delete this.fa
      }),
      (On.prototype.getKey = function() {
        return this.fe
      }),
      (On.prototype.clone = function() {
        return new On(this.fe, this.gd)
      }),
      ((h = Pn.prototype).ce = function(t, e) {
        var n = this.Y
        n.push(new On(t, e)), this.$f(n.length - 1)
      }),
      (h.Lf = function(t) {
        if (t instanceof Pn) {
          var e = t.W()
          if (((t = t.H()), 0 >= this.C())) {
            for (var n = this.Y, r = 0; r < e.length; r++)
              n.push(new On(e[r], t[r]))
            return
          }
        } else (e = $(t)), (t = J(t))
        for (r = 0; r < e.length; r++) this.ce(e[r], t[r])
      }),
      (h.remove = function() {
        var t = this.Y,
          e = t.length,
          n = t[0]
        if (!(0 >= e))
          return 1 == e ? V(t) : ((t[0] = t.pop()), this.Zf()), n.gd
      }),
      (h.Zf = function() {
        for (var t = 0, e = this.Y, n = e.length, r = e[t]; t < n >> 1; ) {
          var i = this.tf(t),
            o = this.zf(t)
          if (
            e[(i = o < n && e[o].getKey() < e[i].getKey() ? o : i)].getKey() >
            r.getKey()
          )
            break
          ;(e[t] = e[i]), (t = i)
        }
        e[t] = r
      }),
      (h.$f = function(t) {
        for (var e = this.Y, n = e[t]; 0 < t; ) {
          var r = this.wf(t)
          if (!(e[r].getKey() > n.getKey())) break
          ;(e[t] = e[r]), (t = r)
        }
        e[t] = n
      }),
      (h.tf = function(t) {
        return 2 * t + 1
      }),
      (h.zf = function(t) {
        return 2 * t + 2
      }),
      (h.wf = function(t) {
        return (t - 1) >> 1
      }),
      (h.H = function() {
        for (var t = this.Y, e = [], n = t.length, r = 0; r < n; r++)
          e.push(t[r].gd)
        return e
      }),
      (h.W = function() {
        for (var t = this.Y, e = [], n = t.length, r = 0; r < n; r++)
          e.push(t[r].getKey())
        return e
      }),
      (h.va = function(t) {
        return F(this.Y, function(e) {
          return e.getKey() == t
        })
      }),
      (h.clone = function() {
        return new Pn(this)
      }),
      (h.C = function() {
        return this.Y.length
      }),
      (h.X = function() {
        return 0 == this.Y.length
      }),
      (h.clear = function() {
        V(this.Y)
      }),
      k(_n, Pn),
      (_n.prototype.enqueue = function(t, e) {
        this.ce(t, e)
      }),
      (_n.prototype.ab = function() {
        return this.remove()
      }),
      k(Ln, Rn),
      ((h = Ln.prototype).Mb = function(t, e) {
        if (!t)
          return (
            (t = Ln.L.Mb.call(this)) &&
              this.Ac &&
              (this.Gd = d.setTimeout(D(this.Nb, this), this.Ac)),
            t
          )
        this.cc.enqueue(void 0 !== e ? e : 100, t), this.Nb()
      }),
      (h.Nb = function() {
        for (var t = this.cc; 0 < t.C(); ) {
          var e = this.Mb()
          if (!e) break
          t.ab().apply(this, [e])
        }
      }),
      (h.nc = function(t) {
        Ln.L.nc.call(this, t), this.Nb()
      }),
      (h.Cb = function() {
        Ln.L.Cb.call(this), this.Nb()
      }),
      (h.F = function() {
        Ln.L.F.call(this),
          d.clearTimeout(this.Gd),
          this.cc.clear(),
          (this.cc = null)
      }),
      k(xn, Ln),
      (xn.prototype.xc = function() {
        var t = new pn(),
          e = this.Jf
        return (
          e &&
            e.forEach(function(e, n) {
              t.headers.set(n, e)
            }),
          this.Ab && t.Je(!0),
          t
        )
      }),
      (xn.prototype.Sc = function(t) {
        return !t.Ka && !t.Ba()
      }),
      (En.prototype.createWebChannel = En.prototype.cf),
      (In.prototype.send = In.prototype.send),
      (In.prototype.open = In.prototype.open),
      (In.prototype.close = In.prototype.close),
      (Re.NO_ERROR = 0),
      (Re.TIMEOUT = 8),
      (Re.HTTP_ERROR = 6),
      (Me.COMPLETE = 'complete'),
      (Pe.EventType = Le),
      (Le.OPEN = 'a'),
      (Le.CLOSE = 'b'),
      (Le.ERROR = 'c'),
      (Le.MESSAGE = 'd'),
      (jt.prototype.listen = jt.prototype.nb),
      (xn.prototype.getObject = xn.prototype.Mb),
      (xn.prototype.releaseObject = xn.prototype.og),
      (pn.prototype.listenOnce = pn.prototype.Oc),
      (pn.prototype.getLastError = pn.prototype.Hc),
      (pn.prototype.getLastErrorCode = pn.prototype.Ud),
      (pn.prototype.getStatus = pn.prototype.za),
      (pn.prototype.getStatusText = pn.prototype.Yd),
      (pn.prototype.getResponseJson = pn.prototype.yf),
      (pn.prototype.getResponseText = pn.prototype.ya),
      (pn.prototype.getResponseText = pn.prototype.ya),
      (pn.prototype.send = pn.prototype.send)
    var qn,
      Fn,
      Vn = {
        createWebChannelTransport: An,
        ErrorCode: Re,
        EventType: Me,
        WebChannel: Pe,
        XhrIoPool: xn,
      },
      Bn = Vn.createWebChannelTransport,
      Un = Vn.ErrorCode,
      Qn = Vn.EventType,
      Kn = Vn.WebChannel,
      Wn = Vn.XhrIoPool,
      jn = e.SDK_VERSION,
      Gn = new o('@firebase/firestore')
    function zn() {
      return Gn.logLevel === n.DEBUG
        ? qn.DEBUG
        : Gn.logLevel === n.SILENT
          ? qn.SILENT
          : qn.ERROR
    }
    function Hn(t) {
      switch (t) {
        case qn.DEBUG:
          Gn.logLevel = n.DEBUG
          break
        case qn.ERROR:
          Gn.logLevel = n.ERROR
          break
        case qn.SILENT:
          Gn.logLevel = n.SILENT
          break
        default:
          Gn.error(
            'Firestore (' + jn + '): Invalid value passed to `setLogLevel`',
          )
      }
    }
    function Xn(t, e) {
      for (var r = [], i = 2; i < arguments.length; i++) r[i - 2] = arguments[i]
      if (Gn.logLevel <= n.DEBUG) {
        var o = r.map(Jn)
        Gn.debug.apply(
          Gn,
          ['Firestore (' + jn + ') [' + t + ']: ' + e].concat(o),
        )
      }
    }
    function Yn(t) {
      for (var e = [], r = 1; r < arguments.length; r++) e[r - 1] = arguments[r]
      if (Gn.logLevel <= n.ERROR) {
        var i = e.map(Jn)
        Gn.error.apply(Gn, ['Firestore (' + jn + '): ' + t].concat(i))
      }
    }
    function Jn(t) {
      if ('string' == typeof t) return t
      var e = tr.getPlatform()
      try {
        return e.formatJSON(t)
      } catch (e) {
        return t
      }
    }
    function $n(t) {
      var e = 'FIRESTORE (' + jn + ') INTERNAL ASSERTION FAILED: ' + t
      throw (Yn(e), new Error(e))
    }
    function Zn(t, e) {
      t || $n(e)
    }
    ;((Fn = qn || (qn = {}))[(Fn.DEBUG = 0)] = 'DEBUG'),
      (Fn[(Fn.ERROR = 1)] = 'ERROR'),
      (Fn[(Fn.SILENT = 2)] = 'SILENT')
    var tr = (function() {
      function t() {}
      return (
        (t.setPlatform = function(e) {
          t.platform && $n('Platform already defined'), (t.platform = e)
        }),
        (t.getPlatform = function() {
          return t.platform || $n('Platform not set'), t.platform
        }),
        t
      )
    })()
    function er() {
      return tr.getPlatform().emptyByteString
    }
    var nr = {
        OK: 'ok',
        CANCELLED: 'cancelled',
        UNKNOWN: 'unknown',
        INVALID_ARGUMENT: 'invalid-argument',
        DEADLINE_EXCEEDED: 'deadline-exceeded',
        NOT_FOUND: 'not-found',
        ALREADY_EXISTS: 'already-exists',
        PERMISSION_DENIED: 'permission-denied',
        UNAUTHENTICATED: 'unauthenticated',
        RESOURCE_EXHAUSTED: 'resource-exhausted',
        FAILED_PRECONDITION: 'failed-precondition',
        ABORTED: 'aborted',
        OUT_OF_RANGE: 'out-of-range',
        UNIMPLEMENTED: 'unimplemented',
        INTERNAL: 'internal',
        UNAVAILABLE: 'unavailable',
        DATA_LOSS: 'data-loss',
      },
      rr = (function(t) {
        function e(e, n) {
          var r = t.call(this, n) || this
          return (
            (r.code = e),
            (r.message = n),
            (r.name = 'FirebaseError'),
            (r.toString = function() {
              return r.name + ': [code=' + r.code + ']: ' + r.message
            }),
            r
          )
        }
        return a(e, t), e
      })(Error)
    function ir(t, e) {
      function n() {
        var t = 'This constructor is private.'
        throw (e && ((t += ' '), (t += e)), new rr(nr.INVALID_ARGUMENT, t))
      }
      for (var r in ((n.prototype = t.prototype), t))
        t.hasOwnProperty(r) && (n[r] = t[r])
      return n
    }
    function or(t, e) {
      return Object.prototype.hasOwnProperty.call(t, e)
    }
    function sr(t, e) {
      return void 0 !== t ? t : e
    }
    function ar(t, e) {
      for (var n in t)
        if (Object.prototype.hasOwnProperty.call(t, n)) {
          var r = Number(n)
          isNaN(r) || e(r, t[n])
        }
    }
    function ur(t, e) {
      for (var n in t) Object.prototype.hasOwnProperty.call(t, n) && e(n, t[n])
    }
    function cr(t) {
      for (var e in (Zn(
        null != t && 'object' == typeof t,
        'isEmpty() expects object parameter.',
      ),
      t))
        if (Object.prototype.hasOwnProperty.call(t, e)) return !1
      return !0
    }
    function hr(t, e, n) {
      if (e.length !== n)
        throw new rr(
          nr.INVALID_ARGUMENT,
          'Function ' +
            t +
            '() requires ' +
            Dr(n, 'argument') +
            ', but was called with ' +
            Dr(e.length, 'argument') +
            '.',
        )
    }
    function lr(t, e, n) {
      if (e.length < n)
        throw new rr(
          nr.INVALID_ARGUMENT,
          'Function ' +
            t +
            '() requires at least ' +
            Dr(n, 'argument') +
            ', but was called with ' +
            Dr(e.length, 'argument') +
            '.',
        )
    }
    function fr(t, e, n, r) {
      if (e.length < n || e.length > r)
        throw new rr(
          nr.INVALID_ARGUMENT,
          'Function ' +
            t +
            '() requires between ' +
            n +
            ' and ' +
            r +
            ' arguments, but was called with ' +
            Dr(e.length, 'argument') +
            '.',
        )
    }
    function dr(t, e, n, r) {
      br(t, e, Cr(n) + ' argument', r)
    }
    function pr(t, e, n, r) {
      void 0 !== r && dr(t, e, n, r)
    }
    function mr(t, e, n, r) {
      br(t, e, n + ' option', r)
    }
    function yr(t, e, n, r) {
      void 0 !== r && mr(t, e, n, r)
    }
    function gr(t, e, n, r, i) {
      void 0 !== r &&
        (function(t, e, n, r, i) {
          if (!(r instanceof Array))
            throw new rr(
              nr.INVALID_ARGUMENT,
              'Function ' +
                t +
                '() requires its ' +
                e +
                ' option to be an array, but it was: ' +
                Tr(r),
            )
          for (var o = 0; o < r.length; ++o)
            if (!i(r[o]))
              throw new rr(
                nr.INVALID_ARGUMENT,
                'Function ' +
                  t +
                  '() requires all ' +
                  e +
                  ' elements to be ' +
                  n +
                  ', but the value at index ' +
                  o +
                  ' was: ' +
                  Tr(r[o]),
              )
        })(t, e, n, r, i)
    }
    function vr(t, e, n, r, i) {
      void 0 !== r &&
        (function(t, e, n, r, i) {
          for (var o = [], s = 0, a = i; s < a.length; s++) {
            var u = a[s]
            if (u === r) return
            o.push(Tr(u))
          }
          var c = Tr(r)
          throw new rr(
            nr.INVALID_ARGUMENT,
            'Invalid value ' +
              c +
              ' provided to function ' +
              t +
              '() for option "' +
              n +
              '". Acceptable values: ' +
              o.join(', '),
          )
        })(t, 0, n, r, i)
    }
    function br(t, e, n, r) {
      if (
        !('object' === e
          ? wr(r)
          : 'non-empty string' === e
            ? 'string' == typeof r && '' !== r
            : typeof r === e)
      ) {
        var i = Tr(r)
        throw new rr(
          nr.INVALID_ARGUMENT,
          'Function ' +
            t +
            '() requires its ' +
            n +
            ' to be of type ' +
            e +
            ', but it was: ' +
            i,
        )
      }
    }
    function wr(t) {
      return (
        'object' == typeof t &&
        null !== t &&
        (Object.getPrototypeOf(t) === Object.prototype ||
          null === Object.getPrototypeOf(t))
      )
    }
    function Tr(t) {
      if (void 0 === t) return 'undefined'
      if (null === t) return 'null'
      if ('string' == typeof t)
        return (
          t.length > 20 && (t = t.substring(0, 20) + '...'), JSON.stringify(t)
        )
      if ('number' == typeof t || 'boolean' == typeof t) return '' + t
      if ('object' == typeof t) {
        if (t instanceof Array) return 'an array'
        var e = (function(t) {
          if (t.constructor) {
            var e = /function\s+([^\s(]+)\s*\(/.exec(t.constructor.toString())
            if (e && e.length > 1) return e[1]
          }
          return null
        })(t)
        return e ? 'a custom ' + e + ' object' : 'an object'
      }
      return 'function' == typeof t
        ? 'a function'
        : $n('Unknown wrong type: ' + typeof t)
    }
    function Sr(t, e, n) {
      if (void 0 === n)
        throw new rr(
          nr.INVALID_ARGUMENT,
          'Function ' +
            t +
            '() requires a valid ' +
            Cr(e) +
            ' argument, but it was undefined.',
        )
    }
    function Er(t, e, n) {
      ur(e, function(e, r) {
        if (n.indexOf(e) < 0)
          throw new rr(
            nr.INVALID_ARGUMENT,
            "Unknown option '" +
              e +
              "' passed to function " +
              t +
              '(). Available options: ' +
              n.join(', '),
          )
      })
    }
    function Ir(t, e, n, r) {
      var i = Tr(r)
      return new rr(
        nr.INVALID_ARGUMENT,
        'Function ' +
          t +
          '() requires its ' +
          Cr(n) +
          ' argument to be a ' +
          e +
          ', but it was: ' +
          i,
      )
    }
    function Cr(t) {
      switch (t) {
        case 1:
          return 'first'
        case 2:
          return 'second'
        case 3:
          return 'third'
        default:
          return t + 'th'
      }
    }
    function Dr(t, e) {
      return t + ' ' + e + (1 === t ? '' : 's')
    }
    var Nr = (function() {
      function t() {}
      return (
        (t.newId = function() {
          for (
            var t =
                'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789',
              e = '',
              n = 0;
            n < 20;
            n++
          )
            e += t.charAt(Math.floor(Math.random() * t.length))
          return Zn(20 === e.length, 'Invalid auto ID: ' + e), e
        }),
        t
      )
    })()
    function Ar(t, e) {
      return t < e ? -1 : t > e ? 1 : 0
    }
    function kr(t, e) {
      if (t.length !== e.length) return !1
      for (var n = 0; n < t.length; n++) if (!t[n].isEqual(e[n])) return !1
      return !0
    }
    function Rr() {
      if ('undefined' == typeof Uint8Array)
        throw new rr(
          nr.UNIMPLEMENTED,
          'Uint8Arrays are not available in this environment.',
        )
    }
    function Mr() {
      if (!tr.getPlatform().base64Available)
        throw new rr(
          nr.UNIMPLEMENTED,
          'Blobs are unavailable in Firestore in this environment.',
        )
    }
    var Or,
      Pr,
      _r = (function() {
        function t(t) {
          Mr(), (this._binaryString = t)
        }
        return (
          (t.fromBase64String = function(e) {
            hr('Blob.fromBase64String', arguments, 1),
              dr('Blob.fromBase64String', 'string', 1, e),
              Mr()
            try {
              return new t(tr.getPlatform().atob(e))
            } catch (t) {
              throw new rr(
                nr.INVALID_ARGUMENT,
                'Failed to construct Blob from Base64 string: ' + t,
              )
            }
          }),
          (t.fromUint8Array = function(e) {
            if (
              (hr('Blob.fromUint8Array', arguments, 1),
              Rr(),
              !(e instanceof Uint8Array))
            )
              throw Ir('Blob.fromUint8Array', 'Uint8Array', 1, e)
            return new t(
              Array.prototype.map
                .call(e, function(t) {
                  return String.fromCharCode(t)
                })
                .join(''),
            )
          }),
          (t.prototype.toBase64 = function() {
            return (
              hr('Blob.toBase64', arguments, 0),
              Mr(),
              tr.getPlatform().btoa(this._binaryString)
            )
          }),
          (t.prototype.toUint8Array = function() {
            hr('Blob.toUint8Array', arguments, 0), Rr()
            for (
              var t = new Uint8Array(this._binaryString.length), e = 0;
              e < this._binaryString.length;
              e++
            )
              t[e] = this._binaryString.charCodeAt(e)
            return t
          }),
          (t.prototype.toString = function() {
            return 'Blob(base64: ' + this.toBase64() + ')'
          }),
          (t.prototype.isEqual = function(t) {
            return this._binaryString === t._binaryString
          }),
          (t.prototype._compareTo = function(t) {
            return Ar(this._binaryString, t._binaryString)
          }),
          t
        )
      })(),
      Lr = ir(
        _r,
        'Use Blob.fromUint8Array() or Blob.fromBase64String() instead.',
      ),
      xr = (function() {
        function t(t, e) {
          if (
            (hr('GeoPoint', arguments, 2),
            dr('GeoPoint', 'number', 1, t),
            dr('GeoPoint', 'number', 2, e),
            !isFinite(t) || t < -90 || t > 90)
          )
            throw new rr(
              nr.INVALID_ARGUMENT,
              'Latitude must be a number between -90 and 90, but was: ' + t,
            )
          if (!isFinite(e) || e < -180 || e > 180)
            throw new rr(
              nr.INVALID_ARGUMENT,
              'Longitude must be a number between -180 and 180, but was: ' + e,
            )
          ;(this._lat = t), (this._long = e)
        }
        return (
          Object.defineProperty(t.prototype, 'latitude', {
            get: function() {
              return this._lat
            },
            enumerable: !0,
            configurable: !0,
          }),
          Object.defineProperty(t.prototype, 'longitude', {
            get: function() {
              return this._long
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.isEqual = function(t) {
            return this._lat === t._lat && this._long === t._long
          }),
          (t.prototype._compareTo = function(t) {
            return Ar(this._lat, t._lat) || Ar(this._long, t._long)
          }),
          t
        )
      })(),
      qr = (function() {
        function t(t, e) {
          if (((this.seconds = t), (this.nanoseconds = e), e < 0))
            throw new rr(
              nr.INVALID_ARGUMENT,
              'Timestamp nanoseconds out of range: ' + e,
            )
          if (e >= 1e9)
            throw new rr(
              nr.INVALID_ARGUMENT,
              'Timestamp nanoseconds out of range: ' + e,
            )
          if (t < -62135596800)
            throw new rr(
              nr.INVALID_ARGUMENT,
              'Timestamp seconds out of range: ' + t,
            )
          if (t >= 253402300800)
            throw new rr(
              nr.INVALID_ARGUMENT,
              'Timestamp seconds out of range: ' + t,
            )
        }
        return (
          (t.now = function() {
            return t.fromMillis(Date.now())
          }),
          (t.fromDate = function(e) {
            return t.fromMillis(e.getTime())
          }),
          (t.fromMillis = function(e) {
            var n = Math.floor(e / 1e3)
            return new t(n, 1e6 * (e - 1e3 * n))
          }),
          (t.prototype.toDate = function() {
            return new Date(this.toMillis())
          }),
          (t.prototype.toMillis = function() {
            return 1e3 * this.seconds + this.nanoseconds / 1e6
          }),
          (t.prototype._compareTo = function(t) {
            return this.seconds === t.seconds
              ? Ar(this.nanoseconds, t.nanoseconds)
              : Ar(this.seconds, t.seconds)
          }),
          (t.prototype.isEqual = function(t) {
            return (
              t.seconds === this.seconds && t.nanoseconds === this.nanoseconds
            )
          }),
          (t.prototype.toString = function() {
            return (
              'Timestamp(seconds=' +
              this.seconds +
              ', nanoseconds=' +
              this.nanoseconds +
              ')'
            )
          }),
          t
        )
      })(),
      Fr = (function() {
        return function(t, e, n, r) {
          ;(this.databaseId = t),
            (this.persistenceKey = e),
            (this.host = n),
            (this.ssl = r)
        }
      })(),
      Vr = '(default)',
      Br = (function() {
        function t(t, e) {
          ;(this.projectId = t), (this.database = e || Vr)
        }
        return (
          Object.defineProperty(t.prototype, 'isDefaultDatabase', {
            get: function() {
              return this.database === Vr
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.isEqual = function(e) {
            return (
              e instanceof t &&
              e.projectId === this.projectId &&
              e.database === this.database
            )
          }),
          (t.prototype.compareTo = function(t) {
            return (
              Ar(this.projectId, t.projectId) || Ar(this.database, t.database)
            )
          }),
          t
        )
      })(),
      Ur = (function() {
        function t(t, e, n) {
          this.init(t, e, n)
        }
        return (
          (t.prototype.init = function(t, e, n) {
            void 0 === e
              ? (e = 0)
              : e > t.length && $n('offset ' + e + ' out of range ' + t.length),
              void 0 === n
                ? (n = t.length - e)
                : n > t.length - e &&
                  $n('length ' + n + ' out of range ' + (t.length - e)),
              (this.segments = t),
              (this.offset = e),
              (this.len = n)
          }),
          (t.prototype.construct = function(t, e, n) {
            var r = Object.create(Object.getPrototypeOf(this))
            return r.init(t, e, n), r
          }),
          Object.defineProperty(t.prototype, 'length', {
            get: function() {
              return this.len
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.isEqual = function(e) {
            return 0 === t.comparator(this, e)
          }),
          (t.prototype.child = function(e) {
            var n = this.segments.slice(this.offset, this.limit())
            return (
              e instanceof t
                ? e.forEach(function(t) {
                    n.push(t)
                  })
                : 'string' == typeof e
                  ? n.push(e)
                  : $n('Unknown parameter type for Path.child(): ' + e),
              this.construct(n)
            )
          }),
          (t.prototype.limit = function() {
            return this.offset + this.length
          }),
          (t.prototype.popFirst = function(t) {
            return (
              (t = void 0 === t ? 1 : t),
              Zn(this.length >= t, "Can't call popFirst() with less segments"),
              this.construct(this.segments, this.offset + t, this.length - t)
            )
          }),
          (t.prototype.popLast = function() {
            return (
              Zn(!this.isEmpty(), "Can't call popLast() on empty path"),
              this.construct(this.segments, this.offset, this.length - 1)
            )
          }),
          (t.prototype.firstSegment = function() {
            return (
              Zn(!this.isEmpty(), "Can't call firstSegment() on empty path"),
              this.segments[this.offset]
            )
          }),
          (t.prototype.lastSegment = function() {
            return (
              Zn(!this.isEmpty(), "Can't call lastSegment() on empty path"),
              this.segments[this.limit() - 1]
            )
          }),
          (t.prototype.get = function(t) {
            return (
              Zn(t < this.length, 'Index out of range'),
              this.segments[this.offset + t]
            )
          }),
          (t.prototype.isEmpty = function() {
            return 0 === this.length
          }),
          (t.prototype.isPrefixOf = function(t) {
            if (t.length < this.length) return !1
            for (var e = 0; e < this.length; e++)
              if (this.get(e) !== t.get(e)) return !1
            return !0
          }),
          (t.prototype.isImmediateParentOf = function(t) {
            if (this.length + 1 !== t.length) return !1
            for (var e = 0; e < this.length; e++)
              if (this.get(e) !== t.get(e)) return !1
            return !0
          }),
          (t.prototype.forEach = function(t) {
            for (var e = this.offset, n = this.limit(); e < n; e++)
              t(this.segments[e])
          }),
          (t.prototype.toArray = function() {
            return this.segments.slice(this.offset, this.limit())
          }),
          (t.comparator = function(t, e) {
            for (var n = Math.min(t.length, e.length), r = 0; r < n; r++) {
              var i = t.get(r),
                o = e.get(r)
              if (i < o) return -1
              if (i > o) return 1
            }
            return t.length < e.length ? -1 : t.length > e.length ? 1 : 0
          }),
          t
        )
      })(),
      Qr = (function(t) {
        function e() {
          return (null !== t && t.apply(this, arguments)) || this
        }
        return (
          a(e, t),
          (e.prototype.canonicalString = function() {
            return this.toArray().join('/')
          }),
          (e.prototype.toString = function() {
            return this.canonicalString()
          }),
          (e.fromString = function(t) {
            if (t.indexOf('//') >= 0)
              throw new rr(
                nr.INVALID_ARGUMENT,
                'Invalid path (' + t + '). Paths must not contain // in them.',
              )
            return new e(
              t.split('/').filter(function(t) {
                return t.length > 0
              }),
            )
          }),
          (e.EMPTY_PATH = new e([])),
          e
        )
      })(Ur),
      Kr = /^[_a-zA-Z][_a-zA-Z0-9]*$/,
      Wr = (function(t) {
        function e() {
          return (null !== t && t.apply(this, arguments)) || this
        }
        return (
          a(e, t),
          (e.isValidIdentifier = function(t) {
            return Kr.test(t)
          }),
          (e.prototype.canonicalString = function() {
            return this.toArray()
              .map(function(t) {
                return (
                  (t = t.replace('\\', '\\\\').replace('`', '\\`')),
                  e.isValidIdentifier(t) || (t = '`' + t + '`'),
                  t
                )
              })
              .join('.')
          }),
          (e.prototype.toString = function() {
            return this.canonicalString()
          }),
          (e.prototype.isKeyField = function() {
            return 1 === this.length && '__name__' === this.get(0)
          }),
          (e.keyField = function() {
            return new e(['__name__'])
          }),
          (e.fromServerFormat = function(t) {
            for (
              var n = [],
                r = '',
                i = 0,
                o = function() {
                  if (0 === r.length)
                    throw new rr(
                      nr.INVALID_ARGUMENT,
                      'Invalid field path (' +
                        t +
                        "). Paths must not be empty, begin with '.', end with '.', or contain '..'",
                    )
                  n.push(r), (r = '')
                },
                s = !1;
              i < t.length;

            ) {
              var a = t[i]
              if ('\\' === a) {
                if (i + 1 === t.length)
                  throw new rr(
                    nr.INVALID_ARGUMENT,
                    'Path has trailing escape character: ' + t,
                  )
                var u = t[i + 1]
                if ('\\' !== u && '.' !== u && '`' !== u)
                  throw new rr(
                    nr.INVALID_ARGUMENT,
                    'Path has invalid escape sequence: ' + t,
                  )
                ;(r += u), (i += 2)
              } else
                '`' === a
                  ? ((s = !s), i++)
                  : '.' !== a || s
                    ? ((r += a), i++)
                    : (o(), i++)
            }
            if ((o(), s))
              throw new rr(nr.INVALID_ARGUMENT, 'Unterminated ` in path: ' + t)
            return new e(n)
          }),
          (e.EMPTY_PATH = new e([])),
          e
        )
      })(Ur),
      jr = (function() {
        function t(e) {
          ;(this.path = e),
            Zn(
              t.isDocumentKey(e),
              'Invalid DocumentKey with an odd number of segments: ' +
                e.toArray().join('/'),
            )
        }
        return (
          (t.prototype.isEqual = function(t) {
            return null !== t && 0 === Qr.comparator(this.path, t.path)
          }),
          (t.prototype.toString = function() {
            return this.path.toString()
          }),
          (t.comparator = function(t, e) {
            return Qr.comparator(t.path, e.path)
          }),
          (t.isDocumentKey = function(t) {
            return t.length % 2 == 0
          }),
          (t.fromSegments = function(e) {
            return new t(new Qr(e.slice()))
          }),
          (t.fromPathString = function(e) {
            return new t(Qr.fromString(e))
          }),
          (t.EMPTY = new t(new Qr([]))),
          t
        )
      })(),
      Gr = (function() {
        function t(t, e) {
          ;(this.key = t), (this.version = e)
        }
        return (
          (t.compareByKey = function(t, e) {
            return jr.comparator(t.key, e.key)
          }),
          t
        )
      })(),
      zr = (function(t) {
        function e(e, n, r, i) {
          var o = t.call(this, e, n) || this
          return (
            (o.data = r),
            (o.hasLocalMutations = !!i.hasLocalMutations),
            (o.hasCommittedMutations = !!i.hasCommittedMutations),
            o
          )
        }
        return (
          a(e, t),
          (e.prototype.field = function(t) {
            return this.data.field(t)
          }),
          (e.prototype.fieldValue = function(t) {
            var e = this.field(t)
            return e ? e.value() : void 0
          }),
          (e.prototype.value = function() {
            return this.data.value()
          }),
          (e.prototype.isEqual = function(t) {
            return (
              t instanceof e &&
              this.key.isEqual(t.key) &&
              this.version.isEqual(t.version) &&
              this.data.isEqual(t.data) &&
              this.hasLocalMutations === t.hasLocalMutations &&
              this.hasCommittedMutations === t.hasCommittedMutations
            )
          }),
          (e.prototype.toString = function() {
            return (
              'Document(' +
              this.key +
              ', ' +
              this.version +
              ', ' +
              this.data.toString() +
              ', {hasLocalMutations: ' +
              this.hasLocalMutations +
              '}), {hasCommittedMutations: ' +
              this.hasCommittedMutations +
              '})'
            )
          }),
          Object.defineProperty(e.prototype, 'hasPendingWrites', {
            get: function() {
              return this.hasLocalMutations || this.hasCommittedMutations
            },
            enumerable: !0,
            configurable: !0,
          }),
          (e.compareByField = function(t, e, n) {
            var r = e.field(t),
              i = n.field(t)
            return void 0 !== r && void 0 !== i
              ? r.compareTo(i)
              : $n("Trying to compare documents on fields that don't exist")
          }),
          e
        )
      })(Gr),
      Hr = (function(t) {
        function e(e, n, r) {
          var i = t.call(this, e, n) || this
          return (
            (i.hasCommittedMutations = !(!r || !r.hasCommittedMutations)), i
          )
        }
        return (
          a(e, t),
          (e.prototype.toString = function() {
            return 'NoDocument(' + this.key + ', ' + this.version + ')'
          }),
          Object.defineProperty(e.prototype, 'hasPendingWrites', {
            get: function() {
              return this.hasCommittedMutations
            },
            enumerable: !0,
            configurable: !0,
          }),
          (e.prototype.isEqual = function(t) {
            return (
              t instanceof e &&
              t.hasCommittedMutations === this.hasCommittedMutations &&
              t.version.isEqual(this.version) &&
              t.key.isEqual(this.key)
            )
          }),
          e
        )
      })(Gr),
      Xr = (function(t) {
        function e(e, n) {
          return t.call(this, e, n) || this
        }
        return (
          a(e, t),
          (e.prototype.toString = function() {
            return 'UnknownDocument(' + this.key + ', ' + this.version + ')'
          }),
          Object.defineProperty(e.prototype, 'hasPendingWrites', {
            get: function() {
              return !0
            },
            enumerable: !0,
            configurable: !0,
          }),
          (e.prototype.isEqual = function(t) {
            return (
              t instanceof e &&
              t.version.isEqual(this.version) &&
              t.key.isEqual(this.key)
            )
          }),
          e
        )
      })(Gr),
      Yr = (function() {
        function t(t, e) {
          ;(this.comparator = t), (this.root = e || $r.EMPTY)
        }
        return (
          (t.prototype.insert = function(e, n) {
            return new t(
              this.comparator,
              this.root
                .insert(e, n, this.comparator)
                .copy(null, null, $r.BLACK, null, null),
            )
          }),
          (t.prototype.remove = function(e) {
            return new t(
              this.comparator,
              this.root
                .remove(e, this.comparator)
                .copy(null, null, $r.BLACK, null, null),
            )
          }),
          (t.prototype.get = function(t) {
            for (var e = this.root; !e.isEmpty(); ) {
              var n = this.comparator(t, e.key)
              if (0 === n) return e.value
              n < 0 ? (e = e.left) : n > 0 && (e = e.right)
            }
            return null
          }),
          (t.prototype.indexOf = function(t) {
            for (var e = 0, n = this.root; !n.isEmpty(); ) {
              var r = this.comparator(t, n.key)
              if (0 === r) return e + n.left.size
              r < 0 ? (n = n.left) : ((e += n.left.size + 1), (n = n.right))
            }
            return -1
          }),
          (t.prototype.isEmpty = function() {
            return this.root.isEmpty()
          }),
          Object.defineProperty(t.prototype, 'size', {
            get: function() {
              return this.root.size
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.minKey = function() {
            return this.root.minKey()
          }),
          (t.prototype.maxKey = function() {
            return this.root.maxKey()
          }),
          (t.prototype.inorderTraversal = function(t) {
            return this.root.inorderTraversal(t)
          }),
          (t.prototype.forEach = function(t) {
            this.inorderTraversal(function(e, n) {
              return t(e, n), !1
            })
          }),
          (t.prototype.reverseTraversal = function(t) {
            return this.root.reverseTraversal(t)
          }),
          (t.prototype.getIterator = function() {
            return new Jr(this.root, null, this.comparator, !1)
          }),
          (t.prototype.getIteratorFrom = function(t) {
            return new Jr(this.root, t, this.comparator, !1)
          }),
          (t.prototype.getReverseIterator = function() {
            return new Jr(this.root, null, this.comparator, !0)
          }),
          (t.prototype.getReverseIteratorFrom = function(t) {
            return new Jr(this.root, t, this.comparator, !0)
          }),
          (t.prototype[Symbol.iterator] = function() {
            var t = this.getIterator()
            return {
              next: function() {
                return t.hasNext()
                  ? { done: !1, value: t.getNext() }
                  : { done: !0, value: {} }
              },
            }
          }),
          t
        )
      })(),
      Jr = (function() {
        function t(t, e, n, r) {
          ;(this.isReverse = r), (this.nodeStack = [])
          for (var i = 1; !t.isEmpty(); )
            if (((i = e ? n(t.key, e) : 1), r && (i *= -1), i < 0))
              t = this.isReverse ? t.left : t.right
            else {
              if (0 === i) {
                this.nodeStack.push(t)
                break
              }
              this.nodeStack.push(t), (t = this.isReverse ? t.right : t.left)
            }
        }
        return (
          (t.prototype.getNext = function() {
            Zn(
              this.nodeStack.length > 0,
              'getNext() called on iterator when hasNext() is false.',
            )
            var t = this.nodeStack.pop(),
              e = { key: t.key, value: t.value }
            if (this.isReverse)
              for (t = t.left; !t.isEmpty(); )
                this.nodeStack.push(t), (t = t.right)
            else
              for (t = t.right; !t.isEmpty(); )
                this.nodeStack.push(t), (t = t.left)
            return e
          }),
          (t.prototype.hasNext = function() {
            return this.nodeStack.length > 0
          }),
          (t.prototype.peek = function() {
            if (0 === this.nodeStack.length) return null
            var t = this.nodeStack[this.nodeStack.length - 1]
            return { key: t.key, value: t.value }
          }),
          t
        )
      })(),
      $r = (function() {
        function t(e, n, r, i, o) {
          ;(this.key = e),
            (this.value = n),
            (this.color = null != r ? r : t.RED),
            (this.left = null != i ? i : t.EMPTY),
            (this.right = null != o ? o : t.EMPTY),
            (this.size = this.left.size + 1 + this.right.size)
        }
        return (
          (t.prototype.copy = function(e, n, r, i, o) {
            return new t(
              null != e ? e : this.key,
              null != n ? n : this.value,
              null != r ? r : this.color,
              null != i ? i : this.left,
              null != o ? o : this.right,
            )
          }),
          (t.prototype.isEmpty = function() {
            return !1
          }),
          (t.prototype.inorderTraversal = function(t) {
            return (
              this.left.inorderTraversal(t) ||
              t(this.key, this.value) ||
              this.right.inorderTraversal(t)
            )
          }),
          (t.prototype.reverseTraversal = function(t) {
            return (
              this.right.reverseTraversal(t) ||
              t(this.key, this.value) ||
              this.left.reverseTraversal(t)
            )
          }),
          (t.prototype.min = function() {
            return this.left.isEmpty() ? this : this.left.min()
          }),
          (t.prototype.minKey = function() {
            return this.min().key
          }),
          (t.prototype.maxKey = function() {
            return this.right.isEmpty() ? this.key : this.right.maxKey()
          }),
          (t.prototype.insert = function(t, e, n) {
            var r = this,
              i = n(t, r.key)
            return (r =
              i < 0
                ? r.copy(null, null, null, r.left.insert(t, e, n), null)
                : 0 === i
                  ? r.copy(null, e, null, null, null)
                  : r.copy(
                      null,
                      null,
                      null,
                      null,
                      r.right.insert(t, e, n),
                    )).fixUp()
          }),
          (t.prototype.removeMin = function() {
            if (this.left.isEmpty()) return t.EMPTY
            var e = this
            return (
              e.left.isRed() || e.left.left.isRed() || (e = e.moveRedLeft()),
              (e = e.copy(null, null, null, e.left.removeMin(), null)).fixUp()
            )
          }),
          (t.prototype.remove = function(e, n) {
            var r,
              i = this
            if (n(e, i.key) < 0)
              i.left.isEmpty() ||
                i.left.isRed() ||
                i.left.left.isRed() ||
                (i = i.moveRedLeft()),
                (i = i.copy(null, null, null, i.left.remove(e, n), null))
            else {
              if (
                (i.left.isRed() && (i = i.rotateRight()),
                i.right.isEmpty() ||
                  i.right.isRed() ||
                  i.right.left.isRed() ||
                  (i = i.moveRedRight()),
                0 === n(e, i.key))
              ) {
                if (i.right.isEmpty()) return t.EMPTY
                ;(r = i.right.min()),
                  (i = i.copy(r.key, r.value, null, null, i.right.removeMin()))
              }
              i = i.copy(null, null, null, null, i.right.remove(e, n))
            }
            return i.fixUp()
          }),
          (t.prototype.isRed = function() {
            return this.color
          }),
          (t.prototype.fixUp = function() {
            var t = this
            return (
              t.right.isRed() && !t.left.isRed() && (t = t.rotateLeft()),
              t.left.isRed() && t.left.left.isRed() && (t = t.rotateRight()),
              t.left.isRed() && t.right.isRed() && (t = t.colorFlip()),
              t
            )
          }),
          (t.prototype.moveRedLeft = function() {
            var t = this.colorFlip()
            return (
              t.right.left.isRed() &&
                (t = (t = (t = t.copy(
                  null,
                  null,
                  null,
                  null,
                  t.right.rotateRight(),
                )).rotateLeft()).colorFlip()),
              t
            )
          }),
          (t.prototype.moveRedRight = function() {
            var t = this.colorFlip()
            return (
              t.left.left.isRed() && (t = (t = t.rotateRight()).colorFlip()), t
            )
          }),
          (t.prototype.rotateLeft = function() {
            var e = this.copy(null, null, t.RED, null, this.right.left)
            return this.right.copy(null, null, this.color, e, null)
          }),
          (t.prototype.rotateRight = function() {
            var e = this.copy(null, null, t.RED, this.left.right, null)
            return this.left.copy(null, null, this.color, null, e)
          }),
          (t.prototype.colorFlip = function() {
            var t = this.left.copy(null, null, !this.left.color, null, null),
              e = this.right.copy(null, null, !this.right.color, null, null)
            return this.copy(null, null, !this.color, t, e)
          }),
          (t.prototype.checkMaxDepth = function() {
            var t = this.check()
            return Math.pow(2, t) <= this.size + 1
          }),
          (t.prototype.check = function() {
            if (this.isRed() && this.left.isRed())
              throw $n(
                'Red node has red child(' + this.key + ',' + this.value + ')',
              )
            if (this.right.isRed())
              throw $n(
                'Right child of (' + this.key + ',' + this.value + ') is red',
              )
            var t = this.left.check()
            if (t !== this.right.check()) throw $n('Black depths differ')
            return t + (this.isRed() ? 0 : 1)
          }),
          (t.EMPTY = null),
          (t.RED = !0),
          (t.BLACK = !1),
          t
        )
      })(),
      Zr = (function() {
        function t() {
          this.size = 0
        }
        return (
          (t.prototype.copy = function(t, e, n, r, i) {
            return this
          }),
          (t.prototype.insert = function(t, e, n) {
            return new $r(t, e)
          }),
          (t.prototype.remove = function(t, e) {
            return this
          }),
          (t.prototype.isEmpty = function() {
            return !0
          }),
          (t.prototype.inorderTraversal = function(t) {
            return !1
          }),
          (t.prototype.reverseTraversal = function(t) {
            return !1
          }),
          (t.prototype.minKey = function() {
            return null
          }),
          (t.prototype.maxKey = function() {
            return null
          }),
          (t.prototype.isRed = function() {
            return !1
          }),
          (t.prototype.checkMaxDepth = function() {
            return !0
          }),
          (t.prototype.check = function() {
            return 0
          }),
          t
        )
      })()
    ;($r.EMPTY = new Zr()),
      (function(t) {
        ;(t[(t.NullValue = 0)] = 'NullValue'),
          (t[(t.BooleanValue = 1)] = 'BooleanValue'),
          (t[(t.NumberValue = 2)] = 'NumberValue'),
          (t[(t.TimestampValue = 3)] = 'TimestampValue'),
          (t[(t.StringValue = 4)] = 'StringValue'),
          (t[(t.BlobValue = 5)] = 'BlobValue'),
          (t[(t.RefValue = 6)] = 'RefValue'),
          (t[(t.GeoPointValue = 7)] = 'GeoPointValue'),
          (t[(t.ArrayValue = 8)] = 'ArrayValue'),
          (t[(t.ObjectValue = 9)] = 'ObjectValue')
      })(Or || (Or = {})),
      (function(t) {
        ;(t[(t.Default = 0)] = 'Default'),
          (t[(t.Estimate = 1)] = 'Estimate'),
          (t[(t.Previous = 2)] = 'Previous')
      })(Pr || (Pr = {}))
    var ti = (function() {
        function t(t, e) {
          ;(this.serverTimestampBehavior = t), (this.timestampsInSnapshots = e)
        }
        return (
          (t.fromSnapshotOptions = function(e, n) {
            switch (e.serverTimestamps) {
              case 'estimate':
                return new t(Pr.Estimate, n)
              case 'previous':
                return new t(Pr.Previous, n)
              case 'none':
              case void 0:
                return new t(Pr.Default, n)
              default:
                return $n('fromSnapshotOptions() called with invalid options.')
            }
          }),
          t
        )
      })(),
      ei = (function() {
        function t() {}
        return (
          (t.prototype.toString = function() {
            var t = this.value()
            return null === t ? 'null' : t.toString()
          }),
          (t.prototype.defaultCompareTo = function(t) {
            return (
              Zn(
                this.typeOrder !== t.typeOrder,
                'Default compareTo should not be used for values of same type.',
              ),
              Ar(this.typeOrder, t.typeOrder)
            )
          }),
          t
        )
      })(),
      ni = (function(t) {
        function e() {
          var e = t.call(this) || this
          return (e.typeOrder = Or.NullValue), (e.internalValue = null), e
        }
        return (
          a(e, t),
          (e.prototype.value = function(t) {
            return null
          }),
          (e.prototype.isEqual = function(t) {
            return t instanceof e
          }),
          (e.prototype.compareTo = function(t) {
            return t instanceof e ? 0 : this.defaultCompareTo(t)
          }),
          (e.INSTANCE = new e()),
          e
        )
      })(ei),
      ri = (function(t) {
        function e(e) {
          var n = t.call(this) || this
          return (n.internalValue = e), (n.typeOrder = Or.BooleanValue), n
        }
        return (
          a(e, t),
          (e.prototype.value = function(t) {
            return this.internalValue
          }),
          (e.prototype.isEqual = function(t) {
            return t instanceof e && this.internalValue === t.internalValue
          }),
          (e.prototype.compareTo = function(t) {
            return t instanceof e ? Ar(this, t) : this.defaultCompareTo(t)
          }),
          (e.of = function(t) {
            return t ? e.TRUE : e.FALSE
          }),
          (e.TRUE = new e(!0)),
          (e.FALSE = new e(!1)),
          e
        )
      })(ei),
      ii = (function(t) {
        function e(e) {
          var n = t.call(this) || this
          return (n.internalValue = e), (n.typeOrder = Or.NumberValue), n
        }
        return (
          a(e, t),
          (e.prototype.value = function(t) {
            return this.internalValue
          }),
          (e.prototype.compareTo = function(t) {
            return t instanceof e
              ? ((n = this.internalValue),
                (r = t.internalValue),
                n < r
                  ? -1
                  : n > r
                    ? 1
                    : n === r
                      ? 0
                      : isNaN(n)
                        ? isNaN(r)
                          ? 0
                          : -1
                        : 1)
              : this.defaultCompareTo(t)
            var n, r
          }),
          e
        )
      })(ei)
    function oi(t, e) {
      return t === e ? 0 !== t || 1 / t == 1 / e : t != t && e != e
    }
    var si = (function(t) {
        function e(e) {
          return t.call(this, e) || this
        }
        return (
          a(e, t),
          (e.prototype.isEqual = function(t) {
            return t instanceof e && oi(this.internalValue, t.internalValue)
          }),
          e
        )
      })(ii),
      ai = (function(t) {
        function e(e) {
          var n = t.call(this, e) || this
          return (n.internalValue = e), n
        }
        return (
          a(e, t),
          (e.prototype.isEqual = function(t) {
            return t instanceof e && oi(this.internalValue, t.internalValue)
          }),
          (e.NAN = new e(NaN)),
          (e.POSITIVE_INFINITY = new e(1 / 0)),
          (e.NEGATIVE_INFINITY = new e(-1 / 0)),
          e
        )
      })(ii),
      ui = (function(t) {
        function e(e) {
          var n = t.call(this) || this
          return (n.internalValue = e), (n.typeOrder = Or.StringValue), n
        }
        return (
          a(e, t),
          (e.prototype.value = function(t) {
            return this.internalValue
          }),
          (e.prototype.isEqual = function(t) {
            return t instanceof e && this.internalValue === t.internalValue
          }),
          (e.prototype.compareTo = function(t) {
            return t instanceof e
              ? Ar(this.internalValue, t.internalValue)
              : this.defaultCompareTo(t)
          }),
          e
        )
      })(ei),
      ci = (function(t) {
        function e(e) {
          var n = t.call(this) || this
          return (n.internalValue = e), (n.typeOrder = Or.TimestampValue), n
        }
        return (
          a(e, t),
          (e.prototype.value = function(t) {
            return t && t.timestampsInSnapshots
              ? this.internalValue
              : this.internalValue.toDate()
          }),
          (e.prototype.isEqual = function(t) {
            return t instanceof e && this.internalValue.isEqual(t.internalValue)
          }),
          (e.prototype.compareTo = function(t) {
            return t instanceof e
              ? this.internalValue._compareTo(t.internalValue)
              : t instanceof hi
                ? -1
                : this.defaultCompareTo(t)
          }),
          e
        )
      })(ei),
      hi = (function(t) {
        function e(e, n) {
          var r = t.call(this) || this
          return (
            (r.localWriteTime = e),
            (r.previousValue = n),
            (r.typeOrder = Or.TimestampValue),
            r
          )
        }
        return (
          a(e, t),
          (e.prototype.value = function(t) {
            return t && t.serverTimestampBehavior === Pr.Estimate
              ? new ci(this.localWriteTime).value(t)
              : t &&
                t.serverTimestampBehavior === Pr.Previous &&
                this.previousValue
                ? this.previousValue.value(t)
                : null
          }),
          (e.prototype.isEqual = function(t) {
            return (
              t instanceof e && this.localWriteTime.isEqual(t.localWriteTime)
            )
          }),
          (e.prototype.compareTo = function(t) {
            return t instanceof e
              ? this.localWriteTime._compareTo(t.localWriteTime)
              : t instanceof ci
                ? 1
                : this.defaultCompareTo(t)
          }),
          (e.prototype.toString = function() {
            return (
              '<ServerTimestamp localTime=' +
              this.localWriteTime.toString() +
              '>'
            )
          }),
          e
        )
      })(ei),
      li = (function(t) {
        function e(e) {
          var n = t.call(this) || this
          return (n.internalValue = e), (n.typeOrder = Or.BlobValue), n
        }
        return (
          a(e, t),
          (e.prototype.value = function(t) {
            return this.internalValue
          }),
          (e.prototype.isEqual = function(t) {
            return t instanceof e && this.internalValue.isEqual(t.internalValue)
          }),
          (e.prototype.compareTo = function(t) {
            return t instanceof e
              ? this.internalValue._compareTo(t.internalValue)
              : this.defaultCompareTo(t)
          }),
          e
        )
      })(ei),
      fi = (function(t) {
        function e(e, n) {
          var r = t.call(this) || this
          return (r.databaseId = e), (r.key = n), (r.typeOrder = Or.RefValue), r
        }
        return (
          a(e, t),
          (e.prototype.value = function(t) {
            return this.key
          }),
          (e.prototype.isEqual = function(t) {
            return (
              t instanceof e &&
              (this.key.isEqual(t.key) && this.databaseId.isEqual(t.databaseId))
            )
          }),
          (e.prototype.compareTo = function(t) {
            if (t instanceof e) {
              var n = this.databaseId.compareTo(t.databaseId)
              return 0 !== n ? n : jr.comparator(this.key, t.key)
            }
            return this.defaultCompareTo(t)
          }),
          e
        )
      })(ei),
      di = (function(t) {
        function e(e) {
          var n = t.call(this) || this
          return (n.internalValue = e), (n.typeOrder = Or.GeoPointValue), n
        }
        return (
          a(e, t),
          (e.prototype.value = function(t) {
            return this.internalValue
          }),
          (e.prototype.isEqual = function(t) {
            return t instanceof e && this.internalValue.isEqual(t.internalValue)
          }),
          (e.prototype.compareTo = function(t) {
            return t instanceof e
              ? this.internalValue._compareTo(t.internalValue)
              : this.defaultCompareTo(t)
          }),
          e
        )
      })(ei),
      pi = (function(t) {
        function e(e) {
          var n = t.call(this) || this
          return (n.internalValue = e), (n.typeOrder = Or.ObjectValue), n
        }
        return (
          a(e, t),
          (e.prototype.value = function(t) {
            var e = {}
            return (
              this.internalValue.inorderTraversal(function(n, r) {
                e[n] = r.value(t)
              }),
              e
            )
          }),
          (e.prototype.forEach = function(t) {
            this.internalValue.inorderTraversal(t)
          }),
          (e.prototype.isEqual = function(t) {
            if (t instanceof e) {
              for (
                var n = this.internalValue.getIterator(),
                  r = t.internalValue.getIterator();
                n.hasNext() && r.hasNext();

              ) {
                var i = n.getNext(),
                  o = r.getNext()
                if (i.key !== o.key || !i.value.isEqual(o.value)) return !1
              }
              return !n.hasNext() && !r.hasNext()
            }
            return !1
          }),
          (e.prototype.compareTo = function(t) {
            if (t instanceof e) {
              for (
                var n = this.internalValue.getIterator(),
                  r = t.internalValue.getIterator();
                n.hasNext() && r.hasNext();

              ) {
                var i = n.getNext(),
                  o = r.getNext(),
                  s = Ar(i.key, o.key) || i.value.compareTo(o.value)
                if (s) return s
              }
              return Ar(n.hasNext(), r.hasNext())
            }
            return this.defaultCompareTo(t)
          }),
          (e.prototype.set = function(t, n) {
            if (
              (Zn(
                !t.isEmpty(),
                'Cannot set field for empty path on ObjectValue',
              ),
              1 === t.length)
            )
              return this.setChild(t.firstSegment(), n)
            var r = this.child(t.firstSegment())
            r instanceof e || (r = e.EMPTY)
            var i = r.set(t.popFirst(), n)
            return this.setChild(t.firstSegment(), i)
          }),
          (e.prototype.delete = function(t) {
            if (
              (Zn(
                !t.isEmpty(),
                'Cannot delete field for empty path on ObjectValue',
              ),
              1 === t.length)
            )
              return new e(this.internalValue.remove(t.firstSegment()))
            var n = this.child(t.firstSegment())
            if (n instanceof e) {
              var r = n.delete(t.popFirst())
              return new e(this.internalValue.insert(t.firstSegment(), r))
            }
            return this
          }),
          (e.prototype.contains = function(t) {
            return void 0 !== this.field(t)
          }),
          (e.prototype.field = function(t) {
            Zn(!t.isEmpty(), "Can't get field of empty path")
            var n = this
            return (
              t.forEach(function(t) {
                n = (n instanceof e && n.internalValue.get(t)) || void 0
              }),
              n
            )
          }),
          (e.prototype.toString = function() {
            return JSON.stringify(this.value())
          }),
          (e.prototype.child = function(t) {
            return this.internalValue.get(t) || void 0
          }),
          (e.prototype.setChild = function(t, n) {
            return new e(this.internalValue.insert(t, n))
          }),
          (e.EMPTY = new e(new Yr(Ar))),
          e
        )
      })(ei),
      mi = (function(t) {
        function e(e) {
          var n = t.call(this) || this
          return (n.internalValue = e), (n.typeOrder = Or.ArrayValue), n
        }
        return (
          a(e, t),
          (e.prototype.value = function(t) {
            return this.internalValue.map(function(e) {
              return e.value(t)
            })
          }),
          (e.prototype.forEach = function(t) {
            this.internalValue.forEach(t)
          }),
          (e.prototype.isEqual = function(t) {
            if (t instanceof e) {
              if (this.internalValue.length !== t.internalValue.length)
                return !1
              for (var n = 0; n < this.internalValue.length; n++)
                if (!this.internalValue[n].isEqual(t.internalValue[n]))
                  return !1
              return !0
            }
            return !1
          }),
          (e.prototype.compareTo = function(t) {
            if (t instanceof e) {
              for (
                var n = Math.min(
                    this.internalValue.length,
                    t.internalValue.length,
                  ),
                  r = 0;
                r < n;
                r++
              ) {
                var i = this.internalValue[r].compareTo(t.internalValue[r])
                if (i) return i
              }
              return Ar(this.internalValue.length, t.internalValue.length)
            }
            return this.defaultCompareTo(t)
          }),
          (e.prototype.toString = function() {
            return JSON.stringify(this.value())
          }),
          e
        )
      })(ei),
      yi = Number,
      gi = yi.MIN_SAFE_INTEGER || -(Math.pow(2, 53) - 1),
      vi = yi.MAX_SAFE_INTEGER || Math.pow(2, 53) - 1,
      bi =
        yi.isInteger ||
        function(t) {
          return 'number' == typeof t && isFinite(t) && Math.floor(t) === t
        }
    function wi(t) {
      return null === t || void 0 === t
    }
    function Ti(t) {
      return bi(t) && t <= vi && t >= gi
    }
    var Si,
      Ei = (function() {
        function t(t, e, n, r, i, o) {
          void 0 === e && (e = []),
            void 0 === n && (n = []),
            void 0 === r && (r = null),
            void 0 === i && (i = null),
            void 0 === o && (o = null),
            (this.path = t),
            (this.explicitOrderBy = e),
            (this.filters = n),
            (this.limit = r),
            (this.startAt = i),
            (this.endAt = o),
            (this.memoizedCanonicalId = null),
            (this.memoizedOrderBy = null),
            this.startAt && this.assertValidBound(this.startAt),
            this.endAt && this.assertValidBound(this.endAt)
        }
        return (
          (t.atPath = function(e) {
            return new t(e)
          }),
          Object.defineProperty(t.prototype, 'orderBy', {
            get: function() {
              if (null === this.memoizedOrderBy) {
                var t = this.getInequalityFilterField(),
                  e = this.getFirstOrderByField()
                if (null !== t && null === e)
                  t.isKeyField()
                    ? (this.memoizedOrderBy = [Oi])
                    : (this.memoizedOrderBy = [new Mi(t), Oi])
                else {
                  Zn(
                    null === t || (null !== e && t.isEqual(e)),
                    'First orderBy should match inequality field.',
                  ),
                    (this.memoizedOrderBy = [])
                  for (
                    var n = !1, r = 0, i = this.explicitOrderBy;
                    r < i.length;
                    r++
                  ) {
                    var o = i[r]
                    this.memoizedOrderBy.push(o),
                      o.field.isKeyField() && (n = !0)
                  }
                  if (!n) {
                    var s =
                      this.explicitOrderBy.length > 0
                        ? this.explicitOrderBy[this.explicitOrderBy.length - 1]
                            .dir
                        : ki.ASCENDING
                    this.memoizedOrderBy.push(s === ki.ASCENDING ? Oi : Pi)
                  }
                }
              }
              return this.memoizedOrderBy
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.addFilter = function(e) {
            Zn(
              null == this.getInequalityFilterField() ||
                !(e instanceof Di) ||
                !e.isInequality() ||
                e.field.isEqual(this.getInequalityFilterField()),
              'Query must only have one inequality field.',
            ),
              Zn(
                !jr.isDocumentKey(this.path),
                'No filtering allowed for document query',
              )
            var n = this.filters.concat([e])
            return new t(
              this.path,
              this.explicitOrderBy.slice(),
              n,
              this.limit,
              this.startAt,
              this.endAt,
            )
          }),
          (t.prototype.addOrderBy = function(e) {
            Zn(
              !jr.isDocumentKey(this.path),
              'No ordering allowed for document query',
            ),
              Zn(
                !this.startAt && !this.endAt,
                'Bounds must be set after orderBy',
              )
            var n = this.explicitOrderBy.concat([e])
            return new t(
              this.path,
              n,
              this.filters.slice(),
              this.limit,
              this.startAt,
              this.endAt,
            )
          }),
          (t.prototype.withLimit = function(e) {
            return new t(
              this.path,
              this.explicitOrderBy.slice(),
              this.filters.slice(),
              e,
              this.startAt,
              this.endAt,
            )
          }),
          (t.prototype.withStartAt = function(e) {
            return new t(
              this.path,
              this.explicitOrderBy.slice(),
              this.filters.slice(),
              this.limit,
              e,
              this.endAt,
            )
          }),
          (t.prototype.withEndAt = function(e) {
            return new t(
              this.path,
              this.explicitOrderBy.slice(),
              this.filters.slice(),
              this.limit,
              this.startAt,
              e,
            )
          }),
          (t.prototype.canonicalId = function() {
            if (null === this.memoizedCanonicalId) {
              var t = this.path.canonicalString()
              t += '|f:'
              for (var e = 0, n = this.filters; e < n.length; e++) {
                ;(t += n[e].canonicalId()), (t += ',')
              }
              t += '|ob:'
              for (var r = 0, i = this.orderBy; r < i.length; r++) {
                ;(t += i[r].canonicalId()), (t += ',')
              }
              wi(this.limit) || ((t += '|l:'), (t += this.limit)),
                this.startAt &&
                  ((t += '|lb:'), (t += this.startAt.canonicalId())),
                this.endAt && ((t += '|ub:'), (t += this.endAt.canonicalId())),
                (this.memoizedCanonicalId = t)
            }
            return this.memoizedCanonicalId
          }),
          (t.prototype.toString = function() {
            var t = 'Query(' + this.path.canonicalString()
            return (
              this.filters.length > 0 &&
                (t += ', filters: [' + this.filters.join(', ') + ']'),
              wi(this.limit) || (t += ', limit: ' + this.limit),
              this.explicitOrderBy.length > 0 &&
                (t += ', orderBy: [' + this.explicitOrderBy.join(', ') + ']'),
              this.startAt && (t += ', startAt: ' + this.startAt.canonicalId()),
              this.endAt && (t += ', endAt: ' + this.endAt.canonicalId()),
              t + ')'
            )
          }),
          (t.prototype.isEqual = function(t) {
            if (this.limit !== t.limit) return !1
            if (this.orderBy.length !== t.orderBy.length) return !1
            for (var e = 0; e < this.orderBy.length; e++)
              if (!this.orderBy[e].isEqual(t.orderBy[e])) return !1
            if (this.filters.length !== t.filters.length) return !1
            for (e = 0; e < this.filters.length; e++)
              if (!this.filters[e].isEqual(t.filters[e])) return !1
            return (
              !!this.path.isEqual(t.path) &&
              (!(null !== this.startAt
                ? !this.startAt.isEqual(t.startAt)
                : null !== t.startAt) &&
                (null !== this.endAt
                  ? this.endAt.isEqual(t.endAt)
                  : null === t.endAt))
            )
          }),
          (t.prototype.docComparator = function(t, e) {
            for (var n = !1, r = 0, i = this.orderBy; r < i.length; r++) {
              var o = i[r],
                s = o.compare(t, e)
              if (0 !== s) return s
              n = n || o.field.isKeyField()
            }
            return Zn(n, "orderBy used that doesn't compare on key field"), 0
          }),
          (t.prototype.matches = function(t) {
            return (
              this.matchesAncestor(t) &&
              this.matchesOrderBy(t) &&
              this.matchesFilters(t) &&
              this.matchesBounds(t)
            )
          }),
          (t.prototype.hasLimit = function() {
            return !wi(this.limit)
          }),
          (t.prototype.getFirstOrderByField = function() {
            return this.explicitOrderBy.length > 0
              ? this.explicitOrderBy[0].field
              : null
          }),
          (t.prototype.getInequalityFilterField = function() {
            for (var t = 0, e = this.filters; t < e.length; t++) {
              var n = e[t]
              if (n instanceof Di && n.isInequality()) return n.field
            }
            return null
          }),
          (t.prototype.hasArrayContainsFilter = function() {
            return (
              void 0 !==
              this.filters.find(function(t) {
                return t instanceof Di && t.op === Ci.ARRAY_CONTAINS
              })
            )
          }),
          (t.prototype.isDocumentQuery = function() {
            return jr.isDocumentKey(this.path) && 0 === this.filters.length
          }),
          (t.prototype.matchesAncestor = function(t) {
            var e = t.key.path
            return jr.isDocumentKey(this.path)
              ? this.path.isEqual(e)
              : this.path.isPrefixOf(e) && this.path.length === e.length - 1
          }),
          (t.prototype.matchesOrderBy = function(t) {
            for (var e = 0, n = this.explicitOrderBy; e < n.length; e++) {
              var r = n[e]
              if (!r.field.isKeyField() && void 0 === t.field(r.field))
                return !1
            }
            return !0
          }),
          (t.prototype.matchesFilters = function(t) {
            for (var e = 0, n = this.filters; e < n.length; e++) {
              if (!n[e].matches(t)) return !1
            }
            return !0
          }),
          (t.prototype.matchesBounds = function(t) {
            return (
              !(
                this.startAt &&
                !this.startAt.sortsBeforeDocument(this.orderBy, t)
              ) &&
              (!this.endAt || !this.endAt.sortsBeforeDocument(this.orderBy, t))
            )
          }),
          (t.prototype.assertValidBound = function(t) {
            Zn(
              t.position.length <= this.orderBy.length,
              'Bound is longer than orderBy',
            )
          }),
          t
        )
      })(),
      Ii = (function() {
        function t() {}
        return (
          (t.create = function(t, e, n) {
            if (n.isEqual(ni.INSTANCE)) {
              if (e !== Ci.EQUAL)
                throw new rr(
                  nr.INVALID_ARGUMENT,
                  'Invalid query. You can only perform equals comparisons on null.',
                )
              return new Ni(t)
            }
            if (n.isEqual(ai.NAN)) {
              if (e !== Ci.EQUAL)
                throw new rr(
                  nr.INVALID_ARGUMENT,
                  'Invalid query. You can only perform equals comparisons on NaN.',
                )
              return new Ai(t)
            }
            return new Di(t, e, n)
          }),
          t
        )
      })(),
      Ci = (function() {
        function t(t) {
          this.name = t
        }
        return (
          (t.fromString = function(e) {
            switch (e) {
              case '<':
                return t.LESS_THAN
              case '<=':
                return t.LESS_THAN_OR_EQUAL
              case '==':
                return t.EQUAL
              case '>=':
                return t.GREATER_THAN_OR_EQUAL
              case '>':
                return t.GREATER_THAN
              case 'array-contains':
                return t.ARRAY_CONTAINS
              default:
                return $n('Unknown relation: ' + e)
            }
          }),
          (t.prototype.toString = function() {
            return this.name
          }),
          (t.prototype.isEqual = function(t) {
            return this.name === t.name
          }),
          (t.LESS_THAN = new t('<')),
          (t.LESS_THAN_OR_EQUAL = new t('<=')),
          (t.EQUAL = new t('==')),
          (t.GREATER_THAN = new t('>')),
          (t.GREATER_THAN_OR_EQUAL = new t('>=')),
          (t.ARRAY_CONTAINS = new t('array-contains')),
          t
        )
      })(),
      Di = (function(t) {
        function e(e, n, r) {
          var i = t.call(this) || this
          return (i.field = e), (i.op = n), (i.value = r), i
        }
        return (
          a(e, t),
          (e.prototype.matches = function(t) {
            if (this.field.isKeyField()) {
              Zn(
                this.value instanceof fi,
                'Comparing on key, but filter value not a RefValue',
              ),
                Zn(
                  this.op !== Ci.ARRAY_CONTAINS,
                  "array-contains queries don't make sense on document keys.",
                )
              var e = this.value,
                n = jr.comparator(t.key, e.key)
              return this.matchesComparison(n)
            }
            var r = t.field(this.field)
            return void 0 !== r && this.matchesValue(r)
          }),
          (e.prototype.matchesValue = function(t) {
            var e = this
            return this.op === Ci.ARRAY_CONTAINS
              ? t instanceof mi &&
                  void 0 !==
                    t.internalValue.find(function(t) {
                      return t.isEqual(e.value)
                    })
              : this.value.typeOrder === t.typeOrder &&
                  this.matchesComparison(t.compareTo(this.value))
          }),
          (e.prototype.matchesComparison = function(t) {
            switch (this.op) {
              case Ci.LESS_THAN:
                return t < 0
              case Ci.LESS_THAN_OR_EQUAL:
                return t <= 0
              case Ci.EQUAL:
                return 0 === t
              case Ci.GREATER_THAN:
                return t > 0
              case Ci.GREATER_THAN_OR_EQUAL:
                return t >= 0
              default:
                return $n('Unknown relation op' + this.op)
            }
          }),
          (e.prototype.isInequality = function() {
            return this.op !== Ci.EQUAL && this.op !== Ci.ARRAY_CONTAINS
          }),
          (e.prototype.canonicalId = function() {
            return (
              this.field.canonicalString() +
              this.op.toString() +
              this.value.toString()
            )
          }),
          (e.prototype.isEqual = function(t) {
            return (
              t instanceof e &&
              (this.op.isEqual(t.op) &&
                this.field.isEqual(t.field) &&
                this.value.isEqual(t.value))
            )
          }),
          (e.prototype.toString = function() {
            return (
              this.field.canonicalString() +
              ' ' +
              this.op +
              ' ' +
              this.value.value()
            )
          }),
          e
        )
      })(Ii),
      Ni = (function(t) {
        function e(e) {
          var n = t.call(this) || this
          return (n.field = e), n
        }
        return (
          a(e, t),
          (e.prototype.matches = function(t) {
            var e = t.field(this.field)
            return void 0 !== e && null === e.value()
          }),
          (e.prototype.canonicalId = function() {
            return this.field.canonicalString() + ' IS null'
          }),
          (e.prototype.toString = function() {
            return this.field.canonicalString() + ' IS null'
          }),
          (e.prototype.isEqual = function(t) {
            return t instanceof e && this.field.isEqual(t.field)
          }),
          e
        )
      })(Ii),
      Ai = (function(t) {
        function e(e) {
          var n = t.call(this) || this
          return (n.field = e), n
        }
        return (
          a(e, t),
          (e.prototype.matches = function(t) {
            var e = t.field(this.field),
              n = e && e.value()
            return 'number' == typeof n && isNaN(n)
          }),
          (e.prototype.canonicalId = function() {
            return this.field.canonicalString() + ' IS NaN'
          }),
          (e.prototype.toString = function() {
            return this.field.canonicalString() + ' IS NaN'
          }),
          (e.prototype.isEqual = function(t) {
            return t instanceof e && this.field.isEqual(t.field)
          }),
          e
        )
      })(Ii),
      ki = (function() {
        function t(t) {
          this.name = t
        }
        return (
          (t.prototype.toString = function() {
            return this.name
          }),
          (t.ASCENDING = new t('asc')),
          (t.DESCENDING = new t('desc')),
          t
        )
      })(),
      Ri = (function() {
        function t(t, e) {
          ;(this.position = t), (this.before = e)
        }
        return (
          (t.prototype.canonicalId = function() {
            for (
              var t = this.before ? 'b:' : 'a:', e = 0, n = this.position;
              e < n.length;
              e++
            ) {
              t += n[e].toString()
            }
            return t
          }),
          (t.prototype.sortsBeforeDocument = function(t, e) {
            Zn(
              this.position.length <= t.length,
              "Bound has more components than query's orderBy",
            )
            for (var n = 0, r = 0; r < this.position.length; r++) {
              var i = t[r],
                o = this.position[r]
              if (i.field.isKeyField())
                Zn(
                  o instanceof fi,
                  'Bound has a non-key value where the key path is being used.',
                ),
                  (n = jr.comparator(o.key, e.key))
              else {
                var s = e.field(i.field)
                Zn(
                  void 0 !== s,
                  'Field should exist since document matched the orderBy already.',
                ),
                  (n = o.compareTo(s))
              }
              if ((i.dir === ki.DESCENDING && (n *= -1), 0 !== n)) break
            }
            return this.before ? n <= 0 : n < 0
          }),
          (t.prototype.isEqual = function(t) {
            if (null === t) return !1
            if (
              this.before !== t.before ||
              this.position.length !== t.position.length
            )
              return !1
            for (var e = 0; e < this.position.length; e++) {
              var n = this.position[e],
                r = t.position[e]
              return n.isEqual(r)
            }
            return !0
          }),
          t
        )
      })(),
      Mi = (function() {
        function t(t, e) {
          ;(this.field = t),
            void 0 === e && (e = ki.ASCENDING),
            (this.dir = e),
            (this.isKeyOrderBy = t.isKeyField())
        }
        return (
          (t.prototype.compare = function(t, e) {
            var n = this.isKeyOrderBy
              ? zr.compareByKey(t, e)
              : zr.compareByField(this.field, t, e)
            switch (this.dir) {
              case ki.ASCENDING:
                return n
              case ki.DESCENDING:
                return -1 * n
              default:
                return $n('Unknown direction: ' + this.dir)
            }
          }),
          (t.prototype.canonicalId = function() {
            return this.field.canonicalString() + this.dir.toString()
          }),
          (t.prototype.toString = function() {
            return this.field.canonicalString() + ' (' + this.dir + ')'
          }),
          (t.prototype.isEqual = function(t) {
            return this.dir === t.dir && this.field.isEqual(t.field)
          }),
          t
        )
      })(),
      Oi = new Mi(Wr.keyField(), ki.ASCENDING),
      Pi = new Mi(Wr.keyField(), ki.DESCENDING),
      _i = (function() {
        function t(t) {
          this.timestamp = t
        }
        return (
          (t.fromMicroseconds = function(e) {
            var n = Math.floor(e / 1e6)
            return new t(new qr(n, (e % 1e6) * 1e3))
          }),
          (t.fromTimestamp = function(e) {
            return new t(e)
          }),
          (t.forDeletedDoc = function() {
            return t.MIN
          }),
          (t.prototype.compareTo = function(t) {
            return this.timestamp._compareTo(t.timestamp)
          }),
          (t.prototype.isEqual = function(t) {
            return this.timestamp.isEqual(t.timestamp)
          }),
          (t.prototype.toMicroseconds = function() {
            return (
              1e6 * this.timestamp.seconds + this.timestamp.nanoseconds / 1e3
            )
          }),
          (t.prototype.toString = function() {
            return 'SnapshotVersion(' + this.timestamp.toString() + ')'
          }),
          (t.prototype.toTimestamp = function() {
            return this.timestamp
          }),
          (t.MIN = new t(new qr(0, 0))),
          t
        )
      })()
    !(function(t) {
      ;(t[(t.Listen = 0)] = 'Listen'),
        (t[(t.ExistenceFilterMismatch = 1)] = 'ExistenceFilterMismatch'),
        (t[(t.LimboResolution = 2)] = 'LimboResolution')
    })(Si || (Si = {}))
    var Li,
      xi = (function() {
        function t(t, e, n, r, i, o) {
          void 0 === i && (i = _i.MIN),
            void 0 === o && (o = er()),
            (this.query = t),
            (this.targetId = e),
            (this.purpose = n),
            (this.sequenceNumber = r),
            (this.snapshotVersion = i),
            (this.resumeToken = o)
        }
        return (
          (t.prototype.copy = function(e) {
            return new t(
              this.query,
              this.targetId,
              this.purpose,
              void 0 === e.sequenceNumber
                ? this.sequenceNumber
                : e.sequenceNumber,
              void 0 === e.snapshotVersion
                ? this.snapshotVersion
                : e.snapshotVersion,
              void 0 === e.resumeToken ? this.resumeToken : e.resumeToken,
            )
          }),
          (t.prototype.isEqual = function(t) {
            return (
              this.targetId === t.targetId &&
              this.purpose === t.purpose &&
              this.sequenceNumber === t.sequenceNumber &&
              this.snapshotVersion.isEqual(t.snapshotVersion) &&
              this.resumeToken === t.resumeToken &&
              this.query.isEqual(t.query)
            )
          }),
          t
        )
      })(),
      qi = (function() {
        function t(t) {
          this.fields = t
        }
        return (
          (t.prototype.covers = function(t) {
            for (var e = 0, n = this.fields; e < n.length; e++) {
              if (n[e].isPrefixOf(t)) return !0
            }
            return !1
          }),
          (t.prototype.isEqual = function(t) {
            return kr(this.fields, t.fields)
          }),
          t
        )
      })(),
      Fi = (function() {
        function t(t, e) {
          ;(this.field = t), (this.transform = e)
        }
        return (
          (t.prototype.isEqual = function(t) {
            return (
              this.field.isEqual(t.field) && this.transform.isEqual(t.transform)
            )
          }),
          t
        )
      })(),
      Vi = (function() {
        return function(t, e) {
          ;(this.version = t), (this.transformResults = e)
        }
      })()
    !(function(t) {
      ;(t[(t.Set = 0)] = 'Set'),
        (t[(t.Patch = 1)] = 'Patch'),
        (t[(t.Transform = 2)] = 'Transform'),
        (t[(t.Delete = 3)] = 'Delete')
    })(Li || (Li = {}))
    var Bi = (function() {
        function t(t, e) {
          ;(this.updateTime = t),
            (this.exists = e),
            Zn(
              void 0 === t || void 0 === e,
              'Precondition can specify "exists" or "updateTime" but not both',
            )
        }
        return (
          (t.exists = function(e) {
            return new t(void 0, e)
          }),
          (t.updateTime = function(e) {
            return new t(e)
          }),
          Object.defineProperty(t.prototype, 'isNone', {
            get: function() {
              return void 0 === this.updateTime && void 0 === this.exists
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.isValidFor = function(t) {
            return void 0 !== this.updateTime
              ? t instanceof zr && t.version.isEqual(this.updateTime)
              : void 0 !== this.exists
                ? this.exists === t instanceof zr
                : (Zn(this.isNone, 'Precondition should be empty'), !0)
          }),
          (t.prototype.isEqual = function(t) {
            return (
              (e = this.updateTime),
              (n = t.updateTime),
              (null !== e && void 0 !== e ? !(!n || !e.isEqual(n)) : e === n) &&
                this.exists === t.exists
            )
            var e, n
          }),
          (t.NONE = new t()),
          t
        )
      })(),
      Ui = (function() {
        function t() {}
        return (
          (t.prototype.verifyKeyMatches = function(t) {
            null != t &&
              Zn(
                t.key.isEqual(this.key),
                'Can only apply a mutation to a document with the same key',
              )
          }),
          (t.getPostMutationVersion = function(t) {
            return t instanceof zr ? t.version : _i.MIN
          }),
          t
        )
      })(),
      Qi = (function(t) {
        function e(e, n, r) {
          var i = t.call(this) || this
          return (
            (i.key = e),
            (i.value = n),
            (i.precondition = r),
            (i.type = Li.Set),
            i
          )
        }
        return (
          a(e, t),
          (e.prototype.applyToRemoteDocument = function(t, e) {
            this.verifyKeyMatches(t),
              Zn(
                null == e.transformResults,
                'Transform results received by SetMutation.',
              )
            var n = e.version
            return new zr(this.key, n, this.value, {
              hasCommittedMutations: !0,
            })
          }),
          (e.prototype.applyToLocalView = function(t, e, n) {
            if ((this.verifyKeyMatches(t), !this.precondition.isValidFor(t)))
              return t
            var r = Ui.getPostMutationVersion(t)
            return new zr(this.key, r, this.value, { hasLocalMutations: !0 })
          }),
          (e.prototype.isEqual = function(t) {
            return (
              t instanceof e &&
              this.key.isEqual(t.key) &&
              this.value.isEqual(t.value) &&
              this.precondition.isEqual(t.precondition)
            )
          }),
          e
        )
      })(Ui),
      Ki = (function(t) {
        function e(e, n, r, i) {
          var o = t.call(this) || this
          return (
            (o.key = e),
            (o.data = n),
            (o.fieldMask = r),
            (o.precondition = i),
            (o.type = Li.Patch),
            o
          )
        }
        return (
          a(e, t),
          (e.prototype.applyToRemoteDocument = function(t, e) {
            if (
              (this.verifyKeyMatches(t),
              Zn(
                null == e.transformResults,
                'Transform results received by PatchMutation.',
              ),
              !this.precondition.isValidFor(t))
            )
              return new Xr(this.key, e.version)
            var n = this.patchDocument(t)
            return new zr(this.key, e.version, n, { hasCommittedMutations: !0 })
          }),
          (e.prototype.applyToLocalView = function(t, e, n) {
            if ((this.verifyKeyMatches(t), !this.precondition.isValidFor(t)))
              return t
            var r = Ui.getPostMutationVersion(t),
              i = this.patchDocument(t)
            return new zr(this.key, r, i, { hasLocalMutations: !0 })
          }),
          (e.prototype.isEqual = function(t) {
            return (
              t instanceof e &&
              this.key.isEqual(t.key) &&
              this.fieldMask.isEqual(t.fieldMask) &&
              this.precondition.isEqual(t.precondition)
            )
          }),
          (e.prototype.patchDocument = function(t) {
            var e
            return (
              (e = t instanceof zr ? t.data : pi.EMPTY), this.patchObject(e)
            )
          }),
          (e.prototype.patchObject = function(t) {
            for (var e = 0, n = this.fieldMask.fields; e < n.length; e++) {
              var r = n[e]
              if (!r.isEmpty()) {
                var i = this.data.field(r)
                t = void 0 !== i ? t.set(r, i) : t.delete(r)
              }
            }
            return t
          }),
          e
        )
      })(Ui),
      Wi = (function(t) {
        function e(e, n) {
          var r = t.call(this) || this
          return (
            (r.key = e),
            (r.fieldTransforms = n),
            (r.type = Li.Transform),
            (r.precondition = Bi.exists(!0)),
            r
          )
        }
        return (
          a(e, t),
          (e.prototype.applyToRemoteDocument = function(t, e) {
            if (
              (this.verifyKeyMatches(t),
              Zn(
                null != e.transformResults,
                'Transform results missing for TransformMutation.',
              ),
              !this.precondition.isValidFor(t))
            )
              return new Xr(this.key, e.version)
            var n = this.requireDocument(t),
              r = this.serverTransformResults(t, e.transformResults),
              i = e.version,
              o = this.transformObject(n.data, r)
            return new zr(this.key, i, o, { hasCommittedMutations: !0 })
          }),
          (e.prototype.applyToLocalView = function(t, e, n) {
            if ((this.verifyKeyMatches(t), !this.precondition.isValidFor(t)))
              return t
            var r = this.requireDocument(t),
              i = this.localTransformResults(n, e),
              o = this.transformObject(r.data, i)
            return new zr(this.key, r.version, o, { hasLocalMutations: !0 })
          }),
          (e.prototype.isEqual = function(t) {
            return (
              t instanceof e &&
              this.key.isEqual(t.key) &&
              kr(this.fieldTransforms, t.fieldTransforms) &&
              this.precondition.isEqual(t.precondition)
            )
          }),
          (e.prototype.requireDocument = function(t) {
            Zn(t instanceof zr, 'Unknown MaybeDocument type ' + t)
            var e = t
            return (
              Zn(
                e.key.isEqual(this.key),
                'Can only transform a document with the same key',
              ),
              e
            )
          }),
          (e.prototype.serverTransformResults = function(t, e) {
            var n = []
            Zn(
              this.fieldTransforms.length === e.length,
              'server transform result count (' +
                e.length +
                ') should match field transform count (' +
                this.fieldTransforms.length +
                ')',
            )
            for (var r = 0; r < e.length; r++) {
              var i = this.fieldTransforms[r],
                o = i.transform,
                s = null
              t instanceof zr && (s = t.field(i.field) || null),
                n.push(o.applyToRemoteDocument(s, e[r]))
            }
            return n
          }),
          (e.prototype.localTransformResults = function(t, e) {
            for (
              var n = [], r = 0, i = this.fieldTransforms;
              r < i.length;
              r++
            ) {
              var o = i[r],
                s = o.transform,
                a = null
              e instanceof zr && (a = e.field(o.field) || null),
                n.push(s.applyToLocalView(a, t))
            }
            return n
          }),
          (e.prototype.transformObject = function(t, e) {
            Zn(
              e.length === this.fieldTransforms.length,
              'TransformResults length mismatch.',
            )
            for (var n = 0; n < this.fieldTransforms.length; n++) {
              var r = this.fieldTransforms[n].field
              t = t.set(r, e[n])
            }
            return t
          }),
          e
        )
      })(Ui),
      ji = (function(t) {
        function e(e, n) {
          var r = t.call(this) || this
          return (r.key = e), (r.precondition = n), (r.type = Li.Delete), r
        }
        return (
          a(e, t),
          (e.prototype.applyToRemoteDocument = function(t, e) {
            return (
              this.verifyKeyMatches(t),
              Zn(
                null == e.transformResults,
                'Transform results received by DeleteMutation.',
              ),
              new Hr(this.key, e.version, { hasCommittedMutations: !0 })
            )
          }),
          (e.prototype.applyToLocalView = function(t, e, n) {
            return (
              this.verifyKeyMatches(t),
              this.precondition.isValidFor(t)
                ? (t &&
                    Zn(
                      t.key.isEqual(this.key),
                      'Can only apply mutation to document with same key',
                    ),
                  new Hr(this.key, _i.forDeletedDoc()))
                : t
            )
          }),
          (e.prototype.isEqual = function(t) {
            return (
              t instanceof e &&
              this.key.isEqual(t.key) &&
              this.precondition.isEqual(t.precondition)
            )
          }),
          e
        )
      })(Ui),
      Gi = (function() {
        function t() {}
        return (
          (t.prototype.applyToLocalView = function(t, e) {
            return new hi(e, t)
          }),
          (t.prototype.applyToRemoteDocument = function(t, e) {
            return e
          }),
          (t.prototype.isEqual = function(e) {
            return e instanceof t
          }),
          (t.instance = new t()),
          t
        )
      })(),
      zi = (function() {
        function t(t) {
          this.elements = t
        }
        return (
          (t.prototype.applyToLocalView = function(t, e) {
            return this.apply(t)
          }),
          (t.prototype.applyToRemoteDocument = function(t, e) {
            return this.apply(t)
          }),
          (t.prototype.apply = function(t) {
            for (
              var e = Xi(t),
                n = function(t) {
                  e.find(function(e) {
                    return e.isEqual(t)
                  }) || e.push(t)
                },
                r = 0,
                i = this.elements;
              r < i.length;
              r++
            ) {
              n(i[r])
            }
            return new mi(e)
          }),
          (t.prototype.isEqual = function(e) {
            return e instanceof t && kr(e.elements, this.elements)
          }),
          t
        )
      })(),
      Hi = (function() {
        function t(t) {
          this.elements = t
        }
        return (
          (t.prototype.applyToLocalView = function(t, e) {
            return this.apply(t)
          }),
          (t.prototype.applyToRemoteDocument = function(t, e) {
            return this.apply(t)
          }),
          (t.prototype.apply = function(t) {
            for (
              var e = Xi(t),
                n = function(t) {
                  e = e.filter(function(e) {
                    return !e.isEqual(t)
                  })
                },
                r = 0,
                i = this.elements;
              r < i.length;
              r++
            ) {
              n(i[r])
            }
            return new mi(e)
          }),
          (t.prototype.isEqual = function(e) {
            return e instanceof t && kr(e.elements, this.elements)
          }),
          t
        )
      })()
    function Xi(t) {
      return t instanceof mi ? t.internalValue.slice() : []
    }
    var Yi,
      Ji = (function() {
        function t(t) {
          this.count = t
        }
        return (
          (t.prototype.isEqual = function(t) {
            return t && t.count === this.count
          }),
          t
        )
      })()
    function $i(t) {
      switch (t) {
        case nr.OK:
          return $n('Treated status OK as error')
        case nr.CANCELLED:
        case nr.UNKNOWN:
        case nr.DEADLINE_EXCEEDED:
        case nr.RESOURCE_EXHAUSTED:
        case nr.INTERNAL:
        case nr.UNAVAILABLE:
        case nr.UNAUTHENTICATED:
          return !1
        case nr.INVALID_ARGUMENT:
        case nr.NOT_FOUND:
        case nr.ALREADY_EXISTS:
        case nr.PERMISSION_DENIED:
        case nr.FAILED_PRECONDITION:
        case nr.ABORTED:
        case nr.OUT_OF_RANGE:
        case nr.UNIMPLEMENTED:
        case nr.DATA_LOSS:
          return !0
        default:
          return $n('Unknown status code: ' + t)
      }
    }
    function Zi(t) {
      if (void 0 === t) return Yn('GRPC error has no .code'), nr.UNKNOWN
      switch (t) {
        case Yi.OK:
          return nr.OK
        case Yi.CANCELLED:
          return nr.CANCELLED
        case Yi.UNKNOWN:
          return nr.UNKNOWN
        case Yi.DEADLINE_EXCEEDED:
          return nr.DEADLINE_EXCEEDED
        case Yi.RESOURCE_EXHAUSTED:
          return nr.RESOURCE_EXHAUSTED
        case Yi.INTERNAL:
          return nr.INTERNAL
        case Yi.UNAVAILABLE:
          return nr.UNAVAILABLE
        case Yi.UNAUTHENTICATED:
          return nr.UNAUTHENTICATED
        case Yi.INVALID_ARGUMENT:
          return nr.INVALID_ARGUMENT
        case Yi.NOT_FOUND:
          return nr.NOT_FOUND
        case Yi.ALREADY_EXISTS:
          return nr.ALREADY_EXISTS
        case Yi.PERMISSION_DENIED:
          return nr.PERMISSION_DENIED
        case Yi.FAILED_PRECONDITION:
          return nr.FAILED_PRECONDITION
        case Yi.ABORTED:
          return nr.ABORTED
        case Yi.OUT_OF_RANGE:
          return nr.OUT_OF_RANGE
        case Yi.UNIMPLEMENTED:
          return nr.UNIMPLEMENTED
        case Yi.DATA_LOSS:
          return nr.DATA_LOSS
        default:
          return $n('Unknown status code: ' + t)
      }
    }
    !(function(t) {
      ;(t[(t.OK = 0)] = 'OK'),
        (t[(t.CANCELLED = 1)] = 'CANCELLED'),
        (t[(t.UNKNOWN = 2)] = 'UNKNOWN'),
        (t[(t.INVALID_ARGUMENT = 3)] = 'INVALID_ARGUMENT'),
        (t[(t.DEADLINE_EXCEEDED = 4)] = 'DEADLINE_EXCEEDED'),
        (t[(t.NOT_FOUND = 5)] = 'NOT_FOUND'),
        (t[(t.ALREADY_EXISTS = 6)] = 'ALREADY_EXISTS'),
        (t[(t.PERMISSION_DENIED = 7)] = 'PERMISSION_DENIED'),
        (t[(t.UNAUTHENTICATED = 16)] = 'UNAUTHENTICATED'),
        (t[(t.RESOURCE_EXHAUSTED = 8)] = 'RESOURCE_EXHAUSTED'),
        (t[(t.FAILED_PRECONDITION = 9)] = 'FAILED_PRECONDITION'),
        (t[(t.ABORTED = 10)] = 'ABORTED'),
        (t[(t.OUT_OF_RANGE = 11)] = 'OUT_OF_RANGE'),
        (t[(t.UNIMPLEMENTED = 12)] = 'UNIMPLEMENTED'),
        (t[(t.INTERNAL = 13)] = 'INTERNAL'),
        (t[(t.UNAVAILABLE = 14)] = 'UNAVAILABLE'),
        (t[(t.DATA_LOSS = 15)] = 'DATA_LOSS')
    })(Yi || (Yi = {}))
    var to = (function() {
        function t(t) {
          ;(this.comparator = t), (this.data = new Yr(this.comparator))
        }
        return (
          (t.fromMapKeys = function(e) {
            var n = new t(e.comparator)
            return (
              e.forEach(function(t) {
                n = n.add(t)
              }),
              n
            )
          }),
          (t.prototype.has = function(t) {
            return null !== this.data.get(t)
          }),
          (t.prototype.first = function() {
            return this.data.minKey()
          }),
          (t.prototype.last = function() {
            return this.data.maxKey()
          }),
          Object.defineProperty(t.prototype, 'size', {
            get: function() {
              return this.data.size
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.indexOf = function(t) {
            return this.data.indexOf(t)
          }),
          (t.prototype.forEach = function(t) {
            this.data.inorderTraversal(function(e, n) {
              return t(e), !1
            })
          }),
          (t.prototype.forEachInRange = function(t, e) {
            for (var n = this.data.getIteratorFrom(t[0]); n.hasNext(); ) {
              var r = n.getNext()
              if (this.comparator(r.key, t[1]) >= 0) return
              e(r.key)
            }
          }),
          (t.prototype.forEachWhile = function(t, e) {
            var n
            for (
              n =
                void 0 !== e
                  ? this.data.getIteratorFrom(e)
                  : this.data.getIterator();
              n.hasNext();

            ) {
              if (!t(n.getNext().key)) return
            }
          }),
          (t.prototype.firstAfterOrEqual = function(t) {
            var e = this.data.getIteratorFrom(t)
            return e.hasNext() ? e.getNext().key : null
          }),
          (t.prototype.add = function(t) {
            return this.copy(this.data.remove(t).insert(t, !0))
          }),
          (t.prototype.delete = function(t) {
            return this.has(t) ? this.copy(this.data.remove(t)) : this
          }),
          (t.prototype.isEmpty = function() {
            return this.data.isEmpty()
          }),
          (t.prototype.unionWith = function(t) {
            var e = this
            return (
              t.forEach(function(t) {
                e = e.add(t)
              }),
              e
            )
          }),
          (t.prototype.isEqual = function(e) {
            if (!(e instanceof t)) return !1
            if (this.size !== e.size) return !1
            for (
              var n = this.data.getIterator(), r = e.data.getIterator();
              n.hasNext();

            ) {
              var i = n.getNext().key,
                o = r.getNext().key
              if (0 !== this.comparator(i, o)) return !1
            }
            return !0
          }),
          (t.prototype.toArray = function() {
            var t = []
            return (
              this.forEach(function(e) {
                t.push(e)
              }),
              t
            )
          }),
          (t.prototype.toString = function() {
            var t = []
            return (
              this.forEach(function(e) {
                return t.push(e)
              }),
              'SortedSet(' + t.toString() + ')'
            )
          }),
          (t.prototype[Symbol.iterator] = function() {
            var t = this.data.getIterator()
            return {
              next: function() {
                return t.hasNext()
                  ? { done: !1, value: t.getNext().key }
                  : { done: !0, value: {} }
              },
            }
          }),
          (t.prototype.copy = function(e) {
            var n = new t(this.comparator)
            return (n.data = e), n
          }),
          t
        )
      })(),
      eo = new Yr(jr.comparator)
    function no() {
      return eo
    }
    var ro = new Yr(jr.comparator)
    function io() {
      return ro
    }
    var oo = new Yr(jr.comparator)
    function so() {
      return oo
    }
    var ao = new to(jr.comparator)
    function uo() {
      return ao
    }
    var co = new to(Ar)
    function ho() {
      return co
    }
    var lo,
      fo,
      po = (function() {
        function t(t) {
          ;(this.comparator = t
            ? function(e, n) {
                return t(e, n) || jr.comparator(e.key, n.key)
              }
            : function(t, e) {
                return jr.comparator(t.key, e.key)
              }),
            (this.keyedMap = io()),
            (this.sortedSet = new Yr(this.comparator))
        }
        return (
          (t.emptySet = function(e) {
            return new t(e.comparator)
          }),
          (t.prototype.has = function(t) {
            return null != this.keyedMap.get(t)
          }),
          (t.prototype.get = function(t) {
            return this.keyedMap.get(t)
          }),
          (t.prototype.first = function() {
            return this.sortedSet.minKey()
          }),
          (t.prototype.last = function() {
            return this.sortedSet.maxKey()
          }),
          (t.prototype.isEmpty = function() {
            return this.sortedSet.isEmpty()
          }),
          (t.prototype.indexOf = function(t) {
            var e = this.keyedMap.get(t)
            return e ? this.sortedSet.indexOf(e) : -1
          }),
          Object.defineProperty(t.prototype, 'size', {
            get: function() {
              return this.sortedSet.size
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.forEach = function(t) {
            this.sortedSet.inorderTraversal(function(e, n) {
              return t(e), !1
            })
          }),
          (t.prototype.add = function(t) {
            var e = this.delete(t.key)
            return e.copy(
              e.keyedMap.insert(t.key, t),
              e.sortedSet.insert(t, null),
            )
          }),
          (t.prototype.delete = function(t) {
            var e = this.get(t)
            return e
              ? this.copy(this.keyedMap.remove(t), this.sortedSet.remove(e))
              : this
          }),
          (t.prototype.isEqual = function(e) {
            if (!(e instanceof t)) return !1
            if (this.size !== e.size) return !1
            for (
              var n = this.sortedSet.getIterator(),
                r = e.sortedSet.getIterator();
              n.hasNext();

            ) {
              var i = n.getNext().key,
                o = r.getNext().key
              if (!i.isEqual(o)) return !1
            }
            return !0
          }),
          (t.prototype.toString = function() {
            var t = []
            return (
              this.forEach(function(e) {
                t.push(e.toString())
              }),
              0 === t.length
                ? 'DocumentSet ()'
                : 'DocumentSet (\n  ' + t.join('  \n') + '\n)'
            )
          }),
          (t.prototype.copy = function(e, n) {
            var r = new t()
            return (
              (r.comparator = this.comparator),
              (r.keyedMap = e),
              (r.sortedSet = n),
              r
            )
          }),
          t
        )
      })()
    !(function(t) {
      ;(t[(t.Added = 0)] = 'Added'),
        (t[(t.Removed = 1)] = 'Removed'),
        (t[(t.Modified = 2)] = 'Modified'),
        (t[(t.Metadata = 3)] = 'Metadata')
    })(lo || (lo = {})),
      (function(t) {
        ;(t[(t.Local = 0)] = 'Local'), (t[(t.Synced = 1)] = 'Synced')
      })(fo || (fo = {}))
    var mo,
      yo = (function() {
        function t() {
          this.changeMap = new Yr(jr.comparator)
        }
        return (
          (t.prototype.track = function(t) {
            var e = t.doc.key,
              n = this.changeMap.get(e)
            n
              ? t.type !== lo.Added && n.type === lo.Metadata
                ? (this.changeMap = this.changeMap.insert(e, t))
                : t.type === lo.Metadata && n.type !== lo.Removed
                  ? (this.changeMap = this.changeMap.insert(e, {
                      type: n.type,
                      doc: t.doc,
                    }))
                  : t.type === lo.Modified && n.type === lo.Modified
                    ? (this.changeMap = this.changeMap.insert(e, {
                        type: lo.Modified,
                        doc: t.doc,
                      }))
                    : t.type === lo.Modified && n.type === lo.Added
                      ? (this.changeMap = this.changeMap.insert(e, {
                          type: lo.Added,
                          doc: t.doc,
                        }))
                      : t.type === lo.Removed && n.type === lo.Added
                        ? (this.changeMap = this.changeMap.remove(e))
                        : t.type === lo.Removed && n.type === lo.Modified
                          ? (this.changeMap = this.changeMap.insert(e, {
                              type: lo.Removed,
                              doc: n.doc,
                            }))
                          : t.type === lo.Added && n.type === lo.Removed
                            ? (this.changeMap = this.changeMap.insert(e, {
                                type: lo.Modified,
                                doc: t.doc,
                              }))
                            : $n(
                                'unsupported combination of changes: ' +
                                  JSON.stringify(t) +
                                  ' after ' +
                                  JSON.stringify(n),
                              )
              : (this.changeMap = this.changeMap.insert(e, t))
          }),
          (t.prototype.getChanges = function() {
            var t = []
            return (
              this.changeMap.inorderTraversal(function(e, n) {
                t.push(n)
              }),
              t
            )
          }),
          t
        )
      })(),
      go = (function() {
        function t(t, e, n, r, i, o, s, a) {
          ;(this.query = t),
            (this.docs = e),
            (this.oldDocs = n),
            (this.docChanges = r),
            (this.mutatedKeys = i),
            (this.fromCache = o),
            (this.syncStateChanged = s),
            (this.excludesMetadataChanges = a)
        }
        return (
          (t.fromInitialDocuments = function(e, n, r, i) {
            var o = []
            return (
              n.forEach(function(t) {
                o.push({ type: lo.Added, doc: t })
              }),
              new t(e, n, po.emptySet(n), o, r, i, !0, !1)
            )
          }),
          Object.defineProperty(t.prototype, 'hasPendingWrites', {
            get: function() {
              return !this.mutatedKeys.isEmpty()
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.isEqual = function(t) {
            if (
              !(
                this.fromCache === t.fromCache &&
                this.syncStateChanged === t.syncStateChanged &&
                this.mutatedKeys.isEqual(t.mutatedKeys) &&
                this.query.isEqual(t.query) &&
                this.docs.isEqual(t.docs) &&
                this.oldDocs.isEqual(t.oldDocs)
              )
            )
              return !1
            var e = this.docChanges,
              n = t.docChanges
            if (e.length !== n.length) return !1
            for (var r = 0; r < e.length; r++)
              if (e[r].type !== n[r].type || !e[r].doc.isEqual(n[r].doc))
                return !1
            return !0
          }),
          t
        )
      })(),
      vo = (function() {
        function t(t, e, n, r, i) {
          ;(this.snapshotVersion = t),
            (this.targetChanges = e),
            (this.targetMismatches = n),
            (this.documentUpdates = r),
            (this.resolvedLimboDocuments = i)
        }
        return (
          (t.createSynthesizedRemoteEventForCurrentChange = function(e, n) {
            var r,
              i = (((r = {})[
                e
              ] = bo.createSynthesizedTargetChangeForCurrentChange(e, n)),
              r)
            return new t(_i.MIN, i, ho(), no(), uo())
          }),
          t
        )
      })(),
      bo = (function() {
        function t(t, e, n, r, i) {
          ;(this.resumeToken = t),
            (this.current = e),
            (this.addedDocuments = n),
            (this.modifiedDocuments = r),
            (this.removedDocuments = i)
        }
        return (
          (t.createSynthesizedTargetChangeForCurrentChange = function(e, n) {
            return new t(er(), n, uo(), uo(), uo())
          }),
          t
        )
      })(),
      wo = (function() {
        return function(t, e, n, r) {
          ;(this.updatedTargetIds = t),
            (this.removedTargetIds = e),
            (this.key = n),
            (this.newDoc = r)
        }
      })(),
      To = (function() {
        return function(t, e) {
          ;(this.targetId = t), (this.existenceFilter = e)
        }
      })()
    !(function(t) {
      ;(t[(t.NoChange = 0)] = 'NoChange'),
        (t[(t.Added = 1)] = 'Added'),
        (t[(t.Removed = 2)] = 'Removed'),
        (t[(t.Current = 3)] = 'Current'),
        (t[(t.Reset = 4)] = 'Reset')
    })(mo || (mo = {}))
    var So = (function() {
        return function(t, e, n, r) {
          void 0 === n && (n = er()),
            void 0 === r && (r = null),
            (this.state = t),
            (this.targetIds = e),
            (this.resumeToken = n),
            (this.cause = r)
        }
      })(),
      Eo = (function() {
        function t() {
          ;(this.pendingResponses = 0),
            (this.documentChanges = Do()),
            (this._resumeToken = er()),
            (this._current = !1),
            (this._hasPendingChanges = !0)
        }
        return (
          Object.defineProperty(t.prototype, 'current', {
            get: function() {
              return this._current
            },
            enumerable: !0,
            configurable: !0,
          }),
          Object.defineProperty(t.prototype, 'resumeToken', {
            get: function() {
              return this._resumeToken
            },
            enumerable: !0,
            configurable: !0,
          }),
          Object.defineProperty(t.prototype, 'isPending', {
            get: function() {
              return 0 !== this.pendingResponses
            },
            enumerable: !0,
            configurable: !0,
          }),
          Object.defineProperty(t.prototype, 'hasPendingChanges', {
            get: function() {
              return this._hasPendingChanges
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.updateResumeToken = function(t) {
            t.length > 0 &&
              ((this._hasPendingChanges = !0), (this._resumeToken = t))
          }),
          (t.prototype.toTargetChange = function() {
            var t = uo(),
              e = uo(),
              n = uo()
            return (
              this.documentChanges.forEach(function(r, i) {
                switch (i) {
                  case lo.Added:
                    t = t.add(r)
                    break
                  case lo.Modified:
                    e = e.add(r)
                    break
                  case lo.Removed:
                    n = n.add(r)
                    break
                  default:
                    $n('Encountered invalid change type: ' + i)
                }
              }),
              new bo(this._resumeToken, this._current, t, e, n)
            )
          }),
          (t.prototype.clearPendingChanges = function() {
            ;(this._hasPendingChanges = !1), (this.documentChanges = Do())
          }),
          (t.prototype.addDocumentChange = function(t, e) {
            ;(this._hasPendingChanges = !0),
              (this.documentChanges = this.documentChanges.insert(t, e))
          }),
          (t.prototype.removeDocumentChange = function(t) {
            ;(this._hasPendingChanges = !0),
              (this.documentChanges = this.documentChanges.remove(t))
          }),
          (t.prototype.recordPendingTargetRequest = function() {
            this.pendingResponses += 1
          }),
          (t.prototype.recordTargetResponse = function() {
            this.pendingResponses -= 1
          }),
          (t.prototype.markCurrent = function() {
            ;(this._hasPendingChanges = !0), (this._current = !0)
          }),
          t
        )
      })(),
      Io = (function() {
        function t(t) {
          ;(this.metadataProvider = t),
            (this.targetStates = {}),
            (this.pendingDocumentUpdates = no()),
            (this.pendingDocumentTargetMapping = Co()),
            (this.pendingTargetResets = new to(Ar))
        }
        return (
          (t.prototype.handleDocumentChange = function(t) {
            for (var e = 0, n = t.updatedTargetIds; e < n.length; e++) {
              var r = n[e]
              t.newDoc instanceof zr
                ? this.addDocumentToTarget(r, t.newDoc)
                : t.newDoc instanceof Hr &&
                  this.removeDocumentFromTarget(r, t.key, t.newDoc)
            }
            for (var i = 0, o = t.removedTargetIds; i < o.length; i++) {
              r = o[i]
              this.removeDocumentFromTarget(r, t.key, t.newDoc)
            }
          }),
          (t.prototype.handleTargetChange = function(t) {
            var e = this
            this.forEachTarget(t, function(n) {
              var r = e.ensureTargetState(n)
              switch (t.state) {
                case mo.NoChange:
                  e.isActiveTarget(n) && r.updateResumeToken(t.resumeToken)
                  break
                case mo.Added:
                  r.recordTargetResponse(),
                    r.isPending || r.clearPendingChanges(),
                    r.updateResumeToken(t.resumeToken)
                  break
                case mo.Removed:
                  r.recordTargetResponse(),
                    r.isPending || e.removeTarget(n),
                    Zn(
                      !t.cause,
                      'WatchChangeAggregator does not handle errored targets',
                    )
                  break
                case mo.Current:
                  e.isActiveTarget(n) &&
                    (r.markCurrent(), r.updateResumeToken(t.resumeToken))
                  break
                case mo.Reset:
                  e.isActiveTarget(n) &&
                    (e.resetTarget(n), r.updateResumeToken(t.resumeToken))
                  break
                default:
                  $n('Unknown target watch change state: ' + t.state)
              }
            })
          }),
          (t.prototype.forEachTarget = function(t, e) {
            t.targetIds.length > 0
              ? t.targetIds.forEach(e)
              : ar(this.targetStates, e)
          }),
          (t.prototype.handleExistenceFilter = function(t) {
            var e = t.targetId,
              n = t.existenceFilter.count,
              r = this.queryDataForActiveTarget(e)
            if (r) {
              var i = r.query
              if (i.isDocumentQuery())
                if (0 === n) {
                  var o = new jr(i.path)
                  this.removeDocumentFromTarget(
                    e,
                    o,
                    new Hr(o, _i.forDeletedDoc()),
                  )
                } else
                  Zn(
                    1 === n,
                    'Single document existence filter with count: ' + n,
                  )
              else
                this.getCurrentDocumentCountForTarget(e) !== n &&
                  (this.resetTarget(e),
                  (this.pendingTargetResets = this.pendingTargetResets.add(e)))
            }
          }),
          (t.prototype.createRemoteEvent = function(t) {
            var e = this,
              n = {}
            ar(this.targetStates, function(r, i) {
              var o = e.queryDataForActiveTarget(r)
              if (o) {
                if (i.current && o.query.isDocumentQuery()) {
                  var s = new jr(o.query.path)
                  null !== e.pendingDocumentUpdates.get(s) ||
                    e.targetContainsDocument(r, s) ||
                    e.removeDocumentFromTarget(r, s, new Hr(s, t))
                }
                i.hasPendingChanges &&
                  ((n[r] = i.toTargetChange()), i.clearPendingChanges())
              }
            })
            var r = uo()
            this.pendingDocumentTargetMapping.forEach(function(t, n) {
              var i = !0
              n.forEachWhile(function(t) {
                var n = e.queryDataForActiveTarget(t)
                return !n || n.purpose === Si.LimboResolution || ((i = !1), !1)
              }),
                i && (r = r.add(t))
            })
            var i = new vo(
              t,
              n,
              this.pendingTargetResets,
              this.pendingDocumentUpdates,
              r,
            )
            return (
              (this.pendingDocumentUpdates = no()),
              (this.pendingDocumentTargetMapping = Co()),
              (this.pendingTargetResets = new to(Ar)),
              i
            )
          }),
          (t.prototype.addDocumentToTarget = function(t, e) {
            if (this.isActiveTarget(t)) {
              var n = this.targetContainsDocument(t, e.key)
                ? lo.Modified
                : lo.Added
              this.ensureTargetState(t).addDocumentChange(e.key, n),
                (this.pendingDocumentUpdates = this.pendingDocumentUpdates.insert(
                  e.key,
                  e,
                )),
                (this.pendingDocumentTargetMapping = this.pendingDocumentTargetMapping.insert(
                  e.key,
                  this.ensureDocumentTargetMapping(e.key).add(t),
                ))
            }
          }),
          (t.prototype.removeDocumentFromTarget = function(t, e, n) {
            if (this.isActiveTarget(t)) {
              var r = this.ensureTargetState(t)
              this.targetContainsDocument(t, e)
                ? r.addDocumentChange(e, lo.Removed)
                : r.removeDocumentChange(e),
                (this.pendingDocumentTargetMapping = this.pendingDocumentTargetMapping.insert(
                  e,
                  this.ensureDocumentTargetMapping(e).delete(t),
                )),
                n &&
                  (this.pendingDocumentUpdates = this.pendingDocumentUpdates.insert(
                    e,
                    n,
                  ))
            }
          }),
          (t.prototype.removeTarget = function(t) {
            delete this.targetStates[t]
          }),
          (t.prototype.getCurrentDocumentCountForTarget = function(t) {
            var e = this.ensureTargetState(t).toTargetChange()
            return (
              this.metadataProvider.getRemoteKeysForTarget(t).size +
              e.addedDocuments.size -
              e.removedDocuments.size
            )
          }),
          (t.prototype.recordPendingTargetRequest = function(t) {
            this.ensureTargetState(t).recordPendingTargetRequest()
          }),
          (t.prototype.ensureTargetState = function(t) {
            return (
              this.targetStates[t] || (this.targetStates[t] = new Eo()),
              this.targetStates[t]
            )
          }),
          (t.prototype.ensureDocumentTargetMapping = function(t) {
            var e = this.pendingDocumentTargetMapping.get(t)
            return (
              e ||
                ((e = new to(Ar)),
                (this.pendingDocumentTargetMapping = this.pendingDocumentTargetMapping.insert(
                  t,
                  e,
                ))),
              e
            )
          }),
          (t.prototype.isActiveTarget = function(t) {
            return null !== this.queryDataForActiveTarget(t)
          }),
          (t.prototype.queryDataForActiveTarget = function(t) {
            var e = this.targetStates[t]
            return e && e.isPending
              ? null
              : this.metadataProvider.getQueryDataForTarget(t)
          }),
          (t.prototype.resetTarget = function(t) {
            var e = this
            Zn(
              !this.targetStates[t].isPending,
              'Should only reset active targets',
            ),
              (this.targetStates[t] = new Eo()),
              this.metadataProvider
                .getRemoteKeysForTarget(t)
                .forEach(function(n) {
                  e.removeDocumentFromTarget(t, n, null)
                })
          }),
          (t.prototype.targetContainsDocument = function(t, e) {
            return this.metadataProvider.getRemoteKeysForTarget(t).has(e)
          }),
          t
        )
      })()
    function Co() {
      return new Yr(jr.comparator)
    }
    function Do() {
      return new Yr(jr.comparator)
    }
    var No,
      Ao,
      ko = (((No = {})[ki.ASCENDING.name] = 'ASCENDING'),
      (No[ki.DESCENDING.name] = 'DESCENDING'),
      No),
      Ro = (((Ao = {})[Ci.LESS_THAN.name] = 'LESS_THAN'),
      (Ao[Ci.LESS_THAN_OR_EQUAL.name] = 'LESS_THAN_OR_EQUAL'),
      (Ao[Ci.GREATER_THAN.name] = 'GREATER_THAN'),
      (Ao[Ci.GREATER_THAN_OR_EQUAL.name] = 'GREATER_THAN_OR_EQUAL'),
      (Ao[Ci.EQUAL.name] = 'EQUAL'),
      (Ao[Ci.ARRAY_CONTAINS.name] = 'ARRAY_CONTAINS'),
      Ao),
      Mo = new RegExp(/^\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d(?:\.(\d+))?Z$/)
    function Oo(t, e) {
      Zn(!wi(t), e + ' is missing')
    }
    function Po(t) {
      return 'number' == typeof t
        ? t
        : 'string' == typeof t
          ? Number(t)
          : $n("can't parse " + t)
    }
    var _o = (function() {
      function t(t, e) {
        ;(this.databaseId = t), (this.options = e)
      }
      return (
        (t.prototype.emptyByteString = function() {
          return this.options.useProto3Json ? '' : new Uint8Array(0)
        }),
        (t.prototype.unsafeCastProtoByteString = function(t) {
          return t
        }),
        (t.prototype.fromRpcStatus = function(t) {
          var e = void 0 === t.code ? nr.UNKNOWN : Zi(t.code)
          return new rr(e, t.message || '')
        }),
        (t.prototype.toInt32Value = function(t) {
          return wi(t) ? void 0 : { value: t }
        }),
        (t.prototype.fromInt32Value = function(t) {
          var e
          return wi((e = 'object' == typeof t ? t.value : t)) ? null : e
        }),
        (t.prototype.toTimestamp = function(t) {
          return { seconds: t.seconds, nanos: t.nanoseconds }
        }),
        (t.prototype.fromTimestamp = function(t) {
          if ('string' == typeof t) return this.fromIso8601String(t)
          Zn(!!t, 'Cannot deserialize null or undefined timestamp.')
          var e = Po(t.seconds || '0'),
            n = t.nanos || 0
          return new qr(e, n)
        }),
        (t.prototype.fromIso8601String = function(t) {
          var e = 0,
            n = Mo.exec(t)
          if ((Zn(!!n, 'invalid timestamp: ' + t), n[1])) {
            var r = n[1]
            ;(r = (r + '000000000').substr(0, 9)), (e = Number(r))
          }
          var i = new Date(t),
            o = Math.floor(i.getTime() / 1e3)
          return new qr(o, e)
        }),
        (t.prototype.toBytes = function(t) {
          return this.options.useProto3Json
            ? t.toBase64()
            : this.unsafeCastProtoByteString(t.toUint8Array())
        }),
        (t.prototype.fromBlob = function(t) {
          return 'string' == typeof t
            ? (Zn(
                this.options.useProto3Json,
                'Expected bytes to be passed in as Uint8Array, but got a string instead.',
              ),
              _r.fromBase64String(t))
            : (Zn(
                !this.options.useProto3Json,
                'Expected bytes to be passed in as string, but got something else instead.',
              ),
              _r.fromUint8Array(t))
        }),
        (t.prototype.toVersion = function(t) {
          return this.toTimestamp(t.toTimestamp())
        }),
        (t.prototype.fromVersion = function(t) {
          return (
            Zn(!!t, "Trying to deserialize version that isn't set"),
            _i.fromTimestamp(this.fromTimestamp(t))
          )
        }),
        (t.prototype.toResourceName = function(t, e) {
          return this.fullyQualifiedPrefixPath(t)
            .child('documents')
            .child(e)
            .canonicalString()
        }),
        (t.prototype.fromResourceName = function(t) {
          var e = Qr.fromString(t)
          return (
            Zn(
              this.isValidResourceName(e),
              'Tried to deserialize invalid key ' + e.toString(),
            ),
            e
          )
        }),
        (t.prototype.toName = function(t) {
          return this.toResourceName(this.databaseId, t.path)
        }),
        (t.prototype.fromName = function(t) {
          var e = this.fromResourceName(t)
          return (
            Zn(
              e.get(1) === this.databaseId.projectId,
              'Tried to deserialize key from different project: ' +
                e.get(1) +
                ' vs ' +
                this.databaseId.projectId,
            ),
            Zn(
              (!e.get(3) && !this.databaseId.database) ||
                e.get(3) === this.databaseId.database,
              'Tried to deserialize key from different database: ' +
                e.get(3) +
                ' vs ' +
                this.databaseId.database,
            ),
            new jr(this.extractLocalPathFromResourceName(e))
          )
        }),
        (t.prototype.toQueryPath = function(t) {
          return 0 === t.length
            ? this.encodedDatabaseId
            : this.toResourceName(this.databaseId, t)
        }),
        (t.prototype.fromQueryPath = function(t) {
          var e = this.fromResourceName(t)
          return 4 === e.length
            ? Qr.EMPTY_PATH
            : this.extractLocalPathFromResourceName(e)
        }),
        Object.defineProperty(t.prototype, 'encodedDatabaseId', {
          get: function() {
            return new Qr([
              'projects',
              this.databaseId.projectId,
              'databases',
              this.databaseId.database,
            ]).canonicalString()
          },
          enumerable: !0,
          configurable: !0,
        }),
        (t.prototype.fullyQualifiedPrefixPath = function(t) {
          return new Qr(['projects', t.projectId, 'databases', t.database])
        }),
        (t.prototype.extractLocalPathFromResourceName = function(t) {
          return (
            Zn(
              t.length > 4 && 'documents' === t.get(4),
              'tried to deserialize invalid key ' + t.toString(),
            ),
            t.popFirst(5)
          )
        }),
        (t.prototype.isValidResourceName = function(t) {
          return (
            t.length >= 4 && 'projects' === t.get(0) && 'databases' === t.get(2)
          )
        }),
        (t.prototype.toValue = function(t) {
          if (t instanceof ni) return { nullValue: 'NULL_VALUE' }
          if (t instanceof ri) return { booleanValue: t.value() }
          if (t instanceof si) return { integerValue: '' + t.value() }
          if (t instanceof ai) {
            var e = t.value()
            if (this.options.useProto3Json) {
              if (isNaN(e)) return { doubleValue: 'NaN' }
              if (e === 1 / 0) return { doubleValue: 'Infinity' }
              if (e === -1 / 0) return { doubleValue: '-Infinity' }
            }
            return { doubleValue: t.value() }
          }
          return t instanceof ui
            ? { stringValue: t.value() }
            : t instanceof pi
              ? { mapValue: this.toMapValue(t) }
              : t instanceof mi
                ? { arrayValue: this.toArrayValue(t) }
                : t instanceof ci
                  ? { timestampValue: this.toTimestamp(t.internalValue) }
                  : t instanceof di
                    ? {
                        geoPointValue: {
                          latitude: t.value().latitude,
                          longitude: t.value().longitude,
                        },
                      }
                    : t instanceof li
                      ? { bytesValue: this.toBytes(t.value()) }
                      : t instanceof fi
                        ? {
                            referenceValue: this.toResourceName(
                              t.databaseId,
                              t.key.path,
                            ),
                          }
                        : $n('Unknown FieldValue ' + JSON.stringify(t))
        }),
        (t.prototype.fromValue = function(t) {
          var e = this,
            n = t.value_type
          if (Lo(t, n, 'nullValue')) return ni.INSTANCE
          if (Lo(t, n, 'booleanValue')) return ri.of(t.booleanValue)
          if (Lo(t, n, 'integerValue')) return new si(Po(t.integerValue))
          if (Lo(t, n, 'doubleValue')) {
            if (this.options.useProto3Json) {
              if ('NaN' === t.doubleValue) return ai.NAN
              if ('Infinity' === t.doubleValue) return ai.POSITIVE_INFINITY
              if ('-Infinity' === t.doubleValue) return ai.NEGATIVE_INFINITY
            }
            return new ai(t.doubleValue)
          }
          if (Lo(t, n, 'stringValue')) return new ui(t.stringValue)
          if (Lo(t, n, 'mapValue'))
            return this.fromFields(t.mapValue.fields || {})
          if (Lo(t, n, 'arrayValue')) {
            Oo(t.arrayValue, 'arrayValue')
            var r = t.arrayValue.values || []
            return new mi(
              r.map(function(t) {
                return e.fromValue(t)
              }),
            )
          }
          if (Lo(t, n, 'timestampValue'))
            return (
              Oo(t.timestampValue, 'timestampValue'),
              new ci(this.fromTimestamp(t.timestampValue))
            )
          if (Lo(t, n, 'geoPointValue')) {
            Oo(t.geoPointValue, 'geoPointValue')
            var i = t.geoPointValue.latitude || 0,
              o = t.geoPointValue.longitude || 0
            return new di(new xr(i, o))
          }
          if (Lo(t, n, 'bytesValue')) {
            Oo(t.bytesValue, 'bytesValue')
            var s = this.fromBlob(t.bytesValue)
            return new li(s)
          }
          if (Lo(t, n, 'referenceValue')) {
            Oo(t.referenceValue, 'referenceValue')
            var a = this.fromResourceName(t.referenceValue),
              u = new Br(a.get(1), a.get(3)),
              c = new jr(this.extractLocalPathFromResourceName(a))
            return new fi(u, c)
          }
          return $n('Unknown Value proto ' + JSON.stringify(t))
        }),
        (t.prototype.toMutationDocument = function(t, e) {
          return { name: this.toName(t), fields: this.toFields(e) }
        }),
        (t.prototype.toDocument = function(t) {
          return (
            Zn(
              !t.hasLocalMutations,
              "Can't serialize documents with mutations.",
            ),
            {
              name: this.toName(t.key),
              fields: this.toFields(t.data),
              updateTime: this.toTimestamp(t.version.toTimestamp()),
            }
          )
        }),
        (t.prototype.fromDocument = function(t, e) {
          return new zr(
            this.fromName(t.name),
            this.fromVersion(t.updateTime),
            this.fromFields(t.fields || {}),
            { hasCommittedMutations: !!e },
          )
        }),
        (t.prototype.toFields = function(t) {
          var e = this,
            n = {}
          return (
            t.forEach(function(t, r) {
              n[t] = e.toValue(r)
            }),
            n
          )
        }),
        (t.prototype.fromFields = function(t) {
          var e = this,
            n = t,
            r = pi.EMPTY
          return (
            ur(n, function(t, n) {
              r = r.set(new Wr([t]), e.fromValue(n))
            }),
            r
          )
        }),
        (t.prototype.toMapValue = function(t) {
          return { fields: this.toFields(t) }
        }),
        (t.prototype.toArrayValue = function(t) {
          var e = this,
            n = []
          return (
            t.forEach(function(t) {
              n.push(e.toValue(t))
            }),
            { values: n }
          )
        }),
        (t.prototype.fromFound = function(t) {
          Zn(
            !!t.found,
            'Tried to deserialize a found document from a missing document.',
          ),
            Oo(t.found.name, 'doc.found.name'),
            Oo(t.found.updateTime, 'doc.found.updateTime')
          var e = this.fromName(t.found.name),
            n = this.fromVersion(t.found.updateTime),
            r = this.fromFields(t.found.fields || {})
          return new zr(e, n, r, {})
        }),
        (t.prototype.fromMissing = function(t) {
          Zn(
            !!t.missing,
            'Tried to deserialize a missing document from a found document.',
          ),
            Zn(
              !!t.readTime,
              'Tried to deserialize a missing document without a read time.',
            )
          var e = this.fromName(t.missing),
            n = this.fromVersion(t.readTime)
          return new Hr(e, n)
        }),
        (t.prototype.fromMaybeDocument = function(t) {
          var e = t.result
          return Lo(t, e, 'found')
            ? this.fromFound(t)
            : Lo(t, e, 'missing')
              ? this.fromMissing(t)
              : $n('invalid batch get response: ' + JSON.stringify(t))
        }),
        (t.prototype.toWatchTargetChangeState = function(t) {
          switch (t) {
            case mo.Added:
              return 'ADD'
            case mo.Current:
              return 'CURRENT'
            case mo.NoChange:
              return 'NO_CHANGE'
            case mo.Removed:
              return 'REMOVE'
            case mo.Reset:
              return 'RESET'
            default:
              return $n('Unknown WatchTargetChangeState: ' + t)
          }
        }),
        (t.prototype.toTestWatchChange = function(t) {
          if (t instanceof To)
            return {
              filter: { count: t.existenceFilter.count, targetId: t.targetId },
            }
          if (t instanceof wo) {
            if (t.newDoc instanceof zr) {
              var e = t.newDoc
              return {
                documentChange: {
                  document: {
                    name: this.toName(e.key),
                    fields: this.toFields(e.data),
                    updateTime: this.toVersion(e.version),
                  },
                  targetIds: t.updatedTargetIds,
                  removedTargetIds: t.removedTargetIds,
                },
              }
            }
            if (t.newDoc instanceof Hr) {
              e = t.newDoc
              return {
                documentDelete: {
                  document: this.toName(e.key),
                  readTime: this.toVersion(e.version),
                  removedTargetIds: t.removedTargetIds,
                },
              }
            }
            if (null === t.newDoc)
              return {
                documentRemove: {
                  document: this.toName(t.key),
                  removedTargetIds: t.removedTargetIds,
                },
              }
          }
          if (t instanceof So) {
            var n = void 0
            return (
              t.cause &&
                (n = {
                  code: (function(t) {
                    if (void 0 === t) return Yi.OK
                    switch (t) {
                      case nr.OK:
                        return Yi.OK
                      case nr.CANCELLED:
                        return Yi.CANCELLED
                      case nr.UNKNOWN:
                        return Yi.UNKNOWN
                      case nr.DEADLINE_EXCEEDED:
                        return Yi.DEADLINE_EXCEEDED
                      case nr.RESOURCE_EXHAUSTED:
                        return Yi.RESOURCE_EXHAUSTED
                      case nr.INTERNAL:
                        return Yi.INTERNAL
                      case nr.UNAVAILABLE:
                        return Yi.UNAVAILABLE
                      case nr.UNAUTHENTICATED:
                        return Yi.UNAUTHENTICATED
                      case nr.INVALID_ARGUMENT:
                        return Yi.INVALID_ARGUMENT
                      case nr.NOT_FOUND:
                        return Yi.NOT_FOUND
                      case nr.ALREADY_EXISTS:
                        return Yi.ALREADY_EXISTS
                      case nr.PERMISSION_DENIED:
                        return Yi.PERMISSION_DENIED
                      case nr.FAILED_PRECONDITION:
                        return Yi.FAILED_PRECONDITION
                      case nr.ABORTED:
                        return Yi.ABORTED
                      case nr.OUT_OF_RANGE:
                        return Yi.OUT_OF_RANGE
                      case nr.UNIMPLEMENTED:
                        return Yi.UNIMPLEMENTED
                      case nr.DATA_LOSS:
                        return Yi.DATA_LOSS
                      default:
                        return $n('Unknown status code: ' + t)
                    }
                  })(t.cause.code),
                  message: t.cause.message,
                }),
              {
                targetChange: {
                  targetChangeType: this.toWatchTargetChangeState(t.state),
                  targetIds: t.targetIds,
                  resumeToken: this.unsafeCastProtoByteString(t.resumeToken),
                  cause: n,
                },
              }
            )
          }
          return $n('Unrecognized watch change: ' + JSON.stringify(t))
        }),
        (t.prototype.fromWatchChange = function(t) {
          var e,
            n = t.response_type
          if (Lo(t, n, 'targetChange')) {
            Oo(t.targetChange, 'targetChange')
            var r = this.fromWatchTargetChangeState(
                t.targetChange.targetChangeType || 'NO_CHANGE',
              ),
              i = t.targetChange.targetIds || [],
              o = t.targetChange.resumeToken || this.emptyByteString(),
              s = t.targetChange.cause,
              a = s && this.fromRpcStatus(s)
            e = new So(r, i, o, a || null)
          } else if (Lo(t, n, 'documentChange')) {
            Oo(t.documentChange, 'documentChange'),
              Oo(t.documentChange.document, 'documentChange.name'),
              Oo(
                t.documentChange.document.name,
                'documentChange.document.name',
              ),
              Oo(
                t.documentChange.document.updateTime,
                'documentChange.document.updateTime',
              )
            var u = t.documentChange,
              c = this.fromName(u.document.name),
              h = this.fromVersion(u.document.updateTime),
              l = this.fromFields(u.document.fields || {}),
              f = new zr(c, h, l, {}),
              d = u.targetIds || [],
              p = u.removedTargetIds || []
            e = new wo(d, p, f.key, f)
          } else if (Lo(t, n, 'documentDelete')) {
            Oo(t.documentDelete, 'documentDelete'),
              Oo(t.documentDelete.document, 'documentDelete.document')
            var m = t.documentDelete
            ;(c = this.fromName(m.document)),
              (h = m.readTime
                ? this.fromVersion(m.readTime)
                : _i.forDeletedDoc()),
              (f = new Hr(c, h)),
              (p = m.removedTargetIds || [])
            e = new wo([], p, f.key, f)
          } else if (Lo(t, n, 'documentRemove')) {
            Oo(t.documentRemove, 'documentRemove'),
              Oo(t.documentRemove.document, 'documentRemove')
            var y = t.documentRemove
            ;(c = this.fromName(y.document)), (p = y.removedTargetIds || [])
            e = new wo([], p, c, null)
          } else {
            if (!Lo(t, n, 'filter'))
              return $n('Unknown change type ' + JSON.stringify(t))
            Oo(t.filter, 'filter'), Oo(t.filter.targetId, 'filter.targetId')
            var g = t.filter,
              v = g.count || 0,
              b = new Ji(v),
              w = g.targetId
            e = new To(w, b)
          }
          return e
        }),
        (t.prototype.fromWatchTargetChangeState = function(t) {
          return 'NO_CHANGE' === t
            ? mo.NoChange
            : 'ADD' === t
              ? mo.Added
              : 'REMOVE' === t
                ? mo.Removed
                : 'CURRENT' === t
                  ? mo.Current
                  : 'RESET' === t
                    ? mo.Reset
                    : $n('Got unexpected TargetChange.state: ' + t)
        }),
        (t.prototype.versionFromListenResponse = function(t) {
          if (!Lo(t, t.response_type, 'targetChange')) return _i.MIN
          var e = t.targetChange
          return e.targetIds && e.targetIds.length
            ? _i.MIN
            : e.readTime
              ? this.fromVersion(e.readTime)
              : _i.MIN
        }),
        (t.prototype.toMutation = function(t) {
          var e,
            n = this
          if (t instanceof Qi)
            e = { update: this.toMutationDocument(t.key, t.value) }
          else if (t instanceof ji) e = { delete: this.toName(t.key) }
          else if (t instanceof Ki)
            e = {
              update: this.toMutationDocument(t.key, t.data),
              updateMask: this.toDocumentMask(t.fieldMask),
            }
          else {
            if (!(t instanceof Wi)) return $n('Unknown mutation type ' + t.type)
            e = {
              transform: {
                document: this.toName(t.key),
                fieldTransforms: t.fieldTransforms.map(function(t) {
                  return n.toFieldTransform(t)
                }),
              },
            }
          }
          return (
            t.precondition.isNone ||
              (e.currentDocument = this.toPrecondition(t.precondition)),
            e
          )
        }),
        (t.prototype.fromMutation = function(t) {
          var e = this,
            n = t.currentDocument
              ? this.fromPrecondition(t.currentDocument)
              : Bi.NONE
          if (t.update) {
            Oo(t.update.name, 'name')
            var r = this.fromName(t.update.name),
              i = this.fromFields(t.update.fields || {})
            if (t.updateMask) {
              var o = this.fromDocumentMask(t.updateMask)
              return new Ki(r, i, o, n)
            }
            return new Qi(r, i, n)
          }
          if (t.delete) {
            r = this.fromName(t.delete)
            return new ji(r, n)
          }
          if (t.transform) {
            r = this.fromName(t.transform.document)
            var s = t.transform.fieldTransforms.map(function(t) {
              return e.fromFieldTransform(t)
            })
            return (
              Zn(
                !0 === n.exists,
                'Transforms only support precondition "exists == true"',
              ),
              new Wi(r, s)
            )
          }
          return $n('unknown mutation proto: ' + JSON.stringify(t))
        }),
        (t.prototype.toPrecondition = function(t) {
          return (
            Zn(!t.isNone, "Can't serialize an empty precondition"),
            void 0 !== t.updateTime
              ? { updateTime: this.toVersion(t.updateTime) }
              : void 0 !== t.exists
                ? { exists: t.exists }
                : $n('Unknown precondition')
          )
        }),
        (t.prototype.fromPrecondition = function(t) {
          return void 0 !== t.updateTime
            ? Bi.updateTime(this.fromVersion(t.updateTime))
            : void 0 !== t.exists
              ? Bi.exists(t.exists)
              : Bi.NONE
        }),
        (t.prototype.fromWriteResult = function(t, e) {
          var n = this,
            r = t.updateTime
              ? this.fromVersion(t.updateTime)
              : this.fromVersion(e),
            i = null
          return (
            t.transformResults &&
              t.transformResults.length > 0 &&
              (i = t.transformResults.map(function(t) {
                return n.fromValue(t)
              })),
            new Vi(r, i)
          )
        }),
        (t.prototype.fromWriteResults = function(t, e) {
          var n = this
          return t && t.length > 0
            ? (Zn(
                void 0 !== e,
                'Received a write result without a commit time',
              ),
              t.map(function(t) {
                return n.fromWriteResult(t, e)
              }))
            : []
        }),
        (t.prototype.toFieldTransform = function(t) {
          var e = this,
            n = t.transform
          if (n instanceof Gi)
            return {
              fieldPath: t.field.canonicalString(),
              setToServerValue: 'REQUEST_TIME',
            }
          if (n instanceof zi)
            return {
              fieldPath: t.field.canonicalString(),
              appendMissingElements: {
                values: n.elements.map(function(t) {
                  return e.toValue(t)
                }),
              },
            }
          if (n instanceof Hi)
            return {
              fieldPath: t.field.canonicalString(),
              removeAllFromArray: {
                values: n.elements.map(function(t) {
                  return e.toValue(t)
                }),
              },
            }
          throw $n('Unknown transform: ' + t.transform)
        }),
        (t.prototype.fromFieldTransform = function(t) {
          var e = this,
            n = t.transform_type,
            r = null
          if (Lo(t, n, 'setToServerValue'))
            Zn(
              'REQUEST_TIME' === t.setToServerValue,
              'Unknown server value transform proto: ' + JSON.stringify(t),
            ),
              (r = Gi.instance)
          else if (Lo(t, n, 'appendMissingElements')) {
            var i = t.appendMissingElements.values || []
            r = new zi(
              i.map(function(t) {
                return e.fromValue(t)
              }),
            )
          } else if (Lo(t, n, 'removeAllFromArray')) {
            i = t.removeAllFromArray.values || []
            r = new Hi(
              i.map(function(t) {
                return e.fromValue(t)
              }),
            )
          } else $n('Unknown transform proto: ' + JSON.stringify(t))
          var o = Wr.fromServerFormat(t.fieldPath)
          return new Fi(o, r)
        }),
        (t.prototype.toDocumentsTarget = function(t) {
          return { documents: [this.toQueryPath(t.path)] }
        }),
        (t.prototype.fromDocumentsTarget = function(t) {
          var e = t.documents.length
          Zn(1 === e, 'DocumentsTarget contained other than 1 document: ' + e)
          var n = t.documents[0]
          return Ei.atPath(this.fromQueryPath(n))
        }),
        (t.prototype.toQueryTarget = function(t) {
          var e = { structuredQuery: {} }
          if (t.path.isEmpty()) e.parent = this.toQueryPath(Qr.EMPTY_PATH)
          else {
            var n = t.path
            Zn(
              n.length % 2 != 0,
              'Document queries with filters are not supported.',
            ),
              (e.parent = this.toQueryPath(n.popLast())),
              (e.structuredQuery.from = [{ collectionId: n.lastSegment() }])
          }
          var r = this.toFilter(t.filters)
          r && (e.structuredQuery.where = r)
          var i = this.toOrder(t.orderBy)
          i && (e.structuredQuery.orderBy = i)
          var o = this.toInt32Value(t.limit)
          return (
            void 0 !== o && (e.structuredQuery.limit = o),
            t.startAt && (e.structuredQuery.startAt = this.toCursor(t.startAt)),
            t.endAt && (e.structuredQuery.endAt = this.toCursor(t.endAt)),
            e
          )
        }),
        (t.prototype.fromQueryTarget = function(t) {
          var e = this.fromQueryPath(t.parent),
            n = t.structuredQuery,
            r = n.from ? n.from.length : 0
          if (r > 0) {
            Zn(
              1 === r,
              'StructuredQuery.from with more than one collection is not supported.',
            )
            var i = n.from[0]
            e = e.child(i.collectionId)
          }
          var o = []
          n.where && (o = this.fromFilter(n.where))
          var s = []
          n.orderBy && (s = this.fromOrder(n.orderBy))
          var a = null
          n.limit && (a = this.fromInt32Value(n.limit))
          var u = null
          n.startAt && (u = this.fromCursor(n.startAt))
          var c = null
          return (
            n.endAt && (c = this.fromCursor(n.endAt)), new Ei(e, s, o, a, u, c)
          )
        }),
        (t.prototype.toListenRequestLabels = function(t) {
          var e = this.toLabel(t.purpose)
          return null == e ? null : { 'goog-listen-tags': e }
        }),
        (t.prototype.toLabel = function(t) {
          switch (t) {
            case Si.Listen:
              return null
            case Si.ExistenceFilterMismatch:
              return 'existence-filter-mismatch'
            case Si.LimboResolution:
              return 'limbo-document'
            default:
              return $n('Unrecognized query purpose: ' + t)
          }
        }),
        (t.prototype.toTarget = function(t) {
          var e,
            n = t.query
          return (
            ((e = n.isDocumentQuery()
              ? { documents: this.toDocumentsTarget(n) }
              : { query: this.toQueryTarget(n) }).targetId = t.targetId),
            t.resumeToken.length > 0 &&
              (e.resumeToken = this.unsafeCastProtoByteString(t.resumeToken)),
            e
          )
        }),
        (t.prototype.toFilter = function(t) {
          var e = this
          if (0 !== t.length) {
            var n = t.map(function(t) {
              return t instanceof Di
                ? e.toRelationFilter(t)
                : e.toUnaryFilter(t)
            })
            return 1 === n.length
              ? n[0]
              : { compositeFilter: { op: 'AND', filters: n } }
          }
        }),
        (t.prototype.fromFilter = function(t) {
          var e = this
          return t
            ? void 0 !== t.unaryFilter
              ? [this.fromUnaryFilter(t)]
              : void 0 !== t.fieldFilter
                ? [this.fromRelationFilter(t)]
                : void 0 !== t.compositeFilter
                  ? t.compositeFilter.filters
                      .map(function(t) {
                        return e.fromFilter(t)
                      })
                      .reduce(function(t, e) {
                        return t.concat(e)
                      })
                  : $n('Unknown filter: ' + JSON.stringify(t))
            : []
        }),
        (t.prototype.toOrder = function(t) {
          var e = this
          if (0 !== t.length)
            return t.map(function(t) {
              return e.toPropertyOrder(t)
            })
        }),
        (t.prototype.fromOrder = function(t) {
          var e = this
          return t.map(function(t) {
            return e.fromPropertyOrder(t)
          })
        }),
        (t.prototype.toCursor = function(t) {
          var e = this
          return {
            before: t.before,
            values: t.position.map(function(t) {
              return e.toValue(t)
            }),
          }
        }),
        (t.prototype.fromCursor = function(t) {
          var e = this,
            n = !!t.before,
            r = t.values.map(function(t) {
              return e.fromValue(t)
            })
          return new Ri(r, n)
        }),
        (t.prototype.toDirection = function(t) {
          return ko[t.name]
        }),
        (t.prototype.fromDirection = function(t) {
          switch (t) {
            case 'ASCENDING':
              return ki.ASCENDING
            case 'DESCENDING':
              return ki.DESCENDING
            default:
              return
          }
        }),
        (t.prototype.toOperatorName = function(t) {
          return Ro[t.name]
        }),
        (t.prototype.fromOperatorName = function(t) {
          switch (t) {
            case 'EQUAL':
              return Ci.EQUAL
            case 'GREATER_THAN':
              return Ci.GREATER_THAN
            case 'GREATER_THAN_OR_EQUAL':
              return Ci.GREATER_THAN_OR_EQUAL
            case 'LESS_THAN':
              return Ci.LESS_THAN
            case 'LESS_THAN_OR_EQUAL':
              return Ci.LESS_THAN_OR_EQUAL
            case 'ARRAY_CONTAINS':
              return Ci.ARRAY_CONTAINS
            case 'OPERATOR_UNSPECIFIED':
              return $n('Unspecified relation')
            default:
              return $n('Unknown relation')
          }
        }),
        (t.prototype.toFieldPathReference = function(t) {
          return { fieldPath: t.canonicalString() }
        }),
        (t.prototype.fromFieldPathReference = function(t) {
          return Wr.fromServerFormat(t.fieldPath)
        }),
        (t.prototype.toPropertyOrder = function(t) {
          return {
            field: this.toFieldPathReference(t.field),
            direction: this.toDirection(t.dir),
          }
        }),
        (t.prototype.fromPropertyOrder = function(t) {
          return new Mi(
            this.fromFieldPathReference(t.field),
            this.fromDirection(t.direction),
          )
        }),
        (t.prototype.toRelationFilter = function(t) {
          return t instanceof Di
            ? {
                fieldFilter: {
                  field: this.toFieldPathReference(t.field),
                  op: this.toOperatorName(t.op),
                  value: this.toValue(t.value),
                },
              }
            : $n('Unrecognized filter: ' + JSON.stringify(t))
        }),
        (t.prototype.fromRelationFilter = function(t) {
          return new Di(
            this.fromFieldPathReference(t.fieldFilter.field),
            this.fromOperatorName(t.fieldFilter.op),
            this.fromValue(t.fieldFilter.value),
          )
        }),
        (t.prototype.toUnaryFilter = function(t) {
          return t instanceof Ai
            ? {
                unaryFilter: {
                  field: this.toFieldPathReference(t.field),
                  op: 'IS_NAN',
                },
              }
            : t instanceof Ni
              ? {
                  unaryFilter: {
                    field: this.toFieldPathReference(t.field),
                    op: 'IS_NULL',
                  },
                }
              : $n('Unrecognized filter: ' + JSON.stringify(t))
        }),
        (t.prototype.fromUnaryFilter = function(t) {
          switch (t.unaryFilter.op) {
            case 'IS_NAN':
              var e = this.fromFieldPathReference(t.unaryFilter.field)
              return new Ai(e)
            case 'IS_NULL':
              var n = this.fromFieldPathReference(t.unaryFilter.field)
              return new Ni(n)
            case 'OPERATOR_UNSPECIFIED':
              return $n('Unspecified filter')
            default:
              return $n('Unknown filter')
          }
        }),
        (t.prototype.toDocumentMask = function(t) {
          return {
            fieldPaths: t.fields.map(function(t) {
              return t.canonicalString()
            }),
          }
        }),
        (t.prototype.fromDocumentMask = function(t) {
          var e = (t.fieldPaths || []).map(function(t) {
            return Wr.fromServerFormat(t)
          })
          return new qi(e)
        }),
        t
      )
    })()
    function Lo(t, e, n) {
      return e === n || (!e && n in t)
    }
    var xo = 'FirebaseError',
      qo = Error.captureStackTrace,
      Fo = (function() {
        return function(t, e) {
          if (((this.code = t), (this.message = e), qo))
            qo(this, Vo.prototype.create)
          else
            try {
              throw Error.apply(this, arguments)
            } catch (t) {
              ;(this.name = xo),
                Object.defineProperty(this, 'stack', {
                  get: function() {
                    return t.stack
                  },
                })
            }
        }
      })()
    ;(Fo.prototype = Object.create(Error.prototype)),
      (Fo.prototype.constructor = Fo),
      (Fo.prototype.name = xo)
    var Vo = (function() {
        function t(t, e, n) {
          ;(this.service = t),
            (this.serviceName = e),
            (this.errors = n),
            (this.pattern = /\{\$([^}]+)}/g)
        }
        return (
          (t.prototype.create = function(t, e) {
            void 0 === e && (e = {})
            var n,
              r = this.errors[t],
              i = this.service + '/' + t
            ;(n =
              void 0 === r
                ? 'Error'
                : r.replace(this.pattern, function(t, n) {
                    var r = e[n]
                    return void 0 !== r ? r.toString() : '<' + n + '?>'
                  })),
              (n = this.serviceName + ': ' + n + ' (' + i + ').')
            var o = new Fo(i, n)
            for (var s in e)
              e.hasOwnProperty(s) && '_' !== s.slice(-1) && (o[s] = e[s])
            return o
          }),
          t
        )
      })(),
      Bo = ((function(t) {
        function e() {
          var e = t.call(this) || this
          ;(e.chain_ = []),
            (e.buf_ = []),
            (e.W_ = []),
            (e.pad_ = []),
            (e.inbuf_ = 0),
            (e.total_ = 0),
            (e.blockSize = 64),
            (e.pad_[0] = 128)
          for (var n = 1; n < e.blockSize; ++n) e.pad_[n] = 0
          return e.reset(), e
        }
        a(e, t),
          (e.prototype.reset = function() {
            ;(this.chain_[0] = 1732584193),
              (this.chain_[1] = 4023233417),
              (this.chain_[2] = 2562383102),
              (this.chain_[3] = 271733878),
              (this.chain_[4] = 3285377520),
              (this.inbuf_ = 0),
              (this.total_ = 0)
          }),
          (e.prototype.compress_ = function(t, e) {
            e || (e = 0)
            var n = this.W_
            if ('string' == typeof t)
              for (var r = 0; r < 16; r++)
                (n[r] =
                  (t.charCodeAt(e) << 24) |
                  (t.charCodeAt(e + 1) << 16) |
                  (t.charCodeAt(e + 2) << 8) |
                  t.charCodeAt(e + 3)),
                  (e += 4)
            else
              for (r = 0; r < 16; r++)
                (n[r] =
                  (t[e] << 24) | (t[e + 1] << 16) | (t[e + 2] << 8) | t[e + 3]),
                  (e += 4)
            for (r = 16; r < 80; r++) {
              var i = n[r - 3] ^ n[r - 8] ^ n[r - 14] ^ n[r - 16]
              n[r] = 4294967295 & ((i << 1) | (i >>> 31))
            }
            var o,
              s,
              a = this.chain_[0],
              u = this.chain_[1],
              c = this.chain_[2],
              h = this.chain_[3],
              l = this.chain_[4]
            for (r = 0; r < 80; r++) {
              r < 40
                ? r < 20
                  ? ((o = h ^ (u & (c ^ h))), (s = 1518500249))
                  : ((o = u ^ c ^ h), (s = 1859775393))
                : r < 60
                  ? ((o = (u & c) | (h & (u | c))), (s = 2400959708))
                  : ((o = u ^ c ^ h), (s = 3395469782))
              i = (((a << 5) | (a >>> 27)) + o + l + s + n[r]) & 4294967295
              ;(l = h),
                (h = c),
                (c = 4294967295 & ((u << 30) | (u >>> 2))),
                (u = a),
                (a = i)
            }
            ;(this.chain_[0] = (this.chain_[0] + a) & 4294967295),
              (this.chain_[1] = (this.chain_[1] + u) & 4294967295),
              (this.chain_[2] = (this.chain_[2] + c) & 4294967295),
              (this.chain_[3] = (this.chain_[3] + h) & 4294967295),
              (this.chain_[4] = (this.chain_[4] + l) & 4294967295)
          }),
          (e.prototype.update = function(t, e) {
            if (null != t) {
              void 0 === e && (e = t.length)
              for (
                var n = e - this.blockSize,
                  r = 0,
                  i = this.buf_,
                  o = this.inbuf_;
                r < e;

              ) {
                if (0 == o)
                  for (; r <= n; ) this.compress_(t, r), (r += this.blockSize)
                if ('string' == typeof t) {
                  for (; r < e; )
                    if (
                      ((i[o] = t.charCodeAt(r)), ++r, ++o == this.blockSize)
                    ) {
                      this.compress_(i), (o = 0)
                      break
                    }
                } else
                  for (; r < e; )
                    if (((i[o] = t[r]), ++r, ++o == this.blockSize)) {
                      this.compress_(i), (o = 0)
                      break
                    }
              }
              ;(this.inbuf_ = o), (this.total_ += e)
            }
          }),
          (e.prototype.digest = function() {
            var t = [],
              e = 8 * this.total_
            this.inbuf_ < 56
              ? this.update(this.pad_, 56 - this.inbuf_)
              : this.update(this.pad_, this.blockSize - (this.inbuf_ - 56))
            for (var n = this.blockSize - 1; n >= 56; n--)
              (this.buf_[n] = 255 & e), (e /= 256)
            this.compress_(this.buf_)
            var r = 0
            for (n = 0; n < 5; n++)
              for (var i = 24; i >= 0; i -= 8)
                (t[r] = (this.chain_[n] >> i) & 255), ++r
            return t
          })
      })(
        (function() {
          return function() {
            this.blockSize = -1
          }
        })(),
      ),
      (function() {
        function t(t) {
          ;(this.sendFn = t.sendFn), (this.closeFn = t.closeFn)
        }
        return (
          (t.prototype.onOpen = function(t) {
            Zn(!this.wrappedOnOpen, 'Called onOpen on stream twice!'),
              (this.wrappedOnOpen = t)
          }),
          (t.prototype.onClose = function(t) {
            Zn(!this.wrappedOnClose, 'Called onClose on stream twice!'),
              (this.wrappedOnClose = t)
          }),
          (t.prototype.onMessage = function(t) {
            Zn(!this.wrappedOnMessage, 'Called onMessage on stream twice!'),
              (this.wrappedOnMessage = t)
          }),
          (t.prototype.close = function() {
            this.closeFn()
          }),
          (t.prototype.send = function(t) {
            this.sendFn(t)
          }),
          (t.prototype.callOnOpen = function() {
            Zn(
              void 0 !== this.wrappedOnOpen,
              'Cannot call onOpen because no callback was set',
            ),
              this.wrappedOnOpen()
          }),
          (t.prototype.callOnClose = function(t) {
            Zn(
              void 0 !== this.wrappedOnClose,
              'Cannot call onClose because no callback was set',
            ),
              this.wrappedOnClose(t)
          }),
          (t.prototype.callOnMessage = function(t) {
            Zn(
              void 0 !== this.wrappedOnMessage,
              'Cannot call onMessage because no callback was set',
            ),
              this.wrappedOnMessage(t)
          }),
          t
        )
      })()),
      Uo = 'Connection',
      Qo = { BatchGetDocuments: 'batchGet', Commit: 'commit' },
      Ko = 'gl-js/ fire/' + jn,
      Wo = (function() {
        function t(t) {
          ;(this.databaseId = t.databaseId), (this.pool = new Wn())
          var e = t.ssl ? 'https' : 'http'
          this.baseUrl = e + '://' + t.host
        }
        return (
          (t.prototype.modifyHeadersForRequest = function(t, e) {
            if (e)
              for (var n in e.authHeaders)
                e.authHeaders.hasOwnProperty(n) && (t[n] = e.authHeaders[n])
            t['X-Goog-Api-Client'] = Ko
          }),
          (t.prototype.invokeRPC = function(t, e, n) {
            var r = this,
              i = this.makeUrl(t)
            return new Promise(function(o, s) {
              r.pool.getObject(function(a) {
                a.listenOnce(Qn.COMPLETE, function() {
                  try {
                    switch (a.getLastErrorCode()) {
                      case Un.NO_ERROR:
                        var e = a.getResponseJson()
                        Xn(Uo, 'XHR received:', JSON.stringify(e)), o(e)
                        break
                      case Un.TIMEOUT:
                        Xn(Uo, 'RPC "' + t + '" timed out'),
                          s(new rr(nr.DEADLINE_EXCEEDED, 'Request time out'))
                        break
                      case Un.HTTP_ERROR:
                        var n = a.getStatus()
                        Xn(
                          Uo,
                          'RPC "' + t + '" failed with status:',
                          n,
                          'response text:',
                          a.getResponseText(),
                        ),
                          n > 0
                            ? s(
                                new rr(
                                  (function(t) {
                                    switch (t) {
                                      case 200:
                                        return nr.OK
                                      case 400:
                                        return nr.INVALID_ARGUMENT
                                      case 401:
                                        return nr.UNAUTHENTICATED
                                      case 403:
                                        return nr.PERMISSION_DENIED
                                      case 404:
                                        return nr.NOT_FOUND
                                      case 409:
                                        return nr.ABORTED
                                      case 416:
                                        return nr.OUT_OF_RANGE
                                      case 429:
                                        return nr.RESOURCE_EXHAUSTED
                                      case 499:
                                        return nr.CANCELLED
                                      case 500:
                                        return nr.UNKNOWN
                                      case 501:
                                        return nr.UNIMPLEMENTED
                                      case 503:
                                        return nr.UNAVAILABLE
                                      case 504:
                                        return nr.DEADLINE_EXCEEDED
                                      default:
                                        return t >= 200 && t < 300
                                          ? nr.OK
                                          : t >= 400 && t < 500
                                            ? nr.FAILED_PRECONDITION
                                            : t >= 500 && t < 600
                                              ? nr.INTERNAL
                                              : nr.UNKNOWN
                                    }
                                  })(n),
                                  'Server responded with status ' +
                                    a.getStatusText(),
                                ),
                              )
                            : (Xn(Uo, 'RPC "' + t + '" failed'),
                              s(new rr(nr.UNAVAILABLE, 'Connection failed.')))
                        break
                      default:
                        $n(
                          'RPC "' +
                            t +
                            '" failed with unanticipated webchannel error ' +
                            a.getLastErrorCode() +
                            ': ' +
                            a.getLastError() +
                            ', giving up.',
                        )
                    }
                  } finally {
                    Xn(Uo, 'RPC "' + t + '" completed.'),
                      r.pool.releaseObject(a)
                  }
                })
                var u = JSON.stringify(e)
                Xn(Uo, 'XHR sending: ', i + ' ' + u)
                var c = { 'Content-Type': 'text/plain' }
                r.modifyHeadersForRequest(c, n), a.send(i, 'POST', u, c, 15)
              })
            })
          }),
          (t.prototype.invokeStreamingRPC = function(t, e, n) {
            return this.invokeRPC(t, e, n)
          }),
          (t.prototype.openStream = function(t, e) {
            var n = [
                this.baseUrl,
                '/',
                'google.firestore.v1beta1.Firestore',
                '/',
                t,
                '/channel',
              ],
              r = Bn(),
              i = {
                backgroundChannelTest: !0,
                httpSessionIdParam: 'gsessionid',
                initMessageHeaders: {},
                messageUrlParams: {
                  database:
                    'projects/' +
                    this.databaseId.projectId +
                    '/databases/' +
                    this.databaseId.database,
                },
                sendRawJson: !0,
                supportsCrossDomainXhr: !0,
              }
            this.modifyHeadersForRequest(i.initMessageHeaders, e),
              ('object' == typeof navigator &&
                'ReactNative' === navigator.product) ||
                (i.httpHeadersOverwriteParam = '$httpHeaders')
            var o = n.join('')
            Xn(Uo, 'Creating WebChannel: ' + o + ' ' + i)
            var s = r.createWebChannel(o, i),
              a = !1,
              u = !1,
              c = new Bo({
                sendFn: function(t) {
                  u
                    ? Xn(Uo, 'Not sending because WebChannel is closed:', t)
                    : (a ||
                        (Xn(Uo, 'Opening WebChannel transport.'),
                        s.open(),
                        (a = !0)),
                      Xn(Uo, 'WebChannel sending:', t),
                      s.send(t))
                },
                closeFn: function() {
                  return s.close()
                },
              }),
              h = function(t, e) {
                s.listen(t, function(t) {
                  try {
                    e(t)
                  } catch (t) {
                    setTimeout(function() {
                      throw t
                    }, 0)
                  }
                })
              }
            return (
              h(Kn.EventType.OPEN, function() {
                u || Xn(Uo, 'WebChannel transport opened.')
              }),
              h(Kn.EventType.CLOSE, function() {
                u ||
                  ((u = !0),
                  Xn(Uo, 'WebChannel transport closed'),
                  c.callOnClose())
              }),
              h(Kn.EventType.ERROR, function(t) {
                u ||
                  ((u = !0),
                  Xn(Uo, 'WebChannel transport errored:', t),
                  c.callOnClose(
                    new rr(
                      nr.UNAVAILABLE,
                      'The operation could not be completed',
                    ),
                  ))
              }),
              h(Kn.EventType.MESSAGE, function(t) {
                if (!u) {
                  var e = t.data[0]
                  Zn(!!e, 'Got a webchannel message without data.')
                  var n = e.error || (e[0] && e[0].error)
                  if (n) {
                    Xn(Uo, 'WebChannel received error:', n)
                    var r = n.status,
                      i = (function(t) {
                        var e = Yi[t]
                        if (void 0 !== e) return Zi(e)
                      })(r),
                      o = n.message
                    void 0 === i &&
                      ((i = nr.INTERNAL),
                      (o =
                        'Unknown error status: ' +
                        r +
                        ' with message ' +
                        n.message)),
                      (u = !0),
                      c.callOnClose(new rr(i, o)),
                      s.close()
                  } else Xn(Uo, 'WebChannel received:', e), c.callOnMessage(e)
                }
              }),
              setTimeout(function() {
                c.callOnOpen()
              }, 0),
              c
            )
          }),
          (t.prototype.makeUrl = function(t) {
            var e = Qo[t]
            Zn(void 0 !== e, 'Unknown REST mapping for: ' + t)
            var n = [this.baseUrl, '/', 'v1beta1']
            return (
              n.push('/projects/'),
              n.push(this.databaseId.projectId),
              n.push('/databases/'),
              n.push(this.databaseId.database),
              n.push('/documents'),
              n.push(':'),
              n.push(e),
              n.join('')
            )
          }),
          t
        )
      })(),
      jo = (function() {
        function t() {
          ;(this.emptyByteString = ''),
            (this.base64Available = 'undefined' != typeof atob)
        }
        return (
          Object.defineProperty(t.prototype, 'document', {
            get: function() {
              return 'undefined' != typeof document ? document : null
            },
            enumerable: !0,
            configurable: !0,
          }),
          Object.defineProperty(t.prototype, 'window', {
            get: function() {
              return 'undefined' != typeof window ? window : null
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.loadConnection = function(t) {
            return Promise.resolve(new Wo(t))
          }),
          (t.prototype.newSerializer = function(t) {
            return new _o(t, { useProto3Json: !0 })
          }),
          (t.prototype.formatJSON = function(t) {
            return JSON.stringify(t)
          }),
          (t.prototype.atob = function(t) {
            return atob(t)
          }),
          (t.prototype.btoa = function(t) {
            return btoa(t)
          }),
          t
        )
      })()
    tr.setPlatform(new jo())
    var Go,
      zo = (function() {
        function t(t, e) {
          var n = this
          ;(this.previousValue = t),
            e &&
              ((e.sequenceNumberHandler = function(t) {
                return n.setPreviousValue(t)
              }),
              (this.writeNewSequenceNumber = function(t) {
                return e.writeSequenceNumber(t)
              }))
        }
        return (
          (t.prototype.setPreviousValue = function(t) {
            return (
              (this.previousValue = Math.max(t, this.previousValue)),
              this.previousValue
            )
          }),
          (t.prototype.next = function() {
            var t = ++this.previousValue
            return (
              this.writeNewSequenceNumber && this.writeNewSequenceNumber(t), t
            )
          }),
          (t.INVALID = -1),
          t
        )
      })(),
      Ho = (function() {
        return function() {
          var t = this
          this.promise = new Promise(function(e, n) {
            ;(t.resolve = e), (t.reject = n)
          })
        }
      })()
    !(function(t) {
      ;(t.All = 'all'),
        (t.ListenStreamIdle = 'listen_stream_idle'),
        (t.ListenStreamConnectionBackoff = 'listen_stream_connection_backoff'),
        (t.WriteStreamIdle = 'write_stream_idle'),
        (t.WriteStreamConnectionBackoff = 'write_stream_connection_backoff'),
        (t.OnlineStateTimeout = 'online_state_timeout'),
        (t.ClientMetadataRefresh = 'client_metadata_refresh')
    })(Go || (Go = {}))
    var Xo = (function() {
        function t(t, e, n, r, i) {
          ;(this.asyncQueue = t),
            (this.timerId = e),
            (this.targetTimeMs = n),
            (this.op = r),
            (this.removalCallback = i),
            (this.deferred = new Ho()),
            (this.then = this.deferred.promise.then.bind(
              this.deferred.promise,
            )),
            (this.catch = this.deferred.promise.catch.bind(
              this.deferred.promise,
            )),
            this.deferred.promise.catch(function(t) {})
        }
        return (
          (t.createAndSchedule = function(e, n, r, i, o) {
            var s = new t(e, n, Date.now() + r, i, o)
            return s.start(r), s
          }),
          (t.prototype.start = function(t) {
            var e = this
            this.timerHandle = setTimeout(function() {
              return e.handleDelayElapsed()
            }, t)
          }),
          (t.prototype.skipDelay = function() {
            return this.handleDelayElapsed()
          }),
          (t.prototype.cancel = function(t) {
            null !== this.timerHandle &&
              (this.clearTimeout(),
              this.deferred.reject(
                new rr(
                  nr.CANCELLED,
                  'Operation cancelled' + (t ? ': ' + t : ''),
                ),
              ))
          }),
          (t.prototype.handleDelayElapsed = function() {
            var t = this
            this.asyncQueue.enqueueAndForget(function() {
              return null !== t.timerHandle
                ? (t.clearTimeout(),
                  t.op().then(function(e) {
                    return t.deferred.resolve(e)
                  }))
                : Promise.resolve()
            })
          }),
          (t.prototype.clearTimeout = function() {
            null !== this.timerHandle &&
              (this.removalCallback(this),
              clearTimeout(this.timerHandle),
              (this.timerHandle = null))
          }),
          t
        )
      })(),
      Yo = (function() {
        function t() {
          ;(this.tail = Promise.resolve()),
            (this.delayedOperations = []),
            (this.operationInProgress = !1)
        }
        return (
          (t.prototype.enqueueAndForget = function(t) {
            this.enqueue(t)
          }),
          (t.prototype.enqueue = function(t) {
            var e = this
            this.verifyNotFailed()
            var n = this.tail.then(function() {
              return (
                (e.operationInProgress = !0),
                t()
                  .catch(function(t) {
                    ;(e.failure = t), (e.operationInProgress = !1)
                    var n = t.stack || t.message || ''
                    throw (Yn('INTERNAL UNHANDLED ERROR: ', n),
                    n.indexOf('Firestore Test Simulated Error') < 0 &&
                      setTimeout(function() {
                        throw t
                      }, 0),
                    t)
                  })
                  .then(function(t) {
                    return (e.operationInProgress = !1), t
                  })
              )
            })
            return (this.tail = n), n
          }),
          (t.prototype.enqueueAfterDelay = function(t, e, n) {
            var r = this
            this.verifyNotFailed(),
              Zn(
                e >= 0,
                'Attempted to schedule an operation with a negative delay of ' +
                  e,
              ),
              Zn(
                !this.containsDelayedOperation(t),
                'Attempted to schedule multiple operations with timer id ' +
                  t +
                  '.',
              )
            var i = Xo.createAndSchedule(this, t, e, n, function(t) {
              return r.removeDelayedOperation(t)
            })
            return this.delayedOperations.push(i), i
          }),
          (t.prototype.verifyNotFailed = function() {
            this.failure &&
              $n(
                'AsyncQueue is already failed: ' +
                  (this.failure.stack || this.failure.message),
              )
          }),
          (t.prototype.verifyOperationInProgress = function() {
            Zn(
              this.operationInProgress,
              'verifyOpInProgress() called when no op in progress on this queue.',
            )
          }),
          (t.prototype.drain = function() {
            return this.enqueue(function() {
              return Promise.resolve()
            })
          }),
          (t.prototype.containsDelayedOperation = function(t) {
            return (
              this.delayedOperations.findIndex(function(e) {
                return e.timerId === t
              }) >= 0
            )
          }),
          (t.prototype.runDelayedOperationsEarly = function(t) {
            var e = this
            return this.drain().then(function() {
              Zn(
                t === Go.All || e.containsDelayedOperation(t),
                'Attempted to drain to missing operation ' + t,
              ),
                e.delayedOperations.sort(function(t, e) {
                  return t.targetTimeMs - e.targetTimeMs
                })
              for (var n = 0, r = e.delayedOperations; n < r.length; n++) {
                var i = r[n]
                if ((i.skipDelay(), t !== Go.All && i.timerId === t)) break
              }
              return e.drain()
            })
          }),
          (t.prototype.removeDelayedOperation = function(t) {
            var e = this.delayedOperations.indexOf(t)
            Zn(e >= 0, 'Delayed operation not found.'),
              this.delayedOperations.splice(e, 1)
          }),
          t
        )
      })(),
      Jo = '',
      $o = '',
      Zo = '',
      ts = ''
    function es(t) {
      for (var e = '', n = 0; n < t.length; n++)
        e.length > 0 && (e = rs(e)), (e = ns(t.get(n), e))
      return rs(e)
    }
    function ns(t, e) {
      for (var n = e, r = t.length, i = 0; i < r; i++) {
        var o = t.charAt(i)
        switch (o) {
          case '\0':
            n += Jo + Zo
            break
          case Jo:
            n += Jo + ts
            break
          default:
            n += o
        }
      }
      return n
    }
    function rs(t) {
      return t + Jo + $o
    }
    function is(t) {
      var e = t.length
      if ((Zn(e >= 2, 'Invalid path ' + t), 2 === e))
        return (
          Zn(
            t.charAt(0) === Jo && t.charAt(1) === $o,
            'Non-empty path ' + t + ' had length 2',
          ),
          Qr.EMPTY_PATH
        )
      for (var n = e - 2, r = [], i = '', o = 0; o < e; ) {
        var s = t.indexOf(Jo, o)
        switch (
          ((s < 0 || s > n) && $n('Invalid encoded resource path: "' + t + '"'),
          t.charAt(s + 1))
        ) {
          case $o:
            var a = t.substring(o, s),
              u = void 0
            0 === i.length ? (u = a) : ((u = i += a), (i = '')), r.push(u)
            break
          case Zo:
            ;(i += t.substring(o, s)), (i += '\0')
            break
          case ts:
            i += t.substring(o, s + 1)
            break
          default:
            $n('Invalid encoded resource path: "' + t + '"')
        }
        o = s + 2
      }
      return new Qr(r)
    }
    var os = -1,
      ss = (function() {
        function t(t, e, n) {
          ;(this.batchId = t), (this.localWriteTime = e), (this.mutations = n)
        }
        return (
          (t.prototype.applyToRemoteDocument = function(t, e, n) {
            e &&
              Zn(
                e.key.isEqual(t),
                'applyToRemoteDocument: key ' +
                  t +
                  ' should match maybeDoc key\n        ' +
                  e.key,
              )
            var r = n.mutationResults
            Zn(
              r.length === this.mutations.length,
              'Mismatch between mutations length\n      (' +
                this.mutations.length +
                ') and mutation results length\n      (' +
                r.length +
                ').',
            )
            for (var i = 0; i < this.mutations.length; i++) {
              var o = this.mutations[i]
              if (o.key.isEqual(t)) {
                var s = r[i]
                e = o.applyToRemoteDocument(e, s)
              }
            }
            return e
          }),
          (t.prototype.applyToLocalView = function(t, e) {
            e &&
              Zn(
                e.key.isEqual(t),
                'applyToLocalDocument: key ' +
                  t +
                  ' should match maybeDoc key\n        ' +
                  e.key,
              )
            for (var n = e, r = 0; r < this.mutations.length; r++) {
              var i = this.mutations[r]
              i.key.isEqual(t) &&
                (e = i.applyToLocalView(e, n, this.localWriteTime))
            }
            return e
          }),
          (t.prototype.keys = function() {
            for (var t = uo(), e = 0, n = this.mutations; e < n.length; e++) {
              var r = n[e]
              t = t.add(r.key)
            }
            return t
          }),
          (t.prototype.isEqual = function(t) {
            return this.batchId === t.batchId && kr(this.mutations, t.mutations)
          }),
          (t.prototype.isTombstone = function() {
            return 0 === this.mutations.length
          }),
          (t.prototype.toTombstone = function() {
            return new t(this.batchId, this.localWriteTime, [])
          }),
          t
        )
      })(),
      as = (function() {
        function t(t, e, n, r, i) {
          ;(this.batch = t),
            (this.commitVersion = e),
            (this.mutationResults = n),
            (this.streamToken = r),
            (this.docVersions = i)
        }
        return (
          (t.from = function(e, n, r, i) {
            Zn(
              e.mutations.length === r.length,
              'Mutations sent ' +
                e.mutations.length +
                ' must equal results received ' +
                r.length,
            )
            for (var o = so(), s = e.mutations, a = 0; a < s.length; a++)
              o = o.insert(s[a].key, r[a].version)
            return new t(e, n, r, i, o)
          }),
          t
        )
      })(),
      us = (function() {
        function t(t) {
          var e = this
          ;(this.nextCallback = null),
            (this.catchCallback = null),
            (this.result = void 0),
            (this.error = void 0),
            (this.isDone = !1),
            (this.callbackAttached = !1),
            t(
              function(t) {
                ;(e.isDone = !0),
                  (e.result = t),
                  e.nextCallback && e.nextCallback(t)
              },
              function(t) {
                ;(e.isDone = !0),
                  (e.error = t),
                  e.catchCallback && e.catchCallback(t)
              },
            )
        }
        return (
          (t.prototype.catch = function(t) {
            return this.next(void 0, t)
          }),
          (t.prototype.next = function(e, n) {
            var r = this
            return (
              this.callbackAttached &&
                $n('Called next() or catch() twice for PersistencePromise'),
              (this.callbackAttached = !0),
              this.isDone
                ? this.error
                  ? this.wrapFailure(n, this.error)
                  : this.wrapSuccess(e, this.result)
                : new t(function(t, i) {
                    ;(r.nextCallback = function(n) {
                      r.wrapSuccess(e, n).next(t, i)
                    }),
                      (r.catchCallback = function(e) {
                        r.wrapFailure(n, e).next(t, i)
                      })
                  })
            )
          }),
          (t.prototype.toPromise = function() {
            var t = this
            return new Promise(function(e, n) {
              t.next(e, n)
            })
          }),
          (t.prototype.wrapUserFunction = function(e) {
            try {
              var n = e()
              return n instanceof t ? n : t.resolve(n)
            } catch (e) {
              return t.reject(e)
            }
          }),
          (t.prototype.wrapSuccess = function(e, n) {
            return e
              ? this.wrapUserFunction(function() {
                  return e(n)
                })
              : t.resolve(n)
          }),
          (t.prototype.wrapFailure = function(e, n) {
            return e
              ? this.wrapUserFunction(function() {
                  return e(n)
                })
              : t.reject(n)
          }),
          (t.resolve = function(e) {
            return new t(function(t, n) {
              t(e)
            })
          }),
          (t.reject = function(e) {
            return new t(function(t, n) {
              n(e)
            })
          }),
          (t.waitFor = function(e) {
            var n = e[Symbol.iterator]()
            return new t(function(t, e) {
              for (var r = 0, i = 0, o = n.next(); !o.done; )
                ++r,
                  o.value.next(
                    function() {
                      ++i, o.done && i === r && t()
                    },
                    function(t) {
                      return e(t)
                    },
                  ),
                  (o = n.next())
              i === r && t()
            })
          }),
          (t.map = function(e) {
            for (
              var n = [],
                r = [],
                i = e[Symbol.iterator](),
                o = i.next(),
                s = 0,
                a = function() {
                  var t = o.value,
                    e = s
                  r.push(
                    t.next(function(t) {
                      n[e] = t
                    }),
                  ),
                    (o = i.next()),
                    ++s
                };
              !o.done;

            )
              a()
            return t.waitFor(r).next(function() {
              return n
            })
          }),
          (t.or = function(e) {
            for (
              var n = t.resolve(!1),
                r = function(e) {
                  n = n.next(function(n) {
                    return n ? t.resolve(n) : e()
                  })
                },
                i = 0,
                o = e;
              i < o.length;
              i++
            ) {
              r(o[i])
            }
            return n
          }),
          (t.forEach = function(t, e) {
            for (
              var n = t[Symbol.iterator](), r = [], i = n.next();
              !i.done;

            ) {
              var o = i.value
              r.push(e(o)), (i = n.next())
            }
            return this.waitFor(r)
          }),
          t
        )
      })(),
      cs = (function() {
        function t(t) {
          ;(this.mapKeyFn = t), (this.inner = {})
        }
        return (
          (t.prototype.get = function(t) {
            var e = this.mapKeyFn(t),
              n = this.inner[e]
            if (void 0 !== n)
              for (var r = 0, i = n; r < i.length; r++) {
                var o = i[r],
                  s = o[0],
                  a = o[1]
                if (s.isEqual(t)) return a
              }
          }),
          (t.prototype.has = function(t) {
            return void 0 !== this.get(t)
          }),
          (t.prototype.set = function(t, e) {
            var n = this.mapKeyFn(t),
              r = this.inner[n]
            if (void 0 !== r) {
              for (var i = 0; i < r.length; i++)
                if (r[i][0].isEqual(t)) return void (r[i] = [t, e])
              r.push([t, e])
            } else this.inner[n] = [[t, e]]
          }),
          (t.prototype.delete = function(t) {
            var e = this.mapKeyFn(t),
              n = this.inner[e]
            if (void 0 === n) return !1
            for (var r = 0; r < n.length; r++)
              if (n[r][0].isEqual(t))
                return (
                  1 === n.length ? delete this.inner[e] : n.splice(r, 1), !0
                )
            return !1
          }),
          (t.prototype.forEach = function(t) {
            ur(this.inner, function(e, n) {
              for (var r = 0, i = n; r < i.length; r++) {
                var o = i[r],
                  s = o[0],
                  a = o[1]
                t(s, a)
              }
            })
          }),
          (t.prototype.isEmpty = function() {
            return cr(this.inner)
          }),
          (t.prototype[Symbol.iterator] = function() {
            var t = []
            return (
              this.forEach(function(e, n) {
                return t.push({ key: e, value: n })
              }),
              t[Symbol.iterator]()
            )
          }),
          t
        )
      })(),
      hs = (function() {
        function t() {
          ;(this.changes = no()),
            (this.documentSizes = new cs(function(t) {
              return t.toString()
            }))
        }
        return (
          (t.prototype.addEntry = function(t) {
            var e = this.assertChanges()
            this.changes = e.insert(t.key, t)
          }),
          (t.prototype.getEntry = function(t, e) {
            var n = this,
              r = this.assertChanges().get(e)
            return r
              ? us.resolve(r)
              : this.getFromCache(t, e).next(function(t) {
                  return null === t
                    ? (n.documentSizes.set(e, 0), null)
                    : (n.documentSizes.set(e, t.size), t.maybeDocument)
                })
          }),
          (t.prototype.apply = function(t) {
            var e = this.applyChanges(t)
            return (this.changes = null), e
          }),
          (t.prototype.assertChanges = function() {
            return (
              Zn(null !== this.changes, 'Changes have already been applied.'),
              this.changes
            )
          }),
          t
        )
      })(),
      ls = (function() {
        function t(t) {
          this.db = t
        }
        return (
          (t.openOrCreate = function(e, n, r) {
            return (
              Zn(
                t.isAvailable(),
                'IndexedDB not supported in current environment.',
              ),
              Xn('SimpleDb', 'Opening database:', e),
              new us(function(i, o) {
                var s = window.indexedDB.open(e, n)
                ;(s.onsuccess = function(e) {
                  var n = e.target.result
                  i(new t(n))
                }),
                  (s.onblocked = function() {
                    o(
                      new rr(
                        nr.FAILED_PRECONDITION,
                        'Cannot upgrade IndexedDB schema while another tab is open. Close all tabs that access Firestore and reload this page to proceed.',
                      ),
                    )
                  }),
                  (s.onerror = function(t) {
                    o(t.target.error)
                  }),
                  (s.onupgradeneeded = function(t) {
                    Xn(
                      'SimpleDb',
                      'Database "' + e + '" requires upgrade from version:',
                      t.oldVersion,
                    )
                    var n = t.target.result,
                      i = new ds(s.transaction)
                    r.createOrUpgrade(n, i, t.oldVersion, Is).next(function() {
                      Xn(
                        'SimpleDb',
                        'Database upgrade to version ' + Is + ' complete',
                      )
                    })
                  })
              }).toPromise()
            )
          }),
          (t.delete = function(t) {
            return (
              Xn('SimpleDb', 'Removing database:', t),
              ms(window.indexedDB.deleteDatabase(t)).toPromise()
            )
          }),
          (t.isAvailable = function() {
            if ('undefined' == typeof window || null == window.indexedDB)
              return !1
            if (void 0 === window.navigator)
              return 'YES' === process.env.USE_MOCK_PERSISTENCE
            var t = window.navigator.userAgent
            return !(
              t.indexOf('MSIE ') > 0 ||
              t.indexOf('Trident/') > 0 ||
              t.indexOf('Edge/') > 0
            )
          }),
          (t.getStore = function(t, e) {
            return t.store(e)
          }),
          (t.prototype.runTransaction = function(t, e, n) {
            var r = ds.open(this.db, t, e),
              i = n(r)
                .catch(function(t) {
                  r.abort(t)
                })
                .toPromise()
            return r.completionPromise.then(function() {
              return i
            })
          }),
          (t.prototype.close = function() {
            this.db.close()
          }),
          t
        )
      })(),
      fs = (function() {
        function t(t) {
          ;(this.dbCursor = t), (this.shouldStop = !1), (this.nextKey = null)
        }
        return (
          Object.defineProperty(t.prototype, 'isDone', {
            get: function() {
              return this.shouldStop
            },
            enumerable: !0,
            configurable: !0,
          }),
          Object.defineProperty(t.prototype, 'skipToKey', {
            get: function() {
              return this.nextKey
            },
            enumerable: !0,
            configurable: !0,
          }),
          Object.defineProperty(t.prototype, 'cursor', {
            set: function(t) {
              this.dbCursor = t
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.done = function() {
            this.shouldStop = !0
          }),
          (t.prototype.skip = function(t) {
            this.nextKey = t
          }),
          (t.prototype.delete = function() {
            return ms(this.dbCursor.delete())
          }),
          t
        )
      })(),
      ds = (function() {
        function t(t) {
          var e = this
          ;(this.transaction = t),
            (this.aborted = !1),
            (this.completionDeferred = new Ho()),
            (this.transaction.oncomplete = function() {
              e.completionDeferred.resolve()
            }),
            (this.transaction.onabort = function() {
              t.error
                ? e.completionDeferred.reject(t.error)
                : e.completionDeferred.resolve()
            }),
            (this.transaction.onerror = function(t) {
              e.completionDeferred.reject(t.target.error)
            })
        }
        return (
          (t.open = function(e, n, r) {
            return new t(e.transaction(r, n))
          }),
          Object.defineProperty(t.prototype, 'completionPromise', {
            get: function() {
              return this.completionDeferred.promise
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.abort = function(t) {
            t && this.completionDeferred.reject(t),
              this.aborted ||
                (Xn(
                  'SimpleDb',
                  'Aborting transaction:',
                  t ? t.message : 'Client-initiated abort',
                ),
                (this.aborted = !0),
                this.transaction.abort())
          }),
          (t.prototype.store = function(t) {
            var e = this.transaction.objectStore(t)
            return (
              Zn(!!e, 'Object store not part of transaction: ' + t), new ps(e)
            )
          }),
          t
        )
      })(),
      ps = (function() {
        function t(t) {
          this.store = t
        }
        return (
          (t.prototype.put = function(t, e) {
            var n
            return (
              void 0 !== e
                ? (Xn('SimpleDb', 'PUT', this.store.name, t, e),
                  (n = this.store.put(e, t)))
                : (Xn('SimpleDb', 'PUT', this.store.name, '<auto-key>', t),
                  (n = this.store.put(t))),
              ms(n)
            )
          }),
          (t.prototype.add = function(t) {
            return (
              Xn('SimpleDb', 'ADD', this.store.name, t, t),
              ms(this.store.add(t))
            )
          }),
          (t.prototype.get = function(t) {
            var e = this
            return ms(this.store.get(t)).next(function(n) {
              return (
                void 0 === n && (n = null),
                Xn('SimpleDb', 'GET', e.store.name, t, n),
                n
              )
            })
          }),
          (t.prototype.delete = function(t) {
            return (
              Xn('SimpleDb', 'DELETE', this.store.name, t),
              ms(this.store.delete(t))
            )
          }),
          (t.prototype.count = function() {
            return (
              Xn('SimpleDb', 'COUNT', this.store.name), ms(this.store.count())
            )
          }),
          (t.prototype.loadAll = function(t, e) {
            var n = this.cursor(this.options(t, e)),
              r = []
            return this.iterateCursor(n, function(t, e) {
              r.push(e)
            }).next(function() {
              return r
            })
          }),
          (t.prototype.deleteAll = function(t, e) {
            Xn('SimpleDb', 'DELETE ALL', this.store.name)
            var n = this.options(t, e)
            n.keysOnly = !1
            var r = this.cursor(n)
            return this.iterateCursor(r, function(t, e, n) {
              return n.delete()
            })
          }),
          (t.prototype.iterate = function(t, e) {
            var n
            e ? (n = t) : ((n = {}), (e = t))
            var r = this.cursor(n)
            return this.iterateCursor(r, e)
          }),
          (t.prototype.iterateSerial = function(t) {
            var e = this.cursor({})
            return new us(function(n, r) {
              ;(e.onerror = function(t) {
                r(t.target.error)
              }),
                (e.onsuccess = function(e) {
                  var r = e.target.result
                  r
                    ? t(r.primaryKey, r.value).next(function(t) {
                        t ? r.continue() : n()
                      })
                    : n()
                })
            })
          }),
          (t.prototype.iterateCursor = function(t, e) {
            var n = []
            return new us(function(r, i) {
              ;(t.onerror = function(t) {
                i(t.target.error)
              }),
                (t.onsuccess = function(t) {
                  var i = t.target.result
                  if (i) {
                    var o = new fs(i),
                      s = e(i.primaryKey, i.value, o)
                    if (s instanceof us) {
                      var a = s.catch(function(t) {
                        return o.done(), us.reject(t)
                      })
                      n.push(a)
                    }
                    o.isDone
                      ? r()
                      : null === o.skipToKey
                        ? i.continue()
                        : i.continue(o.skipToKey)
                  } else r()
                })
            }).next(function() {
              return us.waitFor(n)
            })
          }),
          (t.prototype.options = function(t, e) {
            var n = void 0
            return (
              void 0 !== t &&
                ('string' == typeof t
                  ? (n = t)
                  : (Zn(
                      void 0 === e,
                      '3rd argument must not be defined if 2nd is a range.',
                    ),
                    (e = t))),
              { index: n, range: e }
            )
          }),
          (t.prototype.cursor = function(t) {
            var e = 'next'
            if ((t.reverse && (e = 'prev'), t.index)) {
              var n = this.store.index(t.index)
              return t.keysOnly
                ? n.openKeyCursor(t.range, e)
                : n.openCursor(t.range, e)
            }
            return this.store.openCursor(t.range, e)
          }),
          t
        )
      })()
    function ms(t) {
      return new us(function(e, n) {
        ;(t.onsuccess = function(t) {
          var n = t.target.result
          e(n)
        }),
          (t.onerror = function(t) {
            n(t.target.error)
          })
      })
    }
    var ys =
        'The remote document changelog no longer contains all changes for all local query views. It may be necessary to rebuild these views.',
      gs = (function() {
        function t(t, e) {
          ;(this.serializer = t),
            (this.keepDocumentChangeLog = e),
            (this._lastProcessedDocumentChangeId = 0)
        }
        return (
          Object.defineProperty(t.prototype, 'lastProcessedDocumentChangeId', {
            get: function() {
              return this._lastProcessedDocumentChangeId
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.start = function(t) {
            var e = ls.getStore(t, Vs.store)
            return this.synchronizeLastDocumentChangeId(e)
          }),
          (t.prototype.addEntries = function(t, e, n) {
            var r = []
            if (e.length > 0) {
              for (var i = ws(t), o = uo(), s = 0, a = e; s < a.length; s++) {
                var u = a[s],
                  c = u.key,
                  h = u.doc
                r.push(i.put(Ss(c), h)), (o = o.add(c))
              }
              this.keepDocumentChangeLog &&
                r.push(
                  Ts(t).put({ changes: this.serializer.toDbResourcePaths(o) }),
                ),
                r.push(this.updateSize(t, n))
            }
            return us.waitFor(r)
          }),
          (t.prototype.removeEntry = function(t, e) {
            var n = ws(t),
              r = Ss(e)
            return n.get(r).next(function(t) {
              return t
                ? n.delete(r).next(function() {
                    return Es(t)
                  })
                : us.resolve(0)
            })
          }),
          (t.prototype.getEntry = function(t, e) {
            var n = this
            return ws(t)
              .get(Ss(e))
              .next(function(t) {
                return t ? n.serializer.fromDbRemoteDocument(t) : null
              })
          }),
          (t.prototype.getSizedEntry = function(t, e) {
            var n = this
            return ws(t)
              .get(Ss(e))
              .next(function(t) {
                return t
                  ? {
                      maybeDocument: n.serializer.fromDbRemoteDocument(t),
                      size: Es(t),
                    }
                  : null
              })
          }),
          (t.prototype.getDocumentsMatchingQuery = function(t, e) {
            var n = this,
              r = io(),
              i = e.path.toArray(),
              o = IDBKeyRange.lowerBound(i)
            return ws(t)
              .iterate({ range: o }, function(t, i, o) {
                var s = n.serializer.fromDbRemoteDocument(i)
                e.path.isPrefixOf(s.key.path)
                  ? s instanceof zr && e.matches(s) && (r = r.insert(s.key, s))
                  : o.done()
              })
              .next(function() {
                return r
              })
          }),
          (t.prototype.getNewDocumentChanges = function(t) {
            var e = this
            Zn(
              this.keepDocumentChangeLog,
              'Can only call getNewDocumentChanges() when document change log is enabled',
            )
            var n = uo(),
              r = no(),
              i = IDBKeyRange.lowerBound(
                this._lastProcessedDocumentChangeId + 1,
              ),
              o = !0,
              s = Ts(t)
            return s
              .iterate({ range: i }, function(t, r) {
                if (
                  o &&
                  ((o = !1), e._lastProcessedDocumentChangeId + 1 !== r.id)
                )
                  return e.synchronizeLastDocumentChangeId(s).next(function() {
                    return us.reject(new rr(nr.DATA_LOSS, ys))
                  })
                ;(n = n.unionWith(e.serializer.fromDbResourcePaths(r.changes))),
                  (e._lastProcessedDocumentChangeId = r.id)
              })
              .next(function() {
                var i = []
                return (
                  n.forEach(function(n) {
                    i.push(
                      e.getEntry(t, n).next(function(t) {
                        var e = t || new Hr(n, _i.forDeletedDoc())
                        r = r.insert(n, e)
                      }),
                    )
                  }),
                  us.waitFor(i)
                )
              })
              .next(function() {
                return r
              })
          }),
          (t.prototype.removeDocumentChangesThroughChangeId = function(t, e) {
            var n = IDBKeyRange.upperBound(e)
            return Ts(t).delete(n)
          }),
          (t.prototype.synchronizeLastDocumentChangeId = function(t) {
            var e = this
            return (
              (this._lastProcessedDocumentChangeId = 0),
              t.iterate({ keysOnly: !0, reverse: !0 }, function(t, n, r) {
                ;(e._lastProcessedDocumentChangeId = t), r.done()
              })
            )
          }),
          (t.prototype.newChangeBuffer = function() {
            return new bs(this)
          }),
          (t.prototype.getSize = function(t) {
            return this.getMetadata(t).next(function(t) {
              return t.byteSize
            })
          }),
          (t.prototype.getMetadata = function(t) {
            return vs(t)
              .get(_s.key)
              .next(function(t) {
                return Zn(!!t, 'Missing document cache metadata'), t
              })
          }),
          (t.prototype.setMetadata = function(t, e) {
            return vs(t).put(_s.key, e)
          }),
          (t.prototype.updateSize = function(t, e) {
            var n = this
            return this.getMetadata(t).next(function(r) {
              return (r.byteSize += e), n.setMetadata(t, r)
            })
          }),
          t
        )
      })()
    function vs(t) {
      return fa.getStore(t, _s.store)
    }
    var bs = (function(t) {
      function e(e) {
        var n = t.call(this) || this
        return (n.documentCache = e), n
      }
      return (
        a(e, t),
        (e.prototype.applyChanges = function(t) {
          var e = this,
            n = 0,
            r = []
          return (
            this.assertChanges().forEach(function(t, i) {
              var o = e.documentCache.serializer.toDbRemoteDocument(i),
                s = e.documentSizes.get(t)
              Zn(
                void 0 !== s,
                'Attempting to change document ' +
                  t.toString() +
                  ' without having read it first',
              )
              var a = Es(o)
              ;(n += a - s), r.push({ key: t, doc: o })
            }),
            this.documentCache.addEntries(t, r, n)
          )
        }),
        (e.prototype.getFromCache = function(t, e) {
          return this.documentCache.getSizedEntry(t, e)
        }),
        e
      )
    })(hs)
    function ws(t) {
      return fa.getStore(t, Ps.store)
    }
    function Ts(t) {
      return fa.getStore(t, Vs.store)
    }
    function Ss(t) {
      return t.path.toArray()
    }
    function Es(t) {
      var e
      if (t.document) e = t.document
      else if (t.unknownDocument) e = t.unknownDocument
      else {
        if (!t.noDocument) throw $n('Unknown remote document type')
        e = t.noDocument
      }
      return JSON.stringify(e).length
    }
    var Is = 6,
      Cs = (function() {
        function t(t) {
          this.serializer = t
        }
        return (
          (t.prototype.createOrUpgrade = function(t, e, n, r) {
            var i = this
            Zn(
              n < r && n >= 0 && r <= Is,
              'Unexpected schema upgrade from v' + n + ' to v{toVersion}.',
            ),
              n < 1 &&
                r >= 1 &&
                ((function(t) {
                  t.createObjectStore(Ns.store)
                })(t),
                (function(t) {
                  t.createObjectStore(As.store, { keyPath: As.keyPath }),
                    t
                      .createObjectStore(ks.store, {
                        keyPath: ks.keyPath,
                        autoIncrement: !0,
                      })
                      .createIndex(
                        ks.userMutationsIndex,
                        ks.userMutationsKeyPath,
                        { unique: !0 },
                      ),
                    t.createObjectStore(Rs.store)
                })(t),
                Fs(t),
                (function(t) {
                  t.createObjectStore(Ps.store)
                })(t))
            var o = us.resolve()
            return (
              n < 3 &&
                r >= 3 &&
                (0 !== n &&
                  (!(function(t) {
                    t.deleteObjectStore(xs.store),
                      t.deleteObjectStore(Ls.store),
                      t.deleteObjectStore(qs.store)
                  })(t),
                  Fs(t)),
                (o = o.next(function() {
                  return (function(t) {
                    var e = t.store(qs.store),
                      n = new qs(0, 0, _i.MIN.toTimestamp(), 0)
                    return e.put(qs.key, n)
                  })(e)
                }))),
              n < 4 &&
                r >= 4 &&
                (0 !== n &&
                  (o = o.next(function() {
                    return (function(t, e) {
                      return e
                        .store(ks.store)
                        .loadAll()
                        .next(function(n) {
                          t.deleteObjectStore(ks.store)
                          var r = t.createObjectStore(ks.store, {
                            keyPath: ks.keyPath,
                            autoIncrement: !0,
                          })
                          r.createIndex(
                            ks.userMutationsIndex,
                            ks.userMutationsKeyPath,
                            { unique: !0 },
                          )
                          var i = e.store(ks.store),
                            o = n.map(function(t) {
                              return i.put(t)
                            })
                          return us.waitFor(o)
                        })
                    })(t, e)
                  })),
                (o = o.next(function() {
                  !(function(t) {
                    t.createObjectStore(Bs.store, { keyPath: Bs.keyPath })
                  })(t),
                    (function(t) {
                      t.createObjectStore(Vs.store, {
                        keyPath: 'id',
                        autoIncrement: !0,
                      })
                    })(t)
                }))),
              n < 5 &&
                r >= 5 &&
                (o = o.next(function() {
                  return i.removeAcknowledgedMutations(e)
                })),
              n < 6 &&
                r >= 6 &&
                (o = o.next(function() {
                  return (
                    (function(t) {
                      t.createObjectStore(_s.store)
                    })(t),
                    i.addDocumentGlobal(e)
                  )
                })),
              o
            )
          }),
          (t.prototype.addDocumentGlobal = function(t) {
            var e = 0
            return t
              .store(Ps.store)
              .iterate(function(t, n) {
                e += Es(n)
              })
              .next(function() {
                var n = new _s(e)
                return t.store(_s.store).put(_s.key, n)
              })
          }),
          (t.prototype.removeAcknowledgedMutations = function(t) {
            var e = this,
              n = t.store(As.store),
              r = t.store(ks.store)
            return n.loadAll().next(function(n) {
              return us.forEach(n, function(n) {
                var i = IDBKeyRange.bound(
                  [n.userId, os],
                  [n.userId, n.lastAcknowledgedBatchId],
                )
                return r.loadAll(ks.userMutationsIndex, i).next(function(r) {
                  return us.forEach(r, function(r) {
                    Zn(
                      r.userId === n.userId,
                      'Cannot process batch ' +
                        r.batchId +
                        ' from unexpected user',
                    )
                    var i = e.serializer.fromDbMutationBatch(r)
                    return Ws(t, n.userId, i).next()
                  })
                })
              })
            })
          }),
          t
        )
      })(),
      Ds = (function() {
        return function(t, e) {
          ;(this.seconds = t), (this.nanoseconds = e)
        }
      })(),
      Ns = (function() {
        function t(t, e, n) {
          ;(this.ownerId = t),
            (this.allowTabSynchronization = e),
            (this.leaseTimestampMs = n)
        }
        return (t.store = 'owner'), (t.key = 'owner'), t
      })()
    var As = (function() {
        function t(t, e, n) {
          ;(this.userId = t),
            (this.lastAcknowledgedBatchId = e),
            (this.lastStreamToken = n)
        }
        return (t.store = 'mutationQueues'), (t.keyPath = 'userId'), t
      })(),
      ks = (function() {
        function t(t, e, n, r) {
          ;(this.userId = t),
            (this.batchId = e),
            (this.localWriteTimeMs = n),
            (this.mutations = r)
        }
        return (
          (t.store = 'mutations'),
          (t.keyPath = 'batchId'),
          (t.userMutationsIndex = 'userMutationsIndex'),
          (t.userMutationsKeyPath = ['userId', 'batchId']),
          t
        )
      })()
    var Rs = (function() {
      function t() {}
      return (
        (t.prefixForUser = function(t) {
          return [t]
        }),
        (t.prefixForPath = function(t, e) {
          return [t, es(e)]
        }),
        (t.key = function(t, e, n) {
          return [t, es(e), n]
        }),
        (t.store = 'documentMutations'),
        (t.PLACEHOLDER = new t()),
        t
      )
    })()
    var Ms = (function() {
        return function(t, e) {
          ;(this.path = t), (this.readTime = e)
        }
      })(),
      Os = (function() {
        return function(t, e) {
          ;(this.path = t), (this.version = e)
        }
      })(),
      Ps = (function() {
        function t(t, e, n, r) {
          ;(this.unknownDocument = t),
            (this.noDocument = e),
            (this.document = n),
            (this.hasCommittedMutations = r)
        }
        return (t.store = 'remoteDocuments'), t
      })(),
      _s = (function() {
        function t(t) {
          this.byteSize = t
        }
        return (
          (t.store = 'remoteDocumentGlobal'),
          (t.key = 'remoteDocumentGlobalKey'),
          t
        )
      })()
    var Ls = (function() {
        function t(t, e, n, r, i, o) {
          ;(this.targetId = t),
            (this.canonicalId = e),
            (this.readTime = n),
            (this.resumeToken = r),
            (this.lastListenSequenceNumber = i),
            (this.query = o)
        }
        return (
          (t.store = 'targets'),
          (t.keyPath = 'targetId'),
          (t.queryTargetsIndexName = 'queryTargetsIndex'),
          (t.queryTargetsKeyPath = ['canonicalId', 'targetId']),
          t
        )
      })(),
      xs = (function() {
        function t(t, e, n) {
          ;(this.targetId = t),
            (this.path = e),
            (this.sequenceNumber = n),
            Zn(
              (0 === t) == (void 0 !== n),
              'A target-document row must either have targetId == 0 and a defined sequence number, or a non-zero targetId and no sequence number',
            )
        }
        return (
          (t.store = 'targetDocuments'),
          (t.keyPath = ['targetId', 'path']),
          (t.documentTargetsIndex = 'documentTargetsIndex'),
          (t.documentTargetsKeyPath = ['path', 'targetId']),
          t
        )
      })(),
      qs = (function() {
        function t(t, e, n, r) {
          ;(this.highestTargetId = t),
            (this.highestListenSequenceNumber = e),
            (this.lastRemoteSnapshotVersion = n),
            (this.targetCount = r)
        }
        return (t.key = 'targetGlobalKey'), (t.store = 'targetGlobal'), t
      })()
    function Fs(t) {
      t
        .createObjectStore(xs.store, { keyPath: xs.keyPath })
        .createIndex(xs.documentTargetsIndex, xs.documentTargetsKeyPath, {
          unique: !0,
        }),
        t
          .createObjectStore(Ls.store, { keyPath: Ls.keyPath })
          .createIndex(Ls.queryTargetsIndexName, Ls.queryTargetsKeyPath, {
            unique: !0,
          }),
        t.createObjectStore(qs.store)
    }
    var Vs = (function() {
      function t(t) {
        this.changes = t
      }
      return (t.store = 'remoteDocumentChanges'), (t.keyPath = 'id'), t
    })()
    var Bs = (function() {
      function t(t, e, n, r, i) {
        ;(this.clientId = t),
          (this.updateTimeMs = e),
          (this.networkEnabled = n),
          (this.inForeground = r),
          (this.lastProcessedDocumentChangeId = i)
      }
      return (t.store = 'clientMetadata'), (t.keyPath = 'clientId'), t
    })()
    var Us = [
        As.store,
        ks.store,
        Rs.store,
        Ps.store,
        Ls.store,
        Ns.store,
        qs.store,
        xs.store,
      ]
        .concat([Bs.store, Vs.store])
        .concat([_s.store]),
      Qs = (function() {
        function t(t, e, n) {
          ;(this.userId = t),
            (this.serializer = e),
            (this.referenceDelegate = n),
            (this.documentKeysByBatchId = {})
        }
        return (
          (t.forUser = function(e, n, r) {
            return (
              Zn('' !== e.uid, 'UserID must not be an empty string.'),
              new t(e.isAuthenticated() ? e.uid : '', n, r)
            )
          }),
          (t.prototype.checkEmpty = function(t) {
            var e = !0,
              n = IDBKeyRange.bound(
                [this.userId, Number.NEGATIVE_INFINITY],
                [this.userId, Number.POSITIVE_INFINITY],
              )
            return Gs(t)
              .iterate({ index: ks.userMutationsIndex, range: n }, function(
                t,
                n,
                r,
              ) {
                ;(e = !1), r.done()
              })
              .next(function() {
                return e
              })
          }),
          (t.prototype.acknowledgeBatch = function(t, e, n) {
            return this.getMutationQueueMetadata(t).next(function(r) {
              var i = e.batchId
              return (
                Zn(
                  i > r.lastAcknowledgedBatchId,
                  'Mutation batchIDs must be acknowledged in order',
                ),
                (r.lastAcknowledgedBatchId = i),
                (r.lastStreamToken = js(n)),
                Hs(t).put(r)
              )
            })
          }),
          (t.prototype.getLastStreamToken = function(t) {
            return this.getMutationQueueMetadata(t).next(function(t) {
              return t.lastStreamToken
            })
          }),
          (t.prototype.setLastStreamToken = function(t, e) {
            return this.getMutationQueueMetadata(t).next(function(n) {
              return (n.lastStreamToken = js(e)), Hs(t).put(n)
            })
          }),
          (t.prototype.addMutationBatch = function(t, e, n) {
            var r = this,
              i = zs(t),
              o = Gs(t)
            return o.add({}).next(function(t) {
              Zn('number' == typeof t, 'Auto-generated key is not a number')
              var s = new ss(t, e, n),
                a = r.serializer.toDbMutationBatch(r.userId, s)
              r.documentKeysByBatchId[t] = s.keys()
              for (var u = [], c = 0, h = n; c < h.length; c++) {
                var l = h[c],
                  f = Rs.key(r.userId, l.key.path, t)
                u.push(o.put(a)), u.push(i.put(f, Rs.PLACEHOLDER))
              }
              return us.waitFor(u).next(function() {
                return s
              })
            })
          }),
          (t.prototype.lookupMutationBatch = function(t, e) {
            var n = this
            return Gs(t)
              .get(e)
              .next(function(t) {
                return t
                  ? (Zn(
                      t.userId === n.userId,
                      "Unexpected user '" +
                        t.userId +
                        "' for mutation batch " +
                        e,
                    ),
                    n.serializer.fromDbMutationBatch(t))
                  : null
              })
          }),
          (t.prototype.lookupMutationKeys = function(t, e) {
            var n = this
            return this.documentKeysByBatchId[e]
              ? us.resolve(this.documentKeysByBatchId[e])
              : this.lookupMutationBatch(t, e).next(function(t) {
                  if (t) {
                    var r = t.keys()
                    return (n.documentKeysByBatchId[e] = r), r
                  }
                  return null
                })
          }),
          (t.prototype.getNextMutationBatchAfterBatchId = function(t, e) {
            var n = this
            return this.getMutationQueueMetadata(t).next(function(r) {
              var i = Math.max(e, r.lastAcknowledgedBatchId) + 1,
                o = IDBKeyRange.lowerBound([n.userId, i]),
                s = null
              return Gs(t)
                .iterate({ index: ks.userMutationsIndex, range: o }, function(
                  t,
                  e,
                  r,
                ) {
                  e.userId === n.userId &&
                    (Zn(
                      e.batchId >= i,
                      'Should have found mutation after ' + i,
                    ),
                    (s = n.serializer.fromDbMutationBatch(e))),
                    r.done()
                })
                .next(function() {
                  return s
                })
            })
          }),
          (t.prototype.getAllMutationBatches = function(t) {
            var e = this,
              n = IDBKeyRange.bound(
                [this.userId, os],
                [this.userId, Number.POSITIVE_INFINITY],
              )
            return Gs(t)
              .loadAll(ks.userMutationsIndex, n)
              .next(function(t) {
                return t.map(function(t) {
                  return e.serializer.fromDbMutationBatch(t)
                })
              })
          }),
          (t.prototype.getAllMutationBatchesAffectingDocumentKey = function(
            t,
            e,
          ) {
            var n = this,
              r = Rs.prefixForPath(this.userId, e.path),
              i = IDBKeyRange.lowerBound(r),
              o = []
            return zs(t)
              .iterate({ range: i }, function(r, i, s) {
                var a = r[0],
                  u = r[1],
                  c = r[2],
                  h = is(u)
                if (a === n.userId && e.path.isEqual(h))
                  return Gs(t)
                    .get(c)
                    .next(function(t) {
                      if (!t)
                        throw $n(
                          'Dangling document-mutation reference found: ' +
                            r +
                            ' which points to ' +
                            c,
                        )
                      Zn(
                        t.userId === n.userId,
                        "Unexpected user '" +
                          t.userId +
                          "' for mutation batch " +
                          c,
                      ),
                        o.push(n.serializer.fromDbMutationBatch(t))
                    })
                s.done()
              })
              .next(function() {
                return o
              })
          }),
          (t.prototype.getAllMutationBatchesAffectingDocumentKeys = function(
            t,
            e,
          ) {
            var n = this,
              r = new to(Ar),
              i = []
            return (
              e.forEach(function(e) {
                var o = Rs.prefixForPath(n.userId, e.path),
                  s = IDBKeyRange.lowerBound(o),
                  a = zs(t).iterate({ range: s }, function(t, i, o) {
                    var s = t[0],
                      a = t[1],
                      u = t[2],
                      c = is(a)
                    s === n.userId && e.path.isEqual(c)
                      ? (r = r.add(u))
                      : o.done()
                  })
                i.push(a)
              }),
              us.waitFor(i).next(function() {
                return n.lookupMutationBatches(t, r)
              })
            )
          }),
          (t.prototype.getAllMutationBatchesAffectingQuery = function(t, e) {
            var n = this
            Zn(
              !e.isDocumentQuery(),
              "Document queries shouldn't go down this path",
            )
            var r = e.path,
              i = r.length + 1,
              o = Rs.prefixForPath(this.userId, r),
              s = IDBKeyRange.lowerBound(o),
              a = new to(Ar)
            return zs(t)
              .iterate({ range: s }, function(t, e, o) {
                var s = t[0],
                  u = t[1],
                  c = t[2],
                  h = is(u)
                s === n.userId && r.isPrefixOf(h)
                  ? h.length === i && (a = a.add(c))
                  : o.done()
              })
              .next(function() {
                return n.lookupMutationBatches(t, a)
              })
          }),
          (t.prototype.lookupMutationBatches = function(t, e) {
            var n = this,
              r = [],
              i = []
            return (
              e.forEach(function(e) {
                i.push(
                  Gs(t)
                    .get(e)
                    .next(function(t) {
                      if (null === t)
                        throw $n(
                          'Dangling document-mutation reference found, which points to ' +
                            e,
                        )
                      Zn(
                        t.userId === n.userId,
                        "Unexpected user '" +
                          t.userId +
                          "' for mutation batch " +
                          e,
                      ),
                        r.push(n.serializer.fromDbMutationBatch(t))
                    }),
                )
              }),
              us.waitFor(i).next(function() {
                return r
              })
            )
          }),
          (t.prototype.removeMutationBatch = function(t, e) {
            var n = this
            return Ws(t.simpleDbTransaction, this.userId, e).next(function(r) {
              return (
                n.removeCachedMutationKeys(e.batchId),
                us.forEach(r, function(e) {
                  return n.referenceDelegate.removeMutationReference(t, e)
                })
              )
            })
          }),
          (t.prototype.removeCachedMutationKeys = function(t) {
            delete this.documentKeysByBatchId[t]
          }),
          (t.prototype.performConsistencyCheck = function(t) {
            var e = this
            return this.checkEmpty(t).next(function(n) {
              if (!n) return us.resolve()
              var r = IDBKeyRange.lowerBound(Rs.prefixForUser(e.userId)),
                i = []
              return zs(t)
                .iterate({ range: r }, function(t, n, r) {
                  if (t[0] === e.userId) {
                    var o = is(t[1])
                    i.push(o)
                  } else r.done()
                })
                .next(function() {
                  Zn(
                    0 === i.length,
                    'Document leak -- detected dangling mutation references when queue is empty. Dangling keys: ' +
                      i.map(function(t) {
                        return t.canonicalString()
                      }),
                  )
                })
            })
          }),
          (t.prototype.containsKey = function(t, e) {
            return Ks(t, this.userId, e)
          }),
          (t.prototype.getMutationQueueMetadata = function(t) {
            var e = this
            return Hs(t)
              .get(this.userId)
              .next(function(t) {
                return t || new As(e.userId, os, '')
              })
          }),
          t
        )
      })()
    function Ks(t, e, n) {
      var r = Rs.prefixForPath(e, n.path),
        i = r[1],
        o = IDBKeyRange.lowerBound(r),
        s = !1
      return zs(t)
        .iterate({ range: o, keysOnly: !0 }, function(t, n, r) {
          var o = t[0],
            a = t[1]
          t[2]
          o === e && a === i && (s = !0), r.done()
        })
        .next(function() {
          return s
        })
    }
    function Ws(t, e, n) {
      var r = t.store(ks.store),
        i = t.store(Rs.store),
        o = [],
        s = IDBKeyRange.only(n.batchId),
        a = 0,
        u = r.iterate({ range: s }, function(t, e, n) {
          return a++, n.delete()
        })
      o.push(
        u.next(function() {
          Zn(
            1 === a,
            'Dangling document-mutation reference found: Missing batch ' +
              n.batchId,
          )
        }),
      )
      for (var c = [], h = 0, l = n.mutations; h < l.length; h++) {
        var f = l[h],
          d = Rs.key(e, f.key.path, n.batchId)
        o.push(i.delete(d)), c.push(f.key)
      }
      return us.waitFor(o).next(function() {
        return c
      })
    }
    function js(t) {
      return t instanceof Uint8Array
        ? (Zn(
            'YES' === process.env.USE_MOCK_PERSISTENCE,
            'Persisting non-string stream tokens is only supported with mock persistence.',
          ),
          t.toString())
        : t
    }
    function Gs(t) {
      return fa.getStore(t, ks.store)
    }
    function zs(t) {
      return fa.getStore(t, Rs.store)
    }
    function Hs(t) {
      return fa.getStore(t, As.store)
    }
    var Xs,
      Ys = 1
    !(function(t) {
      ;(t[(t.QueryCache = 0)] = 'QueryCache'),
        (t[(t.SyncEngine = 1)] = 'SyncEngine')
    })(Xs || (Xs = {}))
    var Js = (function() {
        function t(t, e) {
          ;(this.generatorId = t),
            Zn(
              (t & Ys) === t,
              'Generator ID ' +
                t +
                ' contains more than ' +
                Ys +
                ' reserved bits',
            ),
            this.seek(void 0 !== e ? e : this.generatorId)
        }
        return (
          (t.prototype.next = function() {
            var t = this.nextId
            return (this.nextId += 1 << Ys), t
          }),
          (t.prototype.after = function(t) {
            return this.seek(t + (1 << Ys)), this.next()
          }),
          (t.prototype.seek = function(t) {
            Zn(
              (t & Ys) === this.generatorId,
              'Cannot supply target ID from different generator ID',
            ),
              (this.nextId = t)
          }),
          (t.forQueryCache = function() {
            return new t(Xs.QueryCache, 2)
          }),
          (t.forSyncEngine = function() {
            return new t(Xs.SyncEngine)
          }),
          t
        )
      })(),
      $s = (function() {
        function t(t, e) {
          ;(this.referenceDelegate = t),
            (this.serializer = e),
            (this.targetIdGenerator = Js.forQueryCache())
        }
        return (
          (t.prototype.allocateTargetId = function(t) {
            var e = this
            return this.retrieveMetadata(t).next(function(n) {
              return (
                (n.highestTargetId = e.targetIdGenerator.after(
                  n.highestTargetId,
                )),
                e.saveMetadata(t, n).next(function() {
                  return n.highestTargetId
                })
              )
            })
          }),
          (t.prototype.getLastRemoteSnapshotVersion = function(t) {
            return this.retrieveMetadata(t).next(function(t) {
              return _i.fromTimestamp(
                new qr(
                  t.lastRemoteSnapshotVersion.seconds,
                  t.lastRemoteSnapshotVersion.nanoseconds,
                ),
              )
            })
          }),
          (t.prototype.getHighestSequenceNumber = function(t) {
            return ea(t.simpleDbTransaction)
          }),
          (t.prototype.setTargetsMetadata = function(t, e, n) {
            var r = this
            return this.retrieveMetadata(t).next(function(i) {
              return (
                (i.highestListenSequenceNumber = e),
                n && (i.lastRemoteSnapshotVersion = n.toTimestamp()),
                e > i.highestListenSequenceNumber &&
                  (i.highestListenSequenceNumber = e),
                r.saveMetadata(t, i)
              )
            })
          }),
          (t.prototype.addQueryData = function(t, e) {
            var n = this
            return this.saveQueryData(t, e).next(function() {
              return n.retrieveMetadata(t).next(function(r) {
                return (
                  (r.targetCount += 1),
                  n.updateMetadataFromQueryData(e, r),
                  n.saveMetadata(t, r)
                )
              })
            })
          }),
          (t.prototype.updateQueryData = function(t, e) {
            return this.saveQueryData(t, e)
          }),
          (t.prototype.removeQueryData = function(t, e) {
            var n = this
            return this.removeMatchingKeysForTargetId(t, e.targetId)
              .next(function() {
                return Zs(t).delete(e.targetId)
              })
              .next(function() {
                return n.retrieveMetadata(t)
              })
              .next(function(e) {
                return (
                  Zn(e.targetCount > 0, 'Removing from an empty query cache'),
                  (e.targetCount -= 1),
                  n.saveMetadata(t, e)
                )
              })
          }),
          (t.prototype.removeTargets = function(t, e, n) {
            var r = this,
              i = 0,
              o = []
            return Zs(t)
              .iterate(function(s, a) {
                var u = r.serializer.fromDbTarget(a)
                u.sequenceNumber <= e &&
                  void 0 === n[u.targetId] &&
                  (i++, o.push(r.removeQueryData(t, u)))
              })
              .next(function() {
                return us.waitFor(o)
              })
              .next(function() {
                return i
              })
          }),
          (t.prototype.forEachTarget = function(t, e) {
            var n = this
            return Zs(t).iterate(function(t, r) {
              var i = n.serializer.fromDbTarget(r)
              e(i)
            })
          }),
          (t.prototype.retrieveMetadata = function(t) {
            return ta(t.simpleDbTransaction)
          }),
          (t.prototype.saveMetadata = function(t, e) {
            return ((n = t), fa.getStore(n, qs.store)).put(qs.key, e)
            var n
          }),
          (t.prototype.saveQueryData = function(t, e) {
            return Zs(t).put(this.serializer.toDbTarget(e))
          }),
          (t.prototype.updateMetadataFromQueryData = function(t, e) {
            var n = !1
            return (
              t.targetId > e.highestTargetId &&
                ((e.highestTargetId = t.targetId), (n = !0)),
              t.sequenceNumber > e.highestListenSequenceNumber &&
                ((e.highestListenSequenceNumber = t.sequenceNumber), (n = !0)),
              n
            )
          }),
          (t.prototype.getQueryCount = function(t) {
            return this.retrieveMetadata(t).next(function(t) {
              return t.targetCount
            })
          }),
          (t.prototype.getQueryData = function(t, e) {
            var n = this,
              r = e.canonicalId(),
              i = IDBKeyRange.bound(
                [r, Number.NEGATIVE_INFINITY],
                [r, Number.POSITIVE_INFINITY],
              ),
              o = null
            return Zs(t)
              .iterate({ range: i, index: Ls.queryTargetsIndexName }, function(
                t,
                r,
                i,
              ) {
                var s = n.serializer.fromDbTarget(r)
                e.isEqual(s.query) && ((o = s), i.done())
              })
              .next(function() {
                return o
              })
          }),
          (t.prototype.addMatchingKeys = function(t, e, n) {
            var r = this,
              i = [],
              o = na(t)
            return (
              e.forEach(function(e) {
                var s = es(e.path)
                i.push(o.put(new xs(n, s))),
                  i.push(r.referenceDelegate.addReference(t, e))
              }),
              us.waitFor(i)
            )
          }),
          (t.prototype.removeMatchingKeys = function(t, e, n) {
            var r = this,
              i = na(t)
            return us.forEach(e, function(e) {
              var o = es(e.path)
              return us.waitFor([
                i.delete([n, o]),
                r.referenceDelegate.removeReference(t, e),
              ])
            })
          }),
          (t.prototype.removeMatchingKeysForTargetId = function(t, e) {
            var n = na(t),
              r = IDBKeyRange.bound([e], [e + 1], !1, !0)
            return n.delete(r)
          }),
          (t.prototype.getMatchingKeysForTargetId = function(t, e) {
            var n = IDBKeyRange.bound([e], [e + 1], !1, !0),
              r = na(t),
              i = uo()
            return r
              .iterate({ range: n, keysOnly: !0 }, function(t, e, n) {
                var r = is(t[1]),
                  o = new jr(r)
                i = i.add(o)
              })
              .next(function() {
                return i
              })
          }),
          (t.prototype.containsKey = function(t, e) {
            var n,
              r = es(e.path),
              i = IDBKeyRange.bound([r], [((n = r), n + '\0')], !1, !0),
              o = 0
            return na(t)
              .iterate(
                { index: xs.documentTargetsIndex, keysOnly: !0, range: i },
                function(t, e, n) {
                  var r = t[0]
                  t[1]
                  0 !== r && (o++, n.done())
                },
              )
              .next(function() {
                return o > 0
              })
          }),
          (t.prototype.getQueryDataForTarget = function(t, e) {
            var n = this
            return Zs(t)
              .get(e)
              .next(function(t) {
                return t ? n.serializer.fromDbTarget(t) : null
              })
          }),
          t
        )
      })()
    function Zs(t) {
      return fa.getStore(t, Ls.store)
    }
    function ta(t) {
      return ls
        .getStore(t, qs.store)
        .get(qs.key)
        .next(function(t) {
          return Zn(null !== t, 'Missing metadata row.'), t
        })
    }
    function ea(t) {
      return ta(t).next(function(t) {
        return t.highestListenSequenceNumber
      })
    }
    function na(t) {
      return fa.getStore(t, xs.store)
    }
    var ra = (function() {
      function t(t) {
        this.remoteSerializer = t
      }
      return (
        (t.prototype.fromDbRemoteDocument = function(t) {
          if (t.document)
            return this.remoteSerializer.fromDocument(
              t.document,
              !!t.hasCommittedMutations,
            )
          if (t.noDocument) {
            var e = jr.fromSegments(t.noDocument.path),
              n = this.fromDbTimestamp(t.noDocument.readTime)
            return new Hr(e, n, {
              hasCommittedMutations: !!t.hasCommittedMutations,
            })
          }
          if (t.unknownDocument) {
            ;(e = jr.fromSegments(t.unknownDocument.path)),
              (n = this.fromDbTimestamp(t.unknownDocument.version))
            return new Xr(e, n)
          }
          return $n('Unexpected DbRemoteDocument')
        }),
        (t.prototype.toDbRemoteDocument = function(t) {
          if (t instanceof zr) {
            var e = this.remoteSerializer.toDocument(t),
              n = t.hasCommittedMutations
            return new Ps(null, null, e, n)
          }
          if (t instanceof Hr) {
            var r = t.key.path.toArray(),
              i = this.toDbTimestamp(t.version)
            n = t.hasCommittedMutations
            return new Ps(null, new Ms(r, i), null, n)
          }
          if (t instanceof Xr) {
            ;(r = t.key.path.toArray()), (i = this.toDbTimestamp(t.version))
            return new Ps(new Os(r, i), null, null, !0)
          }
          return $n('Unexpected MaybeDocumment')
        }),
        (t.prototype.toDbTimestamp = function(t) {
          var e = t.toTimestamp()
          return new Ds(e.seconds, e.nanoseconds)
        }),
        (t.prototype.fromDbTimestamp = function(t) {
          var e = new qr(t.seconds, t.nanoseconds)
          return _i.fromTimestamp(e)
        }),
        (t.prototype.toDbMutationBatch = function(t, e) {
          var n = this,
            r = e.mutations.map(function(t) {
              return n.remoteSerializer.toMutation(t)
            })
          return new ks(t, e.batchId, e.localWriteTime.toMillis(), r)
        }),
        (t.prototype.fromDbMutationBatch = function(t) {
          var e = this,
            n = t.mutations.map(function(t) {
              return e.remoteSerializer.fromMutation(t)
            }),
            r = qr.fromMillis(t.localWriteTimeMs)
          return new ss(t.batchId, r, n)
        }),
        (t.prototype.toDbResourcePaths = function(t) {
          var e = []
          return (
            t.forEach(function(t) {
              e.push(es(t.path))
            }),
            e
          )
        }),
        (t.prototype.fromDbResourcePaths = function(t) {
          for (var e = uo(), n = 0, r = t; n < r.length; n++) {
            var i = r[n]
            e = e.add(new jr(is(i)))
          }
          return e
        }),
        (t.prototype.fromDbTarget = function(t) {
          var e,
            n = this.fromDbTimestamp(t.readTime)
          return (
            (e =
              void 0 !== t.query.documents
                ? this.remoteSerializer.fromDocumentsTarget(t.query)
                : this.remoteSerializer.fromQueryTarget(t.query)),
            new xi(
              e,
              t.targetId,
              Si.Listen,
              t.lastListenSequenceNumber,
              n,
              t.resumeToken,
            )
          )
        }),
        (t.prototype.toDbTarget = function(t) {
          Zn(
            Si.Listen === t.purpose,
            'Only queries with purpose ' +
              Si.Listen +
              ' may be stored, got ' +
              t.purpose,
          )
          var e,
            n,
            r = this.toDbTimestamp(t.snapshotVersion)
          return (
            (e = t.query.isDocumentQuery()
              ? this.remoteSerializer.toDocumentsTarget(t.query)
              : this.remoteSerializer.toQueryTarget(t.query)),
            t.resumeToken instanceof Uint8Array
              ? (Zn(
                  'YES' === process.env.USE_MOCK_PERSISTENCE,
                  'Persisting non-string stream tokens is only supported with mock persistence .',
                ),
                (n = t.resumeToken.toString()))
              : (n = t.resumeToken),
            new Ls(t.targetId, t.query.canonicalId(), r, n, t.sequenceNumber, e)
          )
        }),
        t
      )
    })()
    function ia(t, e) {
      var n = t[0],
        r = t[1],
        i = e[0],
        o = e[1],
        s = Ar(n, i)
      return 0 === s ? Ar(r, o) : s
    }
    var oa = (function() {
        function t(t) {
          ;(this.maxElements = t),
            (this.buffer = new to(ia)),
            (this.previousIndex = 0)
        }
        return (
          (t.prototype.nextIndex = function() {
            return ++this.previousIndex
          }),
          (t.prototype.addElement = function(t) {
            var e = [t, this.nextIndex()]
            if (this.buffer.size < this.maxElements)
              this.buffer = this.buffer.add(e)
            else {
              var n = this.buffer.last()
              ia(e, n) < 0 && (this.buffer = this.buffer.delete(n).add(e))
            }
          }),
          Object.defineProperty(t.prototype, 'maxValue', {
            get: function() {
              return this.buffer.last()[0]
            },
            enumerable: !0,
            configurable: !0,
          }),
          t
        )
      })(),
      sa = (function() {
        function t(t) {
          this.delegate = t
        }
        return (
          (t.prototype.calculateTargetCount = function(t, e) {
            return this.delegate.getTargetCount(t).next(function(t) {
              return Math.floor((e / 100) * t)
            })
          }),
          (t.prototype.nthSequenceNumber = function(t, e) {
            var n = this
            if (0 === e) return us.resolve(zo.INVALID)
            var r = new oa(e)
            return this.delegate
              .forEachTarget(t, function(t) {
                return r.addElement(t.sequenceNumber)
              })
              .next(function() {
                return n.delegate.forEachOrphanedDocumentSequenceNumber(
                  t,
                  function(t) {
                    return r.addElement(t)
                  },
                )
              })
              .next(function() {
                return r.maxValue
              })
          }),
          (t.prototype.removeTargets = function(t, e, n) {
            return this.delegate.removeTargets(t, e, n)
          }),
          (t.prototype.removeOrphanedDocuments = function(t, e) {
            return this.delegate.removeOrphanedDocuments(t, e)
          }),
          t
        )
      })(),
      aa = 'IndexedDbPersistence',
      ua =
        'The current tab is not in the required state to perform this operation. It might be necessary to refresh the browser tab.',
      ca =
        'Another tab has exclusive access to the persistence layer. To allow shared access, make sure to invoke `enablePersistence()` with `experimentalTabSynchronization:true` in all tabs.',
      ha =
        'This platform is either missing IndexedDB or is known to have an incomplete implementation. Offline persistence has been disabled.',
      la = (function(t) {
        function e(e, n) {
          var r = t.call(this) || this
          return (r.simpleDbTransaction = e), (r.currentSequenceNumber = n), r
        }
        return a(e, t), e
      })(
        (function() {
          return function() {}
        })(),
      ),
      fa = (function() {
        function t(e, n, r, i, o, s) {
          if (
            ((this.persistenceKey = e),
            (this.clientId = n),
            (this.queue = i),
            (this.multiClientParams = s),
            (this._started = !1),
            (this.isPrimary = !1),
            (this.networkEnabled = !0),
            (this.inForeground = !1),
            (this.lastGarbageCollectionTime = Number.NEGATIVE_INFINITY),
            (this.primaryStateListener = function(t) {
              return Promise.resolve()
            }),
            !t.isAvailable())
          )
            throw new rr(nr.UNIMPLEMENTED, ha)
          if (
            ((this.referenceDelegate = new ya(this)),
            (this.dbName = e + t.MAIN_DATABASE),
            (this.serializer = new ra(o)),
            (this.document = r.document),
            (this.allowTabSynchronization = void 0 !== s),
            (this.queryCache = new $s(this.referenceDelegate, this.serializer)),
            (this.remoteDocumentCache = new gs(
              this.serializer,
              this.allowTabSynchronization,
            )),
            !r.window || !r.window.localStorage)
          )
            throw new rr(
              nr.UNIMPLEMENTED,
              'IndexedDB persistence is only available on platforms that support LocalStorage.',
            )
          ;(this.window = r.window),
            (this.webStorage = this.window.localStorage)
        }
        return (
          (t.getStore = function(t, e) {
            if (t instanceof la) return ls.getStore(t.simpleDbTransaction, e)
            throw $n(
              'IndexedDbPersistence must use instances of IndexedDbTransaction',
            )
          }),
          (t.createIndexedDbPersistence = function(e, n, r, i, o) {
            return u(this, void 0, void 0, function() {
              var s
              return c(this, function(a) {
                switch (a.label) {
                  case 0:
                    return [4, (s = new t(e, n, r, i, o)).start()]
                  case 1:
                    return a.sent(), [2, s]
                }
              })
            })
          }),
          (t.createMultiClientIndexedDbPersistence = function(
            e,
            n,
            r,
            i,
            o,
            s,
          ) {
            return u(this, void 0, void 0, function() {
              var a
              return c(this, function(u) {
                switch (u.label) {
                  case 0:
                    return [4, (a = new t(e, n, r, i, o, s)).start()]
                  case 1:
                    return u.sent(), [2, a]
                }
              })
            })
          }),
          (t.prototype.start = function() {
            var t = this
            return (
              Zn(!this.started, 'IndexedDbPersistence double-started!'),
              Zn(null !== this.window, "Expected 'window' to be defined"),
              ls
                .openOrCreate(this.dbName, Is, new Cs(this.serializer))
                .then(function(e) {
                  t.simpleDb = e
                })
                .then(function() {
                  return t.startRemoteDocumentCache()
                })
                .then(function() {
                  return (
                    t.attachVisibilityHandler(),
                    t.attachWindowUnloadHook(),
                    t
                      .updateClientMetadataAndTryBecomePrimary()
                      .then(function() {
                        return t.scheduleClientMetadataAndPrimaryLeaseRefreshes()
                      })
                  )
                })
                .then(function() {
                  return t.simpleDb.runTransaction(
                    'readonly',
                    [qs.store],
                    function(e) {
                      return ea(e).next(function(e) {
                        var n = t.multiClientParams
                          ? t.multiClientParams.sequenceNumberSyncer
                          : void 0
                        t.listenSequence = new zo(e, n)
                      })
                    },
                  )
                })
                .then(function() {
                  t._started = !0
                })
                .catch(function(e) {
                  return t.simpleDb && t.simpleDb.close(), Promise.reject(e)
                })
            )
          }),
          (t.prototype.startRemoteDocumentCache = function() {
            var t = this
            return this.simpleDb.runTransaction('readonly', Us, function(e) {
              return t.remoteDocumentCache.start(e)
            })
          }),
          (t.prototype.setPrimaryStateListener = function(t) {
            var e = this
            return (
              (this.primaryStateListener = function(n) {
                return u(e, void 0, void 0, function() {
                  return c(this, function(e) {
                    return this.started ? [2, t(n)] : [2]
                  })
                })
              }),
              t(this.isPrimary)
            )
          }),
          (t.prototype.setNetworkEnabled = function(t) {
            var e = this
            this.networkEnabled !== t &&
              ((this.networkEnabled = t),
              this.queue.enqueueAndForget(function() {
                return u(e, void 0, void 0, function() {
                  return c(this, function(t) {
                    switch (t.label) {
                      case 0:
                        return this.started
                          ? [4, this.updateClientMetadataAndTryBecomePrimary()]
                          : [3, 2]
                      case 1:
                        t.sent(), (t.label = 2)
                      case 2:
                        return [2]
                    }
                  })
                })
              }))
          }),
          (t.prototype.updateClientMetadataAndTryBecomePrimary = function() {
            var t = this
            return this.simpleDb.runTransaction('readwrite', Us, function(e) {
              return ma(e)
                .put(
                  new Bs(
                    t.clientId,
                    Date.now(),
                    t.networkEnabled,
                    t.inForeground,
                    t.remoteDocumentCache.lastProcessedDocumentChangeId,
                  ),
                )
                .next(function() {
                  if (t.isPrimary)
                    return t.verifyPrimaryLease(e).next(function(e) {
                      e ||
                        ((t.isPrimary = !1),
                        t.queue.enqueueAndForget(function() {
                          return t.primaryStateListener(!1)
                        }))
                    })
                })
                .next(function() {
                  return t.canActAsPrimary(e)
                })
                .next(function(n) {
                  var r = t.isPrimary
                  return (
                    (t.isPrimary = n),
                    r !== t.isPrimary &&
                      t.queue.enqueueAndForget(function() {
                        return t.primaryStateListener(t.isPrimary)
                      }),
                    r && !t.isPrimary
                      ? t.releasePrimaryLeaseIfHeld(e)
                      : t.isPrimary
                        ? t.acquireOrExtendPrimaryLease(e)
                        : void 0
                  )
                })
            })
          }),
          (t.prototype.verifyPrimaryLease = function(t) {
            var e = this
            return pa(t)
              .get(Ns.key)
              .next(function(t) {
                return us.resolve(e.isLocalClient(t))
              })
          }),
          (t.prototype.removeClientMetadata = function(t) {
            return ma(t).delete(this.clientId)
          }),
          (t.prototype.maybeGarbageCollectMultiClientState = function() {
            return u(this, void 0, void 0, function() {
              var e,
                n,
                r = this
              return c(this, function(i) {
                switch (i.label) {
                  case 0:
                    return !this.isPrimary ||
                      this.isWithinAge(this.lastGarbageCollectionTime, 18e5)
                      ? [3, 2]
                      : ((this.lastGarbageCollectionTime = Date.now()),
                        (n = []),
                        [
                          4,
                          this.runTransaction(
                            'maybeGarbageCollectMultiClientState',
                            'readwrite-primary',
                            function(i) {
                              var o = t.getStore(i, Bs.store)
                              return o
                                .loadAll()
                                .next(function(t) {
                                  ;(e = r.filterActiveClients(t, 18e5)),
                                    (n = t.filter(function(t) {
                                      return -1 === e.indexOf(t)
                                    }))
                                })
                                .next(function() {
                                  return us.forEach(n, function(t) {
                                    return o.delete(t.clientId)
                                  })
                                })
                                .next(function() {
                                  if (
                                    (e = e.filter(function(t) {
                                      return t.clientId !== r.clientId
                                    })).length > 0
                                  ) {
                                    var t = e.map(function(t) {
                                        return (
                                          t.lastProcessedDocumentChangeId || 0
                                        )
                                      }),
                                      n = Math.min.apply(Math, t)
                                    return r.remoteDocumentCache.removeDocumentChangesThroughChangeId(
                                      i,
                                      n,
                                    )
                                  }
                                })
                            },
                          ),
                        ])
                  case 1:
                    i.sent(),
                      n.forEach(function(t) {
                        r.window.localStorage.removeItem(
                          r.zombiedClientLocalStorageKey(t.clientId),
                        )
                      }),
                      (i.label = 2)
                  case 2:
                    return [2]
                }
              })
            })
          }),
          (t.prototype.scheduleClientMetadataAndPrimaryLeaseRefreshes = function() {
            var t = this
            this.clientMetadataRefresher = this.queue.enqueueAfterDelay(
              Go.ClientMetadataRefresh,
              4e3,
              function() {
                return t
                  .updateClientMetadataAndTryBecomePrimary()
                  .then(function() {
                    return t.maybeGarbageCollectMultiClientState()
                  })
                  .then(function() {
                    return t.scheduleClientMetadataAndPrimaryLeaseRefreshes()
                  })
              },
            )
          }),
          (t.prototype.isLocalClient = function(t) {
            return !!t && t.ownerId === this.clientId
          }),
          (t.prototype.canActAsPrimary = function(t) {
            var e = this
            return pa(t)
              .get(Ns.key)
              .next(function(n) {
                if (
                  null !== n &&
                  e.isWithinAge(n.leaseTimestampMs, 5e3) &&
                  !e.isClientZombied(n.ownerId)
                ) {
                  if (e.isLocalClient(n) && e.networkEnabled) return !0
                  if (!e.isLocalClient(n)) {
                    if (!n.allowTabSynchronization)
                      throw new rr(nr.FAILED_PRECONDITION, ca)
                    return !1
                  }
                }
                return (
                  !(!e.networkEnabled || !e.inForeground) ||
                  ma(t)
                    .loadAll()
                    .next(function(t) {
                      return (
                        void 0 ===
                        e.filterActiveClients(t, 5e3).find(function(t) {
                          if (e.clientId !== t.clientId) {
                            var n = !e.networkEnabled && t.networkEnabled,
                              r = !e.inForeground && t.inForeground,
                              i = e.networkEnabled === t.networkEnabled
                            if (n || (r && i)) return !0
                          }
                          return !1
                        })
                      )
                    })
                )
              })
              .next(function(t) {
                return (
                  e.isPrimary !== t &&
                    Xn(
                      aa,
                      'Client ' +
                        (t ? 'is' : 'is not') +
                        ' eligible for a primary lease.',
                    ),
                  t
                )
              })
          }),
          (t.prototype.shutdown = function(t) {
            return u(this, void 0, void 0, function() {
              var e = this
              return c(this, function(n) {
                switch (n.label) {
                  case 0:
                    return (
                      (this._started = !1),
                      this.markClientZombied(),
                      this.clientMetadataRefresher &&
                        this.clientMetadataRefresher.cancel(),
                      this.detachVisibilityHandler(),
                      this.detachWindowUnloadHook(),
                      [
                        4,
                        this.simpleDb.runTransaction(
                          'readwrite',
                          [Ns.store, Bs.store],
                          function(t) {
                            return e
                              .releasePrimaryLeaseIfHeld(t)
                              .next(function() {
                                return e.removeClientMetadata(t)
                              })
                          },
                        ),
                      ]
                    )
                  case 1:
                    return (
                      n.sent(),
                      this.simpleDb.close(),
                      this.removeClientZombiedEntry(),
                      t ? [4, ls.delete(this.dbName)] : [3, 3]
                    )
                  case 2:
                    n.sent(), (n.label = 3)
                  case 3:
                    return [2]
                }
              })
            })
          }),
          (t.prototype.filterActiveClients = function(t, e) {
            var n = this
            return t.filter(function(t) {
              return (
                n.isWithinAge(t.updateTimeMs, e) &&
                !n.isClientZombied(t.clientId)
              )
            })
          }),
          (t.prototype.getActiveClients = function() {
            var t = this
            return this.simpleDb.runTransaction(
              'readonly',
              [Bs.store],
              function(e) {
                return ma(e)
                  .loadAll()
                  .next(function(e) {
                    return t.filterActiveClients(e, 18e5).map(function(t) {
                      return t.clientId
                    })
                  })
              },
            )
          }),
          Object.defineProperty(t.prototype, 'started', {
            get: function() {
              return this._started
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.getMutationQueue = function(t) {
            return (
              Zn(
                this.started,
                'Cannot initialize MutationQueue before persistence is started.',
              ),
              Qs.forUser(t, this.serializer, this.referenceDelegate)
            )
          }),
          (t.prototype.getQueryCache = function() {
            return (
              Zn(
                this.started,
                'Cannot initialize QueryCache before persistence is started.',
              ),
              this.queryCache
            )
          }),
          (t.prototype.getRemoteDocumentCache = function() {
            return (
              Zn(
                this.started,
                'Cannot initialize RemoteDocumentCache before persistence is started.',
              ),
              this.remoteDocumentCache
            )
          }),
          (t.prototype.runTransaction = function(t, e, n) {
            var r = this
            return (
              Xn(aa, 'Starting transaction:', t),
              this.simpleDb.runTransaction(
                'readonly' === e ? 'readonly' : 'readwrite',
                Us,
                function(i) {
                  return 'readwrite-primary' === e
                    ? r
                        .verifyPrimaryLease(i)
                        .next(function(e) {
                          if (!e)
                            throw (Yn(
                              "Failed to obtain primary lease for action '" +
                                t +
                                "'.",
                            ),
                            (r.isPrimary = !1),
                            r.queue.enqueueAndForget(function() {
                              return r.primaryStateListener(!1)
                            }),
                            new rr(nr.FAILED_PRECONDITION, ua))
                          return n(new la(i, r.listenSequence.next()))
                        })
                        .next(function(t) {
                          return r
                            .acquireOrExtendPrimaryLease(i)
                            .next(function() {
                              return t
                            })
                        })
                    : r.verifyAllowTabSynchronization(i).next(function() {
                        return n(new la(i, r.listenSequence.next()))
                      })
                },
              )
            )
          }),
          (t.prototype.verifyAllowTabSynchronization = function(t) {
            var e = this
            return pa(t)
              .get(Ns.key)
              .next(function(t) {
                if (
                  null !== t &&
                  e.isWithinAge(t.leaseTimestampMs, 5e3) &&
                  !e.isClientZombied(t.ownerId) &&
                  !e.isLocalClient(t) &&
                  !t.allowTabSynchronization
                )
                  throw new rr(nr.FAILED_PRECONDITION, ca)
              })
          }),
          (t.prototype.acquireOrExtendPrimaryLease = function(t) {
            var e = new Ns(
              this.clientId,
              this.allowTabSynchronization,
              Date.now(),
            )
            return pa(t).put(Ns.key, e)
          }),
          (t.isAvailable = function() {
            return ls.isAvailable()
          }),
          (t.buildStoragePrefix = function(t) {
            var e = t.databaseId.projectId
            return (
              t.databaseId.isDefaultDatabase ||
                (e += '.' + t.databaseId.database),
              'firestore/' + t.persistenceKey + '/' + e + '/'
            )
          }),
          (t.prototype.releasePrimaryLeaseIfHeld = function(t) {
            var e = this,
              n = pa(t)
            return n.get(Ns.key).next(function(t) {
              return e.isLocalClient(t)
                ? (Xn(aa, 'Releasing primary lease.'), n.delete(Ns.key))
                : us.resolve()
            })
          }),
          (t.prototype.isWithinAge = function(t, e) {
            var n = Date.now()
            return (
              !(t < n - e) &&
              (!(t > n) ||
                (Yn(
                  'Detected an update time that is in the future: ' +
                    t +
                    ' > ' +
                    n,
                ),
                !1))
            )
          }),
          (t.prototype.attachVisibilityHandler = function() {
            var t = this
            null !== this.document &&
              'function' == typeof this.document.addEventListener &&
              ((this.documentVisibilityHandler = function() {
                t.queue.enqueueAndForget(function() {
                  return (
                    (t.inForeground = 'visible' === t.document.visibilityState),
                    t.updateClientMetadataAndTryBecomePrimary()
                  )
                })
              }),
              this.document.addEventListener(
                'visibilitychange',
                this.documentVisibilityHandler,
              ),
              (this.inForeground = 'visible' === this.document.visibilityState))
          }),
          (t.prototype.detachVisibilityHandler = function() {
            this.documentVisibilityHandler &&
              (Zn(
                null !== this.document &&
                  'function' == typeof this.document.addEventListener,
                "Expected 'document.addEventListener' to be a function",
              ),
              this.document.removeEventListener(
                'visibilitychange',
                this.documentVisibilityHandler,
              ),
              (this.documentVisibilityHandler = null))
          }),
          (t.prototype.attachWindowUnloadHook = function() {
            var t = this
            'function' == typeof this.window.addEventListener &&
              ((this.windowUnloadHandler = function() {
                t.markClientZombied(),
                  t.queue.enqueueAndForget(function() {
                    return t.shutdown()
                  })
              }),
              this.window.addEventListener('unload', this.windowUnloadHandler))
          }),
          (t.prototype.detachWindowUnloadHook = function() {
            this.windowUnloadHandler &&
              (Zn(
                'function' == typeof this.window.removeEventListener,
                "Expected 'window.removeEventListener' to be a function",
              ),
              this.window.removeEventListener(
                'unload',
                this.windowUnloadHandler,
              ),
              (this.windowUnloadHandler = null))
          }),
          (t.prototype.isClientZombied = function(t) {
            try {
              var e =
                null !==
                this.webStorage.getItem(this.zombiedClientLocalStorageKey(t))
              return (
                Xn(
                  aa,
                  "Client '" +
                    t +
                    "' " +
                    (e ? 'is' : 'is not') +
                    ' zombied in LocalStorage',
                ),
                e
              )
            } catch (t) {
              return Yn(aa, 'Failed to get zombied client id.', t), !1
            }
          }),
          (t.prototype.markClientZombied = function() {
            try {
              this.webStorage.setItem(
                this.zombiedClientLocalStorageKey(this.clientId),
                String(Date.now()),
              )
            } catch (t) {
              Yn('Failed to set zombie client id.', t)
            }
          }),
          (t.prototype.removeClientZombiedEntry = function() {
            try {
              this.webStorage.removeItem(
                this.zombiedClientLocalStorageKey(this.clientId),
              )
            } catch (t) {}
          }),
          (t.prototype.zombiedClientLocalStorageKey = function(t) {
            return 'firestore_zombie_' + this.persistenceKey + '_' + t
          }),
          (t.MAIN_DATABASE = 'main'),
          t
        )
      })()
    function da(t) {
      return t.code === nr.FAILED_PRECONDITION && t.message === ua
    }
    function pa(t) {
      return t.store(Ns.store)
    }
    function ma(t) {
      return t.store(Bs.store)
    }
    var ya = (function() {
      function t(t) {
        ;(this.db = t), (this.garbageCollector = new sa(this))
      }
      return (
        (t.prototype.getTargetCount = function(t) {
          return this.db.getQueryCache().getQueryCount(t)
        }),
        (t.prototype.forEachTarget = function(t, e) {
          return this.db.getQueryCache().forEachTarget(t, e)
        }),
        (t.prototype.forEachOrphanedDocumentSequenceNumber = function(t, e) {
          return this.forEachOrphanedDocument(t, function(t, n) {
            return e(n)
          })
        }),
        (t.prototype.setInMemoryPins = function(t) {
          this.inMemoryPins = t
        }),
        (t.prototype.addReference = function(t, e) {
          return ga(t, e)
        }),
        (t.prototype.removeReference = function(t, e) {
          return ga(t, e)
        }),
        (t.prototype.removeTargets = function(t, e, n) {
          return this.db.getQueryCache().removeTargets(t, e, n)
        }),
        (t.prototype.removeMutationReference = function(t, e) {
          return ga(t, e)
        }),
        (t.prototype.isPinned = function(t, e) {
          return this.inMemoryPins.containsKey(e)
            ? us.resolve(!0)
            : (function(t, e) {
                var n = !1
                return Hs(t)
                  .iterateSerial(function(r) {
                    return Ks(t, r, e).next(function(t) {
                      return t && (n = !0), us.resolve(!t)
                    })
                  })
                  .next(function() {
                    return n
                  })
              })(t, e)
        }),
        (t.prototype.removeOrphanedDocuments = function(t, e) {
          var n = this,
            r = 0,
            i = 0,
            o = []
          return this.forEachOrphanedDocument(t, function(s, a) {
            if (a <= e) {
              var u = n.isPinned(t, s).next(function(e) {
                if (!e)
                  return (
                    r++,
                    n.removeOrphanedDocument(t, s).next(function(t) {
                      i += t
                    })
                  )
              })
              o.push(u)
            }
          })
            .next(function() {
              return us.waitFor(o)
            })
            .next(function() {
              return n.db.getRemoteDocumentCache().updateSize(t, -i)
            })
            .next(function() {
              return r
            })
        }),
        (t.prototype.removeOrphanedDocument = function(t, e) {
          var n,
            r = 0,
            i = this.db.getRemoteDocumentCache()
          return us
            .waitFor([
              na(t).delete(((n = e), [0, es(n.path)])),
              i.removeEntry(t, e).next(function(t) {
                r += t
              }),
            ])
            .next(function() {
              return r
            })
        }),
        (t.prototype.removeTarget = function(t, e) {
          var n = e.copy({ sequenceNumber: t.currentSequenceNumber })
          return this.db.getQueryCache().updateQueryData(t, n)
        }),
        (t.prototype.updateLimboDocument = function(t, e) {
          return ga(t, e)
        }),
        (t.prototype.forEachOrphanedDocument = function(t, e) {
          var n,
            r = na(t),
            i = zo.INVALID
          return r
            .iterate({ index: xs.documentTargetsIndex }, function(t, r) {
              var o = t[0],
                s = (t[1], r.path),
                a = r.sequenceNumber
              0 === o
                ? (i !== zo.INVALID && e(new jr(is(n)), i), (i = a), (n = s))
                : (i = zo.INVALID)
            })
            .next(function() {
              i !== zo.INVALID && e(new jr(is(n)), i)
            })
        }),
        t
      )
    })()
    function ga(t, e) {
      return na(t).put(
        (function(t, e) {
          return new xs(0, es(t.path), e)
        })(e, t.currentSequenceNumber),
      )
    }
    var va = (function() {
        function t(t, e) {
          ;(this.remoteDocumentCache = t), (this.mutationQueue = e)
        }
        return (
          (t.prototype.getDocument = function(t, e) {
            var n = this
            return this.mutationQueue
              .getAllMutationBatchesAffectingDocumentKey(t, e)
              .next(function(r) {
                return n.getDocumentInternal(t, e, r)
              })
          }),
          (t.prototype.getDocumentInternal = function(t, e, n) {
            return this.remoteDocumentCache.getEntry(t, e).next(function(t) {
              for (var r = 0, i = n; r < i.length; r++) {
                t = i[r].applyToLocalView(e, t)
              }
              return t
            })
          }),
          (t.prototype.getDocuments = function(t, e) {
            var n = this
            return this.mutationQueue
              .getAllMutationBatchesAffectingDocumentKeys(t, e)
              .next(function(r) {
                var i = [],
                  o = no()
                return (
                  e.forEach(function(e) {
                    i.push(
                      n.getDocumentInternal(t, e, r).next(function(t) {
                        t || (t = new Hr(e, _i.forDeletedDoc())),
                          (o = o.insert(e, t))
                      }),
                    )
                  }),
                  us.waitFor(i).next(function() {
                    return o
                  })
                )
              })
          }),
          (t.prototype.getDocumentsMatchingQuery = function(t, e) {
            return jr.isDocumentKey(e.path)
              ? this.getDocumentsMatchingDocumentQuery(t, e.path)
              : this.getDocumentsMatchingCollectionQuery(t, e)
          }),
          (t.prototype.getDocumentsMatchingDocumentQuery = function(t, e) {
            return this.getDocument(t, new jr(e)).next(function(t) {
              var e = io()
              return t instanceof zr && (e = e.insert(t.key, t)), e
            })
          }),
          (t.prototype.getDocumentsMatchingCollectionQuery = function(t, e) {
            var n,
              r = this
            return this.remoteDocumentCache
              .getDocumentsMatchingQuery(t, e)
              .next(function(i) {
                return (
                  (n = i),
                  r.mutationQueue.getAllMutationBatchesAffectingQuery(t, e)
                )
              })
              .next(function(t) {
                for (var r = 0, i = t; r < i.length; r++)
                  for (
                    var o = i[r], s = 0, a = o.mutations;
                    s < a.length;
                    s++
                  ) {
                    var u = a[s],
                      c = u.key
                    if (e.path.isImmediateParentOf(c.path)) {
                      var h = n.get(c),
                        l = u.applyToLocalView(h, h, o.localWriteTime)
                      n = l instanceof zr ? n.insert(c, l) : n.remove(c)
                    }
                  }
              })
              .next(function() {
                return (
                  n.forEach(function(t, r) {
                    e.matches(r) || (n = n.remove(t))
                  }),
                  n
                )
              })
          }),
          t
        )
      })(),
      ba = (function() {
        function t() {
          ;(this.refsByKey = new to(wa.compareByKey)),
            (this.refsByTarget = new to(wa.compareByTargetId))
        }
        return (
          (t.prototype.isEmpty = function() {
            return this.refsByKey.isEmpty()
          }),
          (t.prototype.addReference = function(t, e) {
            var n = new wa(t, e)
            ;(this.refsByKey = this.refsByKey.add(n)),
              (this.refsByTarget = this.refsByTarget.add(n))
          }),
          (t.prototype.addReferences = function(t, e) {
            var n = this
            t.forEach(function(t) {
              return n.addReference(t, e)
            })
          }),
          (t.prototype.removeReference = function(t, e) {
            this.removeRef(new wa(t, e))
          }),
          (t.prototype.removeReferences = function(t, e) {
            var n = this
            t.forEach(function(t) {
              return n.removeReference(t, e)
            })
          }),
          (t.prototype.removeReferencesForId = function(t) {
            var e = this,
              n = jr.EMPTY,
              r = new wa(n, t),
              i = new wa(n, t + 1),
              o = []
            return (
              this.refsByTarget.forEachInRange([r, i], function(t) {
                e.removeRef(t), o.push(t.key)
              }),
              o
            )
          }),
          (t.prototype.removeAllReferences = function() {
            var t = this
            this.refsByKey.forEach(function(e) {
              return t.removeRef(e)
            })
          }),
          (t.prototype.removeRef = function(t) {
            ;(this.refsByKey = this.refsByKey.delete(t)),
              (this.refsByTarget = this.refsByTarget.delete(t))
          }),
          (t.prototype.referencesForId = function(t) {
            var e = jr.EMPTY,
              n = new wa(e, t),
              r = new wa(e, t + 1),
              i = uo()
            return (
              this.refsByTarget.forEachInRange([n, r], function(t) {
                i = i.add(t.key)
              }),
              i
            )
          }),
          (t.prototype.containsKey = function(t) {
            var e = new wa(t, 0),
              n = this.refsByKey.firstAfterOrEqual(e)
            return null !== n && t.isEqual(n.key)
          }),
          t
        )
      })(),
      wa = (function() {
        function t(t, e) {
          ;(this.key = t), (this.targetOrBatchId = e)
        }
        return (
          (t.compareByKey = function(t, e) {
            return (
              jr.comparator(t.key, e.key) ||
              Ar(t.targetOrBatchId, e.targetOrBatchId)
            )
          }),
          (t.compareByTargetId = function(t, e) {
            return (
              Ar(t.targetOrBatchId, e.targetOrBatchId) ||
              jr.comparator(t.key, e.key)
            )
          }),
          t
        )
      })(),
      Ta = (function() {
        function t(t, e) {
          ;(this.persistence = t),
            (this.localViewReferences = new ba()),
            (this.queryDataByTarget = {}),
            Zn(
              t.started,
              'LocalStore was passed an unstarted persistence implementation',
            ),
            this.persistence.referenceDelegate.setInMemoryPins(
              this.localViewReferences,
            ),
            (this.mutationQueue = t.getMutationQueue(e)),
            (this.remoteDocuments = t.getRemoteDocumentCache()),
            (this.queryCache = t.getQueryCache()),
            (this.localDocuments = new va(
              this.remoteDocuments,
              this.mutationQueue,
            ))
        }
        return (
          (t.prototype.handleUserChange = function(t) {
            var e = this
            return this.persistence.runTransaction(
              'Handle user change',
              'readonly',
              function(n) {
                var r
                return e.mutationQueue
                  .getAllMutationBatches(n)
                  .next(function(i) {
                    return (
                      (r = i),
                      (e.mutationQueue = e.persistence.getMutationQueue(t)),
                      (e.localDocuments = new va(
                        e.remoteDocuments,
                        e.mutationQueue,
                      )),
                      e.mutationQueue.getAllMutationBatches(n)
                    )
                  })
                  .next(function(t) {
                    for (
                      var i = [], o = [], s = uo(), a = 0, u = r;
                      a < u.length;
                      a++
                    ) {
                      var c = u[a]
                      i.push(c.batchId)
                      for (var h = 0, l = c.mutations; h < l.length; h++) {
                        var f = l[h]
                        s = s.add(f.key)
                      }
                    }
                    for (var d = 0, p = t; d < p.length; d++) {
                      c = p[d]
                      o.push(c.batchId)
                      for (var m = 0, y = c.mutations; m < y.length; m++) {
                        f = y[m]
                        s = s.add(f.key)
                      }
                    }
                    return e.localDocuments
                      .getDocuments(n, s)
                      .next(function(t) {
                        return {
                          affectedDocuments: t,
                          removedBatchIds: i,
                          addedBatchIds: o,
                        }
                      })
                  })
              },
            )
          }),
          (t.prototype.localWrite = function(t) {
            var e = this
            return this.persistence.runTransaction(
              'Locally write mutations',
              'readwrite',
              function(n) {
                var r,
                  i = qr.now()
                return e.mutationQueue
                  .addMutationBatch(n, i, t)
                  .next(function(t) {
                    var i = (r = t).keys()
                    return e.localDocuments.getDocuments(n, i)
                  })
                  .next(function(t) {
                    return { batchId: r.batchId, changes: t }
                  })
              },
            )
          }),
          (t.prototype.lookupMutationDocuments = function(t) {
            var e = this
            return this.persistence.runTransaction(
              'Lookup mutation documents',
              'readonly',
              function(n) {
                return e.mutationQueue
                  .lookupMutationKeys(n, t)
                  .next(function(t) {
                    return t
                      ? e.localDocuments.getDocuments(n, t)
                      : us.resolve(null)
                  })
              },
            )
          }),
          (t.prototype.acknowledgeBatch = function(t) {
            var e = this
            return this.persistence.runTransaction(
              'Acknowledge batch',
              'readwrite-primary',
              function(n) {
                var r = t.batch.keys(),
                  i = e.remoteDocuments.newChangeBuffer()
                return e.mutationQueue
                  .acknowledgeBatch(n, t.batch, t.streamToken)
                  .next(function() {
                    return e.applyWriteToRemoteDocuments(n, t, i)
                  })
                  .next(function() {
                    return i.apply(n)
                  })
                  .next(function() {
                    return e.mutationQueue.performConsistencyCheck(n)
                  })
                  .next(function() {
                    return e.localDocuments.getDocuments(n, r)
                  })
              },
            )
          }),
          (t.prototype.rejectBatch = function(t) {
            var e = this
            return this.persistence.runTransaction(
              'Reject batch',
              'readwrite-primary',
              function(n) {
                var r
                return e.mutationQueue
                  .lookupMutationBatch(n, t)
                  .next(function(t) {
                    return (
                      Zn(null !== t, 'Attempt to reject nonexistent batch!'),
                      (r = t.keys()),
                      e.mutationQueue.removeMutationBatch(n, t)
                    )
                  })
                  .next(function() {
                    return e.mutationQueue.performConsistencyCheck(n)
                  })
                  .next(function() {
                    return e.localDocuments.getDocuments(n, r)
                  })
              },
            )
          }),
          (t.prototype.getLastStreamToken = function() {
            var t = this
            return this.persistence.runTransaction(
              'Get last stream token',
              'readonly',
              function(e) {
                return t.mutationQueue.getLastStreamToken(e)
              },
            )
          }),
          (t.prototype.setLastStreamToken = function(t) {
            var e = this
            return this.persistence.runTransaction(
              'Set last stream token',
              'readwrite-primary',
              function(n) {
                return e.mutationQueue.setLastStreamToken(n, t)
              },
            )
          }),
          (t.prototype.getLastRemoteSnapshotVersion = function() {
            var t = this
            return this.persistence.runTransaction(
              'Get last remote snapshot version',
              'readonly',
              function(e) {
                return t.queryCache.getLastRemoteSnapshotVersion(e)
              },
            )
          }),
          (t.prototype.applyRemoteEvent = function(e) {
            var n = this,
              r = this.remoteDocuments.newChangeBuffer()
            return this.persistence.runTransaction(
              'Apply remote event',
              'readwrite-primary',
              function(i) {
                var o = [],
                  s = uo()
                ar(e.targetChanges, function(r, a) {
                  var u = n.queryDataByTarget[r]
                  if (u) {
                    a.addedDocuments.forEach(function(t) {
                      s = s.add(t)
                    }),
                      a.modifiedDocuments.forEach(function(t) {
                        s = s.add(t)
                      }),
                      o.push(
                        n.queryCache
                          .removeMatchingKeys(i, a.removedDocuments, r)
                          .next(function() {
                            return n.queryCache.addMatchingKeys(
                              i,
                              a.addedDocuments,
                              r,
                            )
                          }),
                      )
                    var c = a.resumeToken
                    if (c.length > 0) {
                      var h = u
                      ;(u = u.copy({
                        resumeToken: c,
                        snapshotVersion: e.snapshotVersion,
                      })),
                        (n.queryDataByTarget[r] = u),
                        t.shouldPersistQueryData(h, u, a) &&
                          o.push(n.queryCache.updateQueryData(i, u))
                    }
                  }
                })
                var a = uo()
                e.documentUpdates.forEach(function(t, u) {
                  ;(a = a.add(t)),
                    o.push(
                      r.getEntry(i, t).next(function(e) {
                        null == e ||
                        u.version.isEqual(_i.MIN) ||
                        (s.has(u.key) && !e.hasPendingWrites) ||
                        u.version.compareTo(e.version) >= 0
                          ? r.addEntry(u)
                          : Xn(
                              'LocalStore',
                              'Ignoring outdated watch update for ',
                              t,
                              '. Current version:',
                              e.version,
                              ' Watch version:',
                              u.version,
                            )
                      }),
                    ),
                    e.resolvedLimboDocuments.has(t) &&
                      o.push(
                        n.persistence.referenceDelegate.updateLimboDocument(
                          i,
                          t,
                        ),
                      )
                })
                var u = e.snapshotVersion
                if (!u.isEqual(_i.MIN)) {
                  var c = n.queryCache
                    .getLastRemoteSnapshotVersion(i)
                    .next(function(t) {
                      return (
                        Zn(
                          u.compareTo(t) >= 0,
                          'Watch stream reverted to previous snapshot?? ' +
                            u +
                            ' < ' +
                            t,
                        ),
                        n.queryCache.setTargetsMetadata(
                          i,
                          i.currentSequenceNumber,
                          u,
                        )
                      )
                    })
                  o.push(c)
                }
                return us
                  .waitFor(o)
                  .next(function() {
                    return r.apply(i)
                  })
                  .next(function() {
                    return n.localDocuments.getDocuments(i, a)
                  })
              },
            )
          }),
          (t.shouldPersistQueryData = function(t, e, n) {
            return (
              0 !== e.resumeToken.length &&
              (0 === t.resumeToken.length ||
                (e.snapshotVersion.toMicroseconds() -
                  t.snapshotVersion.toMicroseconds() >=
                  this.RESUME_TOKEN_MAX_AGE_MICROS ||
                  n.addedDocuments.size +
                    n.modifiedDocuments.size +
                    n.removedDocuments.size >
                    0))
            )
          }),
          (t.prototype.notifyLocalViewChanges = function(t) {
            var e = this
            return this.persistence.runTransaction(
              'notifyLocalViewChanges',
              'readwrite',
              function(n) {
                return us.forEach(t, function(t) {
                  return (
                    e.localViewReferences.addReferences(
                      t.addedKeys,
                      t.targetId,
                    ),
                    e.localViewReferences.removeReferences(
                      t.removedKeys,
                      t.targetId,
                    ),
                    us.forEach(t.removedKeys, function(t) {
                      return e.persistence.referenceDelegate.removeReference(
                        n,
                        t,
                      )
                    })
                  )
                })
              },
            )
          }),
          (t.prototype.nextMutationBatch = function(t) {
            var e = this
            return this.persistence.runTransaction(
              'Get next mutation batch',
              'readonly',
              function(n) {
                return (
                  void 0 === t && (t = os),
                  e.mutationQueue.getNextMutationBatchAfterBatchId(n, t)
                )
              },
            )
          }),
          (t.prototype.readDocument = function(t) {
            var e = this
            return this.persistence.runTransaction(
              'read document',
              'readonly',
              function(n) {
                return e.localDocuments.getDocument(n, t)
              },
            )
          }),
          (t.prototype.allocateQuery = function(t) {
            var e = this
            return this.persistence.runTransaction(
              'Allocate query',
              'readwrite',
              function(n) {
                var r
                return e.queryCache
                  .getQueryData(n, t)
                  .next(function(i) {
                    return i
                      ? ((r = i), us.resolve())
                      : e.queryCache.allocateTargetId(n).next(function(i) {
                          return (
                            (r = new xi(
                              t,
                              i,
                              Si.Listen,
                              n.currentSequenceNumber,
                            )),
                            e.queryCache.addQueryData(n, r)
                          )
                        })
                  })
                  .next(function() {
                    return (
                      Zn(
                        !e.queryDataByTarget[r.targetId],
                        'Tried to allocate an already allocated query: ' + t,
                      ),
                      (e.queryDataByTarget[r.targetId] = r),
                      r
                    )
                  })
              },
            )
          }),
          (t.prototype.releaseQuery = function(t, e) {
            var n = this,
              r = e ? 'readwrite' : 'readwrite-primary'
            return this.persistence.runTransaction('Release query', r, function(
              r,
            ) {
              return n.queryCache.getQueryData(r, t).next(function(i) {
                Zn(null != i, 'Tried to release nonexistent query: ' + t)
                var o = i.targetId,
                  s = n.queryDataByTarget[o],
                  a = n.localViewReferences.removeReferencesForId(o)
                return (
                  delete n.queryDataByTarget[o],
                  e
                    ? us.resolve()
                    : us
                        .forEach(a, function(t) {
                          return n.persistence.referenceDelegate.removeReference(
                            r,
                            t,
                          )
                        })
                        .next(function() {
                          return n.persistence.referenceDelegate.removeTarget(
                            r,
                            s,
                          )
                        })
                )
              })
            })
          }),
          (t.prototype.executeQuery = function(t) {
            var e = this
            return this.persistence.runTransaction(
              'Execute query',
              'readonly',
              function(n) {
                return e.localDocuments.getDocumentsMatchingQuery(n, t)
              },
            )
          }),
          (t.prototype.remoteDocumentKeys = function(t) {
            var e = this
            return this.persistence.runTransaction(
              'Remote document keys',
              'readonly',
              function(n) {
                return e.queryCache.getMatchingKeysForTargetId(n, t)
              },
            )
          }),
          (t.prototype.getActiveClients = function() {
            return this.persistence.getActiveClients()
          }),
          (t.prototype.removeCachedMutationBatchMetadata = function(t) {
            this.mutationQueue.removeCachedMutationKeys(t)
          }),
          (t.prototype.setNetworkEnabled = function(t) {
            this.persistence.setNetworkEnabled(t)
          }),
          (t.prototype.applyWriteToRemoteDocuments = function(t, e, n) {
            var r = this,
              i = e.batch,
              o = i.keys(),
              s = us.resolve()
            return (
              o.forEach(function(r) {
                s = s
                  .next(function() {
                    return n.getEntry(t, r)
                  })
                  .next(function(t) {
                    var o = t,
                      s = e.docVersions.get(r)
                    Zn(
                      null !== s,
                      'ackVersions should contain every doc in the write.',
                    ),
                      (!o || o.version.compareTo(s) < 0) &&
                        ((o = i.applyToRemoteDocument(r, o, e))
                          ? n.addEntry(o)
                          : Zn(
                              !t,
                              'Mutation batch ' +
                                i +
                                ' applied to document ' +
                                t +
                                ' resulted in null',
                            ))
                  })
              }),
              s.next(function() {
                return r.mutationQueue.removeMutationBatch(t, i)
              })
            )
          }),
          (t.prototype.getQueryForTarget = function(t) {
            var e = this
            return this.queryDataByTarget[t]
              ? Promise.resolve(this.queryDataByTarget[t].query)
              : this.persistence.runTransaction(
                  'Get query data',
                  'readonly',
                  function(n) {
                    return e.queryCache
                      .getQueryDataForTarget(n, t)
                      .next(function(t) {
                        return t ? t.query : null
                      })
                  },
                )
          }),
          (t.prototype.getNewDocumentChanges = function() {
            var t = this
            return this.persistence.runTransaction(
              'Get new document changes',
              'readonly',
              function(e) {
                return t.remoteDocuments.getNewDocumentChanges(e)
              },
            )
          }),
          (t.RESUME_TOKEN_MAX_AGE_MICROS = 3e8),
          t
        )
      })(),
      Sa = (function() {
        function t(t) {
          ;(this.referenceDelegate = t),
            (this.mutationQueue = []),
            (this.nextBatchId = 1),
            (this.highestAcknowledgedBatchId = os),
            (this.lastStreamToken = er()),
            (this.batchesByDocumentKey = new to(wa.compareByKey))
        }
        return (
          (t.prototype.checkEmpty = function(t) {
            return us.resolve(0 === this.mutationQueue.length)
          }),
          (t.prototype.acknowledgeBatch = function(t, e, n) {
            var r = e.batchId
            Zn(
              r > this.highestAcknowledgedBatchId,
              'Mutation batchIDs must be acknowledged in order',
            )
            var i = this.indexOfExistingBatchId(r, 'acknowledged'),
              o = this.mutationQueue[i]
            return (
              Zn(
                r === o.batchId,
                'Queue ordering failure: expected batch ' +
                  r +
                  ', got batch ' +
                  o.batchId,
              ),
              Zn(
                !o.isTombstone(),
                "Can't acknowledge a previously removed batch",
              ),
              (this.highestAcknowledgedBatchId = r),
              (this.lastStreamToken = n),
              us.resolve()
            )
          }),
          (t.prototype.getLastStreamToken = function(t) {
            return us.resolve(this.lastStreamToken)
          }),
          (t.prototype.setLastStreamToken = function(t, e) {
            return (this.lastStreamToken = e), us.resolve()
          }),
          (t.prototype.addMutationBatch = function(t, e, n) {
            Zn(0 !== n.length, 'Mutation batches should not be empty')
            var r = this.nextBatchId
            ;(this.nextBatchId++, this.mutationQueue.length > 0) &&
              Zn(
                this.mutationQueue[this.mutationQueue.length - 1].batchId < r,
                'Mutation batchIDs must be monotonically increasing order',
              )
            var i = new ss(r, e, n)
            this.mutationQueue.push(i)
            for (var o = 0, s = n; o < s.length; o++) {
              var a = s[o]
              this.batchesByDocumentKey = this.batchesByDocumentKey.add(
                new wa(a.key, r),
              )
            }
            return us.resolve(i)
          }),
          (t.prototype.lookupMutationBatch = function(t, e) {
            return us.resolve(this.findMutationBatch(e))
          }),
          (t.prototype.lookupMutationKeys = function(t, e) {
            var n = this.findMutationBatch(e)
            return (
              Zn(null != n, 'Failed to find local mutation batch.'),
              us.resolve(n.isTombstone() ? null : n.keys())
            )
          }),
          (t.prototype.getNextMutationBatchAfterBatchId = function(t, e) {
            for (
              var n = this.mutationQueue.length,
                r = Math.max(e, this.highestAcknowledgedBatchId) + 1,
                i = this.indexOfBatchId(r),
                o = i < 0 ? 0 : i;
              o < n;
              o++
            ) {
              var s = this.mutationQueue[o]
              if (!s.isTombstone()) return us.resolve(s)
            }
            return us.resolve(null)
          }),
          (t.prototype.getAllMutationBatches = function(t) {
            return us.resolve(
              this.getAllLiveMutationBatchesBeforeIndex(
                this.mutationQueue.length,
              ),
            )
          }),
          (t.prototype.getAllMutationBatchesAffectingDocumentKey = function(
            t,
            e,
          ) {
            var n = this,
              r = new wa(e, 0),
              i = new wa(e, Number.POSITIVE_INFINITY),
              o = []
            return (
              this.batchesByDocumentKey.forEachInRange([r, i], function(t) {
                Zn(
                  e.isEqual(t.key),
                  "Should only iterate over a single key's batches",
                )
                var r = n.findMutationBatch(t.targetOrBatchId)
                Zn(
                  null !== r,
                  'Batches in the index must exist in the main table',
                ),
                  o.push(r)
              }),
              us.resolve(o)
            )
          }),
          (t.prototype.getAllMutationBatchesAffectingDocumentKeys = function(
            t,
            e,
          ) {
            var n = this,
              r = new to(Ar)
            return (
              e.forEach(function(t) {
                var e = new wa(t, 0),
                  i = new wa(t, Number.POSITIVE_INFINITY)
                n.batchesByDocumentKey.forEachInRange([e, i], function(e) {
                  Zn(
                    t.isEqual(e.key),
                    "For each key, should only iterate over a single key's batches",
                  ),
                    (r = r.add(e.targetOrBatchId))
                })
              }),
              us.resolve(this.findMutationBatches(r))
            )
          }),
          (t.prototype.getAllMutationBatchesAffectingQuery = function(t, e) {
            var n = e.path,
              r = n.length + 1,
              i = n
            jr.isDocumentKey(i) || (i = i.child(''))
            var o = new wa(new jr(i), 0),
              s = new to(Ar)
            return (
              this.batchesByDocumentKey.forEachWhile(function(t) {
                var e = t.key.path
                return (
                  !!n.isPrefixOf(e) &&
                  (e.length === r && (s = s.add(t.targetOrBatchId)), !0)
                )
              }, o),
              us.resolve(this.findMutationBatches(s))
            )
          }),
          (t.prototype.findMutationBatches = function(t) {
            var e = this,
              n = []
            return (
              t.forEach(function(t) {
                var r = e.findMutationBatch(t)
                null !== r && n.push(r)
              }),
              n
            )
          }),
          (t.prototype.removeMutationBatch = function(t, e) {
            var n = this,
              r = this.indexOfExistingBatchId(e.batchId, 'removed')
            if (
              (Zn(
                this.mutationQueue[r].batchId === e.batchId,
                'Removed batches must exist in the queue',
              ),
              0 === r)
            ) {
              for (var i = 1; i < this.mutationQueue.length; i++) {
                if (!this.mutationQueue[i].isTombstone()) break
              }
              this.mutationQueue.splice(0, i)
            } else this.mutationQueue[r] = this.mutationQueue[r].toTombstone()
            var o = this.batchesByDocumentKey
            return us
              .forEach(e.mutations, function(r) {
                var i = new wa(r.key, e.batchId)
                return (
                  (o = o.delete(i)),
                  n.referenceDelegate.removeMutationReference(t, r.key)
                )
              })
              .next(function() {
                n.batchesByDocumentKey = o
              })
          }),
          (t.prototype.removeCachedMutationKeys = function(t) {}),
          (t.prototype.containsKey = function(t, e) {
            var n = new wa(e, 0),
              r = this.batchesByDocumentKey.firstAfterOrEqual(n)
            return us.resolve(e.isEqual(r && r.key))
          }),
          (t.prototype.performConsistencyCheck = function(t) {
            return (
              0 === this.mutationQueue.length &&
                Zn(
                  this.batchesByDocumentKey.isEmpty(),
                  'Document leak -- detected dangling mutation references when queue is empty.',
                ),
              us.resolve()
            )
          }),
          (t.prototype.getAllLiveMutationBatchesBeforeIndex = function(t) {
            for (var e = [], n = 0; n < t; n++) {
              var r = this.mutationQueue[n]
              r.isTombstone() || e.push(r)
            }
            return e
          }),
          (t.prototype.indexOfExistingBatchId = function(t, e) {
            var n = this.indexOfBatchId(t)
            return (
              Zn(
                n >= 0 && n < this.mutationQueue.length,
                'Batches must exist to be ' + e,
              ),
              n
            )
          }),
          (t.prototype.indexOfBatchId = function(t) {
            return 0 === this.mutationQueue.length
              ? 0
              : t - this.mutationQueue[0].batchId
          }),
          (t.prototype.findMutationBatch = function(t) {
            var e = this.indexOfBatchId(t)
            if (e < 0 || e >= this.mutationQueue.length) return null
            var n = this.mutationQueue[e]
            return (
              Zn(n.batchId === t, 'If found batch must match'),
              n.isTombstone() ? null : n
            )
          }),
          t
        )
      })(),
      Ea = (function() {
        function t(t) {
          ;(this.persistence = t),
            (this.queries = new cs(function(t) {
              return t.canonicalId()
            })),
            (this.lastRemoteSnapshotVersion = _i.MIN),
            (this.highestTargetId = 0),
            (this.highestSequenceNumber = 0),
            (this.references = new ba()),
            (this.targetCount = 0),
            (this.targetIdGenerator = Js.forQueryCache())
        }
        return (
          (t.prototype.getTargetCount = function(t) {
            return us.resolve(this.targetCount)
          }),
          (t.prototype.forEachTarget = function(t, e) {
            return (
              this.queries.forEach(function(t, n) {
                return e(n)
              }),
              us.resolve()
            )
          }),
          (t.prototype.getLastRemoteSnapshotVersion = function(t) {
            return us.resolve(this.lastRemoteSnapshotVersion)
          }),
          (t.prototype.getHighestSequenceNumber = function(t) {
            return us.resolve(this.highestSequenceNumber)
          }),
          (t.prototype.allocateTargetId = function(t) {
            var e = this.targetIdGenerator.after(this.highestTargetId)
            return (this.highestTargetId = e), us.resolve(e)
          }),
          (t.prototype.setTargetsMetadata = function(t, e, n) {
            return (
              n && (this.lastRemoteSnapshotVersion = n),
              e > this.highestSequenceNumber &&
                (this.highestSequenceNumber = e),
              us.resolve()
            )
          }),
          (t.prototype.saveQueryData = function(t) {
            this.queries.set(t.query, t)
            var e = t.targetId
            e > this.highestTargetId && (this.highestTargetId = e),
              t.sequenceNumber > this.highestSequenceNumber &&
                (this.highestSequenceNumber = t.sequenceNumber)
          }),
          (t.prototype.addQueryData = function(t, e) {
            return (
              Zn(
                !this.queries.has(e.query),
                'Adding a query that already exists',
              ),
              this.saveQueryData(e),
              (this.targetCount += 1),
              us.resolve()
            )
          }),
          (t.prototype.updateQueryData = function(t, e) {
            return (
              Zn(this.queries.has(e.query), 'Updating a non-existent query'),
              this.saveQueryData(e),
              us.resolve()
            )
          }),
          (t.prototype.removeQueryData = function(t, e) {
            return (
              Zn(this.targetCount > 0, 'Removing a target from an empty cache'),
              Zn(
                this.queries.has(e.query),
                'Removing a non-existent target from the cache',
              ),
              this.queries.delete(e.query),
              this.references.removeReferencesForId(e.targetId),
              (this.targetCount -= 1),
              us.resolve()
            )
          }),
          (t.prototype.removeTargets = function(t, e, n) {
            var r = this,
              i = 0,
              o = []
            return (
              this.queries.forEach(function(s, a) {
                a.sequenceNumber <= e &&
                  !n[a.targetId] &&
                  (r.queries.delete(s),
                  o.push(r.removeMatchingKeysForTargetId(t, a.targetId)),
                  i++)
              }),
              us.waitFor(o).next(function() {
                return i
              })
            )
          }),
          (t.prototype.getQueryCount = function(t) {
            return us.resolve(this.targetCount)
          }),
          (t.prototype.getQueryData = function(t, e) {
            var n = this.queries.get(e) || null
            return us.resolve(n)
          }),
          (t.prototype.getQueryDataForTarget = function(t, e) {
            return $n('Not yet implemented.')
          }),
          (t.prototype.addMatchingKeys = function(t, e, n) {
            this.references.addReferences(e, n)
            var r = this.persistence.referenceDelegate,
              i = []
            return (
              r &&
                e.forEach(function(e) {
                  i.push(r.addReference(t, e))
                }),
              us.waitFor(i)
            )
          }),
          (t.prototype.removeMatchingKeys = function(t, e, n) {
            this.references.removeReferences(e, n)
            var r = this.persistence.referenceDelegate,
              i = []
            return (
              r &&
                e.forEach(function(e) {
                  i.push(r.removeReference(t, e))
                }),
              us.waitFor(i)
            )
          }),
          (t.prototype.removeMatchingKeysForTargetId = function(t, e) {
            return this.references.removeReferencesForId(e), us.resolve()
          }),
          (t.prototype.getMatchingKeysForTargetId = function(t, e) {
            var n = this.references.referencesForId(e)
            return us.resolve(n)
          }),
          (t.prototype.containsKey = function(t, e) {
            return us.resolve(this.references.containsKey(e))
          }),
          t
        )
      })()
    var Ia,
      Ca = (function() {
        function t(t) {
          ;(this.sizer = t),
            (this.docs = new Yr(jr.comparator)),
            (this.newDocumentChanges = uo()),
            (this.size = 0)
        }
        return (
          (t.prototype.addEntries = function(t, e, n) {
            for (var r = 0, i = e; r < i.length; r++) {
              var o = i[r],
                s = o.maybeDocument.key
              ;(this.docs = this.docs.insert(s, o)),
                (this.newDocumentChanges = this.newDocumentChanges.add(s))
            }
            return (this.size += n), us.resolve()
          }),
          (t.prototype.removeEntry = function(t, e) {
            var n = this.docs.get(e)
            return n
              ? ((this.docs = this.docs.remove(e)),
                (this.size -= n.size),
                us.resolve(n.size))
              : us.resolve(0)
          }),
          (t.prototype.getEntry = function(t, e) {
            var n = this.docs.get(e)
            return us.resolve(n ? n.maybeDocument : null)
          }),
          (t.prototype.getSizedEntry = function(t, e) {
            return us.resolve(this.docs.get(e))
          }),
          (t.prototype.getDocumentsMatchingQuery = function(t, e) {
            for (
              var n = io(),
                r = new jr(e.path.child('')),
                i = this.docs.getIteratorFrom(r);
              i.hasNext();

            ) {
              var o = i.getNext(),
                s = o.key,
                a = o.value.maybeDocument
              if (!e.path.isPrefixOf(s.path)) break
              a instanceof zr && e.matches(a) && (n = n.insert(a.key, a))
            }
            return us.resolve(n)
          }),
          (t.prototype.forEachDocumentKey = function(t, e) {
            return us.forEach(this.docs, function(t) {
              return e(t.key)
            })
          }),
          (t.prototype.getNewDocumentChanges = function(t) {
            var e = this,
              n = no()
            return (
              this.newDocumentChanges.forEach(function(t) {
                var r = e.docs.get(t),
                  i = r ? r.maybeDocument : new Hr(t, _i.forDeletedDoc())
                n = n.insert(t, i)
              }),
              (this.newDocumentChanges = uo()),
              us.resolve(n)
            )
          }),
          (t.prototype.newChangeBuffer = function() {
            return new Da(this.sizer, this)
          }),
          (t.prototype.getSize = function(t) {
            return us.resolve(this.size)
          }),
          t
        )
      })(),
      Da = (function(t) {
        function e(e, n) {
          var r = t.call(this) || this
          return (r.sizer = e), (r.documentCache = n), r
        }
        return (
          a(e, t),
          (e.prototype.applyChanges = function(t) {
            var e = this,
              n = 0,
              r = []
            return (
              this.assertChanges().forEach(function(t, i) {
                var o = e.documentSizes.get(t)
                Zn(
                  void 0 !== o,
                  'Attempting to change document ' +
                    t.toString() +
                    ' without having read it first',
                )
                var s = e.sizer(i)
                ;(n += s - o), r.push({ maybeDocument: i, size: s })
              }),
              this.documentCache.addEntries(t, r, n)
            )
          }),
          (e.prototype.getFromCache = function(t, e) {
            return this.documentCache.getSizedEntry(t, e)
          }),
          e
        )
      })(hs),
      Na = (function() {
        function t(t, e, n) {
          var r = this
          ;(this.clientId = t),
            (this.mutationQueues = {}),
            (this.listenSequence = new zo(0)),
            (this._started = !1),
            (this._started = !0),
            (this.referenceDelegate = e
              ? new ka(this)
              : new Ra(this, new ra(n))),
            (this.queryCache = new Ea(this))
          this.remoteDocumentCache = new Ca(function(t) {
            return r.referenceDelegate.documentSize(t)
          })
        }
        return (
          (t.createLruPersistence = function(e, n) {
            return new t(e, !1, n)
          }),
          (t.createEagerPersistence = function(e, n) {
            return new t(e, !0, n)
          }),
          (t.prototype.shutdown = function(t) {
            return (this._started = !1), Promise.resolve()
          }),
          Object.defineProperty(t.prototype, 'started', {
            get: function() {
              return this._started
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.getActiveClients = function() {
            return u(this, void 0, void 0, function() {
              return c(this, function(t) {
                return [2, [this.clientId]]
              })
            })
          }),
          (t.prototype.setPrimaryStateListener = function(t) {
            return t(!0)
          }),
          (t.prototype.setNetworkEnabled = function(t) {}),
          (t.prototype.getMutationQueue = function(t) {
            var e = this.mutationQueues[t.toKey()]
            return (
              e ||
                ((e = new Sa(this.referenceDelegate)),
                (this.mutationQueues[t.toKey()] = e)),
              e
            )
          }),
          (t.prototype.getQueryCache = function() {
            return this.queryCache
          }),
          (t.prototype.getRemoteDocumentCache = function() {
            return this.remoteDocumentCache
          }),
          (t.prototype.runTransaction = function(t, e, n) {
            var r = this
            Xn('MemoryPersistence', 'Starting transaction:', t)
            var i = new Aa(this.listenSequence.next())
            return (
              this.referenceDelegate.onTransactionStarted(),
              n(i)
                .next(function(t) {
                  return r.referenceDelegate
                    .onTransactionCommitted(i)
                    .next(function() {
                      return t
                    })
                })
                .toPromise()
            )
          }),
          (t.prototype.mutationQueuesContainKey = function(t, e) {
            return us.or(
              ((n = this.mutationQueues),
              (r = []),
              ur(n, function(t, e) {
                return r.push(e)
              }),
              r).map(function(n) {
                return function() {
                  return n.containsKey(t, e)
                }
              }),
            )
            var n, r
          }),
          t
        )
      })(),
      Aa = (function() {
        return function(t) {
          this.currentSequenceNumber = t
        }
      })(),
      ka = (function() {
        function t(t) {
          this.persistence = t
        }
        return (
          (t.prototype.setInMemoryPins = function(t) {
            this.inMemoryPins = t
          }),
          (t.prototype.addReference = function(t, e) {
            return this.orphanedDocuments.delete(e), us.resolve()
          }),
          (t.prototype.removeReference = function(t, e) {
            return this.orphanedDocuments.add(e), us.resolve()
          }),
          (t.prototype.removeMutationReference = function(t, e) {
            return this.orphanedDocuments.add(e), us.resolve()
          }),
          (t.prototype.removeTarget = function(t, e) {
            var n = this,
              r = this.persistence.getQueryCache()
            return r
              .getMatchingKeysForTargetId(t, e.targetId)
              .next(function(t) {
                t.forEach(function(t) {
                  return n.orphanedDocuments.add(t)
                })
              })
              .next(function() {
                return r.removeQueryData(t, e)
              })
          }),
          (t.prototype.onTransactionStarted = function() {
            this.orphanedDocuments = new Set()
          }),
          (t.prototype.onTransactionCommitted = function(t) {
            var e = this,
              n = this.persistence.getRemoteDocumentCache()
            return us.forEach(this.orphanedDocuments, function(r) {
              return e.isReferenced(t, r).next(function(e) {
                if (!e) return n.removeEntry(t, r).next()
              })
            })
          }),
          (t.prototype.updateLimboDocument = function(t, e) {
            var n = this
            return this.isReferenced(t, e).next(function(t) {
              t ? n.orphanedDocuments.delete(e) : n.orphanedDocuments.add(e)
            })
          }),
          (t.prototype.documentSize = function(t) {
            return 0
          }),
          (t.prototype.isReferenced = function(t, e) {
            var n = this
            return us.or([
              function() {
                return n.persistence.getQueryCache().containsKey(t, e)
              },
              function() {
                return n.persistence.mutationQueuesContainKey(t, e)
              },
              function() {
                return us.resolve(n.inMemoryPins.containsKey(e))
              },
            ])
          }),
          t
        )
      })(),
      Ra = (function() {
        function t(t, e) {
          ;(this.persistence = t),
            (this.serializer = e),
            (this.orphanedSequenceNumbers = new cs(function(t) {
              return es(t.path)
            })),
            (this.garbageCollector = new sa(this))
        }
        return (
          (t.prototype.onTransactionStarted = function() {}),
          (t.prototype.onTransactionCommitted = function(t) {
            return us.resolve()
          }),
          (t.prototype.forEachTarget = function(t, e) {
            return this.persistence.getQueryCache().forEachTarget(t, e)
          }),
          (t.prototype.getTargetCount = function(t) {
            return this.persistence.getQueryCache().getTargetCount(t)
          }),
          (t.prototype.forEachOrphanedDocumentSequenceNumber = function(t, e) {
            var n = this
            return us.forEach(this.orphanedSequenceNumbers, function(r) {
              var i = r.key,
                o = r.value
              return n.isPinned(t, i, o).next(function(t) {
                return t ? us.resolve() : e(o)
              })
            })
          }),
          (t.prototype.setInMemoryPins = function(t) {
            this.inMemoryPins = t
          }),
          (t.prototype.removeTargets = function(t, e, n) {
            return this.persistence.getQueryCache().removeTargets(t, e, n)
          }),
          (t.prototype.removeOrphanedDocuments = function(t, e) {
            var n = this,
              r = 0,
              i = this.persistence.getRemoteDocumentCache()
            return i
              .forEachDocumentKey(t, function(o) {
                return n.isPinned(t, o, e).next(function(e) {
                  return e ? us.resolve() : (r++, i.removeEntry(t, o).next())
                })
              })
              .next(function() {
                return r
              })
          }),
          (t.prototype.removeMutationReference = function(t, e) {
            return (
              this.orphanedSequenceNumbers.set(e, t.currentSequenceNumber),
              us.resolve()
            )
          }),
          (t.prototype.removeTarget = function(t, e) {
            var n = e.copy({ sequenceNumber: t.currentSequenceNumber })
            return this.persistence.getQueryCache().updateQueryData(t, n)
          }),
          (t.prototype.addReference = function(t, e) {
            return (
              this.orphanedSequenceNumbers.set(e, t.currentSequenceNumber),
              us.resolve()
            )
          }),
          (t.prototype.removeReference = function(t, e) {
            return (
              this.orphanedSequenceNumbers.set(e, t.currentSequenceNumber),
              us.resolve()
            )
          }),
          (t.prototype.updateLimboDocument = function(t, e) {
            return (
              this.orphanedSequenceNumbers.set(e, t.currentSequenceNumber),
              us.resolve()
            )
          }),
          (t.prototype.documentSize = function(t) {
            var e,
              n = this.serializer.toDbRemoteDocument(t)
            if (n.document) e = n.document
            else if (n.unknownDocument) e = n.unknownDocument
            else {
              if (!n.noDocument) throw $n('Unknown remote document type')
              e = n.noDocument
            }
            return JSON.stringify(e).length
          }),
          (t.prototype.isPinned = function(t, e, n) {
            var r = this
            return us.or([
              function() {
                return r.persistence.mutationQueuesContainKey(t, e)
              },
              function() {
                return us.resolve(r.inMemoryPins.containsKey(e))
              },
              function() {
                return r.persistence.getQueryCache().containsKey(t, e)
              },
              function() {
                var t = r.orphanedSequenceNumbers.get(e)
                return us.resolve(void 0 !== t && t > n)
              },
            ])
          }),
          t
        )
      })(),
      Ma = (function() {
        function t(t, e, n, r, i) {
          ;(this.queue = t),
            (this.timerId = e),
            (this.initialDelayMs = n),
            (this.backoffFactor = r),
            (this.maxDelayMs = i),
            (this.timerPromise = null),
            (this.lastAttemptTime = Date.now()),
            this.reset()
        }
        return (
          (t.prototype.reset = function() {
            this.currentBaseMs = 0
          }),
          (t.prototype.resetToMax = function() {
            this.currentBaseMs = this.maxDelayMs
          }),
          (t.prototype.backoffAndRun = function(t) {
            var e = this
            this.cancel()
            var n = Math.floor(this.currentBaseMs + this.jitterDelayMs()),
              r = Math.max(0, Date.now() - this.lastAttemptTime),
              i = Math.max(0, n - r)
            this.currentBaseMs > 0 &&
              Xn(
                'ExponentialBackoff',
                'Backing off for ' +
                  i +
                  ' ms (base delay: ' +
                  this.currentBaseMs +
                  ' ms, delay with jitter: ' +
                  n +
                  ' ms, last attempt: ' +
                  r +
                  ' ms ago)',
              ),
              (this.timerPromise = this.queue.enqueueAfterDelay(
                this.timerId,
                i,
                function() {
                  return (e.lastAttemptTime = Date.now()), t()
                },
              )),
              (this.currentBaseMs *= this.backoffFactor),
              this.currentBaseMs < this.initialDelayMs &&
                (this.currentBaseMs = this.initialDelayMs),
              this.currentBaseMs > this.maxDelayMs &&
                (this.currentBaseMs = this.maxDelayMs)
          }),
          (t.prototype.cancel = function() {
            null !== this.timerPromise &&
              (this.timerPromise.cancel(), (this.timerPromise = null))
          }),
          (t.prototype.jitterDelayMs = function() {
            return (Math.random() - 0.5) * this.currentBaseMs
          }),
          t
        )
      })()
    !(function(t) {
      ;(t[(t.Initial = 0)] = 'Initial'),
        (t[(t.Starting = 1)] = 'Starting'),
        (t[(t.Open = 2)] = 'Open'),
        (t[(t.Error = 3)] = 'Error'),
        (t[(t.Backoff = 4)] = 'Backoff')
    })(Ia || (Ia = {}))
    var Oa,
      Pa,
      _a = 1e3,
      La = 6e4,
      xa = 1.5,
      qa = (function() {
        function t(t, e, n, r, i, o) {
          ;(this.queue = t),
            (this.idleTimerId = n),
            (this.connection = r),
            (this.credentialsProvider = i),
            (this.listener = o),
            (this.state = Ia.Initial),
            (this.closeCount = 0),
            (this.idleTimer = null),
            (this.stream = null),
            (this.backoff = new Ma(t, e, _a, xa, La))
        }
        return (
          (t.prototype.isStarted = function() {
            return (
              this.state === Ia.Starting ||
              this.state === Ia.Open ||
              this.state === Ia.Backoff
            )
          }),
          (t.prototype.isOpen = function() {
            return this.state === Ia.Open
          }),
          (t.prototype.start = function() {
            this.state !== Ia.Error
              ? (Zn(this.state === Ia.Initial, 'Already started'), this.auth())
              : this.performBackoff()
          }),
          (t.prototype.stop = function() {
            return u(this, void 0, void 0, function() {
              return c(this, function(t) {
                switch (t.label) {
                  case 0:
                    return this.isStarted()
                      ? [4, this.close(Ia.Initial)]
                      : [3, 2]
                  case 1:
                    t.sent(), (t.label = 2)
                  case 2:
                    return [2]
                }
              })
            })
          }),
          (t.prototype.inhibitBackoff = function() {
            Zn(
              !this.isStarted(),
              'Can only inhibit backoff in a stopped state',
            ),
              (this.state = Ia.Initial),
              this.backoff.reset()
          }),
          (t.prototype.markIdle = function() {
            var t = this
            this.isOpen() &&
              null === this.idleTimer &&
              (this.idleTimer = this.queue.enqueueAfterDelay(
                this.idleTimerId,
                6e4,
                function() {
                  return t.handleIdleCloseTimer()
                },
              ))
          }),
          (t.prototype.sendRequest = function(t) {
            this.cancelIdleCheck(), this.stream.send(t)
          }),
          (t.prototype.handleIdleCloseTimer = function() {
            return u(this, void 0, void 0, function() {
              return c(this, function(t) {
                return this.isOpen() ? [2, this.close(Ia.Initial)] : [2]
              })
            })
          }),
          (t.prototype.cancelIdleCheck = function() {
            this.idleTimer && (this.idleTimer.cancel(), (this.idleTimer = null))
          }),
          (t.prototype.close = function(t, e) {
            return u(this, void 0, void 0, function() {
              return c(this, function(n) {
                switch (n.label) {
                  case 0:
                    return (
                      Zn(
                        this.isStarted(),
                        'Only started streams should be closed.',
                      ),
                      Zn(
                        t === Ia.Error || wi(e),
                        "Can't provide an error when not in an error state.",
                      ),
                      this.cancelIdleCheck(),
                      this.backoff.cancel(),
                      this.closeCount++,
                      t !== Ia.Error
                        ? this.backoff.reset()
                        : e && e.code === nr.RESOURCE_EXHAUSTED
                          ? (Yn(e.toString()),
                            Yn(
                              'Using maximum backoff delay to prevent overloading the backend.',
                            ),
                            this.backoff.resetToMax())
                          : e &&
                            e.code === nr.UNAUTHENTICATED &&
                            this.credentialsProvider.invalidateToken(),
                      null !== this.stream &&
                        (this.tearDown(),
                        this.stream.close(),
                        (this.stream = null)),
                      (this.state = t),
                      [4, this.listener.onClose(e)]
                    )
                  case 1:
                    return n.sent(), [2]
                }
              })
            })
          }),
          (t.prototype.tearDown = function() {}),
          (t.prototype.auth = function() {
            var t = this
            Zn(this.state === Ia.Initial, 'Must be in initial state to auth'),
              (this.state = Ia.Starting)
            var e = this.getCloseGuardedDispatcher(this.closeCount),
              n = this.closeCount
            this.credentialsProvider.getToken().then(
              function(e) {
                t.closeCount === n && t.startStream(e)
              },
              function(n) {
                e(function() {
                  var e = new rr(
                    nr.UNKNOWN,
                    'Fetching auth token failed: ' + n.message,
                  )
                  return t.handleStreamClose(e)
                })
              },
            )
          }),
          (t.prototype.startStream = function(t) {
            var e = this
            Zn(
              this.state === Ia.Starting,
              'Trying to start stream in a non-starting state',
            )
            var n = this.getCloseGuardedDispatcher(this.closeCount)
            ;(this.stream = this.startRpc(t)),
              this.stream.onOpen(function() {
                n(function() {
                  return (
                    Zn(
                      e.state === Ia.Starting,
                      'Expected stream to be in state Starting, but was ' +
                        e.state,
                    ),
                    (e.state = Ia.Open),
                    e.listener.onOpen()
                  )
                })
              }),
              this.stream.onClose(function(t) {
                n(function() {
                  return e.handleStreamClose(t)
                })
              }),
              this.stream.onMessage(function(t) {
                n(function() {
                  return e.onMessage(t)
                })
              })
          }),
          (t.prototype.performBackoff = function() {
            var t = this
            Zn(
              this.state === Ia.Error,
              'Should only perform backoff when in Error state',
            ),
              (this.state = Ia.Backoff),
              this.backoff.backoffAndRun(function() {
                return u(t, void 0, void 0, function() {
                  return c(this, function(t) {
                    return (
                      Zn(
                        this.state === Ia.Backoff,
                        'Backoff elapsed but state is now: ' + this.state,
                      ),
                      (this.state = Ia.Initial),
                      this.start(),
                      Zn(
                        this.isStarted(),
                        'PersistentStream should have started',
                      ),
                      [2]
                    )
                  })
                })
              })
          }),
          (t.prototype.handleStreamClose = function(t) {
            return (
              Zn(
                this.isStarted(),
                "Can't handle server close on non-started stream",
              ),
              Xn('PersistentStream', 'close with error: ' + t),
              (this.stream = null),
              this.close(Ia.Error, t)
            )
          }),
          (t.prototype.getCloseGuardedDispatcher = function(t) {
            var e = this
            return function(n) {
              e.queue.enqueueAndForget(function() {
                return e.closeCount === t
                  ? n()
                  : (Xn(
                      'PersistentStream',
                      'stream callback skipped by getCloseGuardedDispatcher.',
                    ),
                    Promise.resolve())
              })
            }
          }),
          t
        )
      })(),
      Fa = (function(t) {
        function e(e, n, r, i, o) {
          var s =
            t.call(
              this,
              e,
              Go.ListenStreamConnectionBackoff,
              Go.ListenStreamIdle,
              n,
              r,
              o,
            ) || this
          return (s.serializer = i), s
        }
        return (
          a(e, t),
          (e.prototype.startRpc = function(t) {
            return this.connection.openStream('Listen', t)
          }),
          (e.prototype.onMessage = function(t) {
            this.backoff.reset()
            var e = this.serializer.fromWatchChange(t),
              n = this.serializer.versionFromListenResponse(t)
            return this.listener.onWatchChange(e, n)
          }),
          (e.prototype.watch = function(t) {
            var e = {}
            ;(e.database = this.serializer.encodedDatabaseId),
              (e.addTarget = this.serializer.toTarget(t))
            var n = this.serializer.toListenRequestLabels(t)
            n && (e.labels = n), this.sendRequest(e)
          }),
          (e.prototype.unwatch = function(t) {
            var e = {}
            ;(e.database = this.serializer.encodedDatabaseId),
              (e.removeTarget = t),
              this.sendRequest(e)
          }),
          e
        )
      })(qa),
      Va = (function(t) {
        function e(e, n, r, i, o) {
          var s =
            t.call(
              this,
              e,
              Go.WriteStreamConnectionBackoff,
              Go.WriteStreamIdle,
              n,
              r,
              o,
            ) || this
          return (s.serializer = i), (s.handshakeComplete_ = !1), s
        }
        return (
          a(e, t),
          Object.defineProperty(e.prototype, 'handshakeComplete', {
            get: function() {
              return this.handshakeComplete_
            },
            enumerable: !0,
            configurable: !0,
          }),
          (e.prototype.start = function() {
            ;(this.handshakeComplete_ = !1), t.prototype.start.call(this)
          }),
          (e.prototype.tearDown = function() {
            this.handshakeComplete_ && this.writeMutations([])
          }),
          (e.prototype.startRpc = function(t) {
            return this.connection.openStream('Write', t)
          }),
          (e.prototype.onMessage = function(t) {
            if (
              (Zn(
                !!t.streamToken,
                'Got a write response without a stream token',
              ),
              (this.lastStreamToken = t.streamToken),
              this.handshakeComplete_)
            ) {
              this.backoff.reset()
              var e = this.serializer.fromWriteResults(
                  t.writeResults,
                  t.commitTime,
                ),
                n = this.serializer.fromVersion(t.commitTime)
              return this.listener.onMutationResult(n, e)
            }
            return (
              Zn(
                !t.writeResults || 0 === t.writeResults.length,
                'Got mutation results for handshake',
              ),
              (this.handshakeComplete_ = !0),
              this.listener.onHandshakeComplete()
            )
          }),
          (e.prototype.writeHandshake = function() {
            Zn(this.isOpen(), 'Writing handshake requires an opened stream'),
              Zn(!this.handshakeComplete_, 'Handshake already completed')
            var t = {}
            ;(t.database = this.serializer.encodedDatabaseId),
              this.sendRequest(t)
          }),
          (e.prototype.writeMutations = function(t) {
            var e = this
            Zn(this.isOpen(), 'Writing mutations requires an opened stream'),
              Zn(
                this.handshakeComplete_,
                'Handshake must be complete before writing mutations',
              ),
              Zn(
                this.lastStreamToken.length > 0,
                'Trying to write mutation without a token',
              )
            var n = {
              streamToken: this.lastStreamToken,
              writes: t.map(function(t) {
                return e.serializer.toMutation(t)
              }),
            }
            this.sendRequest(n)
          }),
          e
        )
      })(qa),
      Ba = (function() {
        function t(t, e, n, r) {
          ;(this.queue = t),
            (this.connection = e),
            (this.credentials = n),
            (this.serializer = r)
        }
        return (
          (t.prototype.newPersistentWriteStream = function(t) {
            return new Va(
              this.queue,
              this.connection,
              this.credentials,
              this.serializer,
              t,
            )
          }),
          (t.prototype.newPersistentWatchStream = function(t) {
            return new Fa(
              this.queue,
              this.connection,
              this.credentials,
              this.serializer,
              t,
            )
          }),
          (t.prototype.commit = function(t) {
            var e = this,
              n = {
                database: this.serializer.encodedDatabaseId,
                writes: t.map(function(t) {
                  return e.serializer.toMutation(t)
                }),
              }
            return this.invokeRPC('Commit', n).then(function(t) {
              return e.serializer.fromWriteResults(t.writeResults, t.commitTime)
            })
          }),
          (t.prototype.lookup = function(t) {
            var e = this,
              n = {
                database: this.serializer.encodedDatabaseId,
                documents: t.map(function(t) {
                  return e.serializer.toName(t)
                }),
              }
            return this.invokeStreamingRPC('BatchGetDocuments', n).then(
              function(n) {
                var r = no()
                n.forEach(function(t) {
                  var n = e.serializer.fromMaybeDocument(t)
                  r = r.insert(n.key, n)
                })
                var i = []
                return (
                  t.forEach(function(t) {
                    var e = r.get(t)
                    Zn(!!e, 'Missing entity in write response for ' + t),
                      i.push(e)
                  }),
                  i
                )
              },
            )
          }),
          (t.prototype.invokeRPC = function(t, e) {
            var n = this
            return this.credentials
              .getToken()
              .then(function(r) {
                return n.connection.invokeRPC(t, e, r)
              })
              .catch(function(t) {
                throw (t.code === nr.UNAUTHENTICATED &&
                  n.credentials.invalidateToken(),
                t)
              })
          }),
          (t.prototype.invokeStreamingRPC = function(t, e) {
            var n = this
            return this.credentials
              .getToken()
              .then(function(r) {
                return n.connection.invokeStreamingRPC(t, e, r)
              })
              .catch(function(t) {
                throw (t.code === nr.UNAUTHENTICATED &&
                  n.credentials.invalidateToken(),
                t)
              })
          }),
          t
        )
      })(),
      Ua = (function() {
        function t(t) {
          ;(this.datastore = t),
            (this.readVersions = so()),
            (this.mutations = []),
            (this.committed = !1)
        }
        return (
          (t.prototype.recordVersion = function(t) {
            var e
            if (t instanceof zr) e = t.version
            else {
              if (!(t instanceof Hr))
                throw $n(
                  'Document in a transaction was a ' + t.constructor.name,
                )
              e = _i.forDeletedDoc()
            }
            var n = this.readVersions.get(t.key)
            if (null !== n) {
              if (!e.isEqual(n))
                throw new rr(
                  nr.ABORTED,
                  'Document version changed between two reads.',
                )
            } else this.readVersions = this.readVersions.insert(t.key, e)
          }),
          (t.prototype.lookup = function(t) {
            var e = this
            return this.committed
              ? Promise.reject('Transaction has already completed.')
              : this.mutations.length > 0
                ? Promise.reject(
                    'Transactions lookups are invalid after writes.',
                  )
                : this.datastore.lookup(t).then(function(t) {
                    return (
                      t.forEach(function(t) {
                        t instanceof Hr || t instanceof zr
                          ? e.recordVersion(t)
                          : $n(
                              'Document in a transaction was a ' +
                                t.constructor.name,
                            )
                      }),
                      t
                    )
                  })
          }),
          (t.prototype.write = function(t) {
            if (this.committed)
              throw new rr(
                nr.FAILED_PRECONDITION,
                'Transaction has already completed.',
              )
            this.mutations = this.mutations.concat(t)
          }),
          (t.prototype.precondition = function(t) {
            var e = this.readVersions.get(t)
            return e ? Bi.updateTime(e) : Bi.NONE
          }),
          (t.prototype.preconditionForUpdate = function(t) {
            var e = this.readVersions.get(t)
            if (e && e.isEqual(_i.forDeletedDoc()))
              throw new rr(
                nr.FAILED_PRECONDITION,
                "Can't update a document that doesn't exist.",
              )
            return e ? Bi.updateTime(e) : Bi.exists(!0)
          }),
          (t.prototype.set = function(t, e) {
            this.write(e.toMutations(t, this.precondition(t)))
          }),
          (t.prototype.update = function(t, e) {
            this.write(e.toMutations(t, this.preconditionForUpdate(t)))
          }),
          (t.prototype.delete = function(t) {
            this.write([new ji(t, this.precondition(t))]),
              (this.readVersions = this.readVersions.insert(
                t,
                _i.forDeletedDoc(),
              ))
          }),
          (t.prototype.commit = function() {
            var t = this,
              e = this.readVersions
            return (
              this.mutations.forEach(function(t) {
                e = e.remove(t.key)
              }),
              e.isEmpty()
                ? this.datastore.commit(this.mutations).then(function() {
                    t.committed = !0
                  })
                : Promise.reject(
                    Error(
                      'Every document read in a transaction must also be written.',
                    ),
                  )
            )
          }),
          t
        )
      })()
    !(function(t) {
      ;(t[(t.Unknown = 0)] = 'Unknown'),
        (t[(t.Online = 1)] = 'Online'),
        (t[(t.Offline = 2)] = 'Offline')
    })(Oa || (Oa = {})),
      (function(t) {
        ;(t[(t.RemoteStore = 0)] = 'RemoteStore'),
          (t[(t.SharedClientState = 1)] = 'SharedClientState')
      })(Pa || (Pa = {}))
    var Qa = (function() {
        function t(t, e) {
          ;(this.asyncQueue = t),
            (this.onlineStateHandler = e),
            (this.state = Oa.Unknown),
            (this.watchStreamFailures = 0),
            (this.onlineStateTimer = null),
            (this.shouldWarnClientIsOffline = !0)
        }
        return (
          (t.prototype.handleWatchStreamStart = function() {
            var t = this
            0 === this.watchStreamFailures &&
              (this.setAndBroadcast(Oa.Unknown),
              Zn(
                null === this.onlineStateTimer,
                "onlineStateTimer shouldn't be started yet",
              ),
              (this.onlineStateTimer = this.asyncQueue.enqueueAfterDelay(
                Go.OnlineStateTimeout,
                1e4,
                function() {
                  return (
                    (t.onlineStateTimer = null),
                    Zn(
                      t.state === Oa.Unknown,
                      'Timer should be canceled if we transitioned to a different state.',
                    ),
                    t.logClientOfflineWarningIfNecessary(
                      "Backend didn't respond within 10 seconds.",
                    ),
                    t.setAndBroadcast(Oa.Offline),
                    Promise.resolve()
                  )
                },
              )))
          }),
          (t.prototype.handleWatchStreamFailure = function(t) {
            this.state === Oa.Online
              ? (this.setAndBroadcast(Oa.Unknown),
                Zn(
                  0 === this.watchStreamFailures,
                  'watchStreamFailures must be 0',
                ),
                Zn(
                  null === this.onlineStateTimer,
                  'onlineStateTimer must be null',
                ))
              : (this.watchStreamFailures++,
                this.watchStreamFailures >= 1 &&
                  (this.clearOnlineStateTimer(),
                  this.logClientOfflineWarningIfNecessary(
                    'Connection failed 1 times. Most recent error: ' +
                      t.toString(),
                  ),
                  this.setAndBroadcast(Oa.Offline)))
          }),
          (t.prototype.set = function(t) {
            this.clearOnlineStateTimer(),
              (this.watchStreamFailures = 0),
              t === Oa.Online && (this.shouldWarnClientIsOffline = !1),
              this.setAndBroadcast(t)
          }),
          (t.prototype.setAndBroadcast = function(t) {
            t !== this.state && ((this.state = t), this.onlineStateHandler(t))
          }),
          (t.prototype.logClientOfflineWarningIfNecessary = function(t) {
            var e =
              'Could not reach Cloud Firestore backend. ' +
              t +
              '\nThis typically indicates that your device does not have a healthy Internet connection at the moment. The client will operate in offline mode until it is able to successfully connect to the backend.'
            this.shouldWarnClientIsOffline
              ? (Yn(e), (this.shouldWarnClientIsOffline = !1))
              : Xn('OnlineStateTracker', e)
          }),
          (t.prototype.clearOnlineStateTimer = function() {
            null !== this.onlineStateTimer &&
              (this.onlineStateTimer.cancel(), (this.onlineStateTimer = null))
          }),
          t
        )
      })(),
      Ka = (function() {
        function t(t, e, n, r) {
          ;(this.localStore = t),
            (this.datastore = e),
            (this.writePipeline = []),
            (this.listenTargets = {}),
            (this.watchChangeAggregator = null),
            (this.networkEnabled = !1),
            (this.isPrimary = !1),
            (this.onlineStateTracker = new Qa(n, r)),
            (this.watchStream = this.datastore.newPersistentWatchStream({
              onOpen: this.onWatchStreamOpen.bind(this),
              onClose: this.onWatchStreamClose.bind(this),
              onWatchChange: this.onWatchStreamChange.bind(this),
            })),
            (this.writeStream = this.datastore.newPersistentWriteStream({
              onOpen: this.onWriteStreamOpen.bind(this),
              onClose: this.onWriteStreamClose.bind(this),
              onHandshakeComplete: this.onWriteHandshakeComplete.bind(this),
              onMutationResult: this.onMutationResult.bind(this),
            }))
        }
        return (
          (t.prototype.start = function() {
            return this.enableNetwork()
          }),
          (t.prototype.enableNetwork = function() {
            return u(this, void 0, void 0, function() {
              var t
              return c(this, function(e) {
                switch (e.label) {
                  case 0:
                    return (
                      (this.networkEnabled = !0),
                      this.canUseNetwork()
                        ? ((t = this.writeStream),
                          [4, this.localStore.getLastStreamToken()])
                        : [3, 3]
                    )
                  case 1:
                    return (
                      (t.lastStreamToken = e.sent()),
                      this.shouldStartWatchStream()
                        ? this.startWatchStream()
                        : this.onlineStateTracker.set(Oa.Unknown),
                      [4, this.fillWritePipeline()]
                    )
                  case 2:
                    e.sent(), (e.label = 3)
                  case 3:
                    return [2]
                }
              })
            })
          }),
          (t.prototype.disableNetwork = function() {
            return u(this, void 0, void 0, function() {
              return c(this, function(t) {
                switch (t.label) {
                  case 0:
                    return (
                      (this.networkEnabled = !1),
                      [4, this.disableNetworkInternal()]
                    )
                  case 1:
                    return (
                      t.sent(), this.onlineStateTracker.set(Oa.Offline), [2]
                    )
                }
              })
            })
          }),
          (t.prototype.disableNetworkInternal = function() {
            return u(this, void 0, void 0, function() {
              return c(this, function(t) {
                switch (t.label) {
                  case 0:
                    return [4, this.writeStream.stop()]
                  case 1:
                    return t.sent(), [4, this.watchStream.stop()]
                  case 2:
                    return (
                      t.sent(),
                      this.writePipeline.length > 0 &&
                        (Xn(
                          'RemoteStore',
                          'Stopping write stream with ' +
                            this.writePipeline.length +
                            ' pending writes',
                        ),
                        (this.writePipeline = [])),
                      this.cleanUpWatchStreamState(),
                      [2]
                    )
                }
              })
            })
          }),
          (t.prototype.shutdown = function() {
            return u(this, void 0, void 0, function() {
              return c(this, function(t) {
                switch (t.label) {
                  case 0:
                    return (
                      Xn('RemoteStore', 'RemoteStore shutting down.'),
                      (this.networkEnabled = !1),
                      [4, this.disableNetworkInternal()]
                    )
                  case 1:
                    return (
                      t.sent(), this.onlineStateTracker.set(Oa.Unknown), [2]
                    )
                }
              })
            })
          }),
          (t.prototype.listen = function(t) {
            Zn(
              !or(this.listenTargets, t.targetId),
              'listen called with duplicate targetId!',
            ),
              (this.listenTargets[t.targetId] = t),
              this.shouldStartWatchStream()
                ? this.startWatchStream()
                : this.watchStream.isOpen() && this.sendWatchRequest(t)
          }),
          (t.prototype.unlisten = function(t) {
            Zn(
              or(this.listenTargets, t),
              'unlisten called without assigned target ID!',
            ),
              delete this.listenTargets[t],
              this.watchStream.isOpen() && this.sendUnwatchRequest(t),
              cr(this.listenTargets) &&
                (this.watchStream.isOpen()
                  ? this.watchStream.markIdle()
                  : this.canUseNetwork() &&
                    this.onlineStateTracker.set(Oa.Unknown))
          }),
          (t.prototype.getQueryDataForTarget = function(t) {
            return this.listenTargets[t] || null
          }),
          (t.prototype.getRemoteKeysForTarget = function(t) {
            return this.syncEngine.getRemoteKeysForTarget(t)
          }),
          (t.prototype.sendWatchRequest = function(t) {
            this.watchChangeAggregator.recordPendingTargetRequest(t.targetId),
              this.watchStream.watch(t)
          }),
          (t.prototype.sendUnwatchRequest = function(t) {
            this.watchChangeAggregator.recordPendingTargetRequest(t),
              this.watchStream.unwatch(t)
          }),
          (t.prototype.startWatchStream = function() {
            Zn(
              this.shouldStartWatchStream(),
              'startWatchStream() called when shouldStartWatchStream() is false.',
            ),
              (this.watchChangeAggregator = new Io(this)),
              this.watchStream.start(),
              this.onlineStateTracker.handleWatchStreamStart()
          }),
          (t.prototype.shouldStartWatchStream = function() {
            return (
              this.canUseNetwork() &&
              !this.watchStream.isStarted() &&
              !cr(this.listenTargets)
            )
          }),
          (t.prototype.canUseNetwork = function() {
            return this.isPrimary && this.networkEnabled
          }),
          (t.prototype.cleanUpWatchStreamState = function() {
            this.watchChangeAggregator = null
          }),
          (t.prototype.onWatchStreamOpen = function() {
            return u(this, void 0, void 0, function() {
              var t = this
              return c(this, function(e) {
                return (
                  ar(this.listenTargets, function(e, n) {
                    t.sendWatchRequest(n)
                  }),
                  [2]
                )
              })
            })
          }),
          (t.prototype.onWatchStreamClose = function(t) {
            return u(this, void 0, void 0, function() {
              return c(this, function(e) {
                return (
                  void 0 === t &&
                    Zn(
                      !this.shouldStartWatchStream(),
                      'Watch stream was stopped gracefully while still needed.',
                    ),
                  this.cleanUpWatchStreamState(),
                  this.shouldStartWatchStream()
                    ? (this.onlineStateTracker.handleWatchStreamFailure(t),
                      this.startWatchStream())
                    : this.onlineStateTracker.set(Oa.Unknown),
                  [2]
                )
              })
            })
          }),
          (t.prototype.onWatchStreamChange = function(t, e) {
            return u(this, void 0, void 0, function() {
              var n
              return c(this, function(r) {
                switch (r.label) {
                  case 0:
                    return (
                      this.onlineStateTracker.set(Oa.Online),
                      t instanceof So && t.state === mo.Removed && t.cause
                        ? [2, this.handleTargetError(t)]
                        : (t instanceof wo
                            ? this.watchChangeAggregator.handleDocumentChange(t)
                            : t instanceof To
                              ? this.watchChangeAggregator.handleExistenceFilter(
                                  t,
                                )
                              : (Zn(
                                  t instanceof So,
                                  'Expected watchChange to be an instance of WatchTargetChange',
                                ),
                                this.watchChangeAggregator.handleTargetChange(
                                  t,
                                )),
                          e.isEqual(_i.MIN)
                            ? [3, 3]
                            : [
                                4,
                                this.localStore.getLastRemoteSnapshotVersion(),
                              ])
                    )
                  case 1:
                    return (
                      (n = r.sent()),
                      e.compareTo(n) >= 0
                        ? [4, this.raiseWatchSnapshot(e)]
                        : [3, 3]
                    )
                  case 2:
                    r.sent(), (r.label = 3)
                  case 3:
                    return [2]
                }
              })
            })
          }),
          (t.prototype.raiseWatchSnapshot = function(t) {
            var e = this
            Zn(
              !t.isEqual(_i.MIN),
              "Can't raise event for unknown SnapshotVersion",
            )
            var n = this.watchChangeAggregator.createRemoteEvent(t)
            return (
              ar(n.targetChanges, function(n, r) {
                if (r.resumeToken.length > 0) {
                  var i = e.listenTargets[n]
                  i &&
                    (e.listenTargets[n] = i.copy({
                      resumeToken: r.resumeToken,
                      snapshotVersion: t,
                    }))
                }
              }),
              n.targetMismatches.forEach(function(t) {
                var n = e.listenTargets[t]
                if (n) {
                  ;(e.listenTargets[t] = n.copy({ resumeToken: er() })),
                    e.sendUnwatchRequest(t)
                  var r = new xi(
                    n.query,
                    t,
                    Si.ExistenceFilterMismatch,
                    n.sequenceNumber,
                  )
                  e.sendWatchRequest(r)
                }
              }),
              this.syncEngine.applyRemoteEvent(n)
            )
          }),
          (t.prototype.handleTargetError = function(t) {
            var e = this
            Zn(!!t.cause, 'Handling target error without a cause')
            var n = t.cause,
              r = Promise.resolve()
            return (
              t.targetIds.forEach(function(t) {
                r = r.then(function() {
                  return u(e, void 0, void 0, function() {
                    return c(this, function(e) {
                      return or(this.listenTargets, t)
                        ? (delete this.listenTargets[t],
                          this.watchChangeAggregator.removeTarget(t),
                          [2, this.syncEngine.rejectListen(t, n)])
                        : [2]
                    })
                  })
                })
              }),
              r
            )
          }),
          (t.prototype.fillWritePipeline = function() {
            return u(this, void 0, void 0, function() {
              var t, e
              return c(this, function(n) {
                switch (n.label) {
                  case 0:
                    return this.canAddToWritePipeline()
                      ? ((t =
                          this.writePipeline.length > 0
                            ? this.writePipeline[this.writePipeline.length - 1]
                                .batchId
                            : os),
                        [4, this.localStore.nextMutationBatch(t)])
                      : [3, 4]
                  case 1:
                    return null !== (e = n.sent())
                      ? [3, 2]
                      : (0 === this.writePipeline.length &&
                          this.writeStream.markIdle(),
                        [3, 4])
                  case 2:
                    return (
                      this.addToWritePipeline(e), [4, this.fillWritePipeline()]
                    )
                  case 3:
                    n.sent(), (n.label = 4)
                  case 4:
                    return (
                      this.shouldStartWriteStream() && this.startWriteStream(),
                      [2]
                    )
                }
              })
            })
          }),
          (t.prototype.canAddToWritePipeline = function() {
            return this.canUseNetwork() && this.writePipeline.length < 10
          }),
          (t.prototype.outstandingWrites = function() {
            return this.writePipeline.length
          }),
          (t.prototype.addToWritePipeline = function(t) {
            Zn(
              this.canAddToWritePipeline(),
              'addToWritePipeline called when pipeline is full',
            ),
              this.writePipeline.push(t),
              this.writeStream.isOpen() &&
                this.writeStream.handshakeComplete &&
                this.writeStream.writeMutations(t.mutations)
          }),
          (t.prototype.shouldStartWriteStream = function() {
            return (
              this.canUseNetwork() &&
              !this.writeStream.isStarted() &&
              this.writePipeline.length > 0
            )
          }),
          (t.prototype.startWriteStream = function() {
            Zn(
              this.shouldStartWriteStream(),
              'startWriteStream() called when shouldStartWriteStream() is false.',
            ),
              this.writeStream.start()
          }),
          (t.prototype.onWriteStreamOpen = function() {
            return u(this, void 0, void 0, function() {
              return c(this, function(t) {
                return this.writeStream.writeHandshake(), [2]
              })
            })
          }),
          (t.prototype.onWriteHandshakeComplete = function() {
            var t = this
            return this.localStore
              .setLastStreamToken(this.writeStream.lastStreamToken)
              .then(function() {
                for (var e = 0, n = t.writePipeline; e < n.length; e++) {
                  var r = n[e]
                  t.writeStream.writeMutations(r.mutations)
                }
              })
              .catch(function(e) {
                return t.ignoreIfPrimaryLeaseLoss(e)
              })
          }),
          (t.prototype.ignoreIfPrimaryLeaseLoss = function(t) {
            if (!da(t)) throw t
            Xn('RemoteStore', 'Unexpectedly lost primary lease')
          }),
          (t.prototype.onMutationResult = function(t, e) {
            var n = this
            Zn(
              this.writePipeline.length > 0,
              'Got result for empty write pipeline',
            )
            var r = this.writePipeline.shift(),
              i = as.from(r, t, e, this.writeStream.lastStreamToken)
            return this.syncEngine.applySuccessfulWrite(i).then(function() {
              return n.fillWritePipeline()
            })
          }),
          (t.prototype.onWriteStreamClose = function(t) {
            return u(this, void 0, void 0, function() {
              var e = this
              return c(this, function(n) {
                return (
                  void 0 === t &&
                    Zn(
                      !this.shouldStartWriteStream(),
                      'Write stream was stopped gracefully while still needed.',
                    ),
                  t && this.writePipeline.length > 0
                    ? (void 0,
                      [
                        2,
                        (this.writeStream.handshakeComplete
                          ? this.handleWriteError(t)
                          : this.handleHandshakeError(t)
                        ).then(function() {
                          e.shouldStartWriteStream() && e.startWriteStream()
                        }),
                      ])
                    : [2]
                )
              })
            })
          }),
          (t.prototype.handleHandshakeError = function(t) {
            return u(this, void 0, void 0, function() {
              var e = this
              return c(this, function(n) {
                return $i(t.code) || t.code === nr.ABORTED
                  ? (Xn(
                      'RemoteStore',
                      'RemoteStore error before completed handshake; resetting stream token: ',
                      this.writeStream.lastStreamToken,
                    ),
                    (this.writeStream.lastStreamToken = er()),
                    [
                      2,
                      this.localStore
                        .setLastStreamToken(er())
                        .catch(function(t) {
                          return e.ignoreIfPrimaryLeaseLoss(t)
                        }),
                    ])
                  : [2]
              })
            })
          }),
          (t.prototype.handleWriteError = function(t) {
            return u(this, void 0, void 0, function() {
              var e,
                n = this
              return c(this, function(r) {
                return $i(t.code)
                  ? ((e = this.writePipeline.shift()),
                    this.writeStream.inhibitBackoff(),
                    [
                      2,
                      this.syncEngine
                        .rejectFailedWrite(e.batchId, t)
                        .then(function() {
                          return n.fillWritePipeline()
                        }),
                    ])
                  : [2]
              })
            })
          }),
          (t.prototype.createTransaction = function() {
            return new Ua(this.datastore)
          }),
          (t.prototype.handleCredentialChange = function() {
            return u(this, void 0, void 0, function() {
              return c(this, function(t) {
                switch (t.label) {
                  case 0:
                    return this.canUseNetwork()
                      ? (Xn(
                          'RemoteStore',
                          'RemoteStore restarting streams for new credential',
                        ),
                        (this.networkEnabled = !1),
                        [4, this.disableNetworkInternal()])
                      : [3, 3]
                  case 1:
                    return (
                      t.sent(),
                      this.onlineStateTracker.set(Oa.Unknown),
                      [4, this.enableNetwork()]
                    )
                  case 2:
                    t.sent(), (t.label = 3)
                  case 3:
                    return [2]
                }
              })
            })
          }),
          (t.prototype.applyPrimaryState = function(t) {
            return u(this, void 0, void 0, function() {
              return c(this, function(e) {
                switch (e.label) {
                  case 0:
                    return (
                      (this.isPrimary = t),
                      t && this.networkEnabled
                        ? [4, this.enableNetwork()]
                        : [3, 2]
                    )
                  case 1:
                    return e.sent(), [3, 4]
                  case 2:
                    return t ? [3, 4] : [4, this.disableNetworkInternal()]
                  case 3:
                    e.sent(),
                      this.onlineStateTracker.set(Oa.Unknown),
                      (e.label = 4)
                  case 4:
                    return [2]
                }
              })
            })
          }),
          t
        )
      })(),
      Wa = (function() {
        return function() {
          this.listeners = []
        }
      })(),
      ja = (function() {
        function t(t) {
          ;(this.syncEngine = t),
            (this.queries = new cs(function(t) {
              return t.canonicalId()
            })),
            (this.onlineState = Oa.Unknown),
            this.syncEngine.subscribe(this)
        }
        return (
          (t.prototype.listen = function(t) {
            var e = t.query,
              n = !1,
              r = this.queries.get(e)
            return (
              r || ((n = !0), (r = new Wa()), this.queries.set(e, r)),
              r.listeners.push(t),
              t.applyOnlineStateChange(this.onlineState),
              r.viewSnap && t.onViewSnapshot(r.viewSnap),
              n
                ? this.syncEngine.listen(e).then(function(t) {
                    return (r.targetId = t), t
                  })
                : Promise.resolve(r.targetId)
            )
          }),
          (t.prototype.unlisten = function(t) {
            return u(this, void 0, void 0, function() {
              var e, n, r, i
              return c(this, function(o) {
                return (
                  (e = t.query),
                  (n = !1),
                  (r = this.queries.get(e)) &&
                    (i = r.listeners.indexOf(t)) >= 0 &&
                    (r.listeners.splice(i, 1), (n = 0 === r.listeners.length)),
                  n
                    ? (this.queries.delete(e), [2, this.syncEngine.unlisten(e)])
                    : [2]
                )
              })
            })
          }),
          (t.prototype.onWatchChange = function(t) {
            for (var e = 0, n = t; e < n.length; e++) {
              var r = n[e],
                i = r.query,
                o = this.queries.get(i)
              if (o) {
                for (var s = 0, a = o.listeners; s < a.length; s++) {
                  a[s].onViewSnapshot(r)
                }
                o.viewSnap = r
              }
            }
          }),
          (t.prototype.onWatchError = function(t, e) {
            var n = this.queries.get(t)
            if (n)
              for (var r = 0, i = n.listeners; r < i.length; r++) {
                i[r].onError(e)
              }
            this.queries.delete(t)
          }),
          (t.prototype.onOnlineStateChange = function(t) {
            ;(this.onlineState = t),
              this.queries.forEach(function(e, n) {
                for (var r = 0, i = n.listeners; r < i.length; r++) {
                  i[r].applyOnlineStateChange(t)
                }
              })
          }),
          t
        )
      })(),
      Ga = (function() {
        function t(t, e, n) {
          ;(this.query = t),
            (this.queryObserver = e),
            (this.raisedInitialEvent = !1),
            (this.onlineState = Oa.Unknown),
            (this.options = n || {})
        }
        return (
          (t.prototype.onViewSnapshot = function(t) {
            if (
              (Zn(
                t.docChanges.length > 0 || t.syncStateChanged,
                'We got a new snapshot with no changes?',
              ),
              !this.options.includeMetadataChanges)
            ) {
              for (var e = [], n = 0, r = t.docChanges; n < r.length; n++) {
                var i = r[n]
                i.type !== lo.Metadata && e.push(i)
              }
              t = new go(
                t.query,
                t.docs,
                t.oldDocs,
                e,
                t.mutatedKeys,
                t.fromCache,
                t.syncStateChanged,
                !0,
              )
            }
            this.raisedInitialEvent
              ? this.shouldRaiseEvent(t) && this.queryObserver.next(t)
              : this.shouldRaiseInitialEvent(t, this.onlineState) &&
                this.raiseInitialEvent(t),
              (this.snap = t)
          }),
          (t.prototype.onError = function(t) {
            this.queryObserver.error(t)
          }),
          (t.prototype.applyOnlineStateChange = function(t) {
            ;(this.onlineState = t),
              this.snap &&
                !this.raisedInitialEvent &&
                this.shouldRaiseInitialEvent(this.snap, t) &&
                this.raiseInitialEvent(this.snap)
          }),
          (t.prototype.shouldRaiseInitialEvent = function(t, e) {
            if (
              (Zn(
                !this.raisedInitialEvent,
                'Determining whether to raise first event but already had first event',
              ),
              !t.fromCache)
            )
              return !0
            var n = e !== Oa.Offline
            return this.options.waitForSyncWhenOnline && n
              ? (Zn(
                  t.fromCache,
                  'Waiting for sync, but snapshot is not from cache',
                ),
                !1)
              : !t.docs.isEmpty() || e === Oa.Offline
          }),
          (t.prototype.shouldRaiseEvent = function(t) {
            if (t.docChanges.length > 0) return !0
            var e =
              this.snap && this.snap.hasPendingWrites !== t.hasPendingWrites
            return (
              !(!t.syncStateChanged && !e) &&
              !0 === this.options.includeMetadataChanges
            )
          }),
          (t.prototype.raiseInitialEvent = function(t) {
            Zn(
              !this.raisedInitialEvent,
              'Trying to raise initial events for second time',
            ),
              (t = go.fromInitialDocuments(
                t.query,
                t.docs,
                t.mutatedKeys,
                t.fromCache,
              )),
              (this.raisedInitialEvent = !0),
              this.queryObserver.next(t)
          }),
          t
        )
      })(),
      za = (function() {
        function t(t, e, n) {
          ;(this.targetId = t), (this.addedKeys = e), (this.removedKeys = n)
        }
        return (
          (t.fromSnapshot = function(e, n) {
            for (
              var r = uo(), i = uo(), o = 0, s = n.docChanges;
              o < s.length;
              o++
            ) {
              var a = s[o]
              switch (a.type) {
                case lo.Added:
                  r = r.add(a.doc.key)
                  break
                case lo.Removed:
                  i = i.add(a.doc.key)
              }
            }
            return new t(e, r, i)
          }),
          t
        )
      })(),
      Ha = (function() {
        return function(t) {
          this.key = t
        }
      })(),
      Xa = (function() {
        return function(t) {
          this.key = t
        }
      })(),
      Ya = (function() {
        function t(t, e) {
          ;(this.query = t),
            (this._syncedDocuments = e),
            (this.syncState = null),
            (this.current = !1),
            (this.limboDocuments = uo()),
            (this.mutatedKeys = uo()),
            (this.documentSet = new po(t.docComparator.bind(t)))
        }
        return (
          Object.defineProperty(t.prototype, 'syncedDocuments', {
            get: function() {
              return this._syncedDocuments
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.computeDocChanges = function(t, e) {
            var n = this,
              r = e ? e.changeSet : new yo(),
              i = e ? e.documentSet : this.documentSet,
              o = e ? e.mutatedKeys : this.mutatedKeys,
              s = i,
              a = !1,
              u =
                this.query.hasLimit() && i.size === this.query.limit
                  ? i.last()
                  : null
            if (
              (t.inorderTraversal(function(t, e) {
                var c = i.get(t),
                  h = e instanceof zr ? e : null
                h &&
                  (Zn(
                    t.isEqual(h.key),
                    'Mismatching keys found in document changes: ' +
                      t +
                      ' != ' +
                      h.key,
                  ),
                  (h = n.query.matches(h) ? h : null))
                var l = !!c && n.mutatedKeys.has(c.key),
                  f =
                    !!h &&
                    (h.hasLocalMutations ||
                      (n.mutatedKeys.has(h.key) && h.hasCommittedMutations)),
                  d = !1
                c && h
                  ? c.data.isEqual(h.data)
                    ? l !== f &&
                      (r.track({ type: lo.Metadata, doc: h }), (d = !0))
                    : n.shouldWaitForSyncedDocument(c, h) ||
                      (r.track({ type: lo.Modified, doc: h }),
                      (d = !0),
                      u && n.query.docComparator(h, u) > 0 && (a = !0))
                  : !c && h
                    ? (r.track({ type: lo.Added, doc: h }), (d = !0))
                    : c &&
                      !h &&
                      (r.track({ type: lo.Removed, doc: c }),
                      (d = !0),
                      u && (a = !0))
                d &&
                  (h
                    ? ((s = s.add(h)), (o = f ? o.add(t) : o.delete(t)))
                    : ((s = s.delete(t)), (o = o.delete(t))))
              }),
              this.query.hasLimit())
            )
              for (; s.size > this.query.limit; ) {
                var c = s.last()
                ;(s = s.delete(c.key)),
                  (o = o.delete(c.key)),
                  r.track({ type: lo.Removed, doc: c })
              }
            return (
              Zn(
                !a || !e,
                'View was refilled using docs that themselves needed refilling.',
              ),
              { documentSet: s, changeSet: r, needsRefill: a, mutatedKeys: o }
            )
          }),
          (t.prototype.shouldWaitForSyncedDocument = function(t, e) {
            return (
              t.hasLocalMutations &&
              e.hasCommittedMutations &&
              !e.hasLocalMutations
            )
          }),
          (t.prototype.applyChanges = function(t, e, n) {
            var r = this
            Zn(!t.needsRefill, 'Cannot apply changes that need a refill')
            var i = this.documentSet
            ;(this.documentSet = t.documentSet),
              (this.mutatedKeys = t.mutatedKeys)
            var o = t.changeSet.getChanges()
            o.sort(function(t, e) {
              return (
                (function(t, e) {
                  var n = function(t) {
                    switch (t) {
                      case lo.Added:
                        return 1
                      case lo.Modified:
                      case lo.Metadata:
                        return 2
                      case lo.Removed:
                        return 0
                      default:
                        return $n('Unknown ChangeType: ' + t)
                    }
                  }
                  return n(t) - n(e)
                })(t.type, e.type) || r.query.docComparator(t.doc, e.doc)
              )
            }),
              this.applyTargetChange(n)
            var s = e ? this.updateLimboDocuments() : [],
              a =
                0 === this.limboDocuments.size && this.current
                  ? fo.Synced
                  : fo.Local,
              u = a !== this.syncState
            return (
              (this.syncState = a),
              0 !== o.length || u
                ? {
                    snapshot: new go(
                      this.query,
                      t.documentSet,
                      i,
                      o,
                      t.mutatedKeys,
                      a === fo.Local,
                      u,
                      !1,
                    ),
                    limboChanges: s,
                  }
                : { limboChanges: s }
            )
          }),
          (t.prototype.applyOnlineStateChange = function(t) {
            return this.current && t === Oa.Offline
              ? ((this.current = !1),
                this.applyChanges(
                  {
                    documentSet: this.documentSet,
                    changeSet: new yo(),
                    mutatedKeys: this.mutatedKeys,
                    needsRefill: !1,
                  },
                  !1,
                ))
              : { limboChanges: [] }
          }),
          (t.prototype.shouldBeInLimbo = function(t) {
            return (
              !this._syncedDocuments.has(t) &&
              (!!this.documentSet.has(t) &&
                !this.documentSet.get(t).hasLocalMutations)
            )
          }),
          (t.prototype.applyTargetChange = function(t) {
            var e = this
            t &&
              (t.addedDocuments.forEach(function(t) {
                return (e._syncedDocuments = e._syncedDocuments.add(t))
              }),
              t.modifiedDocuments.forEach(function(t) {
                return Zn(
                  e._syncedDocuments.has(t),
                  'Modified document ' + t + ' not found in view.',
                )
              }),
              t.removedDocuments.forEach(function(t) {
                return (e._syncedDocuments = e._syncedDocuments.delete(t))
              }),
              (this.current = t.current))
          }),
          (t.prototype.updateLimboDocuments = function() {
            var t = this
            if (!this.current) return []
            var e = this.limboDocuments
            ;(this.limboDocuments = uo()),
              this.documentSet.forEach(function(e) {
                t.shouldBeInLimbo(e.key) &&
                  (t.limboDocuments = t.limboDocuments.add(e.key))
              })
            var n = []
            return (
              e.forEach(function(e) {
                t.limboDocuments.has(e) || n.push(new Xa(e))
              }),
              this.limboDocuments.forEach(function(t) {
                e.has(t) || n.push(new Ha(t))
              }),
              n
            )
          }),
          (t.prototype.synchronizeWithPersistedState = function(t, e) {
            ;(this._syncedDocuments = e), (this.limboDocuments = uo())
            var n = this.computeDocChanges(t)
            return this.applyChanges(n, !0)
          }),
          (t.prototype.computeInitialSnapshot = function() {
            return go.fromInitialDocuments(
              this.query,
              this.documentSet,
              this.mutatedKeys,
              this.syncState === fo.Local,
            )
          }),
          t
        )
      })()
    var Ja = (function() {
        return function(t, e, n) {
          ;(this.query = t), (this.targetId = e), (this.view = n)
        }
      })(),
      $a = (function() {
        return function(t) {
          this.key = t
        }
      })(),
      Za = (function() {
        function t(t, e, n, r) {
          ;(this.localStore = t),
            (this.remoteStore = e),
            (this.sharedClientState = n),
            (this.currentUser = r),
            (this.syncEngineListener = null),
            (this.queryViewsByQuery = new cs(function(t) {
              return t.canonicalId()
            })),
            (this.queryViewsByTarget = {}),
            (this.limboTargetsByKey = new Yr(jr.comparator)),
            (this.limboResolutionsByTarget = {}),
            (this.limboDocumentRefs = new ba()),
            (this.mutationUserCallbacks = {}),
            (this.limboTargetIdGenerator = Js.forSyncEngine()),
            (this.isPrimary = void 0),
            (this.onlineState = Oa.Unknown)
        }
        return (
          Object.defineProperty(t.prototype, 'isPrimaryClient', {
            get: function() {
              return !0 === this.isPrimary
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.subscribe = function(t) {
            Zn(null !== t, 'SyncEngine listener cannot be null'),
              Zn(
                null === this.syncEngineListener,
                'SyncEngine already has a subscriber.',
              ),
              (this.syncEngineListener = t)
          }),
          (t.prototype.listen = function(t) {
            return u(this, void 0, void 0, function() {
              var e, n, r, i, o
              return c(this, function(s) {
                switch (s.label) {
                  case 0:
                    return (
                      this.assertSubscribed('listen()'),
                      (r = this.queryViewsByQuery.get(t))
                        ? ((e = r.targetId),
                          this.sharedClientState.addLocalQueryTarget(e),
                          (n = r.view.computeInitialSnapshot()),
                          [3, 4])
                        : [3, 1]
                    )
                  case 1:
                    return [4, this.localStore.allocateQuery(t)]
                  case 2:
                    return (
                      (i = s.sent()),
                      (o = this.sharedClientState.addLocalQueryTarget(
                        i.targetId,
                      )),
                      (e = i.targetId),
                      [
                        4,
                        this.initializeViewAndComputeSnapshot(
                          i,
                          'current' === o,
                        ),
                      ]
                    )
                  case 3:
                    ;(n = s.sent()),
                      this.isPrimary && this.remoteStore.listen(i),
                      (s.label = 4)
                  case 4:
                    return this.syncEngineListener.onWatchChange([n]), [2, e]
                }
              })
            })
          }),
          (t.prototype.initializeViewAndComputeSnapshot = function(t, e) {
            var n = this,
              r = t.query
            return this.localStore.executeQuery(r).then(function(i) {
              return n.localStore
                .remoteDocumentKeys(t.targetId)
                .then(function(o) {
                  var s = new Ya(r, o),
                    a = s.computeDocChanges(i),
                    u = bo.createSynthesizedTargetChangeForCurrentChange(
                      t.targetId,
                      e && n.onlineState !== Oa.Offline,
                    ),
                    c = s.applyChanges(a, !0 === n.isPrimary, u)
                  Zn(
                    0 === c.limboChanges.length,
                    'View returned limbo docs before target ack from the server.',
                  ),
                    Zn(
                      !!c.snapshot,
                      'applyChanges for new view should always return a snapshot',
                    )
                  var h = new Ja(r, t.targetId, s)
                  return (
                    n.queryViewsByQuery.set(r, h),
                    (n.queryViewsByTarget[t.targetId] = h),
                    c.snapshot
                  )
                })
            })
          }),
          (t.prototype.synchronizeViewAndComputeSnapshot = function(t) {
            var e = this
            return this.localStore.executeQuery(t.query).then(function(n) {
              return e.localStore
                .remoteDocumentKeys(t.targetId)
                .then(function(r) {
                  return u(e, void 0, void 0, function() {
                    var e
                    return c(this, function(i) {
                      return (
                        (e = t.view.synchronizeWithPersistedState(n, r)),
                        this.isPrimary &&
                          this.updateTrackedLimbos(t.targetId, e.limboChanges),
                        [2, e]
                      )
                    })
                  })
                })
            })
          }),
          (t.prototype.unlisten = function(t) {
            return u(this, void 0, void 0, function() {
              var e,
                n = this
              return c(this, function(r) {
                switch (r.label) {
                  case 0:
                    return (
                      this.assertSubscribed('unlisten()'),
                      Zn(
                        !!(e = this.queryViewsByQuery.get(t)),
                        'Trying to unlisten on query not found:' + t,
                      ),
                      this.isPrimary
                        ? (this.sharedClientState.removeLocalQueryTarget(
                            e.targetId,
                          ),
                          this.sharedClientState.isActiveQueryTarget(e.targetId)
                            ? [3, 2]
                            : [
                                4,
                                this.localStore
                                  .releaseQuery(t, !1)
                                  .then(function() {
                                    n.sharedClientState.clearQueryState(
                                      e.targetId,
                                    ),
                                      n.remoteStore.unlisten(e.targetId),
                                      n.removeAndCleanupQuery(e)
                                  })
                                  .catch(function(t) {
                                    return n.ignoreIfPrimaryLeaseLoss(t)
                                  }),
                              ])
                        : [3, 3]
                    )
                  case 1:
                    r.sent(), (r.label = 2)
                  case 2:
                    return [3, 5]
                  case 3:
                    return (
                      this.removeAndCleanupQuery(e),
                      [4, this.localStore.releaseQuery(t, !0)]
                    )
                  case 4:
                    r.sent(), (r.label = 5)
                  case 5:
                    return [2]
                }
              })
            })
          }),
          (t.prototype.write = function(t, e) {
            var n = this
            return (
              this.assertSubscribed('write()'),
              this.localStore
                .localWrite(t)
                .then(function(t) {
                  return (
                    n.sharedClientState.addPendingMutation(t.batchId),
                    n.addMutationCallback(t.batchId, e),
                    n.emitNewSnapsAndNotifyLocalStore(t.changes)
                  )
                })
                .then(function() {
                  return n.remoteStore.fillWritePipeline()
                })
            )
          }),
          (t.prototype.wrapUpdateFunctionError = function(t) {
            return t
          }),
          (t.prototype.runTransaction = function(t, e) {
            var n = this
            void 0 === e && (e = 5),
              Zn(e >= 0, 'Got negative number of retries for transaction.')
            var r = this.remoteStore.createTransaction()
            return (function() {
              try {
                var e = t(r)
                return !wi(e) && e.catch && e.then
                  ? e.catch(function(t) {
                      return Promise.reject(n.wrapUpdateFunctionError(t))
                    })
                  : Promise.reject(
                      Error('Transaction callback must return a Promise'),
                    )
              } catch (t) {
                return Promise.reject(n.wrapUpdateFunctionError(t))
              }
            })().then(function(i) {
              return r
                .commit()
                .then(function() {
                  return i
                })
                .catch(function(r) {
                  return 0 === e
                    ? Promise.reject(r)
                    : n.runTransaction(t, e - 1)
                })
            })
          }),
          (t.prototype.applyRemoteEvent = function(t) {
            var e = this
            return (
              this.assertSubscribed('applyRemoteEvent()'),
              this.localStore
                .applyRemoteEvent(t)
                .then(function(n) {
                  return (
                    ur(t.targetChanges, function(t, n) {
                      var r = e.limboResolutionsByTarget[t]
                      r &&
                        (Zn(
                          n.addedDocuments.size +
                            n.modifiedDocuments.size +
                            n.removedDocuments.size <=
                            1,
                          'Limbo resolution for single document contains multiple changes.',
                        ),
                        n.addedDocuments.size > 0
                          ? (r.receivedDocument = !0)
                          : n.modifiedDocuments.size > 0
                            ? Zn(
                                r.receivedDocument,
                                'Received change for limbo target document without add.',
                              )
                            : n.removedDocuments.size > 0 &&
                              (Zn(
                                r.receivedDocument,
                                'Received remove for limbo target document without add.',
                              ),
                              (r.receivedDocument = !1)))
                    }),
                    e.emitNewSnapsAndNotifyLocalStore(n, t)
                  )
                })
                .catch(function(t) {
                  return e.ignoreIfPrimaryLeaseLoss(t)
                })
            )
          }),
          (t.prototype.applyOnlineStateChange = function(t, e) {
            if (
              (this.isPrimary && e === Pa.RemoteStore) ||
              (!this.isPrimary && e === Pa.SharedClientState)
            ) {
              var n = []
              this.queryViewsByQuery.forEach(function(e, r) {
                var i = r.view.applyOnlineStateChange(t)
                Zn(
                  0 === i.limboChanges.length,
                  'OnlineState should not affect limbo documents.',
                ),
                  i.snapshot && n.push(i.snapshot)
              }),
                this.syncEngineListener.onOnlineStateChange(t),
                this.syncEngineListener.onWatchChange(n),
                (this.onlineState = t),
                this.isPrimary && this.sharedClientState.setOnlineState(t)
            }
          }),
          (t.prototype.rejectListen = function(t, e) {
            return u(this, void 0, void 0, function() {
              var n,
                r,
                i,
                o,
                s,
                a,
                u = this
              return c(this, function(c) {
                switch (c.label) {
                  case 0:
                    return (
                      this.assertSubscribed('rejectListens()'),
                      this.sharedClientState.updateQueryState(t, 'rejected', e),
                      (n = this.limboResolutionsByTarget[t]),
                      (r = n && n.key)
                        ? ((this.limboTargetsByKey = this.limboTargetsByKey.remove(
                            r,
                          )),
                          delete this.limboResolutionsByTarget[t],
                          (i = (i = new Yr(jr.comparator)).insert(
                            r,
                            new Hr(r, _i.forDeletedDoc()),
                          )),
                          (o = uo().add(r)),
                          (s = new vo(_i.MIN, {}, new to(Ar), i, o)),
                          [2, this.applyRemoteEvent(s)])
                        : [3, 1]
                    )
                  case 1:
                    return (
                      Zn(
                        !!(a = this.queryViewsByTarget[t]),
                        'Unknown targetId: ' + t,
                      ),
                      [
                        4,
                        this.localStore
                          .releaseQuery(a.query, !1)
                          .then(function() {
                            return u.removeAndCleanupQuery(a)
                          })
                          .catch(function(t) {
                            return u.ignoreIfPrimaryLeaseLoss(t)
                          }),
                      ]
                    )
                  case 2:
                    c.sent(),
                      this.syncEngineListener.onWatchError(a.query, e),
                      (c.label = 3)
                  case 3:
                    return [2]
                }
              })
            })
          }),
          (t.prototype.applyBatchState = function(t, e, n) {
            return u(this, void 0, void 0, function() {
              var r
              return c(this, function(i) {
                switch (i.label) {
                  case 0:
                    return (
                      this.assertSubscribed('applyBatchState()'),
                      [4, this.localStore.lookupMutationDocuments(t)]
                    )
                  case 1:
                    return null === (r = i.sent())
                      ? (Xn(
                          'SyncEngine',
                          'Cannot apply mutation batch with id: ' + t,
                        ),
                        [2])
                      : 'pending' !== e
                        ? [3, 3]
                        : [4, this.remoteStore.fillWritePipeline()]
                  case 2:
                    return i.sent(), [3, 4]
                  case 3:
                    'acknowledged' === e || 'rejected' === e
                      ? (this.processUserCallback(t, n || null),
                        this.localStore.removeCachedMutationBatchMetadata(t))
                      : $n('Unknown batchState: ' + e),
                      (i.label = 4)
                  case 4:
                    return [4, this.emitNewSnapsAndNotifyLocalStore(r)]
                  case 5:
                    return i.sent(), [2]
                }
              })
            })
          }),
          (t.prototype.applySuccessfulWrite = function(t) {
            var e = this
            this.assertSubscribed('applySuccessfulWrite()')
            var n = t.batch.batchId
            return (
              this.processUserCallback(n, null),
              this.localStore
                .acknowledgeBatch(t)
                .then(function(t) {
                  return (
                    e.sharedClientState.updateMutationState(n, 'acknowledged'),
                    e.emitNewSnapsAndNotifyLocalStore(t)
                  )
                })
                .catch(function(t) {
                  return e.ignoreIfPrimaryLeaseLoss(t)
                })
            )
          }),
          (t.prototype.rejectFailedWrite = function(t, e) {
            var n = this
            return (
              this.assertSubscribed('rejectFailedWrite()'),
              this.processUserCallback(t, e),
              this.localStore
                .rejectBatch(t)
                .then(function(r) {
                  return (
                    n.sharedClientState.updateMutationState(t, 'rejected', e),
                    n.emitNewSnapsAndNotifyLocalStore(r)
                  )
                })
                .catch(function(t) {
                  return n.ignoreIfPrimaryLeaseLoss(t)
                })
            )
          }),
          (t.prototype.addMutationCallback = function(t, e) {
            var n = this.mutationUserCallbacks[this.currentUser.toKey()]
            n || (n = new Yr(Ar)),
              (n = n.insert(t, e)),
              (this.mutationUserCallbacks[this.currentUser.toKey()] = n)
          }),
          (t.prototype.processUserCallback = function(t, e) {
            var n = this.mutationUserCallbacks[this.currentUser.toKey()]
            if (n) {
              var r = n.get(t)
              r &&
                (Zn(
                  t === n.minKey(),
                  'Mutation callbacks processed out-of-order?',
                ),
                e ? r.reject(e) : r.resolve(),
                (n = n.remove(t))),
                (this.mutationUserCallbacks[this.currentUser.toKey()] = n)
            }
          }),
          (t.prototype.removeAndCleanupQuery = function(t) {
            var e = this
            if (
              (this.sharedClientState.removeLocalQueryTarget(t.targetId),
              this.queryViewsByQuery.delete(t.query),
              delete this.queryViewsByTarget[t.targetId],
              this.isPrimary)
            ) {
              var n = this.limboDocumentRefs.referencesForId(t.targetId)
              this.limboDocumentRefs.removeReferencesForId(t.targetId),
                n.forEach(function(t) {
                  e.limboDocumentRefs.containsKey(t) || e.removeLimboTarget(t)
                })
            }
          }),
          (t.prototype.removeLimboTarget = function(t) {
            var e = this.limboTargetsByKey.get(t)
            null !== e &&
              (this.remoteStore.unlisten(e),
              (this.limboTargetsByKey = this.limboTargetsByKey.remove(t)),
              delete this.limboResolutionsByTarget[e])
          }),
          (t.prototype.updateTrackedLimbos = function(t, e) {
            for (var n = 0, r = e; n < r.length; n++) {
              var i = r[n]
              if (i instanceof Ha)
                this.limboDocumentRefs.addReference(i.key, t),
                  this.trackLimboChange(i)
              else if (i instanceof Xa) {
                Xn('SyncEngine', 'Document no longer in limbo: ' + i.key),
                  this.limboDocumentRefs.removeReference(i.key, t),
                  this.limboDocumentRefs.containsKey(i.key) ||
                    this.removeLimboTarget(i.key)
              } else $n('Unknown limbo change: ' + JSON.stringify(i))
            }
          }),
          (t.prototype.trackLimboChange = function(t) {
            var e = t.key
            if (!this.limboTargetsByKey.get(e)) {
              Xn('SyncEngine', 'New document in limbo: ' + e)
              var n = this.limboTargetIdGenerator.next(),
                r = Ei.atPath(e.path)
              ;(this.limboResolutionsByTarget[n] = new $a(e)),
                this.remoteStore.listen(
                  new xi(r, n, Si.LimboResolution, zo.INVALID),
                ),
                (this.limboTargetsByKey = this.limboTargetsByKey.insert(e, n))
            }
          }),
          (t.prototype.currentLimboDocs = function() {
            return this.limboTargetsByKey
          }),
          (t.prototype.emitNewSnapsAndNotifyLocalStore = function(t, e) {
            return u(this, void 0, void 0, function() {
              var n,
                r,
                i,
                o = this
              return c(this, function(s) {
                switch (s.label) {
                  case 0:
                    return (
                      (n = []),
                      (r = []),
                      (i = []),
                      this.queryViewsByQuery.forEach(function(s, a) {
                        i.push(
                          Promise.resolve()
                            .then(function() {
                              var e = a.view.computeDocChanges(t)
                              return e.needsRefill
                                ? o.localStore
                                    .executeQuery(a.query)
                                    .then(function(t) {
                                      return a.view.computeDocChanges(t, e)
                                    })
                                : e
                            })
                            .then(function(t) {
                              var i = e && e.targetChanges[a.targetId],
                                s = a.view.applyChanges(
                                  t,
                                  !0 === o.isPrimary,
                                  i,
                                )
                              if (
                                (o.updateTrackedLimbos(
                                  a.targetId,
                                  s.limboChanges,
                                ),
                                s.snapshot)
                              ) {
                                o.isPrimary &&
                                  o.sharedClientState.updateQueryState(
                                    a.targetId,
                                    s.snapshot.fromCache
                                      ? 'not-current'
                                      : 'current',
                                  ),
                                  n.push(s.snapshot)
                                var u = za.fromSnapshot(a.targetId, s.snapshot)
                                r.push(u)
                              }
                            }),
                        )
                      }),
                      [4, Promise.all(i)]
                    )
                  case 1:
                    return (
                      s.sent(),
                      this.syncEngineListener.onWatchChange(n),
                      [4, this.localStore.notifyLocalViewChanges(r)]
                    )
                  case 2:
                    return s.sent(), [2]
                }
              })
            })
          }),
          (t.prototype.ignoreIfPrimaryLeaseLoss = function(t) {
            return u(this, void 0, void 0, function() {
              return c(this, function(e) {
                if (!da(t)) throw t
                return Xn('SyncEngine', 'Unexpectedly lost primary lease'), [2]
              })
            })
          }),
          (t.prototype.assertSubscribed = function(t) {
            Zn(
              null !== this.syncEngineListener,
              'Trying to call ' + t + ' before calling subscribe().',
            )
          }),
          (t.prototype.handleCredentialChange = function(t) {
            return u(this, void 0, void 0, function() {
              var e, n
              return c(this, function(r) {
                switch (r.label) {
                  case 0:
                    return (
                      (e = !this.currentUser.isEqual(t)),
                      (this.currentUser = t),
                      e ? [4, this.localStore.handleUserChange(t)] : [3, 3]
                    )
                  case 1:
                    return (
                      (n = r.sent()),
                      this.sharedClientState.handleUserChange(
                        t,
                        n.removedBatchIds,
                        n.addedBatchIds,
                      ),
                      [
                        4,
                        this.emitNewSnapsAndNotifyLocalStore(
                          n.affectedDocuments,
                        ),
                      ]
                    )
                  case 2:
                    r.sent(), (r.label = 3)
                  case 3:
                    return [4, this.remoteStore.handleCredentialChange()]
                  case 4:
                    return r.sent(), [2]
                }
              })
            })
          }),
          (t.prototype.applyPrimaryState = function(t) {
            return u(this, void 0, void 0, function() {
              var e,
                n,
                r,
                i,
                o,
                s,
                a,
                u = this
              return c(this, function(c) {
                switch (c.label) {
                  case 0:
                    return !0 !== t || !0 === this.isPrimary
                      ? [3, 3]
                      : ((this.isPrimary = !0),
                        [4, this.remoteStore.applyPrimaryState(!0)])
                  case 1:
                    return (
                      c.sent(),
                      (e = this.sharedClientState.getAllActiveQueryTargets()),
                      [
                        4,
                        this.synchronizeQueryViewsAndRaiseSnapshots(
                          e.toArray(),
                        ),
                      ]
                    )
                  case 2:
                    for (n = c.sent(), r = 0, i = n; r < i.length; r++)
                      (o = i[r]), this.remoteStore.listen(o)
                    return [3, 7]
                  case 3:
                    return !1 !== t || !1 === this.isPrimary
                      ? [3, 7]
                      : ((this.isPrimary = !1),
                        (s = []),
                        (a = Promise.resolve()),
                        ar(this.queryViewsByTarget, function(t, e) {
                          u.sharedClientState.isLocalQueryTarget(t)
                            ? s.push(t)
                            : (a = a.then(function() {
                                return u.unlisten(e.query)
                              })),
                            u.remoteStore.unlisten(e.targetId)
                        }),
                        [4, a])
                  case 4:
                    return (
                      c.sent(),
                      [4, this.synchronizeQueryViewsAndRaiseSnapshots(s)]
                    )
                  case 5:
                    return (
                      c.sent(),
                      this.resetLimboDocuments(),
                      [4, this.remoteStore.applyPrimaryState(!1)]
                    )
                  case 6:
                    c.sent(), (c.label = 7)
                  case 7:
                    return [2]
                }
              })
            })
          }),
          (t.prototype.resetLimboDocuments = function() {
            var t = this
            ar(this.limboResolutionsByTarget, function(e) {
              t.remoteStore.unlisten(e)
            }),
              this.limboDocumentRefs.removeAllReferences(),
              (this.limboResolutionsByTarget = []),
              (this.limboTargetsByKey = new Yr(jr.comparator))
          }),
          (t.prototype.synchronizeQueryViewsAndRaiseSnapshots = function(t) {
            for (
              var e = this,
                n = Promise.resolve(),
                r = [],
                i = [],
                o = function(t) {
                  n = n.then(function() {
                    return u(e, void 0, void 0, function() {
                      var e, n, o, s
                      return c(this, function(a) {
                        switch (a.label) {
                          case 0:
                            return (n = this.queryViewsByTarget[t])
                              ? [4, this.localStore.releaseQuery(n.query, !0)]
                              : [3, 4]
                          case 1:
                            return (
                              a.sent(),
                              [4, this.localStore.allocateQuery(n.query)]
                            )
                          case 2:
                            return (
                              (e = a.sent()),
                              [4, this.synchronizeViewAndComputeSnapshot(n)]
                            )
                          case 3:
                            return (
                              (o = a.sent()).snapshot && i.push(o.snapshot),
                              [3, 8]
                            )
                          case 4:
                            return (
                              Zn(
                                !0 === this.isPrimary,
                                'A secondary tab should never have an active query without an active view.',
                              ),
                              [4, this.localStore.getQueryForTarget(t)]
                            )
                          case 5:
                            return (
                              Zn(
                                !!(s = a.sent()),
                                'Query data for target ' + t + ' not found',
                              ),
                              [4, this.localStore.allocateQuery(s)]
                            )
                          case 6:
                            return (
                              (e = a.sent()),
                              [4, this.initializeViewAndComputeSnapshot(e, !1)]
                            )
                          case 7:
                            a.sent(), (a.label = 8)
                          case 8:
                            return r.push(e), [2]
                        }
                      })
                    })
                  })
                },
                s = 0,
                a = t;
              s < a.length;
              s++
            ) {
              o(a[s])
            }
            return n.then(function() {
              return e.syncEngineListener.onWatchChange(i), r
            })
          }),
          (t.prototype.getActiveClients = function() {
            return this.localStore.getActiveClients()
          }),
          (t.prototype.applyTargetState = function(t, e, n) {
            return u(this, void 0, void 0, function() {
              var r,
                i = this
              return c(this, function(o) {
                switch (o.label) {
                  case 0:
                    if (this.isPrimary)
                      return (
                        Xn(
                          'SyncEngine',
                          'Ignoring unexpected query state notification.',
                        ),
                        [2]
                      )
                    if (!this.queryViewsByTarget[t]) return [3, 5]
                    switch (e) {
                      case 'current':
                      case 'not-current':
                        return [3, 1]
                      case 'rejected':
                        return [3, 2]
                    }
                    return [3, 4]
                  case 1:
                    return [
                      2,
                      this.localStore.getNewDocumentChanges().then(
                        function(n) {
                          return u(i, void 0, void 0, function() {
                            var r
                            return c(this, function(i) {
                              switch (i.label) {
                                case 0:
                                  return (
                                    (r = vo.createSynthesizedRemoteEventForCurrentChange(
                                      t,
                                      'current' === e,
                                    )),
                                    [
                                      4,
                                      this.emitNewSnapsAndNotifyLocalStore(
                                        n,
                                        r,
                                      ),
                                    ]
                                  )
                                case 1:
                                  return i.sent(), [2]
                              }
                            })
                          })
                        },
                        function(t) {
                          return u(i, void 0, void 0, function() {
                            var e
                            return c(this, function(n) {
                              switch (n.label) {
                                case 0:
                                  return (function(t) {
                                    return (
                                      t.code === nr.DATA_LOSS &&
                                      t.message === ys
                                    )
                                  })(t)
                                    ? ((e = []),
                                      ar(this.queryViewsByTarget, function(t) {
                                        return e.push(t)
                                      }),
                                      [
                                        4,
                                        this.synchronizeQueryViewsAndRaiseSnapshots(
                                          e,
                                        ),
                                      ])
                                    : [3, 2]
                                case 1:
                                  return n.sent(), [3, 3]
                                case 2:
                                  throw t
                                case 3:
                                  return [2]
                              }
                            })
                          })
                        },
                      ),
                    ]
                  case 2:
                    return (
                      (r = this.queryViewsByTarget[t]),
                      this.removeAndCleanupQuery(r),
                      [4, this.localStore.releaseQuery(r.query, !0)]
                    )
                  case 3:
                    return (
                      o.sent(),
                      this.syncEngineListener.onWatchError(r.query, n),
                      [3, 5]
                    )
                  case 4:
                    $n('Unexpected target state: ' + e), (o.label = 5)
                  case 5:
                    return [2]
                }
              })
            })
          }),
          (t.prototype.applyActiveTargetsChange = function(t, e) {
            return u(this, void 0, void 0, function() {
              var n,
                r,
                i,
                o,
                s,
                a,
                u,
                h,
                l,
                f = this
              return c(this, function(d) {
                switch (d.label) {
                  case 0:
                    if (!this.isPrimary) return [2]
                    ;(n = 0), (r = t), (d.label = 1)
                  case 1:
                    return n < r.length
                      ? ((l = r[n]),
                        Zn(
                          !this.queryViewsByTarget[l],
                          'Trying to add an already active target',
                        ),
                        [4, this.localStore.getQueryForTarget(l)])
                      : [3, 6]
                  case 2:
                    return (
                      Zn(
                        !!(i = d.sent()),
                        'Query data for active target ' + l + ' not found',
                      ),
                      [4, this.localStore.allocateQuery(i)]
                    )
                  case 3:
                    return (
                      (o = d.sent()),
                      [4, this.initializeViewAndComputeSnapshot(o, !1)]
                    )
                  case 4:
                    d.sent(), this.remoteStore.listen(o), (d.label = 5)
                  case 5:
                    return n++, [3, 1]
                  case 6:
                    ;(s = function(t) {
                      var e
                      return c(this, function(n) {
                        switch (n.label) {
                          case 0:
                            return (e = a.queryViewsByTarget[t])
                              ? [
                                  4,
                                  a.localStore
                                    .releaseQuery(e.query, !1)
                                    .then(function() {
                                      f.remoteStore.unlisten(t),
                                        f.removeAndCleanupQuery(e)
                                    })
                                    .catch(function(t) {
                                      return f.ignoreIfPrimaryLeaseLoss(t)
                                    }),
                                ]
                              : [3, 2]
                          case 1:
                            n.sent(), (n.label = 2)
                          case 2:
                            return [2]
                        }
                      })
                    }),
                      (a = this),
                      (u = 0),
                      (h = e),
                      (d.label = 7)
                  case 7:
                    return u < h.length ? ((l = h[u]), [5, s(l)]) : [3, 10]
                  case 8:
                    d.sent(), (d.label = 9)
                  case 9:
                    return u++, [3, 7]
                  case 10:
                    return [2]
                }
              })
            })
          }),
          (t.prototype.enableNetwork = function() {
            return (
              this.localStore.setNetworkEnabled(!0),
              this.remoteStore.enableNetwork()
            )
          }),
          (t.prototype.disableNetwork = function() {
            return (
              this.localStore.setNetworkEnabled(!1),
              this.remoteStore.disableNetwork()
            )
          }),
          (t.prototype.getRemoteKeysForTarget = function(t) {
            var e = this.limboResolutionsByTarget[t]
            return e && e.receivedDocument
              ? uo().add(e.key)
              : this.queryViewsByTarget[t]
                ? this.queryViewsByTarget[t].view.syncedDocuments
                : uo()
          }),
          t
        )
      })(),
      tu = (function() {
        function t(t) {
          this.uid = t
        }
        return (
          (t.prototype.isAuthenticated = function() {
            return null != this.uid
          }),
          (t.prototype.toKey = function() {
            return this.isAuthenticated() ? 'uid:' + this.uid : 'anonymous-user'
          }),
          (t.prototype.isEqual = function(t) {
            return t.uid === this.uid
          }),
          (t.UNAUTHENTICATED = new t(null)),
          (t.GOOGLE_CREDENTIALS = new t('google-credentials-uid')),
          (t.FIRST_PARTY = new t('first-party-uid')),
          t
        )
      })(),
      eu = 'SharedClientState',
      nu = 'firestore_clients',
      ru = 'firestore_mutations',
      iu = 'firestore_targets',
      ou = 'firestore_online_state',
      su = 'firestore_sequence_number',
      au = (function() {
        function t(t, e, n, r) {
          ;(this.user = t),
            (this.batchId = e),
            (this.state = n),
            (this.error = r),
            Zn(
              (void 0 !== r) == ('rejected' === n),
              "MutationMetadata must contain an error iff state is 'rejected'",
            )
        }
        return (
          (t.fromWebStorageEntry = function(e, n, r) {
            var i = JSON.parse(r),
              o =
                'object' == typeof i &&
                -1 !==
                  ['pending', 'acknowledged', 'rejected'].indexOf(i.state) &&
                (void 0 === i.error || 'object' == typeof i.error),
              s = void 0
            return (
              o &&
                i.error &&
                (o =
                  'string' == typeof i.error.message &&
                  'string' == typeof i.error.code) &&
                (s = new rr(i.error.code, i.error.message)),
              o
                ? new t(e, n, i.state, s)
                : (Yn(
                    eu,
                    "Failed to parse mutation state for ID '" + n + "': " + r,
                  ),
                  null)
            )
          }),
          (t.prototype.toWebStorageJSON = function() {
            var t = { state: this.state, updateTimeMs: Date.now() }
            return (
              this.error &&
                (t.error = {
                  code: this.error.code,
                  message: this.error.message,
                }),
              JSON.stringify(t)
            )
          }),
          t
        )
      })(),
      uu = (function() {
        function t(t, e, n) {
          ;(this.targetId = t),
            (this.state = e),
            (this.error = n),
            Zn(
              (void 0 !== n) == ('rejected' === e),
              "QueryTargetMetadata must contain an error iff state is 'rejected'",
            )
        }
        return (
          (t.fromWebStorageEntry = function(e, n) {
            var r = JSON.parse(n),
              i =
                'object' == typeof r &&
                -1 !==
                  ['not-current', 'current', 'rejected'].indexOf(r.state) &&
                (void 0 === r.error || 'object' == typeof r.error),
              o = void 0
            return (
              i &&
                r.error &&
                (i =
                  'string' == typeof r.error.message &&
                  'string' == typeof r.error.code) &&
                (o = new rr(r.error.code, r.error.message)),
              i
                ? new t(e, r.state, o)
                : (Yn(
                    eu,
                    "Failed to parse target state for ID '" + e + "': " + n,
                  ),
                  null)
            )
          }),
          (t.prototype.toWebStorageJSON = function() {
            var t = { state: this.state, updateTimeMs: Date.now() }
            return (
              this.error &&
                (t.error = {
                  code: this.error.code,
                  message: this.error.message,
                }),
              JSON.stringify(t)
            )
          }),
          t
        )
      })(),
      cu = (function() {
        function t(t, e) {
          ;(this.clientId = t), (this.activeTargetIds = e)
        }
        return (
          (t.fromWebStorageEntry = function(e, n) {
            for (
              var r = JSON.parse(n),
                i = 'object' == typeof r && r.activeTargetIds instanceof Array,
                o = ho(),
                s = 0;
              i && s < r.activeTargetIds.length;
              ++s
            )
              (i = Ti(r.activeTargetIds[s])), (o = o.add(r.activeTargetIds[s]))
            return i
              ? new t(e, o)
              : (Yn(
                  eu,
                  "Failed to parse client data for instance '" + e + "': " + n,
                ),
                null)
          }),
          t
        )
      })(),
      hu = (function() {
        function t(t, e) {
          ;(this.clientId = t), (this.onlineState = e)
        }
        return (
          (t.fromWebStorageEntry = function(e) {
            var n = JSON.parse(e)
            return 'object' == typeof n &&
              void 0 !== Oa[n.onlineState] &&
              'string' == typeof n.clientId
              ? new t(n.clientId, Oa[n.onlineState])
              : (Yn(eu, 'Failed to parse online state: ' + e), null)
          }),
          t
        )
      })(),
      lu = (function() {
        function t() {
          this.activeTargetIds = ho()
        }
        return (
          (t.prototype.addQueryTarget = function(t) {
            Zn(
              !this.activeTargetIds.has(t),
              "Target with ID '" + t + "' already active.",
            ),
              (this.activeTargetIds = this.activeTargetIds.add(t))
          }),
          (t.prototype.removeQueryTarget = function(t) {
            this.activeTargetIds = this.activeTargetIds.delete(t)
          }),
          (t.prototype.toWebStorageJSON = function() {
            var t = {
              activeTargetIds: this.activeTargetIds.toArray(),
              updateTimeMs: Date.now(),
            }
            return JSON.stringify(t)
          }),
          t
        )
      })(),
      fu = (function() {
        function t(e, n, r, i, o) {
          if (
            ((this.queue = e),
            (this.platform = n),
            (this.persistenceKey = r),
            (this.localClientId = i),
            (this.syncEngine = null),
            (this.onlineStateHandler = null),
            (this.sequenceNumberHandler = null),
            (this.activeClients = {}),
            (this.storageListener = this.handleWebStorageEvent.bind(this)),
            (this.started = !1),
            (this.earlyEvents = []),
            !t.isAvailable(this.platform))
          )
            throw new rr(
              nr.UNIMPLEMENTED,
              'LocalStorage is not available on this platform.',
            )
          var s = r.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
          ;(this.storage = this.platform.window.localStorage),
            (this.currentUser = o),
            (this.localClientStorageKey = this.toWebStorageClientStateKey(
              this.localClientId,
            )),
            (this.sequenceNumberKey = su + '_' + r),
            (this.activeClients[this.localClientId] = new lu()),
            (this.clientStateKeyRe = new RegExp(
              '^' + nu + '_' + s + '_([^_]*)$',
            )),
            (this.mutationBatchKeyRe = new RegExp(
              '^' + ru + '_' + s + '_(\\d+)(?:_(.*))?$',
            )),
            (this.queryTargetKeyRe = new RegExp(
              '^' + iu + '_' + s + '_(\\d+)$',
            )),
            (this.onlineStateKey = ou + '_' + r),
            this.platform.window.addEventListener(
              'storage',
              this.storageListener,
            )
        }
        return (
          (t.isAvailable = function(t) {
            return !(!t.window || null == t.window.localStorage)
          }),
          (t.prototype.start = function() {
            return u(this, void 0, void 0, function() {
              var t,
                e,
                n,
                r,
                i,
                o,
                s,
                a,
                u,
                h,
                l,
                f = this
              return c(this, function(c) {
                switch (c.label) {
                  case 0:
                    return (
                      Zn(
                        !this.started,
                        'WebStorageSharedClientState already started',
                      ),
                      Zn(
                        null !== this.syncEngine,
                        'syncEngine property must be set before calling start()',
                      ),
                      Zn(
                        null !== this.onlineStateHandler,
                        'onlineStateHandler property must be set before calling start()',
                      ),
                      [4, this.syncEngine.getActiveClients()]
                    )
                  case 1:
                    for (t = c.sent(), e = 0, n = t; e < n.length; e++)
                      (r = n[e]) !== this.localClientId &&
                        (i = this.getItem(
                          this.toWebStorageClientStateKey(r),
                        )) &&
                        (o = cu.fromWebStorageEntry(r, i)) &&
                        (this.activeClients[o.clientId] = o)
                    for (
                      this.persistClientState(),
                        (s = this.storage.getItem(this.onlineStateKey)) &&
                          (a = this.fromWebStorageOnlineState(s)) &&
                          this.handleOnlineStateEvent(a),
                        u = 0,
                        h = this.earlyEvents;
                      u < h.length;
                      u++
                    )
                      (l = h[u]), this.handleWebStorageEvent(l)
                    return (
                      (this.earlyEvents = []),
                      this.platform.window.addEventListener(
                        'unload',
                        function() {
                          return f.shutdown()
                        },
                      ),
                      (this.started = !0),
                      [2]
                    )
                }
              })
            })
          }),
          (t.prototype.writeSequenceNumber = function(t) {
            this.setItem(this.sequenceNumberKey, JSON.stringify(t))
          }),
          (t.prototype.getAllActiveQueryTargets = function() {
            var t = ho()
            return (
              ur(this.activeClients, function(e, n) {
                t = t.unionWith(n.activeTargetIds)
              }),
              t
            )
          }),
          (t.prototype.isActiveQueryTarget = function(t) {
            for (var e in this.activeClients)
              if (
                this.activeClients.hasOwnProperty(e) &&
                this.activeClients[e].activeTargetIds.has(t)
              )
                return !0
            return !1
          }),
          (t.prototype.addPendingMutation = function(t) {
            this.persistMutationState(t, 'pending')
          }),
          (t.prototype.updateMutationState = function(t, e, n) {
            this.persistMutationState(t, e, n), this.removeMutationState(t)
          }),
          (t.prototype.addLocalQueryTarget = function(t) {
            var e = 'not-current'
            if (this.isActiveQueryTarget(t)) {
              var n = this.storage.getItem(
                this.toWebStorageQueryTargetMetadataKey(t),
              )
              if (n) {
                var r = uu.fromWebStorageEntry(t, n)
                r && (e = r.state)
              }
            }
            return (
              this.localClientState.addQueryTarget(t),
              this.persistClientState(),
              e
            )
          }),
          (t.prototype.removeLocalQueryTarget = function(t) {
            this.localClientState.removeQueryTarget(t),
              this.persistClientState()
          }),
          (t.prototype.isLocalQueryTarget = function(t) {
            return this.localClientState.activeTargetIds.has(t)
          }),
          (t.prototype.clearQueryState = function(t) {
            this.removeItem(this.toWebStorageQueryTargetMetadataKey(t))
          }),
          (t.prototype.updateQueryState = function(t, e, n) {
            this.persistQueryTargetState(t, e, n)
          }),
          (t.prototype.handleUserChange = function(t, e, n) {
            var r = this
            e.forEach(function(t) {
              r.removeMutationState(t)
            }),
              (this.currentUser = t),
              n.forEach(function(t) {
                r.addPendingMutation(t)
              })
          }),
          (t.prototype.setOnlineState = function(t) {
            this.persistOnlineState(t)
          }),
          (t.prototype.shutdown = function() {
            this.started &&
              (this.platform.window.removeEventListener(
                'storage',
                this.storageListener,
              ),
              this.removeItem(this.localClientStorageKey),
              (this.started = !1))
          }),
          (t.prototype.getItem = function(t) {
            var e = this.storage.getItem(t)
            return Xn(eu, 'READ', t, e), e
          }),
          (t.prototype.setItem = function(t, e) {
            Xn(eu, 'SET', t, e), this.storage.setItem(t, e)
          }),
          (t.prototype.removeItem = function(t) {
            Xn(eu, 'REMOVE', t), this.storage.removeItem(t)
          }),
          (t.prototype.handleWebStorageEvent = function(t) {
            var e = this
            if (t.storageArea === this.storage) {
              if (
                (Xn(eu, 'EVENT', t.key, t.newValue),
                t.key === this.localClientStorageKey)
              )
                return void Yn(
                  'Received WebStorage notification for local change. Another client might have garbage-collected our state',
                )
              this.queue.enqueueAndForget(function() {
                return u(e, void 0, void 0, function() {
                  var e, n, r, i, o, s
                  return c(this, function(a) {
                    if (!this.started) return this.earlyEvents.push(t), [2]
                    if (null === t.key) return [2]
                    if (this.clientStateKeyRe.test(t.key)) {
                      if (null == t.newValue)
                        return (
                          (n = this.fromWebStorageClientStateKey(t.key)),
                          [2, this.handleClientStateEvent(n, null)]
                        )
                      if (
                        (e = this.fromWebStorageClientState(t.key, t.newValue))
                      )
                        return [2, this.handleClientStateEvent(e.clientId, e)]
                    } else if (this.mutationBatchKeyRe.test(t.key)) {
                      if (
                        null !== t.newValue &&
                        (r = this.fromWebStorageMutationMetadata(
                          t.key,
                          t.newValue,
                        ))
                      )
                        return [2, this.handleMutationBatchEvent(r)]
                    } else if (this.queryTargetKeyRe.test(t.key)) {
                      if (
                        null !== t.newValue &&
                        (i = this.fromWebStorageQueryTargetMetadata(
                          t.key,
                          t.newValue,
                        ))
                      )
                        return [2, this.handleQueryTargetEvent(i)]
                    } else if (t.key === this.onlineStateKey) {
                      if (
                        null !== t.newValue &&
                        (o = this.fromWebStorageOnlineState(t.newValue))
                      )
                        return [2, this.handleOnlineStateEvent(o)]
                    } else
                      t.key === this.sequenceNumberKey &&
                        (Zn(
                          !!this.sequenceNumberHandler,
                          'Missing sequenceNumberHandler',
                        ),
                        (s = (function(t) {
                          var e = zo.INVALID
                          if (null != t)
                            try {
                              var n = JSON.parse(t)
                              Zn(
                                'number' == typeof n,
                                'Found non-numeric sequence number',
                              ),
                                (e = n)
                            } catch (t) {
                              Yn(
                                eu,
                                'Failed to read sequence number from WebStorage',
                                t,
                              )
                            }
                          return e
                        })(t.newValue)) !== zo.INVALID &&
                          this.sequenceNumberHandler(s))
                    return [2]
                  })
                })
              })
            }
          }),
          Object.defineProperty(t.prototype, 'localClientState', {
            get: function() {
              return this.activeClients[this.localClientId]
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.persistClientState = function() {
            this.setItem(
              this.localClientStorageKey,
              this.localClientState.toWebStorageJSON(),
            )
          }),
          (t.prototype.persistMutationState = function(t, e, n) {
            var r = new au(this.currentUser, t, e, n),
              i = this.toWebStorageMutationBatchKey(t)
            this.setItem(i, r.toWebStorageJSON())
          }),
          (t.prototype.removeMutationState = function(t) {
            var e = this.toWebStorageMutationBatchKey(t)
            this.removeItem(e)
          }),
          (t.prototype.persistOnlineState = function(t) {
            var e = { clientId: this.localClientId, onlineState: Oa[t] }
            this.storage.setItem(this.onlineStateKey, JSON.stringify(e))
          }),
          (t.prototype.persistQueryTargetState = function(t, e, n) {
            var r = this.toWebStorageQueryTargetMetadataKey(t),
              i = new uu(t, e, n)
            this.setItem(r, i.toWebStorageJSON())
          }),
          (t.prototype.toWebStorageClientStateKey = function(t) {
            return (
              Zn(
                -1 === t.indexOf('_'),
                "Client key cannot contain '_', but was '" + t + "'",
              ),
              nu + '_' + this.persistenceKey + '_' + t
            )
          }),
          (t.prototype.toWebStorageQueryTargetMetadataKey = function(t) {
            return iu + '_' + this.persistenceKey + '_' + t
          }),
          (t.prototype.toWebStorageMutationBatchKey = function(t) {
            var e = ru + '_' + this.persistenceKey + '_' + t
            return (
              this.currentUser.isAuthenticated() &&
                (e += '_' + this.currentUser.uid),
              e
            )
          }),
          (t.prototype.fromWebStorageClientStateKey = function(t) {
            var e = this.clientStateKeyRe.exec(t)
            return e ? e[1] : null
          }),
          (t.prototype.fromWebStorageClientState = function(t, e) {
            var n = this.fromWebStorageClientStateKey(t)
            return (
              Zn(null !== n, "Cannot parse client state key '" + t + "'"),
              cu.fromWebStorageEntry(n, e)
            )
          }),
          (t.prototype.fromWebStorageMutationMetadata = function(t, e) {
            var n = this.mutationBatchKeyRe.exec(t)
            Zn(null !== n, "Cannot parse mutation batch key '" + t + "'")
            var r = Number(n[1]),
              i = void 0 !== n[2] ? n[2] : null
            return au.fromWebStorageEntry(new tu(i), r, e)
          }),
          (t.prototype.fromWebStorageQueryTargetMetadata = function(t, e) {
            var n = this.queryTargetKeyRe.exec(t)
            Zn(null !== n, "Cannot parse query target key '" + t + "'")
            var r = Number(n[1])
            return uu.fromWebStorageEntry(r, e)
          }),
          (t.prototype.fromWebStorageOnlineState = function(t) {
            return hu.fromWebStorageEntry(t)
          }),
          (t.prototype.handleMutationBatchEvent = function(t) {
            return u(this, void 0, void 0, function() {
              return c(this, function(e) {
                return t.user.uid !== this.currentUser.uid
                  ? (Xn(
                      eu,
                      'Ignoring mutation for non-active user ' + t.user.uid,
                    ),
                    [2])
                  : [
                      2,
                      this.syncEngine.applyBatchState(
                        t.batchId,
                        t.state,
                        t.error,
                      ),
                    ]
              })
            })
          }),
          (t.prototype.handleQueryTargetEvent = function(t) {
            return this.syncEngine.applyTargetState(
              t.targetId,
              t.state,
              t.error,
            )
          }),
          (t.prototype.handleClientStateEvent = function(t, e) {
            var n = this,
              r = this.getAllActiveQueryTargets()
            e ? (this.activeClients[t] = e) : delete this.activeClients[t]
            var i = this.getAllActiveQueryTargets(),
              o = [],
              s = []
            return (
              i.forEach(function(t) {
                return u(n, void 0, void 0, function() {
                  return c(this, function(e) {
                    return r.has(t) || o.push(t), [2]
                  })
                })
              }),
              r.forEach(function(t) {
                return u(n, void 0, void 0, function() {
                  return c(this, function(e) {
                    return i.has(t) || s.push(t), [2]
                  })
                })
              }),
              this.syncEngine.applyActiveTargetsChange(o, s)
            )
          }),
          (t.prototype.handleOnlineStateEvent = function(t) {
            this.activeClients[t.clientId] &&
              this.onlineStateHandler(t.onlineState)
          }),
          t
        )
      })()
    var du = (function() {
        function t() {
          ;(this.localState = new lu()),
            (this.queryState = {}),
            (this.syncEngine = null),
            (this.onlineStateHandler = null),
            (this.sequenceNumberHandler = null)
        }
        return (
          (t.prototype.addPendingMutation = function(t) {}),
          (t.prototype.updateMutationState = function(t, e, n) {}),
          (t.prototype.addLocalQueryTarget = function(t) {
            return (
              this.localState.addQueryTarget(t),
              this.queryState[t] || 'not-current'
            )
          }),
          (t.prototype.updateQueryState = function(t, e, n) {
            this.queryState[t] = e
          }),
          (t.prototype.removeLocalQueryTarget = function(t) {
            this.localState.removeQueryTarget(t)
          }),
          (t.prototype.isLocalQueryTarget = function(t) {
            return this.localState.activeTargetIds.has(t)
          }),
          (t.prototype.clearQueryState = function(t) {
            delete this.queryState[t]
          }),
          (t.prototype.getAllActiveQueryTargets = function() {
            return this.localState.activeTargetIds
          }),
          (t.prototype.isActiveQueryTarget = function(t) {
            return this.localState.activeTargetIds.has(t)
          }),
          (t.prototype.start = function() {
            return (this.localState = new lu()), Promise.resolve()
          }),
          (t.prototype.handleUserChange = function(t, e, n) {}),
          (t.prototype.setOnlineState = function(t) {}),
          (t.prototype.shutdown = function() {}),
          (t.prototype.writeSequenceNumber = function(t) {}),
          t
        )
      })(),
      pu = (function() {
        function t(t, e, n, r) {
          ;(this.platform = t),
            (this.databaseInfo = e),
            (this.credentials = n),
            (this.asyncQueue = r),
            (this.clientId = Nr.newId())
        }
        return (
          (t.prototype.start = function(t) {
            var e = this,
              n = new Ho(),
              r = new Ho(),
              i = !1
            return (
              this.credentials.setChangeListener(function(o) {
                i
                  ? e.asyncQueue.enqueueAndForget(function() {
                      return e.handleCredentialChange(o)
                    })
                  : ((i = !0),
                    e
                      .initializePersistence(t, r, o)
                      .then(function() {
                        return e.initializeRest(o)
                      })
                      .then(n.resolve, n.reject))
              }),
              this.asyncQueue.enqueueAndForget(function() {
                return n.promise
              }),
              r.promise
            )
          }),
          (t.prototype.enableNetwork = function() {
            var t = this
            return this.asyncQueue.enqueue(function() {
              return t.syncEngine.enableNetwork()
            })
          }),
          (t.prototype.initializePersistence = function(t, e, n) {
            var r = this
            return t.enabled
              ? this.startIndexedDbPersistence(n, t)
                  .then(e.resolve)
                  .catch(function(t) {
                    return (
                      e.reject(t),
                      r.canFallback(t)
                        ? (console.warn(
                            'Error enabling offline storage. Falling back to storage disabled: ' +
                              t,
                          ),
                          r.startMemoryPersistence())
                        : Promise.reject(t)
                    )
                  })
              : (e.resolve(), this.startMemoryPersistence())
          }),
          (t.prototype.canFallback = function(t) {
            return t instanceof rr
              ? t.code === nr.FAILED_PRECONDITION || t.code === nr.UNIMPLEMENTED
              : !(
                  'undefined' != typeof DOMException &&
                  t instanceof DOMException
                ) ||
                  (22 === t.code || 20 === t.code)
          }),
          (t.prototype.startIndexedDbPersistence = function(t, e) {
            var n = this
            Zn(
              e.enabled,
              'Should only start IndexedDb persitence with offline persistence enabled.',
            )
            var r = fa.buildStoragePrefix(this.databaseInfo),
              i = new _o(this.databaseInfo.databaseId, { useProto3Json: !0 })
            return Promise.resolve().then(function() {
              return u(n, void 0, void 0, function() {
                var n, o
                return c(this, function(s) {
                  switch (s.label) {
                    case 0:
                      if (
                        e.experimentalTabSynchronization &&
                        !fu.isAvailable(this.platform)
                      )
                        throw new rr(
                          nr.UNIMPLEMENTED,
                          'IndexedDB persistence is only available on platforms that support LocalStorage.',
                        )
                      return e.experimentalTabSynchronization
                        ? ((this.sharedClientState = new fu(
                            this.asyncQueue,
                            this.platform,
                            r,
                            this.clientId,
                            t,
                          )),
                          (n = this),
                          [
                            4,
                            fa.createMultiClientIndexedDbPersistence(
                              r,
                              this.clientId,
                              this.platform,
                              this.asyncQueue,
                              i,
                              { sequenceNumberSyncer: this.sharedClientState },
                            ),
                          ])
                        : [3, 2]
                    case 1:
                      return (n.persistence = s.sent()), [3, 4]
                    case 2:
                      return (
                        (this.sharedClientState = new du()),
                        (o = this),
                        [
                          4,
                          fa.createIndexedDbPersistence(
                            r,
                            this.clientId,
                            this.platform,
                            this.asyncQueue,
                            i,
                          ),
                        ]
                      )
                    case 3:
                      ;(o.persistence = s.sent()), (s.label = 4)
                    case 4:
                      return [2]
                  }
                })
              })
            })
          }),
          (t.prototype.startMemoryPersistence = function() {
            var t = new _o(this.databaseInfo.databaseId, { useProto3Json: !0 })
            return (
              (this.persistence = Na.createEagerPersistence(this.clientId, t)),
              (this.sharedClientState = new du()),
              Promise.resolve()
            )
          }),
          (t.prototype.initializeRest = function(t) {
            var e = this
            return (
              Xn('FirestoreClient', 'Initializing. user=', t.uid),
              this.platform.loadConnection(this.databaseInfo).then(function(n) {
                return u(e, void 0, void 0, function() {
                  var e,
                    r,
                    i,
                    o,
                    s = this
                  return c(this, function(a) {
                    switch (a.label) {
                      case 0:
                        return (
                          (this.localStore = new Ta(this.persistence, t)),
                          (e = this.platform.newSerializer(
                            this.databaseInfo.databaseId,
                          )),
                          (r = new Ba(this.asyncQueue, n, this.credentials, e)),
                          (i = function(t) {
                            return s.syncEngine.applyOnlineStateChange(
                              t,
                              Pa.RemoteStore,
                            )
                          }),
                          (o = function(t) {
                            return s.syncEngine.applyOnlineStateChange(
                              t,
                              Pa.SharedClientState,
                            )
                          }),
                          (this.remoteStore = new Ka(
                            this.localStore,
                            r,
                            this.asyncQueue,
                            i,
                          )),
                          (this.syncEngine = new Za(
                            this.localStore,
                            this.remoteStore,
                            this.sharedClientState,
                            t,
                          )),
                          (this.sharedClientState.onlineStateHandler = o),
                          (this.remoteStore.syncEngine = this.syncEngine),
                          (this.sharedClientState.syncEngine = this.syncEngine),
                          (this.eventMgr = new ja(this.syncEngine)),
                          [4, this.sharedClientState.start()]
                        )
                      case 1:
                        return a.sent(), [4, this.remoteStore.start()]
                      case 2:
                        return (
                          a.sent(),
                          [
                            4,
                            this.persistence.setPrimaryStateListener(function(
                              t,
                            ) {
                              return s.syncEngine.applyPrimaryState(t)
                            }),
                          ]
                        )
                      case 3:
                        return a.sent(), [2]
                    }
                  })
                })
              })
            )
          }),
          (t.prototype.handleCredentialChange = function(t) {
            return (
              this.asyncQueue.verifyOperationInProgress(),
              Xn(
                'FirestoreClient',
                'Credential Changed. Current user: ' + t.uid,
              ),
              this.syncEngine.handleCredentialChange(t)
            )
          }),
          (t.prototype.disableNetwork = function() {
            var t = this
            return this.asyncQueue.enqueue(function() {
              return t.syncEngine.disableNetwork()
            })
          }),
          (t.prototype.shutdown = function(t) {
            var e = this
            return this.asyncQueue.enqueue(function() {
              return u(e, void 0, void 0, function() {
                return c(this, function(e) {
                  switch (e.label) {
                    case 0:
                      return [4, this.remoteStore.shutdown()]
                    case 1:
                      return e.sent(), [4, this.sharedClientState.shutdown()]
                    case 2:
                      return (
                        e.sent(),
                        [
                          4,
                          this.persistence.shutdown(
                            t && t.purgePersistenceWithDataLoss,
                          ),
                        ]
                      )
                    case 3:
                      return (
                        e.sent(), this.credentials.removeChangeListener(), [2]
                      )
                  }
                })
              })
            })
          }),
          (t.prototype.listen = function(t, e, n) {
            var r = this,
              i = new Ga(t, e, n)
            return (
              this.asyncQueue.enqueueAndForget(function() {
                return r.eventMgr.listen(i)
              }),
              i
            )
          }),
          (t.prototype.unlisten = function(t) {
            var e = this
            this.asyncQueue.enqueueAndForget(function() {
              return e.eventMgr.unlisten(t)
            })
          }),
          (t.prototype.getDocumentFromLocalCache = function(t) {
            var e = this
            return this.asyncQueue
              .enqueue(function() {
                return e.localStore.readDocument(t)
              })
              .then(function(t) {
                if (t instanceof zr) return t
                if (t instanceof Hr) return null
                throw new rr(
                  nr.UNAVAILABLE,
                  "Failed to get document from cache. (However, this document may exist on the server. Run again without setting 'source' in the GetOptions to attempt to retrieve the document from the server.)",
                )
              })
          }),
          (t.prototype.getDocumentsFromLocalCache = function(t) {
            var e = this
            return this.asyncQueue
              .enqueue(function() {
                return e.localStore.executeQuery(t)
              })
              .then(function(e) {
                var n = uo(),
                  r = new Ya(t, n),
                  i = r.computeDocChanges(e)
                return r.applyChanges(i, !1).snapshot
              })
          }),
          (t.prototype.write = function(t) {
            var e = this,
              n = new Ho()
            return (
              this.asyncQueue.enqueueAndForget(function() {
                return e.syncEngine.write(t, n)
              }),
              n.promise
            )
          }),
          (t.prototype.databaseId = function() {
            return this.databaseInfo.databaseId
          }),
          (t.prototype.transaction = function(t) {
            var e = this
            return this.asyncQueue
              .enqueue(function() {
                return u(e, void 0, void 0, function() {
                  return c(this, function(t) {
                    return [2]
                  })
                })
              })
              .then(function() {
                return e.syncEngine.runTransaction(t)
              })
          }),
          t
        )
      })(),
      mu = (function() {
        function t(t) {
          ;(this.observer = t), (this.muted = !1)
        }
        return (
          (t.prototype.next = function(t) {
            this.scheduleEvent(this.observer.next, t)
          }),
          (t.prototype.error = function(t) {
            this.scheduleEvent(this.observer.error, t)
          }),
          (t.prototype.mute = function() {
            this.muted = !0
          }),
          (t.prototype.scheduleEvent = function(t, e) {
            var n = this
            this.muted ||
              setTimeout(function() {
                n.muted || t(e)
              }, 0)
          }),
          t
        )
      })(),
      yu = (function() {
        function t() {
          for (var t = [], e = 0; e < arguments.length; e++) t[e] = arguments[e]
          !(function(t, e, n, r) {
            if (!(e instanceof Array) || e.length < r)
              throw new rr(
                nr.INVALID_ARGUMENT,
                'Function ' +
                  t +
                  '() requires its ' +
                  n +
                  ' argument to be an array with at least ' +
                  Dr(r, 'element') +
                  '.',
              )
          })('FieldPath', t, 'fieldNames', 1)
          for (var n = 0; n < t.length; ++n)
            if ((dr('FieldPath', 'string', n, t[n]), 0 === t[n].length))
              throw new rr(
                nr.INVALID_ARGUMENT,
                'Invalid field name at argument $(i + 1). Field names must not be empty.',
              )
          this._internalPath = new Wr(t)
        }
        return (
          (t.documentId = function() {
            return t._DOCUMENT_ID
          }),
          (t.prototype.isEqual = function(e) {
            if (!(e instanceof t)) throw Ir('isEqual', 'FieldPath', 1, e)
            return this._internalPath.isEqual(e._internalPath)
          }),
          (t._DOCUMENT_ID = new t(Wr.keyField().canonicalString())),
          t
        )
      })(),
      gu = new RegExp('[~\\*/\\[\\]]')
    var vu = (function() {
        return function(t, e) {
          ;(this.user = e),
            (this.type = 'OAuth'),
            (this.authHeaders = { Authorization: 'Bearer ' + t })
        }
      })(),
      bu = (function() {
        function t() {
          this.changeListener = null
        }
        return (
          (t.prototype.getToken = function() {
            return Promise.resolve(null)
          }),
          (t.prototype.invalidateToken = function() {}),
          (t.prototype.setChangeListener = function(t) {
            Zn(!this.changeListener, 'Can only call setChangeListener() once.'),
              (this.changeListener = t),
              t(tu.UNAUTHENTICATED)
          }),
          (t.prototype.removeChangeListener = function() {
            Zn(
              null !== this.changeListener,
              'removeChangeListener() when no listener registered',
            ),
              (this.changeListener = null)
          }),
          t
        )
      })(),
      wu = (function() {
        function t(t) {
          var e = this
          ;(this.app = t),
            (this.tokenListener = null),
            (this.tokenCounter = 0),
            (this.changeListener = null),
            (this.forceRefresh = !1),
            (this.tokenListener = function() {
              e.tokenCounter++,
                (e.currentUser = e.getUser()),
                e.changeListener && e.changeListener(e.currentUser)
            }),
            (this.tokenCounter = 0),
            this.app.INTERNAL.addAuthTokenListener(this.tokenListener)
        }
        return (
          (t.prototype.getToken = function() {
            var t = this
            Zn(
              null != this.tokenListener,
              'getToken cannot be called after listener removed.',
            )
            var e = this.tokenCounter,
              n = this.forceRefresh
            return (
              (this.forceRefresh = !1),
              this.app.INTERNAL.getToken(n).then(function(n) {
                if (t.tokenCounter !== e)
                  throw new rr(
                    nr.ABORTED,
                    'getToken aborted due to token change.',
                  )
                return n
                  ? (Zn(
                      'string' == typeof n.accessToken,
                      'Invalid tokenData returned from getToken():' + n,
                    ),
                    new vu(n.accessToken, t.currentUser))
                  : null
              })
            )
          }),
          (t.prototype.invalidateToken = function() {
            this.forceRefresh = !0
          }),
          (t.prototype.setChangeListener = function(t) {
            Zn(!this.changeListener, 'Can only call setChangeListener() once.'),
              (this.changeListener = t),
              this.currentUser && t(this.currentUser)
          }),
          (t.prototype.removeChangeListener = function() {
            Zn(
              null != this.tokenListener,
              'removeChangeListener() called twice',
            ),
              Zn(
                null !== this.changeListener,
                'removeChangeListener() called when no listener registered',
              ),
              this.app.INTERNAL.removeAuthTokenListener(this.tokenListener),
              (this.tokenListener = null),
              (this.changeListener = null)
          }),
          (t.prototype.getUser = function() {
            var t = this.app.INTERNAL.getUid()
            return (
              Zn(
                null === t || 'string' == typeof t,
                'Received invalid UID: ' + t,
              ),
              new tu(t)
            )
          }),
          t
        )
      })(),
      Tu = (function() {
        function t(t, e) {
          ;(this.gapi = t),
            (this.sessionIndex = e),
            (this.type = 'FirstParty'),
            (this.user = tu.FIRST_PARTY),
            Zn(
              this.gapi &&
                this.gapi.auth &&
                this.gapi.auth.getAuthHeaderValueForFirstParty,
              'unexpected gapi interface',
            )
        }
        return (
          Object.defineProperty(t.prototype, 'authHeaders', {
            get: function() {
              return {
                Authorization: this.gapi.auth.getAuthHeaderValueForFirstParty(
                  [],
                ),
                'X-Goog-AuthUser': this.sessionIndex,
              }
            },
            enumerable: !0,
            configurable: !0,
          }),
          t
        )
      })(),
      Su = (function() {
        function t(t, e) {
          ;(this.gapi = t),
            (this.sessionIndex = e),
            Zn(
              this.gapi &&
                this.gapi.auth &&
                this.gapi.auth.getAuthHeaderValueForFirstParty,
              'unexpected gapi interface',
            )
        }
        return (
          (t.prototype.getToken = function() {
            return Promise.resolve(new Tu(this.gapi, this.sessionIndex))
          }),
          (t.prototype.setChangeListener = function(t) {
            t(tu.FIRST_PARTY)
          }),
          (t.prototype.removeChangeListener = function() {}),
          (t.prototype.invalidateToken = function() {}),
          t
        )
      })()
    function Eu(t) {
      return (function(t, e) {
        if ('object' != typeof t || null === t) return !1
        for (var n = t, r = 0, i = e; r < i.length; r++) {
          var o = i[r]
          if (o in n && 'function' == typeof n[o]) return !0
        }
        return !1
      })(t, ['next', 'error', 'complete'])
    }
    var Iu,
      Cu = (function() {
        function t(t) {
          this._methodName = t
        }
        return (
          (t.delete = function() {
            return Du.instance
          }),
          (t.serverTimestamp = function() {
            return Nu.instance
          }),
          (t.arrayUnion = function() {
            for (var t = [], e = 0; e < arguments.length; e++)
              t[e] = arguments[e]
            return lr('FieldValue.arrayUnion', arguments, 1), new Au(t)
          }),
          (t.arrayRemove = function() {
            for (var t = [], e = 0; e < arguments.length; e++)
              t[e] = arguments[e]
            return lr('FieldValue.arrayRemove', arguments, 1), new ku(t)
          }),
          (t.prototype.isEqual = function(t) {
            return this === t
          }),
          t
        )
      })(),
      Du = (function(t) {
        function e() {
          return t.call(this, 'FieldValue.delete') || this
        }
        return a(e, t), (e.instance = new e()), e
      })(Cu),
      Nu = (function(t) {
        function e() {
          return t.call(this, 'FieldValue.serverTimestamp') || this
        }
        return a(e, t), (e.instance = new e()), e
      })(Cu),
      Au = (function(t) {
        function e(e) {
          var n = t.call(this, 'FieldValue.arrayUnion') || this
          return (n._elements = e), n
        }
        return a(e, t), e
      })(Cu),
      ku = (function(t) {
        function e(e) {
          var n = t.call(this, 'FieldValue.arrayRemove') || this
          return (n._elements = e), n
        }
        return a(e, t), e
      })(Cu),
      Ru = ir(Cu, 'Use FieldValue.<field>() instead.'),
      Mu = /^__.*__$/,
      Ou = (function() {
        function t(t, e, n) {
          ;(this.data = t), (this.fieldMask = e), (this.fieldTransforms = n)
        }
        return (
          (t.prototype.toMutations = function(t, e) {
            var n = []
            return (
              null !== this.fieldMask
                ? n.push(new Ki(t, this.data, this.fieldMask, e))
                : n.push(new Qi(t, this.data, e)),
              this.fieldTransforms.length > 0 &&
                n.push(new Wi(t, this.fieldTransforms)),
              n
            )
          }),
          t
        )
      })(),
      Pu = (function() {
        function t(t, e, n) {
          ;(this.data = t), (this.fieldMask = e), (this.fieldTransforms = n)
        }
        return (
          (t.prototype.toMutations = function(t, e) {
            var n = [new Ki(t, this.data, this.fieldMask, e)]
            return (
              this.fieldTransforms.length > 0 &&
                n.push(new Wi(t, this.fieldTransforms)),
              n
            )
          }),
          t
        )
      })()
    function _u(t) {
      switch (t) {
        case Iu.Set:
        case Iu.MergeSet:
        case Iu.Update:
          return !0
        case Iu.Argument:
          return !1
        default:
          throw $n('Unexpected case for UserDataSource: ' + t)
      }
    }
    !(function(t) {
      ;(t[(t.Set = 0)] = 'Set'),
        (t[(t.Update = 1)] = 'Update'),
        (t[(t.MergeSet = 2)] = 'MergeSet'),
        (t[(t.Argument = 3)] = 'Argument')
    })(Iu || (Iu = {}))
    var Lu = (function() {
        function t(t, e, n, r, i, o) {
          ;(this.dataSource = t),
            (this.methodName = e),
            (this.path = n),
            (this.arrayElement = r),
            void 0 === i && this.validatePath(),
            (this.arrayElement = void 0 !== r && r),
            (this.fieldTransforms = i || []),
            (this.fieldMask = o || [])
        }
        return (
          (t.prototype.childContextForField = function(e) {
            var n = null == this.path ? null : this.path.child(e),
              r = new t(
                this.dataSource,
                this.methodName,
                n,
                !1,
                this.fieldTransforms,
                this.fieldMask,
              )
            return r.validatePathSegment(e), r
          }),
          (t.prototype.childContextForFieldPath = function(e) {
            var n = null == this.path ? null : this.path.child(e),
              r = new t(
                this.dataSource,
                this.methodName,
                n,
                !1,
                this.fieldTransforms,
                this.fieldMask,
              )
            return r.validatePath(), r
          }),
          (t.prototype.childContextForArray = function(e) {
            return new t(
              this.dataSource,
              this.methodName,
              null,
              !0,
              this.fieldTransforms,
              this.fieldMask,
            )
          }),
          (t.prototype.createError = function(t) {
            var e =
              null === this.path || this.path.isEmpty()
                ? ''
                : ' (found in field ' + this.path.toString() + ')'
            return new rr(
              nr.INVALID_ARGUMENT,
              'Function ' +
                this.methodName +
                '() called with invalid data. ' +
                t +
                e,
            )
          }),
          (t.prototype.contains = function(t) {
            return (
              void 0 !==
                this.fieldMask.find(function(e) {
                  return t.isPrefixOf(e)
                }) ||
              void 0 !==
                this.fieldTransforms.find(function(e) {
                  return t.isPrefixOf(e.field)
                })
            )
          }),
          (t.prototype.validatePath = function() {
            if (null !== this.path)
              for (var t = 0; t < this.path.length; t++)
                this.validatePathSegment(this.path.get(t))
          }),
          (t.prototype.validatePathSegment = function(t) {
            if (_u(this.dataSource) && Mu.test(t))
              throw this.createError(
                'Document fields cannot begin and end with __',
              )
          }),
          t
        )
      })(),
      xu = (function() {
        return function(t, e) {
          ;(this.databaseId = t), (this.key = e)
        }
      })(),
      qu = (function() {
        function t(t) {
          this.preConverter = t
        }
        return (
          (t.prototype.parseSetData = function(t, e) {
            var n = new Lu(Iu.Set, t, Wr.EMPTY_PATH)
            Vu('Data must be an object, but it was:', n, e)
            var r = this.parseData(e, n)
            return new Ou(r, null, n.fieldTransforms)
          }),
          (t.prototype.parseMergeData = function(t, e, n) {
            var r = new Lu(Iu.MergeSet, t, Wr.EMPTY_PATH)
            Vu('Data must be an object, but it was:', r, e)
            var i,
              o,
              s = this.parseData(e, r)
            if (n) {
              for (var a = [], u = 0, c = n; u < c.length; u++) {
                var h = c[u],
                  l = void 0
                if (h instanceof yu) l = h._internalPath
                else {
                  if ('string' != typeof h)
                    throw $n(
                      'Expected stringOrFieldPath to be a string or a FieldPath',
                    )
                  l = Uu(t, h)
                }
                if (!r.contains(l))
                  throw new rr(
                    nr.INVALID_ARGUMENT,
                    "Field '" +
                      l +
                      "' is specified in your field mask but missing from your input data.",
                  )
                a.push(l)
              }
              ;(i = new qi(a)),
                (o = r.fieldTransforms.filter(function(t) {
                  return i.covers(t.field)
                }))
            } else (i = new qi(r.fieldMask)), (o = r.fieldTransforms)
            return new Ou(s, i, o)
          }),
          (t.prototype.parseUpdateData = function(t, e) {
            var n = this,
              r = new Lu(Iu.Update, t, Wr.EMPTY_PATH)
            Vu('Data must be an object, but it was:', r, e)
            var i = [],
              o = pi.EMPTY
            ur(e, function(e, s) {
              var a = Uu(t, e),
                u = r.childContextForFieldPath(a)
              if ((s = n.runPreConverter(s, u)) instanceof Du) i.push(a)
              else {
                var c = n.parseData(s, u)
                null != c && (i.push(a), (o = o.set(a, c)))
              }
            })
            var s = new qi(i)
            return new Pu(o, s, r.fieldTransforms)
          }),
          (t.prototype.parseUpdateVarargs = function(t, e, n, r) {
            var i = new Lu(Iu.Update, t, Wr.EMPTY_PATH),
              o = [Bu(t, e)],
              s = [n]
            if (r.length % 2 != 0)
              throw new rr(
                nr.INVALID_ARGUMENT,
                'Function ' +
                  t +
                  '() needs to be called with an even number of arguments that alternate between field names and values.',
              )
            for (var a = 0; a < r.length; a += 2)
              o.push(Bu(t, r[a])), s.push(r[a + 1])
            var u = [],
              c = pi.EMPTY
            for (a = 0; a < o.length; ++a) {
              var h = o[a],
                l = i.childContextForFieldPath(h),
                f = this.runPreConverter(s[a], l)
              if (f instanceof Du) u.push(h)
              else {
                var d = this.parseData(f, l)
                null != d && (u.push(h), (c = c.set(h, d)))
              }
            }
            var p = new qi(u)
            return new Pu(c, p, i.fieldTransforms)
          }),
          (t.prototype.parseQueryValue = function(t, e) {
            var n = new Lu(Iu.Argument, t, Wr.EMPTY_PATH),
              r = this.parseData(e, n)
            return (
              Zn(null != r, 'Parsed data should not be null.'),
              Zn(
                0 === n.fieldTransforms.length,
                'Field transforms should have been disallowed.',
              ),
              r
            )
          }),
          (t.prototype.runPreConverter = function(t, e) {
            try {
              return this.preConverter(t)
            } catch (t) {
              var n = Qu(t)
              throw e.createError(n)
            }
          }),
          (t.prototype.parseData = function(t, e) {
            if (Fu((t = this.runPreConverter(t, e))))
              return (
                Vu('Unsupported field value:', e, t), this.parseObject(t, e)
              )
            if (t instanceof Cu) return this.parseSentinelFieldValue(t, e), null
            if ((e.path && e.fieldMask.push(e.path), t instanceof Array)) {
              if (e.arrayElement)
                throw e.createError('Nested arrays are not supported')
              return this.parseArray(t, e)
            }
            return this.parseScalarValue(t, e)
          }),
          (t.prototype.parseObject = function(t, e) {
            var n = this,
              r = new Yr(Ar)
            return (
              cr(t)
                ? e.path && e.path.length > 0 && e.fieldMask.push(e.path)
                : ur(t, function(t, i) {
                    var o = n.parseData(i, e.childContextForField(t))
                    null != o && (r = r.insert(t, o))
                  }),
              new pi(r)
            )
          }),
          (t.prototype.parseArray = function(t, e) {
            for (var n = [], r = 0, i = 0, o = t; i < o.length; i++) {
              var s = o[i],
                a = this.parseData(s, e.childContextForArray(r))
              null == a && (a = ni.INSTANCE), n.push(a), r++
            }
            return new mi(n)
          }),
          (t.prototype.parseSentinelFieldValue = function(t, e) {
            if (!_u(e.dataSource))
              throw e.createError(
                t._methodName + '() can only be used with update() and set()',
              )
            if (null === e.path)
              throw e.createError(
                t._methodName + '() is not currently supported inside arrays',
              )
            if (t instanceof Du) {
              if (e.dataSource !== Iu.MergeSet)
                throw e.dataSource === Iu.Update
                  ? (Zn(
                      e.path.length > 0,
                      'FieldValue.delete() at the top level should have already been handled.',
                    ),
                    e.createError(
                      'FieldValue.delete() can only appear at the top level of your update data',
                    ))
                  : e.createError(
                      'FieldValue.delete() cannot be used with set() unless you pass {merge:true}',
                    )
              e.fieldMask.push(e.path)
            } else if (t instanceof Nu)
              e.fieldTransforms.push(new Fi(e.path, Gi.instance))
            else if (t instanceof Au) {
              var n = this.parseArrayTransformElements(
                  t._methodName,
                  t._elements,
                ),
                r = new zi(n)
              e.fieldTransforms.push(new Fi(e.path, r))
            } else if (t instanceof ku) {
              n = this.parseArrayTransformElements(t._methodName, t._elements)
              var i = new Hi(n)
              e.fieldTransforms.push(new Fi(e.path, i))
            } else $n('Unknown FieldValue type: ' + t)
          }),
          (t.prototype.parseScalarValue = function(t, e) {
            if (null === t) return ni.INSTANCE
            if ('number' == typeof t) return Ti(t) ? new si(t) : new ai(t)
            if ('boolean' == typeof t) return ri.of(t)
            if ('string' == typeof t) return new ui(t)
            if (t instanceof Date) return new ci(qr.fromDate(t))
            if (t instanceof qr)
              return new ci(
                new qr(t.seconds, 1e3 * Math.floor(t.nanoseconds / 1e3)),
              )
            if (t instanceof xr) return new di(t)
            if (t instanceof _r) return new li(t)
            if (t instanceof xu) return new fi(t.databaseId, t.key)
            throw e.createError('Unsupported field value: ' + Tr(t))
          }),
          (t.prototype.parseArrayTransformElements = function(t, e) {
            var n = this
            return e.map(function(e, r) {
              var i = new Lu(Iu.Argument, t, Wr.EMPTY_PATH)
              return n.parseData(e, i.childContextForArray(r))
            })
          }),
          t
        )
      })()
    function Fu(t) {
      return !(
        'object' != typeof t ||
        null === t ||
        t instanceof Array ||
        t instanceof Date ||
        t instanceof qr ||
        t instanceof xr ||
        t instanceof _r ||
        t instanceof xu ||
        t instanceof Cu
      )
    }
    function Vu(t, e, n) {
      if (!Fu(n) || !wr(n)) {
        var r = Tr(n)
        throw 'an object' === r
          ? e.createError(t + ' a custom object')
          : e.createError(t + ' ' + r)
      }
    }
    function Bu(t, e) {
      if (e instanceof yu) return e._internalPath
      if ('string' == typeof e) return Uu(t, e)
      throw new rr(
        nr.INVALID_ARGUMENT,
        'Function ' +
          t +
          '() called with invalid data. Field path arguments must be of type string or FieldPath.',
      )
    }
    function Uu(t, e) {
      try {
        return (function(t) {
          if (t.search(gu) >= 0)
            throw new rr(
              nr.INVALID_ARGUMENT,
              'Invalid field path (' +
                t +
                "). Paths must not contain '~', '*', '/', '[', or ']'",
            )
          try {
            return new (yu.bind.apply(yu, [void 0].concat(t.split('.'))))()
          } catch (e) {
            throw new rr(
              nr.INVALID_ARGUMENT,
              'Invalid field path (' +
                t +
                "). Paths must not be empty, begin with '.', end with '.', or contain '..'",
            )
          }
        })(e)._internalPath
      } catch (e) {
        var n = Qu(e)
        throw new rr(
          nr.INVALID_ARGUMENT,
          'Function ' + t + '() called with invalid data. ' + n,
        )
      }
    }
    function Qu(t) {
      return t instanceof Error ? t.message : t.toString()
    }
    var Ku = 'firestore.googleapis.com',
      Wu = !0,
      ju = !1,
      Gu = !1,
      zu = (function() {
        function t(t) {
          if (void 0 === t.host) {
            if (void 0 !== t.ssl)
              throw new rr(
                nr.INVALID_ARGUMENT,
                "Can't provide ssl option if host option is not set",
              )
            ;(this.host = Ku), (this.ssl = Wu)
          } else
            mr('settings', 'non-empty string', 'host', t.host),
              (this.host = t.host),
              yr('settings', 'boolean', 'ssl', t.ssl),
              (this.ssl = sr(t.ssl, Wu))
          Er('settings', t, [
            'host',
            'ssl',
            'credentials',
            'timestampsInSnapshots',
          ]),
            yr('settings', 'object', 'credentials', t.credentials),
            (this.credentials = t.credentials),
            yr(
              'settings',
              'boolean',
              'timestampsInSnapshots',
              t.timestampsInSnapshots,
            ),
            (this.timestampsInSnapshots = sr(t.timestampsInSnapshots, ju))
        }
        return (
          (t.prototype.isEqual = function(t) {
            return (
              this.host === t.host &&
              this.ssl === t.ssl &&
              this.timestampsInSnapshots === t.timestampsInSnapshots &&
              this.credentials === t.credentials
            )
          }),
          t
        )
      })(),
      Hu = (function() {
        return function() {}
      })(),
      Xu = (function() {
        function t(t, e) {
          ;(this.enabled = t),
            Zn(
              t || !e,
              'Can only provide PersistenceSettings with persistence enabled',
            ),
            (e = e || {}),
            (this.experimentalTabSynchronization = sr(
              e.experimentalTabSynchronization,
              Gu,
            ))
        }
        return (
          (t.prototype.isEqual = function(t) {
            return (
              this.enabled === t.enabled &&
              this.experimentalTabSynchronization ===
                t.experimentalTabSynchronization
            )
          }),
          t
        )
      })(),
      Yu = (function() {
        function t(e) {
          var n = this
          ;(this._queue = new Yo()),
            (this.INTERNAL = {
              delete: function(t) {
                return u(n, void 0, void 0, function() {
                  return c(this, function(e) {
                    return this._firestoreClient
                      ? [2, this._firestoreClient.shutdown(t)]
                      : [2]
                  })
                })
              },
            })
          var r = new Hu()
          if ('object' == typeof e.options) {
            var i = e
            ;(r.firebaseApp = i),
              (r.databaseId = t.databaseIdFromApp(i)),
              (r.persistenceKey = r.firebaseApp.name),
              (r.credentials = new wu(i))
          } else {
            var o = e
            if (!o.projectId)
              throw new rr(nr.INVALID_ARGUMENT, 'Must provide projectId')
            ;(r.databaseId = new Br(o.projectId, o.database)),
              (r.persistenceKey = '[DEFAULT]'),
              (r.credentials = new bu())
          }
          ;(r.settings = new zu({})),
            (this._config = r),
            (this._databaseId = r.databaseId)
        }
        return (
          (t.prototype.settings = function(t) {
            if (
              (hr('Firestore.settings', arguments, 1),
              dr('Firestore.settings', 'object', 1, t),
              or(t, 'persistence'))
            )
              throw new rr(
                nr.INVALID_ARGUMENT,
                '"persistence" is now specified with a separate call to firestore.enablePersistence().',
              )
            var e = new zu(t)
            if (this._firestoreClient && !this._config.settings.isEqual(e))
              throw new rr(
                nr.FAILED_PRECONDITION,
                'Firestore has already been started and its settings can no longer be changed. You can only call settings() before calling any other methods on a Firestore object.',
              )
            ;(this._config.settings = e),
              void 0 !== e.credentials &&
                (this._config.credentials = (function(t) {
                  if (!t) return new bu()
                  switch (t.type) {
                    case 'gapi':
                      return new Su(t.client, t.sessionIndex || '0')
                    case 'provider':
                      return t.client
                    default:
                      throw new rr(
                        nr.INVALID_ARGUMENT,
                        'makeCredentialsProvider failed due to invalid credential type',
                      )
                  }
                })(e.credentials))
          }),
          (t.prototype.enableNetwork = function() {
            return (
              this.ensureClientConfigured(),
              this._firestoreClient.enableNetwork()
            )
          }),
          (t.prototype.disableNetwork = function() {
            return (
              this.ensureClientConfigured(),
              this._firestoreClient.disableNetwork()
            )
          }),
          (t.prototype.enablePersistence = function(t) {
            if (this._firestoreClient)
              throw new rr(
                nr.FAILED_PRECONDITION,
                'Firestore has already been started and persistence can no longer be enabled. You can only call enablePersistence() before calling any other methods on a Firestore object.',
              )
            return this.configureClient(new Xu(!0, t))
          }),
          (t.prototype.ensureClientConfigured = function() {
            return (
              this._firestoreClient || this.configureClient(new Xu(!1)),
              this._firestoreClient
            )
          }),
          (t.prototype.configureClient = function(t) {
            var e = this
            Zn(
              !!this._config.settings.host,
              'FirestoreSettings.host cannot be falsey',
            ),
              this._config.settings.timestampsInSnapshots ||
                Yn(
                  "\nThe behavior for Date objects stored in Firestore is going to change\nAND YOUR APP MAY BREAK.\nTo hide this warning and ensure your app does not break, you need to add the\nfollowing code to your app before calling any other Cloud Firestore methods:\n\n  const firestore = firebase.firestore();\n  const settings = {/* your settings... */ timestampsInSnapshots: true};\n  firestore.settings(settings);\n\nWith this change, timestamps stored in Cloud Firestore will be read back as\nFirebase Timestamp objects instead of as system Date objects. So you will also\nneed to update code expecting a Date to instead expect a Timestamp. For example:\n\n  // Old:\n  const date = snapshot.get('created_at');\n  // New:\n  const timestamp = snapshot.get('created_at');\n  const date = timestamp.toDate();\n\nPlease audit all existing usages of Date when you enable the new behavior. In a\nfuture release, the behavior will change to the new behavior, so if you do not\nfollow these steps, YOUR APP MAY BREAK.",
                ),
              Zn(
                !this._firestoreClient,
                'configureClient() called multiple times',
              )
            var n = new Fr(
              this._config.databaseId,
              this._config.persistenceKey,
              this._config.settings.host,
              this._config.settings.ssl,
            )
            return (
              (this._dataConverter = new qu(function(t) {
                if (t instanceof Zu) {
                  var n = e._config.databaseId,
                    r = t.firestore._config.databaseId
                  if (!r.isEqual(n))
                    throw new rr(
                      nr.INVALID_ARGUMENT,
                      'Document reference is for database ' +
                        r.projectId +
                        '/' +
                        r.database +
                        ' but should be for database ' +
                        n.projectId +
                        '/' +
                        n.database,
                    )
                  return new xu(e._config.databaseId, t._key)
                }
                return t
              })),
              (this._firestoreClient = new pu(
                tr.getPlatform(),
                n,
                this._config.credentials,
                this._queue,
              )),
              this._firestoreClient.start(t)
            )
          }),
          (t.databaseIdFromApp = function(t) {
            var e = t.options
            if (!or(e, 'projectId'))
              throw new rr(
                nr.INVALID_ARGUMENT,
                '"projectId" not provided in firebase.initializeApp.',
              )
            var n = e.projectId
            if (!n || 'string' != typeof n)
              throw new rr(
                nr.INVALID_ARGUMENT,
                'projectId must be a string in FirebaseApp.options',
              )
            return new Br(n)
          }),
          Object.defineProperty(t.prototype, 'app', {
            get: function() {
              if (!this._config.firebaseApp)
                throw new rr(
                  nr.FAILED_PRECONDITION,
                  "Firestore was not initialized using the Firebase SDK. 'app' is not available",
                )
              return this._config.firebaseApp
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.collection = function(t) {
            if (
              (hr('Firestore.collection', arguments, 1),
              dr('Firestore.collection', 'non-empty string', 1, t),
              !t)
            )
              throw new rr(
                nr.INVALID_ARGUMENT,
                'Must provide a non-empty collection path to collection()',
              )
            return this.ensureClientConfigured(), new oc(Qr.fromString(t), this)
          }),
          (t.prototype.doc = function(t) {
            if (
              (hr('Firestore.doc', arguments, 1),
              dr('Firestore.doc', 'non-empty string', 1, t),
              !t)
            )
              throw new rr(
                nr.INVALID_ARGUMENT,
                'Must provide a non-empty document path to doc()',
              )
            return (
              this.ensureClientConfigured(), Zu.forPath(Qr.fromString(t), this)
            )
          }),
          (t.prototype.runTransaction = function(t) {
            var e = this
            return (
              hr('Firestore.runTransaction', arguments, 1),
              dr('Firestore.runTransaction', 'function', 1, t),
              this.ensureClientConfigured().transaction(function(n) {
                return t(new Ju(e, n))
              })
            )
          }),
          (t.prototype.batch = function() {
            return this.ensureClientConfigured(), new $u(this)
          }),
          Object.defineProperty(t, 'logLevel', {
            get: function() {
              switch (zn()) {
                case qn.DEBUG:
                  return 'debug'
                case qn.ERROR:
                  return 'error'
                case qn.SILENT:
                  return 'silent'
                default:
                  return $n('Unknown log level: ' + zn())
              }
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.setLogLevel = function(t) {
            switch (
              (hr('Firestore.setLogLevel', arguments, 1),
              dr('Firestore.setLogLevel', 'non-empty string', 1, t),
              t)
            ) {
              case 'debug':
                Hn(qn.DEBUG)
                break
              case 'error':
                Hn(qn.ERROR)
                break
              case 'silent':
                Hn(qn.SILENT)
                break
              default:
                throw new rr(nr.INVALID_ARGUMENT, 'Invalid log level: ' + t)
            }
          }),
          (t.prototype._areTimestampsInSnapshotsEnabled = function() {
            return this._config.settings.timestampsInSnapshots
          }),
          t
        )
      })(),
      Ju = (function() {
        function t(t, e) {
          ;(this._firestore = t), (this._transaction = e)
        }
        return (
          (t.prototype.get = function(t) {
            var e = this
            hr('Transaction.get', arguments, 1)
            var n = cc('Transaction.get', t, this._firestore)
            return this._transaction.lookup([n._key]).then(function(t) {
              if (!t || 1 !== t.length)
                return $n('Mismatch in docs returned from document lookup.')
              var r = t[0]
              if (r instanceof Hr)
                return new ec(e._firestore, n._key, null, !1, !1)
              if (r instanceof zr)
                return new ec(e._firestore, n._key, r, !1, !1)
              throw $n(
                'BatchGetDocumentsRequest returned unexpected document type: ' +
                  r.constructor.name,
              )
            })
          }),
          (t.prototype.set = function(t, e, n) {
            fr('Transaction.set', arguments, 2, 3)
            var r = cc('Transaction.set', t, this._firestore),
              i =
                (n = sc('Transaction.set', n)).merge || n.mergeFields
                  ? this._firestore._dataConverter.parseMergeData(
                      'Transaction.set',
                      e,
                      n.mergeFields,
                    )
                  : this._firestore._dataConverter.parseSetData(
                      'Transaction.set',
                      e,
                    )
            return this._transaction.set(r._key, i), this
          }),
          (t.prototype.update = function(t, e, n) {
            for (var r, i, o = [], s = 3; s < arguments.length; s++)
              o[s - 3] = arguments[s]
            return (
              'string' == typeof e || e instanceof yu
                ? (lr('Transaction.update', arguments, 3),
                  (r = cc('Transaction.update', t, this._firestore)),
                  (i = this._firestore._dataConverter.parseUpdateVarargs(
                    'Transaction.update',
                    e,
                    n,
                    o,
                  )))
                : (hr('Transaction.update', arguments, 2),
                  (r = cc('Transaction.update', t, this._firestore)),
                  (i = this._firestore._dataConverter.parseUpdateData(
                    'Transaction.update',
                    e,
                  ))),
              this._transaction.update(r._key, i),
              this
            )
          }),
          (t.prototype.delete = function(t) {
            hr('Transaction.delete', arguments, 1)
            var e = cc('Transaction.delete', t, this._firestore)
            return this._transaction.delete(e._key), this
          }),
          t
        )
      })(),
      $u = (function() {
        function t(t) {
          ;(this._firestore = t), (this._mutations = []), (this._committed = !1)
        }
        return (
          (t.prototype.set = function(t, e, n) {
            fr('WriteBatch.set', arguments, 2, 3), this.verifyNotCommitted()
            var r = cc('WriteBatch.set', t, this._firestore),
              i =
                (n = sc('WriteBatch.set', n)).merge || n.mergeFields
                  ? this._firestore._dataConverter.parseMergeData(
                      'WriteBatch.set',
                      e,
                      n.mergeFields,
                    )
                  : this._firestore._dataConverter.parseSetData(
                      'WriteBatch.set',
                      e,
                    )
            return (
              (this._mutations = this._mutations.concat(
                i.toMutations(r._key, Bi.NONE),
              )),
              this
            )
          }),
          (t.prototype.update = function(t, e, n) {
            for (var r, i, o = [], s = 3; s < arguments.length; s++)
              o[s - 3] = arguments[s]
            return (
              this.verifyNotCommitted(),
              'string' == typeof e || e instanceof yu
                ? (lr('WriteBatch.update', arguments, 3),
                  (r = cc('WriteBatch.update', t, this._firestore)),
                  (i = this._firestore._dataConverter.parseUpdateVarargs(
                    'WriteBatch.update',
                    e,
                    n,
                    o,
                  )))
                : (hr('WriteBatch.update', arguments, 2),
                  (r = cc('WriteBatch.update', t, this._firestore)),
                  (i = this._firestore._dataConverter.parseUpdateData(
                    'WriteBatch.update',
                    e,
                  ))),
              (this._mutations = this._mutations.concat(
                i.toMutations(r._key, Bi.exists(!0)),
              )),
              this
            )
          }),
          (t.prototype.delete = function(t) {
            hr('WriteBatch.delete', arguments, 1), this.verifyNotCommitted()
            var e = cc('WriteBatch.delete', t, this._firestore)
            return (
              (this._mutations = this._mutations.concat(
                new ji(e._key, Bi.NONE),
              )),
              this
            )
          }),
          (t.prototype.commit = function() {
            return u(this, void 0, void 0, function() {
              return c(this, function(t) {
                return (
                  this.verifyNotCommitted(),
                  (this._committed = !0),
                  this._mutations.length > 0
                    ? [
                        2,
                        this._firestore
                          .ensureClientConfigured()
                          .write(this._mutations),
                      ]
                    : [2]
                )
              })
            })
          }),
          (t.prototype.verifyNotCommitted = function() {
            if (this._committed)
              throw new rr(
                nr.FAILED_PRECONDITION,
                'A write batch can no longer be used after commit() has been called.',
              )
          }),
          t
        )
      })(),
      Zu = (function() {
        function t(t, e) {
          ;(this._key = t),
            (this.firestore = e),
            (this._firestoreClient = this.firestore.ensureClientConfigured())
        }
        return (
          (t.forPath = function(e, n) {
            if (e.length % 2 != 0)
              throw new rr(
                nr.INVALID_ARGUMENT,
                'Invalid document reference. Document references must have an even number of segments, but ' +
                  e.canonicalString() +
                  ' has ' +
                  e.length,
              )
            return new t(new jr(e), n)
          }),
          Object.defineProperty(t.prototype, 'id', {
            get: function() {
              return this._key.path.lastSegment()
            },
            enumerable: !0,
            configurable: !0,
          }),
          Object.defineProperty(t.prototype, 'parent', {
            get: function() {
              return new oc(this._key.path.popLast(), this.firestore)
            },
            enumerable: !0,
            configurable: !0,
          }),
          Object.defineProperty(t.prototype, 'path', {
            get: function() {
              return this._key.path.canonicalString()
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.collection = function(t) {
            if (
              (hr('DocumentReference.collection', arguments, 1),
              dr('DocumentReference.collection', 'non-empty string', 1, t),
              !t)
            )
              throw new rr(
                nr.INVALID_ARGUMENT,
                'Must provide a non-empty collection name to collection()',
              )
            var e = Qr.fromString(t)
            return new oc(this._key.path.child(e), this.firestore)
          }),
          (t.prototype.isEqual = function(e) {
            if (!(e instanceof t))
              throw Ir('isEqual', 'DocumentReference', 1, e)
            return this.firestore === e.firestore && this._key.isEqual(e._key)
          }),
          (t.prototype.set = function(t, e) {
            fr('DocumentReference.set', arguments, 1, 2)
            var n =
              (e = sc('DocumentReference.set', e)).merge || e.mergeFields
                ? this.firestore._dataConverter.parseMergeData(
                    'DocumentReference.set',
                    t,
                    e.mergeFields,
                  )
                : this.firestore._dataConverter.parseSetData(
                    'DocumentReference.set',
                    t,
                  )
            return this._firestoreClient.write(
              n.toMutations(this._key, Bi.NONE),
            )
          }),
          (t.prototype.update = function(t, e) {
            for (var n, r = [], i = 2; i < arguments.length; i++)
              r[i - 2] = arguments[i]
            return (
              'string' == typeof t || t instanceof yu
                ? (lr('DocumentReference.update', arguments, 2),
                  (n = this.firestore._dataConverter.parseUpdateVarargs(
                    'DocumentReference.update',
                    t,
                    e,
                    r,
                  )))
                : (hr('DocumentReference.update', arguments, 1),
                  (n = this.firestore._dataConverter.parseUpdateData(
                    'DocumentReference.update',
                    t,
                  ))),
              this._firestoreClient.write(
                n.toMutations(this._key, Bi.exists(!0)),
              )
            )
          }),
          (t.prototype.delete = function() {
            return (
              hr('DocumentReference.delete', arguments, 0),
              this._firestoreClient.write([new ji(this._key, Bi.NONE)])
            )
          }),
          (t.prototype.onSnapshot = function() {
            for (var t = [], e = 0; e < arguments.length; e++)
              t[e] = arguments[e]
            fr('DocumentReference.onSnapshot', arguments, 1, 4)
            var n,
              r = { includeMetadataChanges: !1 },
              i = 0
            'object' != typeof t[i] ||
              Eu(t[i]) ||
              (Er('DocumentReference.onSnapshot', (r = t[i]), [
                'includeMetadataChanges',
              ]),
              yr(
                'DocumentReference.onSnapshot',
                'boolean',
                'includeMetadataChanges',
                r.includeMetadataChanges,
              ),
              i++)
            var o = { includeMetadataChanges: r.includeMetadataChanges }
            return (
              Eu(t[i])
                ? (n = t[i])
                : (dr('DocumentReference.onSnapshot', 'function', i, t[i]),
                  pr(
                    'DocumentReference.onSnapshot',
                    'function',
                    i + 1,
                    t[i + 1],
                  ),
                  pr(
                    'DocumentReference.onSnapshot',
                    'function',
                    i + 2,
                    t[i + 2],
                  ),
                  (n = { next: t[i], error: t[i + 1], complete: t[i + 2] })),
              this.onSnapshotInternal(o, n)
            )
          }),
          (t.prototype.onSnapshotInternal = function(t, e) {
            var n = this,
              r = function(t) {
                console.error('Uncaught Error in onSnapshot:', t)
              }
            e.error && (r = e.error.bind(e))
            var i = new mu({
                next: function(t) {
                  if (e.next) {
                    Zn(
                      t.docs.size <= 1,
                      'Too many documents returned on a document query',
                    )
                    var r = t.docs.get(n._key)
                    e.next(
                      new ec(
                        n.firestore,
                        n._key,
                        r,
                        t.fromCache,
                        t.hasPendingWrites,
                      ),
                    )
                  }
                },
                error: r,
              }),
              o = this._firestoreClient.listen(Ei.atPath(this._key.path), i, t)
            return function() {
              i.mute(), n._firestoreClient.unlisten(o)
            }
          }),
          (t.prototype.get = function(t) {
            var e = this
            return (
              fr('DocumentReference.get', arguments, 0, 1),
              uc('DocumentReference.get', t),
              new Promise(function(n, r) {
                t && 'cache' === t.source
                  ? e.firestore
                      .ensureClientConfigured()
                      .getDocumentFromLocalCache(e._key)
                      .then(function(t) {
                        n(
                          new ec(
                            e.firestore,
                            e._key,
                            t,
                            !0,
                            t instanceof zr && t.hasLocalMutations,
                          ),
                        )
                      }, r)
                  : e.getViaSnapshotListener(n, r, t)
              })
            )
          }),
          (t.prototype.getViaSnapshotListener = function(t, e, n) {
            var r = this.onSnapshotInternal(
              { includeMetadataChanges: !0, waitForSyncWhenOnline: !0 },
              {
                next: function(i) {
                  r(),
                    !i.exists && i.metadata.fromCache
                      ? e(
                          new rr(
                            nr.UNAVAILABLE,
                            'Failed to get document because the client is offline.',
                          ),
                        )
                      : i.exists &&
                        i.metadata.fromCache &&
                        n &&
                        'server' === n.source
                        ? e(
                            new rr(
                              nr.UNAVAILABLE,
                              'Failed to get document from server. (However, this document does exist in the local cache. Run again without setting source to "server" to retrieve the cached document.)',
                            ),
                          )
                        : t(i)
                },
                error: e,
              },
            )
          }),
          t
        )
      })(),
      tc = (function() {
        function t(t, e) {
          ;(this.hasPendingWrites = t), (this.fromCache = e)
        }
        return (
          (t.prototype.isEqual = function(t) {
            return (
              this.hasPendingWrites === t.hasPendingWrites &&
              this.fromCache === t.fromCache
            )
          }),
          t
        )
      })(),
      ec = (function() {
        function t(t, e, n, r, i) {
          ;(this._firestore = t),
            (this._key = e),
            (this._document = n),
            (this._fromCache = r),
            (this._hasPendingWrites = i)
        }
        return (
          (t.prototype.data = function(t) {
            return (
              fr('DocumentSnapshot.data', arguments, 0, 1),
              (t = ac('DocumentSnapshot.data', t)),
              this._document
                ? this.convertObject(
                    this._document.data,
                    ti.fromSnapshotOptions(
                      t,
                      this._firestore._areTimestampsInSnapshotsEnabled(),
                    ),
                  )
                : void 0
            )
          }),
          (t.prototype.get = function(t, e) {
            if (
              (fr('DocumentSnapshot.get', arguments, 1, 2),
              (e = ac('DocumentSnapshot.get', e)),
              this._document)
            ) {
              var n = this._document.data.field(Bu('DocumentSnapshot.get', t))
              if (void 0 !== n)
                return this.convertValue(
                  n,
                  ti.fromSnapshotOptions(
                    e,
                    this._firestore._areTimestampsInSnapshotsEnabled(),
                  ),
                )
            }
          }),
          Object.defineProperty(t.prototype, 'id', {
            get: function() {
              return this._key.path.lastSegment()
            },
            enumerable: !0,
            configurable: !0,
          }),
          Object.defineProperty(t.prototype, 'ref', {
            get: function() {
              return new Zu(this._key, this._firestore)
            },
            enumerable: !0,
            configurable: !0,
          }),
          Object.defineProperty(t.prototype, 'exists', {
            get: function() {
              return null !== this._document
            },
            enumerable: !0,
            configurable: !0,
          }),
          Object.defineProperty(t.prototype, 'metadata', {
            get: function() {
              return new tc(this._hasPendingWrites, this._fromCache)
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.isEqual = function(e) {
            if (!(e instanceof t)) throw Ir('isEqual', 'DocumentSnapshot', 1, e)
            return (
              this._firestore === e._firestore &&
              this._fromCache === e._fromCache &&
              this._key.isEqual(e._key) &&
              (null === this._document
                ? null === e._document
                : this._document.isEqual(e._document))
            )
          }),
          (t.prototype.convertObject = function(t, e) {
            var n = this,
              r = {}
            return (
              t.forEach(function(t, i) {
                r[t] = n.convertValue(i, e)
              }),
              r
            )
          }),
          (t.prototype.convertValue = function(t, e) {
            if (t instanceof pi) return this.convertObject(t, e)
            if (t instanceof mi) return this.convertArray(t, e)
            if (t instanceof fi) {
              var n = t.value(e),
                r = this._firestore.ensureClientConfigured().databaseId()
              return (
                t.databaseId.isEqual(r) ||
                  Yn(
                    'Document ' +
                      this._key.path +
                      ' contains a document reference within a different database (' +
                      t.databaseId.projectId +
                      '/' +
                      t.databaseId.database +
                      ') which is not supported. It will be treated as a reference in the current database (' +
                      r.projectId +
                      '/' +
                      r.database +
                      ') instead.',
                  ),
                new Zu(n, this._firestore)
              )
            }
            return t.value(e)
          }),
          (t.prototype.convertArray = function(t, e) {
            var n = this
            return t.internalValue.map(function(t) {
              return n.convertValue(t, e)
            })
          }),
          t
        )
      })(),
      nc = (function(t) {
        function e(e, n, r, i, o) {
          return t.call(this, e, n, r, i, o) || this
        }
        return (
          a(e, t),
          (e.prototype.data = function(e) {
            var n = t.prototype.data.call(this, e)
            return (
              Zn(
                'object' == typeof n,
                'Document in a QueryDocumentSnapshot should exist',
              ),
              n
            )
          }),
          e
        )
      })(ec),
      rc = (function() {
        function t(t, e) {
          ;(this._query = t), (this.firestore = e)
        }
        return (
          (t.prototype.where = function(e, n, r) {
            var i
            hr('Query.where', arguments, 3),
              dr('Query.where', 'non-empty string', 2, n),
              Sr('Query.where', 3, r)
            var o = Bu('Query.where', e),
              s = Ci.fromString(n)
            if (o.isKeyField()) {
              if (s === Ci.ARRAY_CONTAINS)
                throw new rr(
                  nr.INVALID_ARGUMENT,
                  "Invalid Query. You can't perform array-contains queries on FieldPath.documentId() since document IDs are not arrays.",
                )
              if ('string' == typeof r) {
                if (-1 !== r.indexOf('/'))
                  throw new rr(
                    nr.INVALID_ARGUMENT,
                    'Function Query.where() requires its third parameter to be a valid document ID if the first parameter is FieldPath.documentId(), but it contains a slash.',
                  )
                if ('' === r)
                  throw new rr(
                    nr.INVALID_ARGUMENT,
                    'Function Query.where() requires its third parameter to be a valid document ID if the first parameter is FieldPath.documentId(), but it was an empty string.',
                  )
                var a = this._query.path.child(new Qr([r]))
                Zn(a.length % 2 == 0, 'Path should be a document key'),
                  (i = new fi(this.firestore._databaseId, new jr(a)))
              } else {
                if (!(r instanceof Zu))
                  throw new rr(
                    nr.INVALID_ARGUMENT,
                    'Function Query.where() requires its third parameter to be a string or a DocumentReference if the first parameter is FieldPath.documentId(), but it was: ' +
                      Tr(r) +
                      '.',
                  )
                var u = r
                i = new fi(this.firestore._databaseId, u._key)
              }
            } else
              i = this.firestore._dataConverter.parseQueryValue(
                'Query.where',
                r,
              )
            var c = Ii.create(o, s, i)
            return (
              this.validateNewFilter(c),
              new t(this._query.addFilter(c), this.firestore)
            )
          }),
          (t.prototype.orderBy = function(e, n) {
            var r
            if (
              (fr('Query.orderBy', arguments, 1, 2),
              pr('Query.orderBy', 'non-empty string', 2, n),
              void 0 === n || 'asc' === n)
            )
              r = ki.ASCENDING
            else {
              if ('desc' !== n)
                throw new rr(
                  nr.INVALID_ARGUMENT,
                  "Function Query.orderBy() has unknown direction '" +
                    n +
                    "', expected 'asc' or 'desc'.",
                )
              r = ki.DESCENDING
            }
            if (null !== this._query.startAt)
              throw new rr(
                nr.INVALID_ARGUMENT,
                'Invalid query. You must not call Query.startAt() or Query.startAfter() before calling Query.orderBy().',
              )
            if (null !== this._query.endAt)
              throw new rr(
                nr.INVALID_ARGUMENT,
                'Invalid query. You must not call Query.endAt() or Query.endBefore() before calling Query.orderBy().',
              )
            var i = Bu('Query.orderBy', e),
              o = new Mi(i, r)
            return (
              this.validateNewOrderBy(o),
              new t(this._query.addOrderBy(o), this.firestore)
            )
          }),
          (t.prototype.limit = function(e) {
            if (
              (hr('Query.limit', arguments, 1),
              dr('Query.limit', 'number', 1, e),
              e <= 0)
            )
              throw new rr(
                nr.INVALID_ARGUMENT,
                'Invalid Query. Query limit (' +
                  e +
                  ') is invalid. Limit must be positive.',
              )
            return new t(this._query.withLimit(e), this.firestore)
          }),
          (t.prototype.startAt = function(e) {
            for (var n = [], r = 1; r < arguments.length; r++)
              n[r - 1] = arguments[r]
            lr('Query.startAt', arguments, 1)
            var i = this.boundFromDocOrFields('Query.startAt', e, n, !0)
            return new t(this._query.withStartAt(i), this.firestore)
          }),
          (t.prototype.startAfter = function(e) {
            for (var n = [], r = 1; r < arguments.length; r++)
              n[r - 1] = arguments[r]
            lr('Query.startAfter', arguments, 1)
            var i = this.boundFromDocOrFields('Query.startAfter', e, n, !1)
            return new t(this._query.withStartAt(i), this.firestore)
          }),
          (t.prototype.endBefore = function(e) {
            for (var n = [], r = 1; r < arguments.length; r++)
              n[r - 1] = arguments[r]
            lr('Query.endBefore', arguments, 1)
            var i = this.boundFromDocOrFields('Query.endBefore', e, n, !0)
            return new t(this._query.withEndAt(i), this.firestore)
          }),
          (t.prototype.endAt = function(e) {
            for (var n = [], r = 1; r < arguments.length; r++)
              n[r - 1] = arguments[r]
            lr('Query.endAt', arguments, 1)
            var i = this.boundFromDocOrFields('Query.endAt', e, n, !1)
            return new t(this._query.withEndAt(i), this.firestore)
          }),
          (t.prototype.isEqual = function(e) {
            if (!(e instanceof t)) throw Ir('isEqual', 'Query', 1, e)
            return (
              this.firestore === e.firestore && this._query.isEqual(e._query)
            )
          }),
          (t.prototype.boundFromDocOrFields = function(t, e, n, r) {
            if ((Sr(t, 1, e), e instanceof ec)) {
              if (n.length > 0)
                throw new rr(
                  nr.INVALID_ARGUMENT,
                  'Too many arguments provided to ' + t + '().',
                )
              var i = e
              if (!i.exists)
                throw new rr(
                  nr.NOT_FOUND,
                  "Can't use a DocumentSnapshot that doesn't exist for " +
                    t +
                    '().',
                )
              return this.boundFromDocument(t, i._document, r)
            }
            var o = [e].concat(n)
            return this.boundFromFields(t, o, r)
          }),
          (t.prototype.boundFromDocument = function(t, e, n) {
            for (
              var r = [], i = 0, o = this._query.orderBy;
              i < o.length;
              i++
            ) {
              var s = o[i]
              if (s.field.isKeyField())
                r.push(new fi(this.firestore._databaseId, e.key))
              else {
                var a = e.field(s.field)
                if (void 0 === a) {
                  var u = s.field.canonicalString()
                  throw new rr(
                    nr.INVALID_ARGUMENT,
                    "Invalid query. You are trying to start or end a query using a document for which the field '" +
                      u +
                      "' (used as the orderBy) does not exist.",
                  )
                }
                r.push(a)
              }
            }
            return new Ri(r, n)
          }),
          (t.prototype.boundFromFields = function(t, e, n) {
            var r = this._query.explicitOrderBy
            if (e.length > r.length)
              throw new rr(
                nr.INVALID_ARGUMENT,
                'Too many arguments provided to ' +
                  t +
                  '(). The number of arguments must be less than or equal to the number of Query.orderBy() clauses',
              )
            for (var i = [], o = 0; o < e.length; o++) {
              var s = e[o]
              if (r[o].field.isKeyField()) {
                if ('string' != typeof s)
                  throw new rr(
                    nr.INVALID_ARGUMENT,
                    'Invalid query. Expected a string for document ID in ' +
                      t +
                      '(), but got a ' +
                      typeof s,
                  )
                if (-1 !== s.indexOf('/'))
                  throw new rr(
                    nr.INVALID_ARGUMENT,
                    "Invalid query. Document ID '" +
                      s +
                      "' contains a slash in " +
                      t +
                      '()',
                  )
                var a = new jr(this._query.path.child(s))
                i.push(new fi(this.firestore._databaseId, a))
              } else {
                var u = this.firestore._dataConverter.parseQueryValue(t, s)
                i.push(u)
              }
            }
            return new Ri(i, n)
          }),
          (t.prototype.onSnapshot = function() {
            for (var t = [], e = 0; e < arguments.length; e++)
              t[e] = arguments[e]
            fr('Query.onSnapshot', arguments, 1, 4)
            var n,
              r = {},
              i = 0
            return (
              'object' != typeof t[i] ||
                Eu(t[i]) ||
                (Er('Query.onSnapshot', (r = t[i]), ['includeMetadataChanges']),
                yr(
                  'Query.onSnapshot',
                  'boolean',
                  'includeMetadataChanges',
                  r.includeMetadataChanges,
                ),
                i++),
              Eu(t[i])
                ? (n = t[i])
                : (dr('Query.onSnapshot', 'function', i, t[i]),
                  pr('Query.onSnapshot', 'function', i + 1, t[i + 1]),
                  pr('Query.onSnapshot', 'function', i + 2, t[i + 2]),
                  (n = { next: t[i], error: t[i + 1], complete: t[i + 2] })),
              this.onSnapshotInternal(r, n)
            )
          }),
          (t.prototype.onSnapshotInternal = function(t, e) {
            var n = this,
              r = function(t) {
                console.error('Uncaught Error in onSnapshot:', t)
              }
            e.error && (r = e.error.bind(e))
            var i = new mu({
                next: function(t) {
                  e.next && e.next(new ic(n.firestore, n._query, t))
                },
                error: r,
              }),
              o = this.firestore.ensureClientConfigured(),
              s = o.listen(this._query, i, t)
            return function() {
              i.mute(), o.unlisten(s)
            }
          }),
          (t.prototype.get = function(t) {
            var e = this
            return (
              fr('Query.get', arguments, 0, 1),
              uc('Query.get', t),
              new Promise(function(n, r) {
                t && 'cache' === t.source
                  ? e.firestore
                      .ensureClientConfigured()
                      .getDocumentsFromLocalCache(e._query)
                      .then(function(t) {
                        n(new ic(e.firestore, e._query, t))
                      }, r)
                  : e.getViaSnapshotListener(n, r, t)
              })
            )
          }),
          (t.prototype.getViaSnapshotListener = function(t, e, n) {
            var r = this.onSnapshotInternal(
              { includeMetadataChanges: !0, waitForSyncWhenOnline: !0 },
              {
                next: function(i) {
                  r(),
                    i.metadata.fromCache && n && 'server' === n.source
                      ? e(
                          new rr(
                            nr.UNAVAILABLE,
                            'Failed to get documents from server. (However, these documents may exist in the local cache. Run again without setting source to "server" to retrieve the cached documents.)',
                          ),
                        )
                      : t(i)
                },
                error: e,
              },
            )
          }),
          (t.prototype.validateNewFilter = function(t) {
            if (t instanceof Di)
              if (t.isInequality()) {
                var e = this._query.getInequalityFilterField()
                if (null !== e && !e.isEqual(t.field))
                  throw new rr(
                    nr.INVALID_ARGUMENT,
                    "Invalid query. All where filters with an inequality (<, <=, >, or >=) must be on the same field. But you have inequality filters on '" +
                      e.toString() +
                      "' and '" +
                      t.field.toString() +
                      "'",
                  )
                var n = this._query.getFirstOrderByField()
                null !== n && this.validateOrderByAndInequalityMatch(t.field, n)
              } else if (
                t.op === Ci.ARRAY_CONTAINS &&
                this._query.hasArrayContainsFilter()
              )
                throw new rr(
                  nr.INVALID_ARGUMENT,
                  'Invalid query. Queries only support a single array-contains filter.',
                )
          }),
          (t.prototype.validateNewOrderBy = function(t) {
            if (null === this._query.getFirstOrderByField()) {
              var e = this._query.getInequalityFilterField()
              null !== e && this.validateOrderByAndInequalityMatch(e, t.field)
            }
          }),
          (t.prototype.validateOrderByAndInequalityMatch = function(t, e) {
            if (!e.isEqual(t))
              throw new rr(
                nr.INVALID_ARGUMENT,
                "Invalid query. You have a where filter with an inequality (<, <=, >, or >=) on field '" +
                  t.toString() +
                  "' and so you must also use '" +
                  t.toString() +
                  "' as your first Query.orderBy(), but your first Query.orderBy() is on field '" +
                  e.toString() +
                  "' instead.",
              )
          }),
          t
        )
      })(),
      ic = (function() {
        function t(t, e, n) {
          ;(this._firestore = t),
            (this._originalQuery = e),
            (this._snapshot = n),
            (this._cachedChanges = null),
            (this._cachedChangesIncludeMetadataChanges = null),
            (this.metadata = new tc(n.hasPendingWrites, n.fromCache))
        }
        return (
          Object.defineProperty(t.prototype, 'docs', {
            get: function() {
              var t = []
              return (
                this.forEach(function(e) {
                  return t.push(e)
                }),
                t
              )
            },
            enumerable: !0,
            configurable: !0,
          }),
          Object.defineProperty(t.prototype, 'empty', {
            get: function() {
              return this._snapshot.docs.isEmpty()
            },
            enumerable: !0,
            configurable: !0,
          }),
          Object.defineProperty(t.prototype, 'size', {
            get: function() {
              return this._snapshot.docs.size
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.forEach = function(t, e) {
            var n = this
            fr('QuerySnapshot.forEach', arguments, 1, 2),
              dr('QuerySnapshot.forEach', 'function', 1, t),
              this._snapshot.docs.forEach(function(r) {
                t.call(e, n.convertToDocumentImpl(r))
              })
          }),
          Object.defineProperty(t.prototype, 'query', {
            get: function() {
              return new rc(this._originalQuery, this._firestore)
            },
            enumerable: !0,
            configurable: !0,
          }),
          (t.prototype.docChanges = function(t) {
            t &&
              (Er('QuerySnapshot.docChanges', t, ['includeMetadataChanges']),
              yr(
                'QuerySnapshot.docChanges',
                'boolean',
                'includeMetadataChanges',
                t.includeMetadataChanges,
              ))
            var e = !(!t || !t.includeMetadataChanges)
            if (e && this._snapshot.excludesMetadataChanges)
              throw new rr(
                nr.INVALID_ARGUMENT,
                'To include metadata changes with your document changes, you must also pass { includeMetadataChanges:true } to onSnapshot().',
              )
            return (
              (this._cachedChanges &&
                this._cachedChangesIncludeMetadataChanges === e) ||
                ((this._cachedChanges = (function(t, e, n) {
                  if (n.oldDocs.isEmpty()) {
                    var r,
                      i = 0
                    return n.docChanges.map(function(e) {
                      var o = new nc(
                        t,
                        e.doc.key,
                        e.doc,
                        n.fromCache,
                        n.mutatedKeys.has(e.doc.key),
                      )
                      return (
                        Zn(
                          e.type === lo.Added,
                          'Invalid event type for first snapshot',
                        ),
                        Zn(
                          !r || n.query.docComparator(r, e.doc) < 0,
                          'Got added events in wrong order',
                        ),
                        (r = e.doc),
                        { type: 'added', doc: o, oldIndex: -1, newIndex: i++ }
                      )
                    })
                  }
                  var o = n.oldDocs
                  return n.docChanges
                    .filter(function(t) {
                      return e || t.type !== lo.Metadata
                    })
                    .map(function(e) {
                      var r = new nc(
                          t,
                          e.doc.key,
                          e.doc,
                          n.fromCache,
                          n.mutatedKeys.has(e.doc.key),
                        ),
                        i = -1,
                        s = -1
                      return (
                        e.type !== lo.Added &&
                          (Zn(
                            (i = o.indexOf(e.doc.key)) >= 0,
                            'Index for document not found',
                          ),
                          (o = o.delete(e.doc.key))),
                        e.type !== lo.Removed &&
                          ((o = o.add(e.doc)), (s = o.indexOf(e.doc.key))),
                        {
                          type: (function(t) {
                            switch (t) {
                              case lo.Added:
                                return 'added'
                              case lo.Modified:
                              case lo.Metadata:
                                return 'modified'
                              case lo.Removed:
                                return 'removed'
                              default:
                                return $n('Unknown change type: ' + t)
                            }
                          })(e.type),
                          doc: r,
                          oldIndex: i,
                          newIndex: s,
                        }
                      )
                    })
                })(this._firestore, e, this._snapshot)),
                (this._cachedChangesIncludeMetadataChanges = e)),
              this._cachedChanges
            )
          }),
          (t.prototype.isEqual = function(e) {
            if (!(e instanceof t)) throw Ir('isEqual', 'QuerySnapshot', 1, e)
            return (
              this._firestore === e._firestore &&
              this._originalQuery.isEqual(e._originalQuery) &&
              this._snapshot.isEqual(e._snapshot)
            )
          }),
          (t.prototype.convertToDocumentImpl = function(t) {
            return new nc(
              this._firestore,
              t.key,
              t,
              this.metadata.fromCache,
              this._snapshot.mutatedKeys.has(t.key),
            )
          }),
          t
        )
      })()
    ;['length', 'forEach', 'map']
      .concat('undefined' != typeof Symbol ? [Symbol.iterator] : [])
      .forEach(function(t) {
        try {
          Object.defineProperty(ic.prototype.docChanges, t, {
            get: function() {
              return (function() {
                throw new rr(
                  nr.INVALID_ARGUMENT,
                  'QuerySnapshot.docChanges has been changed from a property into a method, so usages like "querySnapshot.docChanges" should become "querySnapshot.docChanges()"',
                )
              })()
            },
          })
        } catch (t) {}
      })
    var oc = (function(t) {
      function e(e, n) {
        var r = t.call(this, Ei.atPath(e), n) || this
        if (e.length % 2 != 1)
          throw new rr(
            nr.INVALID_ARGUMENT,
            'Invalid collection reference. Collection references must have an odd number of segments, but ' +
              e.canonicalString() +
              ' has ' +
              e.length,
          )
        return r
      }
      return (
        a(e, t),
        Object.defineProperty(e.prototype, 'id', {
          get: function() {
            return this._query.path.lastSegment()
          },
          enumerable: !0,
          configurable: !0,
        }),
        Object.defineProperty(e.prototype, 'parent', {
          get: function() {
            var t = this._query.path.popLast()
            return t.isEmpty() ? null : new Zu(new jr(t), this.firestore)
          },
          enumerable: !0,
          configurable: !0,
        }),
        Object.defineProperty(e.prototype, 'path', {
          get: function() {
            return this._query.path.canonicalString()
          },
          enumerable: !0,
          configurable: !0,
        }),
        (e.prototype.doc = function(t) {
          if (
            (fr('CollectionReference.doc', arguments, 0, 1),
            0 === arguments.length && (t = Nr.newId()),
            dr('CollectionReference.doc', 'non-empty string', 1, t),
            '' === t)
          )
            throw new rr(
              nr.INVALID_ARGUMENT,
              'Document path must be a non-empty string',
            )
          var e = Qr.fromString(t)
          return Zu.forPath(this._query.path.child(e), this.firestore)
        }),
        (e.prototype.add = function(t) {
          hr('CollectionReference.add', arguments, 1),
            dr('CollectionReference.add', 'object', 1, t)
          var e = this.doc()
          return e.set(t).then(function() {
            return e
          })
        }),
        e
      )
    })(rc)
    function sc(t, e) {
      if (void 0 === e) return { merge: !1 }
      if (
        (Er(t, e, ['merge', 'mergeFields']),
        yr(t, 'boolean', 'merge', e.merge),
        gr(t, 'mergeFields', 'a string or a FieldPath', e.mergeFields, function(
          t,
        ) {
          return 'string' == typeof t || t instanceof yu
        }),
        void 0 !== e.mergeFields && void 0 !== e.merge)
      )
        throw new rr(
          nr.INVALID_ARGUMENT,
          'Invalid options passed to function ' +
            t +
            '(): You cannot specify both "merge" and "mergeFields".',
        )
      return e
    }
    function ac(t, e) {
      return void 0 === e
        ? {}
        : (Er(t, e, ['serverTimestamps']),
          vr(t, 0, 'serverTimestamps', e.serverTimestamps, [
            'estimate',
            'previous',
            'none',
          ]),
          e)
    }
    function uc(t, e) {
      pr(t, 'object', 1, e),
        e &&
          (Er(t, e, ['source']),
          vr(t, 0, 'source', e.source, ['default', 'server', 'cache']))
    }
    function cc(t, e, n) {
      if (e instanceof Zu) {
        if (e.firestore !== n)
          throw new rr(
            nr.INVALID_ARGUMENT,
            'Provided document reference is from a different Firestore instance.',
          )
        return e
      }
      throw Ir(t, 'DocumentReference', 1, e)
    }
    var hc = ir(Yu, 'Use firebase.firestore() instead.'),
      lc = ir(Ju, 'Use firebase.firestore().runTransaction() instead.'),
      fc = ir($u, 'Use firebase.firestore().batch() instead.'),
      dc = ir(Zu, 'Use firebase.firestore().doc() instead.'),
      pc = ir(ec),
      mc = ir(nc),
      yc = ir(rc),
      gc = ir(ic),
      vc = ir(oc, 'Use firebase.firestore().collection() instead.'),
      bc = {
        Firestore: hc,
        GeoPoint: xr,
        Timestamp: qr,
        Blob: Lr,
        Transaction: lc,
        WriteBatch: fc,
        DocumentReference: dc,
        DocumentSnapshot: pc,
        Query: yc,
        QueryDocumentSnapshot: mc,
        QuerySnapshot: gc,
        CollectionReference: vc,
        FieldPath: yu,
        FieldValue: Ru,
        setLogLevel: Yu.setLogLevel,
      }
    function wc(t) {
      t.INTERNAL.registerService(
        'firestore',
        function(t) {
          return new Yu(t)
        },
        (function(t) {
          Zn(
            t && 'object' == typeof t,
            'shallowCopy() expects object parameter.',
          )
          var e = {}
          for (var n in t)
            Object.prototype.hasOwnProperty.call(t, n) && (e[n] = t[n])
          return e
        })(bc),
      )
    }
    wc(e)
  })((this.firebase = this.firebase || {}), firebase)
} catch (t) {
  throw (console.error(t),
  new Error(
    'Cannot instantiate firebase-firestore - be sure to load firebase-app.js first.',
  ))
}
//# sourceMappingURL=firebase-firestore.js.map
