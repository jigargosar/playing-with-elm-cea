import 'tachyons'
import './main.css'
// noinspection ES6CheckImport
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'
import { compose, curry, forEachObjIndexed, isNil, map, partial, pathOr, pick, tap, toPairs } from 'ramda'

import flyd from 'flyd'
import Popper from 'popper.js'

// console.log = (partial(console.log, ['[LogWrapper]']))

// Helpers
export const tapLog = m => tap(partial(console.log, [m]))

// tapLog("foo")("bar","james")
// console.log("foo","bar","james")

const sendTo = curry(function sendTo(app, port, data) {
  if (!pathOr(null, ['ports', port, 'send'])(app)) {
    console.error('sendTo port not found', port, 'data ignored', data)
    return
  }
  app.ports[port].send(data)
})

function subscribe(options, app) {
  if (!app || !app.ports) {
    console.error('no ports found', app)
    return
  }

  forEachObjIndexed((fn, sub) => {
    if (!pathOr(null, ['ports', sub, 'subscribe'])(app)) {
      console.error('sub port not found', sub)
      return
    }
    // noinspection JSIgnoredPromiseFromCall, JSCheckFunctionSignatures
    app.ports[sub].subscribe(data => fn(data, sendTo(app)))
  })(options)
}

function storageGetOr(defaultValue, key) {
  try {
    let item = localStorage.getItem(key)
    if (isNil(item)) return defaultValue
    return JSON.parse(item)
  } catch (e) {
    return defaultValue
  }
}

function storageSet(key, value) {
  if (isNil(value) || isNil(key)) {
    console.warn('Invalid Args', 'storageSet', key, value)
    return
  }
  localStorage.setItem(key, JSON.stringify(value))
}

// function focusSelector(selector) {
//   ifElse(isNil)(
//     partial(console.warn, ['[focus] selector', selector, 'not found']),
//   )(invoker(0, 'focus'))(document.querySelector(selector))
// }

// App

let initialTodos = storageGetOr({}, 'todos')
let initialContexts = storageGetOr({}, 'contexts')
// console.log("initialTodos",initialTodos)

// noinspection JSUnresolvedVariable
const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
    todos: initialTodos,
    contexts: initialContexts,
  },
})

const sendToApp = curry(function sendToApp(port, data) {
  return sendTo(app)(port)(data)
})

console.log(`sendToApp.name`, sendToApp.name)
console.log(app.ports)

// let documentHasFocus = document.hasFocus

// const winFocus$ = flyd.stream()
// const winBlur$  = flyd.stream()

const winFocusBlur$ = flyd.stream()

winFocusBlur$(document.hasFocus())

window.addEventListener('focus', winFocusBlur$)
window.addEventListener('blur', winFocusBlur$)

const documentHasFocus$ = flyd.map(
  compose(() => document.hasFocus()),
  winFocusBlur$,
)

flyd.on(
  compose(
    sendToApp('documentFocusChanged') /*, tapLog('documentFocusChanged')*/,
  ),
  documentHasFocus$,
)

window.addEventListener('wheel', function (e) {
  // console.log(e)
  const data = pick(['deltaX', 'deltaY'])(e)
  // sendTo(app, 'wheel', data)
})

let popper = null

subscribe(
  {
    warn: data => {
      console.warn(data)
    },
    cacheTodoStore: todos => {
      storageSet('todos', todos)
    },
    cacheContextStore: contexts => {
      storageSet('contexts', contexts)
    },
    // focusSelector,
    destroyPopper: ([refDomId, popperDomId]) => {
      console.log('destroyPopper', [refDomId, popperDomId])
      if (popper && (popper.reference.id === refDomId && popper.popper.id === popperDomId)) {
        popper.destroy()
        popper = null
      }
    },
    createPopper: ([refDomId, popperDomId]) => {
      console.log('createPopper', [refDomId, popperDomId])
      if (popper) {
        popper.destroy()
        popper = null
      }
      let showPopper = () => {
        const refEl = document.getElementById(refDomId)
        const popEl = document.getElementById(popperDomId)
        if (!refEl || !popEl) {
          debugger
          return
        }
        let toStringPairs = compose(toPairs, map(val => `${val}`))
        popper = new Popper(refEl, popEl, {

          modifiers: {
            applyStyle: {
              enabled: false,
            },
            preventOverflow: {
              // boundariesElement: document.getElementById('popper-container'),
              boundariesElement: 'viewport',
            },
          },

          // flip: {
          // enabled: false,
          // behavior: ['right']
          // },
          // placement: 'right-start',
          // positionFixed:true,
          onCreate(data) {

            let portData = { styles: toStringPairs(data.styles), attributes: toStringPairs(data.attributes) }
            console.log(`onCreate data`, data, portData)
            sendToApp('popperStylesSet', portData)
          },
          onUpdate(data) {
            let portData = { styles: toStringPairs(data.styles), attributes: toStringPairs(data.attributes) }
            // console.log(`onUpdate data`, data, portData)
            sendToApp('popperStylesChanged', portData)
          },
        })
      }
      requestAnimationFrame(showPopper)
      // showPopper()
    },
  },
  app,
)

registerServiceWorker()
