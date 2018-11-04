import 'tachyons'
import './main.css'
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'
import {
  compose,
  curry,
  forEachObjIndexed,
  ifElse,
  invoker,
  isEmpty,
  isNil,
  partial,
  pathOr,
  pick,
  reject,
  unfold,
} from 'ramda'
import debounce from 'lodash.debounce'

import flyd from 'flyd'

// Helpers

const sendTo = curry(function sendTo(app, port, data) {
    if (!pathOr(null, ['ports', port, 'send'])(app)) {
      console.error('sendTo port not found', port, 'data ignored', data)
      return
    }
    app.ports[port].send(data)
  },
)

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
    app.ports[sub].subscribe(data => fn(data, (sendTo([app]))))
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

function focusSelector(selector) {
  ifElse(isNil)(
    partial(console.warn, ['[focus] selector', selector, 'not found']),
  )(invoker(0, 'focus'))(document.querySelector(selector))
}

let lastActiveElement = document.activeElement

export const recordActiveElement = debounce(function recordActiveElement(app) {
  if (lastActiveElement !== document.activeElement) {
    lastActiveElement = document.activeElement
    const parentIds = compose(reject(isEmpty), unfold(node => node ? [node.id, node.parentElement] : false),
    )(document.activeElement)

    sendTo(app, 'activeElementsParentIdList', parentIds)
  }
}, 0, { trailing: true, leading: false })

// App

let initialTodos = storageGetOr({}, 'todos')
let initialContexts = storageGetOr({}, 'contexts')
// console.log("initialTodos",initialTodos)
const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
    todos: initialTodos,
    contexts: initialContexts,
  },
})

const sendToApp = partial(sendTo, [app])

console.log(app.ports)

// let documentHasFocus = document.hasFocus

// const winFocus$ = flyd.stream()
// const winBlur$  = flyd.stream()

const winFocusBlur$ = flyd.stream()

winFocusBlur$(document.hasFocus())

window.addEventListener('focus', winFocusBlur$)
window.addEventListener('blur', winFocusBlur$)

const documentHasFocus$ = flyd.map(compose(() => document.hasFocus()), winFocusBlur$)

flyd.on(console.warn, documentHasFocus$)
flyd.on(sendToApp('documentFocusChanged'), documentHasFocus$)

// window.addEventListener('focus', function (e) {
//   console.log("Out",document.hasFocus(), e)
//   // recordActiveElement(app)
// })
//
// window.addEventListener('blur', function (e) {
//   console.log("In",document.hasFocus(), e)
//
//   // recordActiveElement(app)
// })

// window.document.addEventListener('focusout', function (e) {
//   console.log("Out",document.hasFocus(), e)
//   // recordActiveElement(app)
// })
//
// window.document.addEventListener('focusin', function (e) {
//   console.log("In",document.hasFocus(), e)
//
//   // recordActiveElement(app)
// })

window.addEventListener('wheel', function (e) {
  // console.log(e)
  const data = pick(['deltaX', 'deltaY'])(e)
  sendTo(app, 'wheel', data)
})

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
  },
  app,
)

registerServiceWorker()

