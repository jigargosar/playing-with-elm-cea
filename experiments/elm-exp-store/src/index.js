import 'tachyons'
import './main.css'
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'
import {
  compose,
  forEachObjIndexed,
  ifElse,
  invoker,
  isEmpty,
  isNil,
  partial,
  partialRight,
  pathOr,
  pick,
  reject,
  unfold,
} from 'ramda'
import debounce from 'lodash.debounce'

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

console.log(app.ports)


window.addEventListener('focusout', function (e) {
  recordActiveElement()
})

window.addEventListener('focusin', function (e) {
  recordActiveElement()
})

window.addEventListener('wheel', function (e) {
  // console.log(e)
  const data = pick(['deltaX', 'deltaY'])(e)
  sendData(data, 'wheel', app)
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

// Helpers

function sendData(data, port, app) {
  if (!pathOr(null, ['ports', port, 'send'])(app)) {
    console.error('sendData port not found', port, 'data ignored', data)
    return
  }
  app.ports[port].send(data)
}

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
    app.ports[sub].subscribe(data => fn(data, partialRight(sendData, [app])))
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

export const recordActiveElement = debounce(function recordActiveElement() {
  if (lastActiveElement !== document.activeElement) {
    lastActiveElement = document.activeElement
    const parentIds = compose(reject(isEmpty), unfold(node => node ? [node.id, node.parentElement] : false),
    )(document.activeElement)

    // sendData(parentIds, 'activeElementsParentIdList', app)
  }
}, 0, { trailing: true, leading: false })
