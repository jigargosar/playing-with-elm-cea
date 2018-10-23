import 'tachyons'
import './main.css'
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'
import {
  forEachObjIndexed,
  ifElse,
  isNil,
  partial,
  partialRight,
  pick,
} from 'ramda'
import pathOr from 'ramda/es/pathOr'
import invoker from 'ramda/es/invoker'

var myWorker = new Worker('/worker.js', {})

myWorker.postMessage('foo')

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
    todos: storageGetOr({}, 'todos'),
  },
})

window.addEventListener('wheel', function(e) {
  // console.log(e)
  let data = pick(['deltaX', 'deltaY'])(e)
  // console.log(data)
  send(data, 'wheel', app)
})

subscribe(
  {
    logS: data => {
      console.warn(data)
    },
    warn: data => {
      console.warn(data)
    },
    cacheTodoC: todos => {
      storageSet('todos', todos)
    },
    focus,
  },
  app,
)

registerServiceWorker()

// Helpers

function send(data, port, app) {
  if (!pathOr(null, ['ports', port, 'send'])(app)) {
    console.error('send port not found', port, 'data ignored', data)
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
    app.ports[sub].subscribe(data => fn(data, partialRight(send, [app])))
  })(options)
}

function storageGetOr(or, key) {
  try {
    return JSON.parse(localStorage.getItem(key))
  } catch (e) {
    return or
  }
}

function storageSet(key, value) {
  if (isNil(value) || isNil(key)) {
    console.warn('Invalid Args', 'storageSet', key, value)
    return
  }
  localStorage.setItem(key, JSON.stringify(value))
}

function focus(selector) {
  ifElse(isNil)(
    partial(console.warn, ['[focus] selector', selector, 'not found']),
  )(invoker(0, 'focus'))(document.querySelector(selector))
}
