import 'tachyons'
import './main.css'
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'
import { isNil } from 'ramda'

if (module.hot) {
  try {
    module.hot.addStatusHandler(status => {
      console.log(status)
      if (status === 'idle') {
        console.clear()
        console.log('HMR Idle')
      }
    })
  } catch (e) {console.log(e) }
}

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
    noteList: storageGetOr({}, 'noteCollection'),
  },
})

registerServiceWorker()

subscribe(
  'persistNoteCollection',
  nc => storageSet('noteCollection', nc),
  app)

function subscribe(port, fn, app) {
  app.ports[port].subscribe(fn)
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

