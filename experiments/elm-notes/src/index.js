import 'tachyons'
import './main.css'
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'
import { isNil, pick, unless } from 'ramda'
import { getOrCreateFirebaseApp } from './fire'

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
  app,
)

const auth = getOrCreateFirebaseApp().auth()

auth.onAuthStateChanged(function (user) {
  console.log(user)
  console.log(app.ports.sessionChanged)
  app.ports.sessionChanged.send(unless(isNil)(pick(['']))(user))
})

// HELPER FUNCTIONS

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

if (module.hot) {
  try {
    module.hot.addStatusHandler(status => {
      // console.log(status)
      if (status === 'idle') {
        console.clear()
        console.log('HMR Idle')
      }
    })
  } catch (e) {console.log(e) }
}
