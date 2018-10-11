import 'tachyons'
import './main.css'
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'
import { forEachObjIndexed, identity, isNil, map, pick, unless } from 'ramda'
import { auth, firestore, signIn, signOut, userCRef } from './fire'

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
  nc => {
    storageSet('noteCollection', nc)
    let user = auth().currentUser
    if (user) {
      const batch = firestore().batch()
      const cRef = userCRef(user.uid, elmNotesCollectionName)
      forEachObjIndexed(note => {
        batch.set(cRef.doc(note.id), note)
      })(nc)
      batch.commit().catch(console.error)
    }
  },
  app,
)

subscribe('signIn', signIn, app)
subscribe('signOut', signOut, app)
let elmNotesListener = identity
auth().onAuthStateChanged(function (user) {
  if (user) {
    elmNotesListener()
    const cRef = userCRef(user.uid, elmNotesCollectionName)
    elmNotesListener = cRef.onSnapshot(qSnap => {
      console.log(map(d => d.data())(qSnap.docs))

    })
  }
  app.ports.sessionChanged.send(
    unless(isNil)(pick(['uid', 'email', 'displayName']))(user),
  )

})

// HELPER FUNCTIONS

const elmNotesCollectionName = `elm-notes`

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
