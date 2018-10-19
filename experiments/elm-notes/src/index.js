import 'tachyons'
import './main.css'
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'
import {
  compose,
  forEachObjIndexed,
  fromPairs,
  identity,
  isNil,
  map,
  pathOr,
  pick,
  prop,
  unless,
} from 'ramda'
import {
  auth,
  firestore,
  onAuthStateChanged,
  signIn,
  signOut,
  userCRef,
} from './fire'
import loki from 'lokijs'

const db = new loki('DB', {
  verbose: true,
  autosave: true,
  env: 'BROWSER',
  /*autoload: true, autoloadCallback: function () {
    console.log('db.serialize()', db.serialize())
  },*/
})
// const users = db.addCollection('users', { indices: ['email'] })
//
// var odin = users.insert( { name : 'odin', email: 'odin.soap@lokijs.org', age: 38 } );
// var thor = users.insert( { name : 'thor', email : 'thor.soap@lokijs.org', age: 25 } );
// var stan = users.insert( { name : 'stan', email : 'stan.soap@lokijs.org', age: 29 } );
// var oliver = users.insert( { name : 'oliver', email : 'oliver.soap@lokijs.org', age: 31 } );
// var hector = users.insert( { name : 'hector', email : 'hector.soap@lokijs.org', age: 15} );
// var achilles = users.insert( { name : 'achilles', email : 'achilles.soap@lokijs.org', age: 31 } );
//
// stan.name = 'Stan Laurel';
// update object (this really only syncs the index)
// users.update(stan);

db.loadJSON(localStorage.getItem('DB') || '')
console.log('db.serialize()', db.serialize())

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
    noteList: storageGetOr({}, 'noteCollection'),
  },
})

registerServiceWorker()

// subscribe(
//   'persistNoteCollection',
//   nc => {
//     storageSet('noteCollection', nc)
//     let user = auth().currentUser
//     if (user) {
//       const batch = firestore().batch()
//       const cRef = userCRef(user.uid, elmNotesCollectionName)
//       forEachObjIndexed(note => {
//         batch.set(cRef.doc(note.id), note)
//       })(nc)
//       batch.commit().catch(console.error)
//     }
//   },
//   app,
// )
//
subscribe(
  'persistNote',
  note => {
    let user = auth().currentUser
    if (user) {
      const batch = firestore().batch()
      const cRef = userCRef(user.uid, elmNotesCollectionName)
      batch.set(cRef.doc(note.id), note)
      batch.commit().catch(console.error)
    }
  },
  app,
)

subscribe(
  'cacheNotesCollection',
  nc => {
    storageSet('noteCollection', nc)
  },
  app,
)

subscribe('signIn', signIn, app)
subscribe('signOut', signOut, app)
let elmNotesListener = identity
onAuthStateChanged(authStateChangeHandler)

function authStateChangeHandler(user) {
  if (user) {
    if (process.env.NODE_ENV !== 'production') {
      global.user = user
    }
    elmNotesListener()
    const cRef = userCRef(user.uid, elmNotesCollectionName)
    elmNotesListener = cRef.onSnapshot(qSnap => {
      let qSnapToAllDocsDict = compose(
        fromPairs,
        map(d => [d.id, d.data()]),
        prop('docs'),
      )
      let docsDict = qSnapToAllDocsDict(qSnap)
      // console.log(docsDict)
      send('notesCollectionChanged', docsDict, app)
    })
  }
  send(
    'authStateChanged',
    unless(isNil)(
      pick([
        'uid',
        'email',
        'displayName',
        'photoURL',
        'isAnonymous',
        'metadata',
        'phoneNumber',
        'providerData',
        'emailVerified',
        'refreshToken',
      ]),
    )(user),
    app,
  )
}

// HELPER FUNCTIONS

const elmNotesCollectionName = `elm-notes`

function subscribe(port, fn, app) {
  if (pathOr(null, ['ports', port, 'subscribe'])(app)) {
    app.ports[port].subscribe(fn)
  } else {
    console.error('Port Not Found', port, app.ports)
  }
}

function send(port, data, app) {
  if (app.ports && app.ports[port]) {
    app.ports[port].send(data)
  } else {
    console.error('Port Not Found', port, app.ports)
  }
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
  } catch (e) {
    console.log(e)
  }
}
