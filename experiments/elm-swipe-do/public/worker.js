importScripts('firebase-app.js')
importScripts('firebase-auth.js')
importScripts('firebase-firestore.js')
importScripts('ramda.min.js')

console.log('worker')

const fire = firebase

function getOrCreateFirebaseApp() {
  const config = {
    apiKey: 'AIzaSyAve3E-llOy2_ly87mJMSvcWDG6Uqyq8PA',
    authDomain: 'not-now-142808.firebaseapp.com',
    databaseURL: 'https://not-now-142808.firebaseio.com',
    projectId: 'not-now-142808',
    storageBucket: 'not-now-142808.appspot.com',
    messagingSenderId: '476064436883',
  }

  function createApp() {
    const app = fire.initializeApp(config)
    const firestore = firebase.firestore()
    firestore.settings({
      timestampsInSnapshots: true,
    })
    // firestore
    //   .enablePersistence({ experimentalTabSynchronization: false })
    //   .catch(console.error)
    return app
  }

  return fire.apps[0] || createApp()
}
const auth = () => getOrCreateFirebaseApp().auth()
const firestore = () => getOrCreateFirebaseApp().firestore()
const userCRef = (uid, cName) => {
  return firestore().collection(`/users/${uid}/${cName}`)
}

const signOut = () => {
  const promise = getOrCreateFirebaseApp()
    .auth()
    .signOut()
  return promise.catch(e => console.warn(e))
}

const signIn = () => {
  const auth = getOrCreateFirebaseApp().auth()
  const googleAuthProvider = new firebase.auth.GoogleAuthProvider()
  googleAuthProvider.setCustomParameters({
    prompt: 'select_account',
  })
  // return auth.signInWithRedirect(googleAuthProvider)
  return auth.signInWithPopup(googleAuthProvider)
}

function onAuthStateChanged(cb) {
  return auth().onAuthStateChanged(
    R.tryCatch(cb, R.partial(console.error, ['[fire] onAuthStateChanged'])),
  )
}

onAuthStateChanged(function(user) {
  console.log('user', user)
})

// signIn()
