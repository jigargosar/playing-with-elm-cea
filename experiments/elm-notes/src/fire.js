import firebase from 'firebase/app'
import 'firebase/auth'
import 'firebase/firestore'

const fire = firebase

export function getOrCreateFirebaseApp() {
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
    firestore.enablePersistence().catch(console.error)
    return app
  }

  return fire.apps[0] || createApp()
}

export const signOut = () => {
  const promise = getOrCreateFirebaseApp()
    .auth()
    .signOut()
  return promise.catch(e => console.warn(e))
}

export const signIn = () => {
  const auth = getOrCreateFirebaseApp().auth()
  const googleAuthProvider = new firebase.auth.GoogleAuthProvider()
  googleAuthProvider.setCustomParameters({
    prompt: 'select_account',
  })
  // return auth.signInWithRedirect(googleAuthProvider)
  return auth.signInWithPopup(googleAuthProvider)
}
