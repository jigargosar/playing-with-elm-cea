import './main.css'
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'

let documentElement = document.documentElement
Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
    vw: Math.max(documentElement.clientWidth, window.innerWidth || 0),
    vh: Math.max(documentElement.clientHeight, window.innerHeight || 0),
  },
})

registerServiceWorker()
