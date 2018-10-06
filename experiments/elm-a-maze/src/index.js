import 'tachyons'
import './main.css'
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'

let documentElement = document.documentElement
const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
    vw: Math.max(documentElement.clientWidth, window.innerWidth || 0),
    vh: Math.max(documentElement.clientHeight, window.innerHeight || 0),
  },
})

function onWindowBlur() {
  app.ports.onWindowBlur.send(null)
}

window.addEventListener('blur', onWindowBlur)

registerServiceWorker()

// requestAnimationFrame(onAF)
//
// function onAF() {
//   // new Array(1000).fill(1000).map(x=>x)
//   app.ports.onAnimationFrame.send(null)
//   requestAnimationFrame(onAF)
// }
