import 'tachyons'
import './main.css'
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
  },
})

registerServiceWorker()

subscribe(
  'persistNoteCollection',
  nc => localStorage.setItem('noteCollection', JSON.stringify(nc)),
  app)

function subscribe(port, fn, app) {
  app.ports[port].subscribe(fn)
}
