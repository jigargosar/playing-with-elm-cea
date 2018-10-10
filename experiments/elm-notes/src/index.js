import 'tachyons'
import './main.css'
import { Elm } from './App.elm'
import registerServiceWorker from './registerServiceWorker'

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
  },
})

registerServiceWorker()

