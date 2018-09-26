import 'tachyons'
import './main.css'
// import './shadow.scss'
// import '@material/elevation/mdc-elevation.scss'
import '@material/elevation/dist/mdc.elevation.css'
import registerServiceWorker from './registerServiceWorker'
import { Elm } from './Main.elm'

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {time: Date.now(),config:JSON.parse(localStorage.getItem('config'))},
})

console.log('app', app, Elm)

app.ports.store.subscribe(function ([key, data]) {
  localStorage.setItem(key, JSON.stringify(data))
})

registerServiceWorker()
