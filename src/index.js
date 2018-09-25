import 'tachyons'
import './main.css'
// import './shadow.scss'
// import '@material/elevation/mdc-elevation.scss'
import '@material/elevation/dist/mdc.elevation.css'
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'
//</editor-fold>

Elm.Main.init({
  node: document.getElementById('root'),
})

//<editor-fold desc="Description">
registerServiceWorker()
