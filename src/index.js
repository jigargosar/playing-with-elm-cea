import 'tachyons'
import './main.css'
// import './shadow.scss'
// import '@material/elevation/mdc-elevation.scss'
import '@material/elevation/dist/mdc.elevation.css'
import registerServiceWorker from './registerServiceWorker'
import { Elm } from './Main.elm'
//</editor-fold>

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: { isConfigCollapsed: true },
})



console.log("app",app, Elm)

app.ports.cache.subscribe(function(data) {
  localStorage.setItem('cache', JSON.stringify(data));
});

//<editor-fold desc="Description">
registerServiceWorker()
