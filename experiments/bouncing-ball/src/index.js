import './main.css'
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'

const cache = JSON.parse(localStorage.getItem("cache"))
// console.log("cache",cache)
const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags:{ cache}
});

app.ports.cache.subscribe((cache)=>{
  localStorage.setItem("cache", JSON.stringify(cache))
})



registerServiceWorker();
