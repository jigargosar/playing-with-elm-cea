import 'tachyons'
import './main.css'
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'
import { forEachObjIndexed, partialRight, pick } from 'ramda'
import pathOr from 'ramda/es/pathOr'

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
  },
})

window.addEventListener('wheel', function(e) {
  // console.log(e)
  let data = pick(['deltaX', 'deltaY'])(e)
  // console.log(data)
  send(data, 'wheel', app)
})

registerServiceWorker()

// Helpers

function send(data, port, app) {
  if (!pathOr(null, ['ports', port, 'send'])(app)) {
    console.error('send port not found', port, 'data ignored', data)
    return
  }
  app.ports[port].send(data)
}

function subscribe(options, app) {
  if (!app || !app.ports) {
    console.error('no ports found', app)
    return
  }

  forEachObjIndexed((fn, sub) => {
    if (!pathOr(null, ['ports', sub, 'subscribe'])(app)) {
      console.error('sub port not found', sub)
      return
    }
    app.ports[sub].subscribe(fn(partialRight(send, [app])))
  })(options)
}
