const R = require('ramda')
const { fetchElmSearchJSON } = require('./elm-util')

const { isEmpty } = require('ramda')
const inquirer = require('inquirer')
const ora = require('ora')
const got = require('got')
const { SubProcess, exec } = require('teen_process')
const pSeries = require('p-series')
const Fuse = require('fuse.js')
const decamelize = require('decamelize')

// const boot = async () => {
//   try {
//     const result = await exec('elm', [
//       'make',
//       '--report=json',
//       '--output=/dev/null',
//       'src/Main.elm',
//     ])
//   } catch (e) {
//     let jsonErr = JSON.parse(e.stderr)
//     if (jsonErr.title === 'UNKNOWN IMPORT') {
//       let importModule = jsonErr.message[1].string.split(' ')[1]
//       console.log(importModule)
//     }
//   }
// }

async function boot() {
  const searchJson = await fetchElmSearchJSON()
  const fuse = new Fuse(searchJson, {
    shouldSort: true,
    tokenize: true,
    matchAllTokens: true,
    includeScore: true,
    // threshold: 0.6,

    location: 0,
    // distance: 100,
    // maxPatternLength: 32,
    // minMatchCharLength: 2,
    keys: [
      'name',
      // 'summary',
    ],
  })
  let importName = 'JsonCodec'

  const pattern = decamelize(importName, '-')
  console.log(pattern)
  const searchResult = fuse.search(importName)
  console.log(R.take(5)(searchResult))
}

boot(process.argv.slice(2)).catch(e => console.error('Boot ERROR', e))
