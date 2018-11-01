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
const pMap = require('p-map')

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

async function fetchPackageElmJson(packageInfo) {
  const ejUrl = `raw.githubusercontent.com/${packageInfo.name}/${R.last(
    packageInfo.versions,
  )}/elm.json`
  console.log('URL', ejUrl)
  const spinner = ora({ text: 'Fetching elm.json from GitHub' }).start()
  try {
    const response = await got(ejUrl, {
      json: true,
    })
    return response.body
  } catch (error) {
    console.log(error.response.body)
    throw error
  } finally {
    spinner.stop()
  }
}

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
  const searchResults = R.take(3)(fuse.search(importName))
  console.log(searchResults)

  const elmJSONResults = await pMap(
    searchResults,
    sr => fetchPackageElmJson(sr.item),
    { concurrency: 3 },
  )

  console.log(elmJSONResults)
}

boot(process.argv.slice(2)).catch(e => console.error('Boot ERROR', e))
