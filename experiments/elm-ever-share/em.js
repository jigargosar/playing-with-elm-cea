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

const elmMakeToUnknownModuleName = async () => {
  const spinner = ora({ text: 'Elm Make' }).start()
  try {
    await exec('elm', [
      'make',
      '--report=json',
      '--output=/dev/null',
      'src/Main.elm',
    ])
    return null
  } catch (e) {
    let jsonErr = JSON.parse(e.stderr)
    if (jsonErr.title === 'UNKNOWN IMPORT') {
      let importModule = jsonErr.message[1].string.split(' ')[1]
      console.log('Unknown Import', importModule)
      return importModule
    }
  } finally {
    spinner.stop()
  }
}

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

function findPackageWithExposedModule(moduleName, elmJSONResults) {
  return R.compose(
    R.map(R.pick(['name', 'exposed-modules'])),
    R.filter(ejs => ejs['exposed-modules'].includes(moduleName)),
  )(elmJSONResults)
}

async function findPackagesWithModule(searchJson, moduleName) {
  const fuse = new Fuse(
    searchJson.map(mi => R.assoc('repoName')(mi.name.split('/')[1])(mi)),
    {
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
        'repoName',
        // 'summary',
      ],
    },
  )

  // const pattern = decamelize(moduleName, '-')
  // console.log(pattern)
  const searchResults = R.take(3)(fuse.search(moduleName))
  console.log(searchResults)

  const elmJSONResults = await pMap(
    searchResults,
    sr => fetchPackageElmJson(sr.item),
    { concurrency: 3 },
  )

  // console.log(elmJSONResults)

  const matchingModules = findPackageWithExposedModule(
    moduleName,
    elmJSONResults,
  )
  // console.log(matchingModules)
  return matchingModules
}

async function boot() {
  const searchJson = await fetchElmSearchJSON()
  const matchingModules = await findPackagesWithModule(
    searchJson,
    await elmMakeToUnknownModuleName(),
  )
  console.log(matchingModules)
  if (matchingModules.length === 1) {
    const matchingModuleName = matchingModules[0].name
    console.log('Installing Module', matchingModuleName)
    await exec('node', ['ei', matchingModuleName])
  }
}

boot(process.argv.slice(2)).catch(e => console.error('Boot ERROR', e))
