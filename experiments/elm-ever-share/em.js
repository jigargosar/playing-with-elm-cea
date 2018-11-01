const R = require('ramda')

const { isEmpty } = require('ramda')
const inquirer = require('inquirer')
const ora = require('ora')
const got = require('got')
const { SubProcess, exec } = require('teen_process')
const pSeries = require('p-series')

const boot = async () => {
  try {
    const result = await exec('elm', [
      'make',
      '--report=json',
      '--output=/dev/null',
      'src/Main.elm',
    ])
  } catch (e) {
    let jsonErr = JSON.parse(e.stderr)
    if (jsonErr.title === 'UNKNOWN IMPORT') {
      let importModule = jsonErr.message[1].string.split(' ')[1]
      console.log(importModule)
    }
  }
}

boot(process.argv.slice(2)).catch(e => console.error('Boot ERROR', e))
