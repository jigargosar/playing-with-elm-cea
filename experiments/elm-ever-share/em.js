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
    console.log(JSON.parse(e.stderr))
  }
}

boot(process.argv.slice(2)).catch(e => console.error('Boot ERROR', e))
