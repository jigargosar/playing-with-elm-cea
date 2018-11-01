const { isEmpty } = require('ramda')
const inquirer = require('inquirer')
const ora = require('ora')
const got = require('got')
const { SubProcess } = require('teen_process')
const pSeries = require('p-series')

async function elmInstall(packageName) {
  console.log('Installing ', packageName)
  let subProcess = new SubProcess('elm', ['install', packageName])
  subProcess.on('stream-line', console.log)
  await subProcess.start(0)
  subProcess.proc.stdin.write('\n')
  await subProcess.join()
  return packageName
}

inquirer.registerPrompt('search-checkbox', require('inquirer-search-checkbox'))

async function fetchElmSearchJSON() {
  const spinner = ora({ text: 'Fetching Elm Package Index' }).start()
  try {
    const response = await got('package.elm-lang.org/search.json', {
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

async function installPackages(elmPackages) {
  const result = await pSeries(
    elmPackages.map(packageName => () => elmInstall(packageName)),
  )
  console.log('Successfully Installed', result)
}

const boot = async packageNames => {
  if (isEmpty(packageNames)) {
    const searchJSON = await fetchElmSearchJSON()
    const answers = await inquirer.prompt([
      {
        type: 'search-checkbox',
        message: 'Select Elm Packages to Install',
        name: 'elmPackages',
        choices: searchJSON,
        validate: function(answer) {
          if (answer.length < 1) {
            return 'You must choose at least one topping.'
          }
          return true
        },
      },
      {
        type: 'confirm',
        message: 'Proceed with install',
        name: 'installConfirmed',
      },
    ])
    console.log('answers', answers)

    if (answers.installConfirmed) {
      await installPackages(answers.elmPackages)
      // await Promise.all(answers.elmPackages.map(elmInstall))
    }
  } else {
    await installPackages(packageNames)
  }
}

boot(process.argv.slice(2)).catch(console.error)
