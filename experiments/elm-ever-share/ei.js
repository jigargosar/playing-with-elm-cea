const inquirer = require('inquirer')
const ora = require('ora')
const got = require('got')

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

const boot = async () => {
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
  ])
  console.log('answers', answers)
}

boot().catch(console.error)
