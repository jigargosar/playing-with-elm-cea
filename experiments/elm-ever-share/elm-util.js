const ora = require('ora')
const got = require('got')

module.exports.fetchElmSearchJSON = async function fetchElmSearchJSON() {
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
