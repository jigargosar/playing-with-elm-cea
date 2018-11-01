const inquirer = require('inquirer')
const ora = require('ora')

inquirer.registerPrompt('search-checkbox', require('inquirer-search-checkbox'))

const spinner = ora({ text: 'Loading unicorns' }).start()

setTimeout(() => {
  // spinner.color = 'yellow';
  // spinner.text = 'Loading rainbows';
  spinner.stop()
  startInquirer().catch(console.error)
}, 1000)

async function startInquirer() {
  return inquirer
    .prompt([
      {
        type: 'search-checkbox',
        message: 'Select toppings',
        name: 'toppings',
        choices: [
          {
            name: 'Pepperoni',
          },
          {
            name: 'Ham',
          },
          {
            name: 'Ground Meat',
          },
          {
            name: 'Bacon',
          },
          {
            name: 'Mozzarella',
          },
        ],
        validate: function(answer) {
          if (answer.length < 1) {
            return 'You must choose at least one topping.'
          }
          return true
        },
      },
    ])
    .then(function(answers) {
      console.log(JSON.stringify(answers, null, '  '))
    })
    .catch(e => console.log(e))
}
