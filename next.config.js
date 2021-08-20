const fs = require('fs')

const withPlugins = require('next-compose-plugins')

const d3packages = fs.readdirSync('node_modules').filter((name) => name.startsWith('d3-'))
const withTM = require('next-transpile-modules')(d3packages)

module.exports = withPlugins([withTM], {
  distDir: 'build',
})
