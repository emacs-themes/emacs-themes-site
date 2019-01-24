const fs = require('fs');
const path = require('path');
const Handlebars = require('handlebars');
const helpers = require('../helpers/generalHelpers');
const log = require('../helpers/simpleLogger');
const CONSTANTS = require('../helpers/constants');
const readJsonFromDisk = helpers.readJsonFromDisk;
const ROOT_DIR = CONSTANTS.ROOT_DIR;
const indexFilePath = ROOT_DIR + 'popular/index.html';
const DATA_SRC = path.join(__dirname, '../popular/cache.json');

function writeToFile(filePath, text) {
  fs.writeFile(filePath, text, function() {
    log.created('Index page', filePath);
  });
}

function createIndexPage(data, allTemplates) {
  const template = Handlebars.compile(allTemplates['popular']);

  writeToFile(indexFilePath, template({ themes: data }));
}

// build the popular/index.html page
async function buildPopularPage(allTemplates) {
  try {
    const data = await readJsonFromDisk(DATA_SRC);

    createIndexPage(data, allTemplates);
  } catch (err) {
    console.log(err);
  }
}

module.exports = buildPopularPage;
