'use strict';
// external dependencies
var fs = require('fs');
var Q = require('q');
var Handlebars = require('handlebars');
// local dependencies
var readJsonFromDisk = require('../helpers/generalHelpers').readJsonFromDisk;
var parseRecipie = require('../helpers/generalHelpers').parseRecipie;
var log = require('../helpers/simpleLogger');
var CONSTANTS = require('../helpers/constants');
// config vars
var ROOT_DIR = CONSTANTS.ROOT_DIR;
var CONFIG_JSON_PATH = CONSTANTS.CONFIG_JSON_PATH;
var RECIPES_DIR_PATH = CONSTANTS.RECIPES_DIR_PATH;
var outputFilePath = ROOT_DIR + 'index.html';

// reads an array of names from the disk returning an array of promises
function readAllRecipesFromDisk(namesArr) {
    return namesArr.map(function(name) {
        var filePath = RECIPES_DIR_PATH + name + '.json';

        return readJsonFromDisk(filePath);
    });
}
// returns a promise
function parseAllRecipes(promisesArr) {
    return promisesArr.map(function(promise) {
        return promise.then(function(recipie) {
            return parseRecipie(recipie);
        });
    });
}
// writes text to a file
function writeToFile(filePath, text) {
    fs.writeFile(filePath, text, function() {
        log.created('Index page', filePath);
    });
}
// build the index.html page
function buildMainPage(allTemplates) {
    Q.all(readJsonFromDisk(CONFIG_JSON_PATH)
        .then(readAllRecipesFromDisk)
        .then(parseAllRecipes))
        .then(function(recipes) {
            var template = Handlebars.compile(allTemplates.main);
            writeToFile(outputFilePath, template({themes: recipes}));
        });
}

module.exports = buildMainPage;
