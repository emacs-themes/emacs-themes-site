'use strict';
// external dependencies
var fs = require('fs');
var Q = require('q');
var Handlebars = require('handlebars');
// local dependencies
var readJsonFromDisk = require('../helpers/generalHelpers').readJsonFromDisk;
var parseRecipie = require('../helpers/generalHelpers').parseRecipie;
var log = require('../helpers/simpleLogger');
// config vars
var rootDir = __dirname + '/../../root/';
var configJson = __dirname + '/../config/main.json';
var recipesDir = __dirname + '/../../recipes/';
var outputFilePath = rootDir + 'index.html';

// reads an array of names from the disk returning an array of promises
function readAllRecipesFromDisk(namesArr) {
    return namesArr.map(function(name) {
        var filePath = recipesDir + name + '.json';

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
    Q.all(readJsonFromDisk(configJson)
        .then(readAllRecipesFromDisk)
        .then(parseAllRecipes))
        .then(function(recipes) {
            var template = Handlebars.compile(allTemplates.main);
            writeToFile(outputFilePath, template({themes: recipes}));
        });
}

module.exports = buildMainPage;
