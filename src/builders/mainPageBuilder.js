'use strict';
// external dependencies
var fs = require('fs');
var Q = require('q');
var Handlebars = require('handlebars');
// local dependencies
var log = require('../helpers/simpleLogger');
var CONSTANTS = require('../helpers/constants');
var helpers = require('../helpers/generalHelpers')
var readJsonFromDisk = helpers.readJsonFromDisk;
var parseRecipie = helpers.parseRecipie;
var getAllFileNamesFrom = helpers.getAllFileNamesFrom;
// config vars
var ROOT_DIR = CONSTANTS.ROOT_DIR;
var RECIPES_DIR_PATH = CONSTANTS.RECIPES_DIR_PATH;
var THEMES_PER_PAGE = 2 * CONSTANTS.THEMES_PER_PAGE;
var outputFilePath = ROOT_DIR + 'index.html';

function handleError(err) {
    console.log(err.stack);
}

function createFullRecipePath(recipeName) {
    return RECIPES_DIR_PATH + recipeName;
}

function createAllFullRecipesPaths(allNames) {
    return allNames.map(createFullRecipePath);
}

function getRecipeNameWithDate(filePath) {
    return Q.nfcall(fs.stat, filePath)
       .then(function(stats) {
           return {
               filePath: filePath,
               modifiedTime: stats.mtime
           };
       }, handleError);
}

function getAllRecipesNamesWithDates(allPaths) {
    return Q.all(allPaths.map(getRecipeNameWithDate));
}

function sortRecipeNamesWithDatesByDate(recipesWithDates) {
    return recipesWithDates.sort(function(a, b) {
        return (b.modifiedTime.getTime() - a.modifiedTime.getTime());
    });
}

function getLatestModifiedRecipesFilePaths(allRecipesWithDate) {
    return allRecipesWithDate.slice(0, THEMES_PER_PAGE)
        .map(function(recipeWithDate) {
            return recipeWithDate.filePath;
        });
}
// reads an array of names from the disk returning an array of promises
function readRecipesFromDisk(namesArr) {
    return namesArr.map(function(filePath) {
        return readJsonFromDisk(filePath);
    });
}

function parseAllRecipes(promisesArr) {
    return Q.all(promisesArr.map(function(promise) {
        return promise.then(function(recipie) {
            return parseRecipie(recipie);
        });
    }));
}
// writes text to a file
function writeToFile(filePath, text) {
    fs.writeFile(filePath, text, function() {
        log.created('Index page', filePath);
    });
}
// build the index.html page
function buildMainPage(allTemplates) {
    getAllFileNamesFrom(RECIPES_DIR_PATH)
        .then(createAllFullRecipesPaths)
        .then(getAllRecipesNamesWithDates)
        .then(sortRecipeNamesWithDatesByDate)
        .then(getLatestModifiedRecipesFilePaths)
        .then(readRecipesFromDisk)
        .then(parseAllRecipes)
        .then(function(recipes) {
            var template = Handlebars.compile(allTemplates.main);
            writeToFile(outputFilePath, template({themes: recipes}));
        });
}

module.exports = buildMainPage;
