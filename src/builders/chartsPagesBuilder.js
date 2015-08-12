'use strict';
// external dependencies
var fs = require('fs');
var Q = require('q');
var Handlebars = require('handlebars');
// local dependencies
var helpers = require('../helpers/generalHelpers');
var log = require('../helpers/simpleLogger');
var CONSTANTS = require('../helpers/constants');
//  vars
var readJsonFromDisk = helpers.readJsonFromDisk;
var parseRecipie = helpers.parseRecipie;
var getAllFileNamesFrom = helpers.getAllFileNamesFrom;
var ROOT_DIR = CONSTANTS.ROOT_DIR;
var CHARTS_DIR_PATH = CONSTANTS.CHARTS_DIR_PATH;
var RECIPES_DIR_PATH = CONSTANTS.RECIPES_DIR_PATH;
var indexFilePath = ROOT_DIR + '/charts/index.html';

function handleError(err) {
    console.log(err.stack);
}

function getFullChartJSONPath(fileName) {
    return CHARTS_DIR_PATH + fileName;
}

function createAllJSONPaths(allNames) {
    return allNames.map(getFullChartJSONPath);
}

function getNameWithData(filePath) {
    return readJsonFromDisk(filePath).then(function(data) {
        return {
            filePath: filePath,
            data: data
        };
    });
}

function getAllJSONNamesWithData(allPaths) {
    return Q.all(allPaths.map(getNameWithData));
}


// writes text to a file
function writeToFile(filePath, text) {
    fs.writeFile(filePath, text, function() {
        log.created('Index page', filePath);
    });
}
// build the index.html page
function buildAllChartsPages(allTemplates) {
    getAllFileNamesFrom(CHARTS_DIR_PATH)
    .then(createAllJSONPaths)
        .then(getAllJSONNamesWithData)
        .then(console.log);
}
buildAllChartsPages();

module.exports = buildAllChartsPages;
