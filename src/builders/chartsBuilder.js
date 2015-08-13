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
var getAllFileNamesFrom = helpers.getAllFileNamesFrom;
var hyphanize = helpers.hyphanize;
var ROOT_DIR = CONSTANTS.ROOT_DIR;
var CHARTS_DIR_PATH = CONSTANTS.CHARTS_DIR_PATH;
var indexFilePath = ROOT_DIR + 'charts/index.html';


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
    return readJsonFromDisk(filePath);
}

function getAllJSONNamesWithData(allPaths) {
    return Q.all(allPaths.map(getNameWithData));
}

function transformPathWithDataForIndex(data) {
    var url = hyphanize(data.date).toLowerCase() + '.html';
    var text = 'Charts for ' + data.date;
    return {
        url: url,
        text: text
    };
}

function parseDataForIndex(allData) {
    return allData.map(transformPathWithDataForIndex).reverse();
}

function transformPathWithDataForCharts(data) {
    var filePath = ROOT_DIR + 'charts/' +
        hyphanize(data.date).toLowerCase() + '.html';
    var date = data.date;
    var themes = data.themes;

    return {
        filePath: filePath,
        date: date,
        themes: themes
    };
}

function parseDataForCharts(allData) {
    return allData.map(transformPathWithDataForCharts);
}

// writes text to a file
function writeToFile(filePath, text) {
    fs.writeFile(filePath, text, function() {
        log.created('Index page', filePath);
    });
}

function createIndexPage(allPathsWithData, allTemplates) {
    var data = parseDataForIndex(allPathsWithData);
    var template = Handlebars.compile(allTemplates['charts-index']);

    writeToFile(indexFilePath, template({charts: data}));
}

function createAllPages(allData, allTemplates) {
    var template = Handlebars.compile(allTemplates.chart);
    var newData = parseDataForCharts(allData);

    newData.forEach(function(data) {
        writeToFile(data.filePath, template(data));
    });
}

// build the index.html page
function buildAllChartsPages(allTemplates) {
    getAllFileNamesFrom(CHARTS_DIR_PATH)
    .then(createAllJSONPaths)
    .then(getAllJSONNamesWithData)
    .then(function(allData) {
        createIndexPage(allData, allTemplates);
        createAllPages(allData, allTemplates);
    });
}

module.exports = buildAllChartsPages;
