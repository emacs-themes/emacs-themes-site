'use strict';
// external dependencies
var fs = require('fs');
var Handlebars = require('handlebars');
// local dependencies
var log = require('../helpers/simpleLogger');
var CONSTANTS = require('../helpers/constants');
// config vars
var ROOT_DIR = CONSTANTS.ROOT_DIR;
var THEMES_PER_PAGE = CONSTANTS.THEMES_PER_PAGE;

// builds individual index theme page
function buildThemePage(obj) {
    var data = [];
    var allPages = [];
    var page = {};
    var filePath;
    var theme;
    var recipie;
    var i;

    for (i = obj.startIndex; i < obj.endIndex; i++) {
        recipie = obj.allRecipes[i];
        theme = {};
        theme.title = recipie.name.spacedValue;
        theme.img = '.' + recipie.smallImg;
        theme.link = '../themes/' + recipie.name.hyphenedValue + '.html';
        data.push(theme);
    }
    for (i = 0; i < obj.pageLimit; i++) {
        allPages.push(i + 1);
    }
    page.themes = data;
    allPages[obj.currentPage] = {
        current: true,
        number: (obj.currentPage + 1)
    };
    page.pages = allPages;
    filePath = ROOT_DIR + 'index/' + (obj.currentPage + 1) + '.html';

    fs.writeFile(filePath, obj.template(page), function() {
        log.created('Theme index page', filePath);
    });
}
// returns the total number of theme pages
function calculateNumberOfPages(allRecipes) {
    return Math.ceil(allRecipes.length / THEMES_PER_PAGE);
}
// returns the upper limit for the pages index
function getHighLimit(lowLimit, recipesNumber) {
    var limit = lowLimit + THEMES_PER_PAGE;

    if (limit > recipesNumber) {
        return recipesNumber;
    }
    return limit;
}
// builds theme index page
function buildThemeIndex(allTemplates, allRecipes) {
    var template = Handlebars.compile(allTemplates['theme-index']);
    var numberOfPages = calculateNumberOfPages(allRecipes);
    var recipesNumber = allRecipes.length;
    var lowLimit;
    var highLimit;
    var i;

    for (i = 0; i < numberOfPages; i++) {
        lowLimit = i * THEMES_PER_PAGE;
        highLimit = getHighLimit(lowLimit, recipesNumber);
        buildThemePage({
            allRecipes: allRecipes,
            startIndex: lowLimit,
            endIndex: highLimit,
            currentPage: i,
            pageLimit: numberOfPages,
            template: template
        });
    }
}

module.exports = buildThemeIndex;
