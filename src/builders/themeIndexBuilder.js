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

// sets the data for the HTML title and description
function setPageHeadData(obj, currentPage, limit) {
    obj.pageNumber = currentPage + 1;
    obj.allPagesNumber = limit;
}
// builds individual index theme page
function buildThemePage(allRecipes, startIndex, endIndex, currentPage, pageLimit, template) {
    var data = [];
    var allPages = [];
    var page = {};
    var filePath;
    var theme;
    var recipie;
    var i;

    for (i = startIndex; i < endIndex; i++) {
        recipie = allRecipes[i];
        theme = {};
        theme.title = recipie.name.spacedValue;
        theme.img = '.' + recipie.smallImg;
        theme.link = '../themes/' + recipie.name.hyphenedValue + '.html';
        data.push(theme);
    }
    for (i = 0; i < pageLimit; i++) {
        allPages.push(i + 1);
    }
    page.themes = data;
    allPages[currentPage] = {
        current: true,
        number: (currentPage + 1)
    };
    page.pages = allPages;
    filePath = ROOT_DIR + 'index/' + (currentPage + 1) + '.html';

    setPageHeadData(page, currentPage, pageLimit);

    fs.writeFile(filePath, template(page), function() {
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
        buildThemePage(allRecipes, lowLimit, highLimit, i, numberOfPages, template);
    }
}

module.exports = buildThemeIndex;
