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

function getStringThemeTags(tags) {
    var tagsString = '';

    for (var i = 0; i < tags.length; i++) {
        tagsString += tags[i].spacedValue + ' ';
    }

    return tagsString;
}

// builds theme index page
function buildThemeSearch(allTemplates, allRecipes) {
    var template = Handlebars.compile(allTemplates['theme-search']);
    var filePath = ROOT_DIR + 'pages/search.html';
    var themes = [];
    var page = {};
    var theme;
    var tags = '';
    var recipe;
    var i;

    for (i = 0; i < allRecipes.length; i++) {
        recipe = allRecipes[i];
        theme = {};
        theme.title = recipe.name.spacedValue;
        theme.img = '.' + recipe.smallImg;
        theme.link = '../themes/' + recipe.name.hyphenedValue + '.html';
        theme.tags = getStringThemeTags(recipe.tags);
        themes.push(theme);
    }

    page.themes = themes;

    fs.writeFile(filePath, template(page), function() {
        log.created('Theme search page', filePath);
    });
}

module.exports = buildThemeSearch;
