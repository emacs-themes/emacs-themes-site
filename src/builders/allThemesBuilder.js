'use strict';
// external dependencies
var fs = require('fs');
var Handlebars = require('handlebars');
// local dependencies
var log = require('../helpers/simpleLogger');
var CONSTANTS = require('../helpers/constants');
// config vars
var THEMES_DIR_PATH = CONSTANTS.THEMES_DIR_PATH;
// build an individual HTML theme page
function writeThemeToFile(info, template) {
    var htmlPath = THEMES_DIR_PATH + info.name.hyphenedValue + '.html';
    console.log(htmlPath);
    fs.writeFile(htmlPath, template(info), function() {
        log.built(info.name.spacedValue);
    });
}
// build all Themes as HTML pages
function buildAllThemes(allTemplates, allRecipes) {
    var template = Handlebars.compile(allTemplates['theme']);
    allRecipes.forEach(function(info) {
        writeThemeToFile(info, template);
    });
}

module.exports = buildAllThemes;
