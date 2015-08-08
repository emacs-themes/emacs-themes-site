'use strict';
// external dependencies
var fs = require('fs');
var Handlebars = require('handlebars');
// local dependencies
var log = require('../helpers/simpleLogger');
// config vars
var themesDir = __dirname + '/../../root/themes/';

// build an individual HTML theme page
function writeThemeToFile(info, template) {
    var htmlPath = themesDir + info.name.hyphenedValue + '.html';
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
