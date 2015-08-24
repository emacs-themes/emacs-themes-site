'use strict';
// local dependencies
var readAndParseRecipes = require('./recipesParser');
var generateDirs = require('./generators/dirsGenerator');
var generateTemplates = require('./generators/templatesGenerator');
var buildMainPage = require('./builders/mainPageBuilder');
var buildThemeIndex = require('./builders/themeIndexBuilder');
var buildAllThemes = require('./builders/allThemesBuilder');
var buildAllTags = require('./builders/allTagsBuilder');
var buildTagsIndex = require('./builders/tagIndexBuilder');
var buildCharts = require('./builders/chartsBuilder');
// config vars
var recipesDir = __dirname + '/../recipes/';

function buildAllPages(allTemplates, allRecipes) {
    buildMainPage(allTemplates);
    buildAllThemes(allTemplates, allRecipes);
    buildThemeIndex(allTemplates, allRecipes);
    buildAllTags(allTemplates, allRecipes);
    buildTagsIndex(allTemplates, allRecipes);
    buildCharts(allTemplates);
}

function buildSite() {
    readAndParseRecipes(recipesDir)
        .then(function(allRecipes) {
            generateDirs()
                .then(generateTemplates)
                .then(function(allTemplates) {
                    buildAllPages(allTemplates, allRecipes);
                });
        }).done();
}

module.exports = buildSite;
