'use strict';
// local dependencies
const path = require('path');
const readAndParseRecipes = require('./../helpers/recipesParser');
const generateDirs = require('./../generators/dirsGenerator');
const generateTemplates = require('./../generators/templatesGenerator');
const buildMainPage = require('./../builders/mainPageBuilder');
const buildThemeIndex = require('./../builders/themeIndexBuilder');
const buildThemeSearch = require('./../builders/themeSearchBuilder');
const buildAllThemes = require('./../builders/allThemesBuilder');
const buildAllTags = require('./../builders/allTagsBuilder');
const buildTagsIndex = require('./../builders/tagIndexBuilder');
const buildAboutPage = require('./../builders/aboutPageBuilder');
const buildPopularPage = require('./../builders/popularBuilder');
// config const
const recipesDir = path.join(__dirname, '/../../recipes/');

function buildAllPages(allTemplates, allRecipes) {
  buildMainPage(allTemplates);
  buildAllThemes(allTemplates, allRecipes);
  buildThemeIndex(allTemplates, allRecipes);
  buildThemeSearch(allTemplates, allRecipes);
  buildAllTags(allTemplates, allRecipes);
  buildTagsIndex(allTemplates, allRecipes);
  buildAboutPage(allTemplates);
  buildPopularPage(allTemplates);
}

function buildSite() {
  readAndParseRecipes(recipesDir)
    .then(function(allRecipes) {
      generateDirs()
        .then(generateTemplates)
        .then(function(allTemplates) {
          buildAllPages(allTemplates, allRecipes);
        });
    })
    .done();
}

module.exports = buildSite;
