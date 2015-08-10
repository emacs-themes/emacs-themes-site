'use strict';
// external dependencies
var fs = require('fs');
var Handlebars = require('handlebars');
// local dependencies
var log = require('../helpers/simpleLogger');
var CONSTANTS = require('../helpers/constants');
// config vars
var ROOT_DIR = CONSTANTS.ROOT_DIR;
// Build the tags
function buildTagsIndex(allTemplates, allRecipes) {
    var allTags = {};
    var arr = [];
    var htmlPath = ROOT_DIR + 'tags/index.html';
    var template = Handlebars.compile(allTemplates['tags']);

    allRecipes.forEach(function(recipie) {
        recipie.tags.forEach(function(tag) {
            var key = tag.hyphenedValue;
            var word = tag.spacedValue;

            if (allTags[key]) {
                allTags[key].count++;
            } else {
                allTags[key] = {
                    hyphenedValue: key,
                    spacedValue: word,
                    count: 1
                };
            }
        });
    });
    Object.keys(allTags).forEach(function(tag) {
        arr.push(allTags[tag]);
    });
    fs.writeFile(htmlPath, template({tags: arr}), function() {
        log.created('Tags page', htmlPath);
    });
}

module.exports = buildTagsIndex;
