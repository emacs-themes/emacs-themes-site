// external dependencies
var fs = require('fs');
var Q = require('q');
var Handlebars = require('handlebars');
// local dependencies
var log = require('../helpers/simpleLogger');
// config vars
var rootDir = __dirname + '/../../root/';
// Build the tags
function buildTagsIndex(allTemplates, allRecipes) {
    var allTags = {};
    var arr = [];
    var htmlPath = rootDir + 'tags/index.html';
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
