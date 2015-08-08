'use strict';
// external dependencies
var fs = require('fs');
var Q = require('q');
var Handlebars = require('handlebars');
// local dependencies
var log = require('../helpers/simpleLogger');
// config vars
var rootDir = __dirname + '/../../root/';
var themesPerPage = 12;

// build the individual index page for a tag
function buildTagIndexPage(all, template) {
    var filePath = rootDir + 'tags/' + all.tag.hyphenedValue +
            '/' + all.currentPage + '.html';

    fs.writeFile(filePath, template(all), function() {
        log.created('Index tag page', filePath);
    });
}

function buildTagIndexPages(all, template) {
    var numberOfPages = Math.ceil(all.themes.length / themesPerPage);
    var filePath = rootDir + 'tags/' + all.tag.hyphenedValue;
    var obj = {};
    var currentPage;
    var low;
    var high;
    var i;
    var j;

    fs.mkdir(filePath, function() {
        log.created('Directory', filePath);
    });

    for (i = 0; i < numberOfPages; i++) {
        obj.pages = [];
        low = i * themesPerPage;
        high = low + themesPerPage;
        obj.themes = all.themes.slice(low, high);
        currentPage = i + 1;
        obj.currentPage = currentPage;
        obj.tag = all.tag;
        for (j = 0; j < numberOfPages; j++) {
            obj.pages.push(j + 1);
        }
        obj.pages[i] = {
            current: true,
            number: currentPage
        };
        buildTagIndexPage(obj, template);
    }
}

function buildAllTags(allTemplates, allRecipes) {
    var all = {};
    var template = Handlebars.compile(allTemplates['tag-index']);

    allRecipes.forEach(function(recipie) {
        recipie.tags.forEach(function(tag) {
            if (all[tag.hyphenedValue]) {
                all[tag.hyphenedValue].themes.push({
                    name: recipie.name,
                    img: recipie.smallImg
                });
            } else {
                all[tag.hyphenedValue] = {
                    themes: [{
                        name: recipie.name,
                        img: recipie.smallImg
                    }],
                    tag: tag
                };
            }
        });
    });

    Object.keys(all).forEach(function(key) {
        buildTagIndexPages(all[key], template);
    });
}

module.exports = buildAllTags;
