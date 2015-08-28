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
function buildTagsIndex(allTemplates) {
    var htmlPath = ROOT_DIR + 'pages/about.html';
    var template = Handlebars.compile(allTemplates['about']);

    fs.writeFile(htmlPath, template({}), function() {
        log.created('About page', htmlPath);
    });
}

module.exports = buildTagsIndex;
