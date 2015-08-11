'use strict';
// external dependencies
var fs = require('fs');
var Q = require('q');
// local dependencies
var log = require('../helpers/simpleLogger');
var CONSTANTS = require('../helpers/constants');
// constants
var TEMPLATES_SRC_PATH = CONSTANTS.TEMPLATES_SRC_PATH;
// files (these go in template data and are concatenated with the relative path)
var fileNames = {
    homeFile: 'index.html',
    allThemesFile: 'index/1.html',
    tagsFile: 'tags/index.html',
    chartsFile: 'charts/index.html',
    aboutFile: 'pages/about.html',
    cssFile: 'assets/styles/main.css'
};
// data for creating templates from partials
var allData = [
    {
        templateData: {
            name: 'main',
            title: 'A GNU Emacs Themes Gallery',
            description: 'A list of old and new Emacs Themes.',
            relativePath: ''
        },
        schema: ['header', 'main', 'footer']
    },
    {
        templateData: {
            name: 'theme',
            title: '{{ name.spacedValue }} | Emacs Themes',
            description: '{{ name.spacedValue }} for GNU Emacs.' +
                'Screenshots, source code and additional information. ' +
                '| Emacs Themes',
            relativePath: '../'
        },
        schema: ['header', 'theme', 'footer']
    },
    {
        templateData: {
            name: 'theme-index',
            title: 'Page {{ pageNumber }} | Emacs Themes',
            description: 'Page {{ pageNumber }} of {{ allPagesNumber }} Themes.' +
                ' | Emacs Themes',
            relativePath: '../'
        },
        schema: ['header', 'theme-index', 'footer']
    },
    {
        templateData: {
            name: 'tags',
            title: 'TAGS | Emacs Themes',
            description: 'A list of tags for all GNU Emacs Themes',
            relativePath: '../'
        },
        schema: ['header', 'tags', 'footer']
    },
    {
        templateData: {
            name: 'tag-index',
            title: 'Tag "{{ tag.spacedValue }}" - Page {{ currentPage }} ' +
                   '| Emacs Themes',
            description: 'Page {{ currentPage }} for the tag {{ tag.spacedValue }}.' +
                ' | Emacs Themes',
            relativePath: '../../'
        },
        schema: ['header', 'tag-index', 'footer']
    }
];
// returns a promise which resolves with the file names from partialSrcRoot Dir
function getPartialTemplatesNames() {
    return Q.nfcall(fs.readdir, TEMPLATES_SRC_PATH);
}
// transforms an array of template objects into a single object
function normalizeTemplatesText(arr) {
    var res = {};

    arr.forEach(function(templateObj) {
        res[templateObj.name] = templateObj.text;
    });

    return res;
}
// reads an parses all the partial templates
function readAndParseAllFiles(templatePaths) {
    return Q.all(
        templatePaths.map(function(template) {
            var name = template.substr(0, template.length - 4);

            return Q.nfcall(fs.readFile, TEMPLATES_SRC_PATH + '/' + template, 'utf-8')
                .then(function(text) {
                    return {
                        name: name,
                        text: text
                    };
                });
        }));
}
// creates a regexp from a string
function generateRegExpForStr(str) {
    return new RegExp(/\{\{\s*/.source + str + /\s*\}\}/.source);
}
// replaces template with data, mustache like
function replaceInTemplate(template, data) {
    Object.keys(data).forEach(function(key) {
        var regEx = generateRegExpForStr(key);

        template = template.replace(regEx, data[key]);
    });
    return template;
}
// generates a full template from an array of partial templates(schema)
function generateBasicTemplate(allPartialTemplates, schema) {
    var data = {};
    var allTemplateParts = schema.map(function(part) {
        return allPartialTemplates[part];
    });

    data.head = allPartialTemplates.head;
    data.body = allTemplateParts.reduce(function(first, second) {
        return first + second;
    }, '');

    return replaceInTemplate(allPartialTemplates.doc, data);
}
// generates all templates from partial text/templates
function generateAllTemplates(allPartialTexts) {
    var res = {};
    var allPartialTemplates = normalizeTemplatesText(allPartialTexts);

    allData.forEach(function(data) {
        var basicTemplate = generateBasicTemplate(allPartialTemplates, data.schema);

        Object.keys(fileNames).forEach(function(key) {
            data.templateData[key] = data.templateData.relativePath + fileNames[key];
        });
        res[data.templateData.name] =
            replaceInTemplate(basicTemplate, data.templateData);
    });
    log.text('All templates successfully generated');
    return res;
}
// returns a promise which resolves with an object containing all templates
function generateTemplates() {
    return getPartialTemplatesNames()
        .then(readAndParseAllFiles)
        .then(generateAllTemplates);
}
module.exports = generateTemplates;
