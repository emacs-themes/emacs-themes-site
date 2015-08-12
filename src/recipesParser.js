'use strict';

// external dependencies
var fs = require('fs');
var Q = require('q');
// local dependencies
var helpers = require('./helpers/generalHelpers');
var log = require('./helpers/simpleLogger');
var parseRecipie = helpers.parseRecipie;
var readFileNamesFromDir = helpers.getAllFileNamesFrom;

// parses a recipie's content
function parseRecipieContent(recipieText) {
    return parseRecipie(JSON.parse(recipieText));
}

// returns an array of recipie paths
function createFullPaths(recipieNames, recipesDir) {
    return recipieNames.map(function(recipieName) {
        return (recipesDir + recipieName);
    });
}

// returns an array of objects, each representing a recipie
function readAllRecipes(paths) {
    var res = [];

    return Q.all(paths.map(function(path) {
        return Q.nfcall(fs.readFile, path, 'utf-8')
            .then(function(recipieText) {
                res.push(parseRecipieContent(recipieText));
            });
    })).thenResolve(res);
}

// returns an array of objects, each representing a parsed recipie
function readAndParseRecipes(recipesDir) {
    log.text('Starting to parse recipes...');

    return readFileNamesFromDir(recipesDir)
        .then(function(recipieNames) {
            return readAllRecipes(createFullPaths(recipieNames, recipesDir));
        })
        .then(function(data) {
            log.text('All recipes successfully read');
            return data;
        });
}

module.exports = readAndParseRecipes;
