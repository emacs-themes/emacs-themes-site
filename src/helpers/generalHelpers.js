'use strict';
// external dependencies
var Q = require('q');
var fs = require('fs');

var helpers = {
    // returns an hyphanized string
    hyphanize: function(str) {
        return str.split(' ').join('-').toLowerCase();
    },

    // reads and parses json file and returns a promise
    readJsonFromDisk: function(filePath) {
        return Q.nfcall(fs.readFile, filePath, 'utf-8')
            .then(JSON.parse);
    },

    // parses a recipie
    parseRecipie: function(recipie) {
        recipie.name = {
            spacedValue: recipie.name,
            hyphenedValue: helpers.hyphanize(recipie.name)
        };

        recipie.tags = recipie.tags.map(function(tag) {
            return {
                spacedValue: tag,
                hyphenedValue: helpers.hyphanize(tag)
            };
        });

        return recipie;
    },

    existsInArray: function(name, arr) {
        var res = arr.filter(function(el) {
            return el === name;
        });
        return res.length > 0;
    }
};

module.exports = helpers;
