'use strict';
// external dependencies
var path = require('path');

exports.created = function(type, filePath) {
    console.log(type + ' "' + path.normalize(filePath) + '" created');
};

exports.deleted = function(type, filePath) {
    console.log(type + ' "' + path.normalize(filePath) + '" deleted');
};

exports.read = function(type, filePath) {
    console.log(type + ' "' + path.normalize(filePath) + '" read');
};

exports.text = function(text) {
    console.log(text);
};

exports.built = function(theme) {
    console.log('Theme "' + theme + '" built');
};
