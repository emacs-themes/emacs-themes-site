'use strict';
// external dependencies
var path = require('path');

var CONSTANTS = {
    ROOT_DIR: path.normalize(__dirname + '/../../root/'),
    INDEX_FILE_PATH: path.normalize(__dirname + '/../../root/index.html'),
    THEMES_DIR_PATH: path.normalize(__dirname + '/../../root/themes/'),
    TEMPLATES_SRC_PATH: path.normalize(__dirname + '/../partial-templates/'),
    RECIPES_DIR_PATH: path.normalize(__dirname + '/../../recipes/'),
    CHARTS_DIR_PATH: path.normalize(__dirname + '/../config/charts/'),
    ROOT_DIR_NAMES: ['themes', 'tags', 'index', 'charts', 'pages'],
    THEMES_PER_PAGE: 12
};

module.exports = CONSTANTS;
