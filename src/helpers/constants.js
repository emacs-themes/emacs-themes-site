'use strict';
// external dependencies
var path = require('path');

var CONSTANTS = {
  ROOT_DIR: path.join(__dirname, '/../../root/'),
  INDEX_FILE_PATH: path.join(__dirname, '/../../root/index.html'),
  THEMES_DIR_PATH: path.join(__dirname, '/../../root/themes/'),
  TEMPLATES_SRC_PATH: path.join(__dirname, '/../partial-templates/'),
  RECIPES_DIR_PATH: path.join(__dirname, '/../../recipes/'),
  ROOT_DIR_NAMES: ['themes', 'tags', 'index', 'popular', 'pages'],
  THEMES_PER_PAGE: 12,
};

module.exports = CONSTANTS;
