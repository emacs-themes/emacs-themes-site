'use strict';
// external dependencies
var fs = require('fs');
var path = require('path');
var Q = require('q');
// internal dependencies
var log = require('../helpers/simpleLogger');
var helpers = require('../helpers/generalHelpers');
var CONSTANTS = require('../helpers/constants');
var existsInArray = helpers.existsInArray;
var getAllFileNamesFrom = helpers.getAllFileNamesFrom;
// constants
var ROOT_DIR = CONSTANTS.ROOT_DIR;
var ROOT_DIR_NAMES = CONSTANTS.ROOT_DIR_NAMES;
// move this to helpers
var handleError = function(err) {
  console.log(err.stack);
};
// creates single directory
function makeDir(dirName) {
  var dirPath = ROOT_DIR + dirName;

  return Q.nfcall(fs.mkdir, dirPath).then(function() {
    log.created('Dir', dirPath);
  });
}
// recreates all directories
function makeAllDirs() {
  return Q.all(ROOT_DIR_NAMES.map(makeDir));
}

// gets all paths for the files inside a directory
function getAllFilePathsFromDir(dirPath) {
  return getAllFileNamesFrom(dirPath).then(function(fileNames) {
    var allPaths = fileNames.map(function(fileName) {
      return path.normalize(dirPath + '/' + fileName);
    });
    return allPaths;
  });
}
// delete a single file
function deleteFile(filePath) {
  return Q.nfcall(fs.unlink, filePath).then(function() {
    log.deleted('File', filePath);
  });
}
// delete all files
function deleteAllFiles(filePaths) {
  return Q.all(filePaths.map(deleteFile));
}

// deletes an empty directory
function deleteEmptyDir(dirPath) {
  return Q.nfcall(fs.rmdir, dirPath).then(function() {
    log.deleted('Dir', dirPath);
  }, handleError);
}

// returns a promise with all dirs that need to be deleted
function getAllDirsToDelete(dirsInRoot) {
  return Q(
    dirsInRoot.filter(function(dirName) {
      return existsInArray(dirName, ROOT_DIR_NAMES);
    }),
  );
}

function makeFullPaths(partials) {
  return partials.map(function(partial) {
    return path.normalize(ROOT_DIR + partial);
  });
}

function getFileWithType(filePath) {
  return Q.nfcall(fs.stat, filePath).then(function(stats) {
    return {
      filePath: filePath,
      isFile: stats.isFile(),
    };
  }, handleError);
}

function getAllFilesWithTypes(filePaths) {
  return Q.all(filePaths.map(getFileWithType));
}

function isSimpleFile(fileWithType) {
  return fileWithType.isFile;
}

function isDir(fileWithType) {
  return !fileWithType.isFile;
}
function getFilePath(fileWithType) {
  return fileWithType.filePath;
}

function readDirContents(dirPath) {
  return getAllFilePathsFromDir(dirPath).then(
    getAllFilesWithTypes,
    handleError,
  );
}
// deletes recursively a dir
function deleteDirWithContents(dirPath) {
  return readDirContents(dirPath)
    .then(function(filesWithTypes) {
      var filePaths = filesWithTypes.filter(isSimpleFile).map(getFilePath);
      return deleteAllFiles(filePaths).then(function() {
        return filesWithTypes;
      }, handleError);
    })
    .then(function(filesWithTypes) {
      var dirPaths = filesWithTypes.filter(isDir).map(getFilePath);
      return deleteAllDirsWithContents(dirPaths);
    }, handleError)
    .then(function(filesWithTypes) {
      return deleteEmptyDir(dirPath);
    }, handleError);
}
// deletes recursively multiple dirs
function deleteAllDirsWithContents(dirPaths) {
  return Q.all(dirPaths.map(deleteDirWithContents));
}
// export
function deleteAndRegenerateDirs() {
  return getAllFileNamesFrom(ROOT_DIR)
    .then(getAllDirsToDelete, handleError)
    .then(makeFullPaths, handleError)
    .then(deleteAllDirsWithContents, handleError)
    .then(makeAllDirs, handleError);
}

module.exports = deleteAndRegenerateDirs;
