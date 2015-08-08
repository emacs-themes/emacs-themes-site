'use strict';
// external dependencies
var fs = require('fs');
var path = require('path');
var Q = require('q');
// internal dependencies
var log = require('../helpers/simpleLogger');
var existsInArray = require('../helpers/generalHelpers').existsInArray;
// globals
var rootDir = __dirname + '/../../root/';
var indexFilePath = rootDir + 'index.html';
var allDirs = ['themes', 'tags', 'index', 'charts', 'pages'];

// creates single directory
function makeDir(dirName) {
    var dirPath = rootDir + dirName;

    return Q.nfcall(fs.mkdir, dirPath).then(function() {
        log.created('Dir', dirPath);
    });
}
// recreates all directories
function makeAllDirs() {
    return Q.all(allDirs.map(makeDir));
}

function deleteIndexIfExists(allFileNames) {
    var indexPath = path.normalize(indexFilePath);

    if (existsInArray('index.html', allFileNames)) {
        return Q.nfcall(fs.unlink, indexPath);
    }
    return Q()
        .then(function() {
            log.text('File "' + indexPath + '" already deleted');
        });
}
function readAllFilesFromRoot() {
    return Q.nfcall(fs.readdir, rootDir);
}
// gets all paths for the files inside a directory
function getAllFilePathsFromDir(dirPath) {
    return Q.nfcall(fs.readdir, dirPath)
        .then(function(fileNames) {
            var allPaths = fileNames.map(function(fileName) {
                return path.normalize(dirPath + '/' + fileName);
            });
            return allPaths;
        });
}
// delete a single file
function deleteFile(filePath) {
    return Q.nfcall(fs.unlink, filePath)
        .then(function() {
            log.deleted('File', filePath);
        });
}
// delete all files
function deleteAllFiles(filePaths) {
    return Q.all(filePaths.map(deleteFile));
}
// deletes all files from a directory
function deleteAllFilesFromDir(dirPath) {
    console.log('Deleting all files from ', path.normalize(dirPath));
    return getAllFilePathsFromDir(dirPath)
       .then(deleteAllFiles);
}

// deletes a directory
function deleteDir(dirPath) {
    return Q.nfcall(fs.rmdir, dirPath)
        .then(function() {
            log.deleted('Dir', dirPath);
        });
}
// deletes a directory and all sub files
function deleteDirAndFiles(dirPath, allFileNamesInRoot) {
    var dirName = path.parse(dirPath).name;

    if (existsInArray(dirName, allFileNamesInRoot)) {
        return deleteAllFilesFromDir(dirPath)
            .then(function() {
                return deleteDir(dirPath);
            });
    }
    return Q()
       .then(function() {
           log.text('Dir "' + dirPath + '" already deleted');
       });
}
// deletes all directories
function deleteAllDirs(allFileNamesInRoot) {
    return Q.all(allDirs.map(function(dirName) {
        var dirPath = rootDir + dirName;

        return deleteDirAndFiles(dirPath, allFileNamesInRoot);
    }));
}
// export
function deleteAndRegenerateDirs() {
    return readAllFilesFromRoot()
        .then(function(allFileNamesInRoot) {
            deleteIndexIfExists(allFileNamesInRoot)
                .then(function() {
                    deleteAllDirs(allFileNamesInRoot)
                        .then(makeAllDirs);
                });
        });
}

module.exports = deleteAndRegenerateDirs;
