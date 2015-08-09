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
// move this to helpers
var handleError = function(err) {
    console.log(err.stack);
};
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
    return getAllFilePathsFromDir(dirPath)
       .then(deleteAllFiles);
}

// deletes an empty directory
function deleteEmptyDir(dirPath) {
    return Q.nfcall(fs.rmdir, dirPath)
        .then(function() {
            log.deleted('Dir', dirPath);
        }, handleError);
}

// deletes all empty directories
function deleteAllEmptyDirs(allDirPaths) {
    return allDirPaths.map(deleteEmptyDir);
}
// returns a promise with all dirs that need to be deleted
function getAllDirsToDelete(dirsInRoot) {
    return Q(dirsInRoot.filter(function(dirName) {
        return existsInArray(dirName, allDirs);
    }));
}

function makeFullPaths(partials) {
    return partials.map(function(partial) {
        return path.normalize(rootDir + partial);
    });
}

function getFileWithType(filePath) {
    return Q.nfcall(fs.stat, filePath).then(function(stats) {
        return {
            filePath: filePath,
            isFile: stats.isFile()
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
    return getAllFilePathsFromDir(dirPath)
        .then(getAllFilesWithTypes, handleError);
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
    return readAllFilesFromRoot()
        .then(getAllDirsToDelete, handleError)
        .then(makeFullPaths, handleError)
        .then(deleteAllDirsWithContents, handleError)
        .then(makeAllDirs, handleError);
}

module.exports = deleteAndRegenerateDirs;
