'use strict';

var errorHandler = {
    onlyThrow: function(err) {
        throw err;
    },

    onlyPrint: function(err) {
        console.log(err);
    },

    printThrow: function(err, fun) {
        console.log('Error inside function: "' + fun + '"');
        throw err;
    },

    handle: function(err, fun) {
        if (err) {
            errorHandler.printThrow(err, fun);
        }
    }
};

module.exports = errorHandler;
