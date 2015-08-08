'use strict';
// external dependencies
var expect = require('chai').expect;
var helpers = require('../src/helpers/generalHelpers');
var fs = require('fs');
// local vars
var recipiePath = __dirname + '/assets/recipies/aalto-dark.json';

describe('Testing General Helpers: ', function() {
    var recipie;
    // read recipie
    before(function(done) {
        fs.readFile(recipiePath, 'utf-8', function(err, data) {
            if (err) {
                throw err;
            }
            recipie = JSON.parse(data);
            done();
        });
    });

    describe('hyphanize()', function() {
        it('should hyphanize a simple spaced text', function() {
            expect(helpers.hyphanize('this is a text')).to.equal('this-is-a-text');
        });
    });

    describe('readJsonFromDisk()', function() {
        var name;

        before(function(done) {
            helpers.readJsonFromDisk(recipiePath).then(function(data) {
                name = data.name;
                done();
            });
        });
        it('should read a simple JSON', function() {
            expect(name).to.equal(recipie.name);
        });
    });

    describe('parseRecipie()', function() {
        it('should parse a JSON recipe', function() {
            var parsedData = helpers.parseRecipie(recipie);
            expect(parsedData.name).to.have.property('spacedValue');
            expect(parsedData.name).to.have.property('hyphenedValue');
            expect(parsedData.tags[0]).to.have.property('spacedValue');
            expect(parsedData.tags[0]).to.have.property('hyphenedValue');
            expect(parsedData.description).to.equal('Color Theme');
        });
    });

    describe('exitsInArray()', function() {
        it('should return true if name is in array', function() {
            expect(helpers.existsInArray('c', ['a', 'b', 'c', 'd'])).to.be.true;
        });

        it('should return false if name is not in the array', function() {
            expect(helpers.existsInArray('x', ['a', 'b', 'c', 'd'])).to.be.false;
        });
    });
});
