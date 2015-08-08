'use strict';
// paths
var testRecipiesPath = __dirname + '/assets/recipies/';

var expect = require('chai').expect;
var readAndParse = require('../src/recipiesParser');

describe('Testing Recipies Parser: ', function() {
    describe('readAndParse(path)', function() {
        var theme;

        before(function(done) {
            readAndParse(testRecipiesPath).then(function(res) {
                theme = res[0];

                done();
            });
        });

        it('should read an parse recipies', function() {
            expect(theme.name.spacedValue).to.not.equal('Aalto Dark Theme');
            expect(theme.name.hyphenedValue).to.equal('aalto-dark-theme');

            expect(theme.tags[0]).to.have.property('spacedValue');
            expect(theme.tags[0]).to.have.property('hyphenedValue');

            expect(theme.description).to.equal('Color Theme');

            expect(theme).to.have.property('remoteSrc');

            expect(theme).to.have.property('localSrc');

            expect(theme).to.have.property('largeImgs');

            expect(theme.available).to.equal(false);

            expect(theme.smallImg).to.equal('.\/assets\/imgs-small\/aalto-dark.png');
        });
    });
});
