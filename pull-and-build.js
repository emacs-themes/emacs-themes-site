const path = require('path');
const exec = require('child_process').exec;
const fs = require('fs');
const command = 'git pull origin master';
const projectPath = __dirname;
const errorLogPath = path.join(__dirname, '/logs/error.log');
const mainLogPath = path.join(__dirname, '/logs/main.log');
// execute a git pull
const child = exec(command, { cwd: projectPath });
const buildSite = require(path.join(__dirname, '/src/builders/buildSite'));
let successText;

function gitIsUpToDate(data) {
  return data.indexOf('Already up-to-date') > -1;
}

function tryToBuild() {
  var time = new Date().toString();
  var text = time + ' - Build has been initiated!\n';

  fs.appendFile(mainLogPath, text, function(err) {
    var errorText = time + ' - Could not write to ' + mainLogPath + ' file\n';
    if (err) {
      fs.appendFileSync(errorLogPath, errorText);
    }
  });
  buildSite();
}
// grab stdout
child.stdout.on('data', function(data) {
  successText = data;
});
// log error
child.stderr.on('data', function(data) {
  var time = new Date().toString();
  var errorText = time + ' - Git stderr: \n   ' + data;

  fs.appendFileSync(errorLogPath, errorText);
});

// try to build the project on exit
child.on('close', function(code) {
  var time = new Date().toString();
  var text;
  // log exit code
  if (gitIsUpToDate(successText)) {
    text = time + ' - Git is up to date. Code: ' + code + '\n';
    fs.appendFileSync(errorLogPath, text);
  } else {
    tryToBuild();
  }
});
