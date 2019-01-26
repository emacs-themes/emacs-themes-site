const path = require('path');
const saveThemesToCache = require(path.join(
  __dirname,
  '/src/popular/cache-themes',
));
const buildSite = require(path.join(__dirname, '/src/builders/buildSite'));

const saveAndBuild = async () => {
  await saveThemesToCache();
  buildSite();
};

saveAndBuild();
