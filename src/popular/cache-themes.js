const https = require('https');
const fs = require('fs');
const path = require('path');
const errorLogPath = path.join(__dirname, '../../logs/error.log');
const mainLogPath = path.join(__dirname, '../../logs/main.log');
const recipesUrl = 'https://melpa.org/recipes.json';
const downloadsUrl = 'https://melpa.org/download_counts.json';
const cacheFile = path.join(__dirname, './cache.json');
const themeStr = '-theme';
const ignored = {
  'helm-themes': true,
};

const composeUrl = pck => pck && `https://${pck.fetcher}.com/${pck.repo}`;

const formatName = name => name.replace(/-/g, ' ');

const filterPackages = (packages, recipes) =>
  Object.keys(packages)
    .filter(
      key => key.includes(themeStr) && !key.startsWith('/') && !ignored[key],
    )
    .sort((k1, k2) => {
      if (packages[k1] >= packages[k2]) {
        return -1;
      }
      return 1;
    })
    .map(k => ({
      name: formatName(k),
      downloads: packages[k],
      url: composeUrl(recipes[k]),
    }));

const fetch = url =>
  new Promise((resolve, reject) => {
    https
      .get(url, resp => {
        let data = '';

        resp.on('data', chunk => {
          data += chunk;
        });

        resp.on('end', () => {
          resolve(JSON.parse(data));
        });
      })
      .on('error', err => {
        reject(err);
      });
  });

const fetchAll = async () => {
  try {
    const [packages, recipes] = await Promise.all([
      fetch(downloadsUrl),
      fetch(recipesUrl),
    ]);

    return filterPackages(packages, recipes).slice(0, 50);
  } catch (err) {
    const errorText = `${new Date()} Error fetching from MELPA: ${err}\n`;

    fs.appendFileSync(errorLogPath, errorText);
  }
};

const saveThemesToCache = async () => {
  const themes = await fetchAll();

  fs.writeFile(cacheFile, JSON.stringify(themes), err => {
    if (err) {
      const errorText = `${new Date()} Error writing to file: ${err}\n`;

      fs.appendFileSync(errorLogPath, errorText);
    }

    fs.appendFileSync(
      mainLogPath,
      `${new Date()} Fetched themes successfully\n`,
    );
  });
};

module.exports = saveThemesToCache;
