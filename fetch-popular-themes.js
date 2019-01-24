const path = require('path');
const saveThemesToCache = require(path.join(
  __dirname,
  '/src/popular/cache-themes',
));

saveThemesToCache();
