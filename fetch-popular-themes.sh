#!/bin/sh

# run cronjob
# cron: 0 0 * * * /home/caisah/emacs-themes-site/fetch-popular-themes.sh
node ./pull-and-build.js && node ./fetch-popular-themes.js
