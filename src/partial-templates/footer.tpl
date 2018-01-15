<div class="footer">
    <div class="footer-info center-wrapper">
        This site was generated using <a class="no-hover" href="http://nodejs.org/">node.js</a>. License: <a class="no-hover" rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img class="license-logo" alt="Creative Commons License" src="https://i.creativecommons.org/l/by/4.0/80x15.png" /></a> See more on <a class="no-hover" href="https://github.com/emacs-themes">❦ Github</a>.
    </div>
</div>

<script>
    function getParameterByName(name) {
        var queryString = window.location.href;
        var parameters = queryString.split('?')[1];

        if (parameters) {
            var parametersData = {};
            var parametersList = parameters.split('&');

            for (var i = 0; i < parametersList.length; i++) {
                var key = parametersList[i].split('=')[0];
                var value = parametersList[i].split('=')[1];

                parametersData[key] = value;
            }
        }

        if (parametersData[name]) {
            return parametersData[name];
        }

        return undefined;
    }

    function search(stack, needle) {
        var searchExp = new RegExp(needle.join("|"),"gi");

        return (searchExp.test(stack)) ? true : false;
    }

    function initSearchForm() {
        var searchForm = document.querySelector('#searchForm');

        if (searchForm) {
            var searchParameters = getParameterByName('search').split('+');
            var themeCells = document.getElementsByClassName('js-theme-cell');

            for (var i = 0; i < themeCells.length; i++) {
                var cell = themeCells[i];
                var themeParameters = cell.getAttribute('data-title') + ' ' + cell.getAttribute('data-tags');

                if (!search(themeParameters, searchParameters)) {
                    cell.style.display = 'none';
                }
            }
        }
    }

    initSearchForm();
</script>