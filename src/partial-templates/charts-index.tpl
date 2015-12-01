<div class="content center-wrapper">

    <h1>Charts:</h1>
    <p>
        These charts are created based on the number of searches (Google seraches) a theme gets every month.
    </br>
    The <strong>all times chart</strong> contains a list of the most installed themes according to <strong>MELPA</strong> numbers.
    </p>
    </br>
    <!-- Theme list start -->
    <ul class="charts-list">
        {{#each charts}}
        <li class="chart">
            <a href="{{this.url}}">
                <span>{{this.text}}</span>
            </a>
        </li>
        {{/each}}
    </ul>
    <!-- Theme list end -->

</div>
