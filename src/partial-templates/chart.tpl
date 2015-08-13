<div class="content center-wrapper">

    <h1>Charts for {{ date }}:</h1>

    <!-- Charts list start -->
    <ol class="charts-themes-list">
        {{#each themes}}
        <li class="chart">
            <a href="../themes/{{this}}.html">
                <span>{{this}}</span>
            </a>
        </li>
        {{/each}}
    </ol>
    <!-- Charts list end -->

</div>
