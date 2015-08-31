<div class="content center-wrapper">

    <h1>Charts:</h1>

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
