<div class="content center-wrapper">

    <h1>Latest additions to the gallery:</h1>

    <!-- Theme list start -->
    <ul class="themes-list">
        {{#each themes}}
        <li class="cell">
            <p class="theme-title">{{this.name.spacedValue}}</p>
            <a class="no-hover" href="themes/{{this.name.hyphenedValue}}.html">
                <img class="theme-small-img" alt="{{this.name.spacedValue}}" title="{{this.name.spacedValue}}" src="{{this.smallImg}}"/>
            </a>
        </li>
        {{/each}}
    </ul>
    <!-- Theme list end -->

</div>
