<div class="content center-wrapper">

    <!-- Tag name starts here -->
    <h1>Tag: {{ tag.spacedValue }}</h1>
    <!-- Tag name ends here -->

    <!-- Themes list starts here -->
    <ul class="themes-list">
        {{#each themes}}
        <li class="cell">
            <p class="theme-title">{{this.name.spacedValue}}</p>
            <a class="no-hover" href="../../themes/{{this.name.hyphenedValue}}.html">
                <img class="theme-small-img" alt="{{this.name.spacedValue}}" title="{{this.name.spacedValue}}" src="../../{{this.img}}"/>
            </a>
        </li>
        {{/each}}
    </ul>
    <!-- Themes list ends here -->

    <!-- Pages index starts here -->
    <div>Pages:</div>
    <ul>
        {{#each pages}}
        {{#if this.current}}
        <li>{{this.number}}</li>
        {{else}}
        <li>
            <a href="./{{this}}.html">{{this}}</a>
        </li>
        {{/if}}
        {{/each}}
    </ul>
    <!-- Pages index ends here -->

</div>
