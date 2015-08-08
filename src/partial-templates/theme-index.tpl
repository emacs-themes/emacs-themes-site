<div class="content center-wrapper">

    <!-- Theme list start -->
    <ul class="themes-list">
        {{#each themes}}
        <li class="cell">
            <p class="theme-title">{{this.title}}</p>
            <a class="no-hover" href="{{this.link}}">
                <img class="theme-small-img" alt="{{this.title}}" title="{{this.title}}" src="{{this.img}}"/>
            </a>
        </li>
        {{/each}}
    </ul>
    <!-- Theme list end -->

    <!-- Pagination start -->
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
    <!-- Pagination end -->

</div>
