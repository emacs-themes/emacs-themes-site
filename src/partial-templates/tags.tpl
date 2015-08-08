<!-- Tags list starts here -->
<div class="content center-wrapper">
    <h1>Tags:</h1>
    {{#each tags}}
    <li><a href="./{{this.hyphenedValue}}/1.html">{{this.spacedValue}}</a></li>,&nbsp
    {{/each}}
</div>
<!-- Tags list ends here -->
