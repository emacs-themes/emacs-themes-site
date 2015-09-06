<div class="content center-wrapper">

    <!-- Title start -->
    <h1>{{ name.spacedValue }}</h1>
    <!-- Title end -->

    <!-- Info start -->
    <p><em>Info: </em>{{description}}</p>
    <div class="details">
        <ul>
            <li class="detail"><em>Author</em>: {{author}}</li>
            <li class="detail"><em>Link</em>: <a href="{{remoteSrc}}">Official Source</a></li>
            <li class="detail"><em>Link</em>: <a href=".{{localSrc}}">Local Source Code</a></li>
            {{#if available}}
            <li class="detail"><em>Available</em> in <a href="http://melpa.org">MELPA</a></li>
            {{else}}
            <li class="detail"><em>Not</em> Available as a <a href="https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html">package</a></li>
            {{/if}}
        </ul>
    </div>
    <!-- Info end -->

    <!-- Images start -->
    <div class="screenshots">
        <ul>
            {{#each largeImgs}}
            <li class="screenshot">
                <a class="no-hover" href=".{{this}}" title="Click to see full size image"><img class="theme-large-img" src=".{{this}}" alt="Emacs {{name.spacedValue}} screenshot"></img></a>
            </li>
            {{/each}}
        </ul>
    </div>
    <!-- Images end -->

    <!-- Tags start-->
    <div class="tags">
        <em>Tags</em>:
        <ul>
            {{#each tags}}
            <li class="tag">
                <a href="../tags/{{this.hyphenedValue}}/1.html">{{this.spacedValue}}</a>,
            </li>
            {{/each}}
        </ul>
    </div>
    <!-- Tags end -->

</div>
