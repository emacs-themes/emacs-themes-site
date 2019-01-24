<div class="content center-wrapper">

  <h1>Most popular themes:</h1>

  <p>This top is created based on numbers extracted from MELPA. These are the most downloaded themes since MELPA started recording stats. The list is updated daily.
  </p>

    </br>

    <!-- Theme list start -->
    <ol class="top-list">
      {{#each themes}}
      <li class="top-item">
        <div class="top-theme">
          <a href="{{this.url}}">
            <span>{{this.name}}</span>
          </a>
        </div>
        <span class="download-number">{{this.downloads}}</span>
        <span class="download-text">downloads</span>
      </li>
      {{/each}}
    </ol>
    <!-- Theme list end -->

</div>
