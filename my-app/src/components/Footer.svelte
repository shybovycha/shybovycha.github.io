<script lang="ts">
let { page, totalPages } = $props();
</script>

<footer>
  <nav>
    {#if page > 1}
      <div class="prev">
        <a href={`/page${page - 1}.html`}>Previous</a>
      </div>
    {/if}

    <div class="current">Page #{`${ page }`} of {`${ totalPages }`}</div>

    {#if page < totalPages}
      <div class="next">
        <a href={`/page${page + 1}.html`}>Next</a>
      </div>
    {/if}
  </nav>
</footer>

