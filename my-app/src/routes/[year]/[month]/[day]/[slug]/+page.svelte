<script lang="ts">
  import type { PageProps } from './$types';
  import { format } from 'date-fns';

  let { data }: PageProps = $props();
</script>

<style>
.content {}
</style>

<article>
  <h1>{data.title}</h1>

  <div>
    <time>{format(data.timestamp, 'dd MMM yyyy')}</time>
  </div>

  <div class="content">
    <h1>posts:</h1>
    {#each data.posts as post}
      {#if post.component}
        <post.component />
      {/if}
    {/each}
    {@html data.content}
  </div>
</article>

