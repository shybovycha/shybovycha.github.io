<script lang="ts">
import type { PageProps } from './$types';

let { data }: PageProps = $props();

let posts = data.pages[data.page];
</script>

<h1>Page {data.page}</h1>

{#each posts as post}
  <h1>{post.title}</h1>
  <timestamp>{post.timestamp}</timestamp>
  <post.component />
{/each}

