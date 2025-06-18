<script lang="ts">
import orderBy from 'lodash/orderBy';

import Post from './Post.svelte';

let { posts: posts1 } = $props();

let posts = orderBy(posts1, 'timestamp', 'asc');
</script>

{#each posts as post}
  <Post title={post.title} link={post.link} timestamp={post.timestamp} isPreview="true">
    <post.component />
  </Post>
{/each}

