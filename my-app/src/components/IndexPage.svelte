<script lang="ts">
import orderBy from 'lodash/orderBy';

import PostPreview from './PostPreview.svelte';

let { posts: posts1 } = $props();

let posts = orderBy(posts1, 'timestamp', 'asc');
</script>

{#each posts as post}
  <PostPreview title={post.title} link={post.link} timestamp={post.timestamp}>
    <post.component />
  </PostPreview>
{/each}

