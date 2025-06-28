<script lang="ts">
  import type { PageProps } from './$types';

  import Post from '../../../../../components/Post.svelte';

  let { data } = $props();

  const { post } = data;
</script>

<style>
.content {}
</style>

<main>
  <Post title={post.title} timestamp={post.timestamp} link={post.link}>
    <post.component />
  </Post>
</main>

