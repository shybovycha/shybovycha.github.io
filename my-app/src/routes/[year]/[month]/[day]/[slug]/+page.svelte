<script lang="ts">
  import type { PageProps } from './$types';

  import PostPreview from '../../../../../components/PostPreview.svelte';

  let { data } = $props();

  const { post } = data;
</script>

<style>
.content {}
</style>

<div class="content">
  <PostPreview title={post.title} timestamp={post.timestamp} link={post.link}>
    <post.component />
  </PostPreview>
</div>

