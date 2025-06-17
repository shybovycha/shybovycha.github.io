<script lang="ts">
  import type { PageProps } from './$types';

  import PostPreview from '../../../../../components/PostPreview.svelte';

  let { data }: PageProps = $props();
</script>

<style>
.content {}
</style>

<div class="content">
  {#each data.posts as post}
    <PostPreview title={post.title} timestamp={post.timestamp} link={post.slug}>
      <post.component />
    </PostPreview>
  {/each}
</div>

