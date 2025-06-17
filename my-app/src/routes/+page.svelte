<script lang="ts">
  import lodash from 'lodash';
  const { take } = lodash;

  let { data } = $props(); 

  let posts = take(data.posts, 10);
</script>

{#each posts as post}
  {#if post.component}
    <post.component />
  {/if}
{/each}
