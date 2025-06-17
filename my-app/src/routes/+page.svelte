<script lang="ts">
  import take from 'lodash/take';

  import IndexPage from '../components/IndexPage.svelte';

  let { data } = $props(); 

  let posts = take(data.posts, 10);
</script>

<IndexPage posts={posts} />

