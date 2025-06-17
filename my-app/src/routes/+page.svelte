<script lang="ts">
  import take from 'lodash/take';

  import IndexPage from '../components/IndexPage.svelte';
  import Footer from '../components/Footer.svelte';

  let { data } = $props(); 

  let posts = take(data.posts, 10);
</script>

<IndexPage posts={posts} />

<Footer page={1} totalPages={Math.floor(data.posts.length / 10)} />

