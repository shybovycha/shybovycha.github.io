<script lang="ts">
import type { PageProps } from './$types';

import IndexPage from '../../components/IndexPage.svelte';
import Footer from '../../components/Footer.svelte';

let { data }: PageProps = $props();

let posts = data.pages[data.page];
</script>

<IndexPage posts={posts} />

<Footer page={data.page + 1} totalPages={data.pages.length} />

