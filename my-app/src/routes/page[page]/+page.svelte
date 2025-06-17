<script lang="ts">
import type { PageProps } from './$types';

import IndexPage from '../../components/IndexPage.svelte';

let { data }: PageProps = $props();

let posts = data.pages[data.page];
</script>

<IndexPage posts={posts} />

