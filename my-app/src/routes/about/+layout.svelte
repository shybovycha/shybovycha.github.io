<script lang="ts">
  import type { LayoutProps } from './$types';
  
  let { children }: LayoutProps = $props();
</script>

<main>
    {@render children()}
</main>
