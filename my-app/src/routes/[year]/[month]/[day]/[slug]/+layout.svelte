<script lang="ts">
  import type { LayoutProps } from './$types';
  
  let { data, children }: LayoutProps = $props();
</script>

<style>
.nav-link {}

.nav-item {}
</style>

<nav class="top">
  <div class="links">
    <a class="nav-link nav-item" href="/">Home</a>

    <a class="nav-link nav-item" href="/about.html">About</a>
  </div>
</nav>

<main>
  {@render children()}
</main>

