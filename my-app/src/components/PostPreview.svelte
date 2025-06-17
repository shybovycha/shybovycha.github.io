<script lang="ts">
  import { format, parseISO } from 'date-fns';

  let { link, title, timestamp, children } = $props();

  let originalTime = new Date(), time = format(originalTime, 'dd MMM yyyy');

  try {
    originalTime = parseISO(timestamp);
  } catch (e) {
    console.error('Parsing time failed', timestamp);
  }

  try {
    time = format(originalTime, 'dd MMM yyyy');
  } catch (e) {
    console.error('Formatting time failed', timestamp, '>>', originalTime);
  }
</script>

<article>
  <h1>
    <a href={link}>{title}</a>
  </h1>

  <div>
    <time>{time}</time>
  </div>

  <div class="content">
    {@render children?.()}
  </div> 
</article>
