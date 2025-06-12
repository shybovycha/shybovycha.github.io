import type { PageServerLoad, PageProps } from './$types';

export const load: PageServerLoad = async ({ parent, params }) => {
  const parents = await parent();

  console.log(parents, params);

  return {
    path: `${params.year}-${params.month}-${params.day}-${params.slug}`,
    title: params.slug,
    timestamp: `${params.year}-${params.month}-${params.day}`,
    content: `<pre><code>${JSON.stringify(parents)}</code></pre>`, 
  };
};

