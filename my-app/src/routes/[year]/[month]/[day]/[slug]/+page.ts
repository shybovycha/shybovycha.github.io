export const load = async ({ params, parent }) => {
  const { posts } = await parent();

  let { year, month, day, slug: slug1 } = params;

  const slug = slug1.replace(/^(.+)\..+$/, '$1');

  console.log('??', {...params, slug});

  const post = posts.find(p => p.date.year === year && p.date.month === month && p.date.day === day && p.slug === slug);

  console.log('>>>', posts.find((p) => p.date.year === year && p.date.month === month && p.date.day === day && p.slug === slug));

  return {
    post,
  };
};

