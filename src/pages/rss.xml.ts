import rss from '@astrojs/rss';
import { getCollection } from 'astro:content';
import type { APIContext } from 'astro';
import { sortPosts, parsePostId, postUrl } from '../utils/posts';

export async function GET(context: APIContext) {
  const posts = await getCollection('posts');
  const sorted = sortPosts(posts);

  return rss({
    title: "shybovycha's blog",
    description: 'Thoughts on software engineering, game development, and more.',
    site: context.site!,
    items: sorted.map((entry) => {
      const meta = parsePostId(entry.id);
      const pubDate = meta
        ? new Date(`${meta.year}-${meta.month}-${meta.day}`)
        : new Date();
      return {
        title: entry.data.title ?? entry.id.slice(11).replace(/-/g, ' '),
        pubDate,
        link: postUrl(entry.id),
      };
    }),
    customData: '<language>en-us</language>',
  });
}
