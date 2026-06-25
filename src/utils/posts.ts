import { unified } from 'unified';
import remarkParse from 'remark-parse';
import remarkRehype from 'remark-rehype';
import rehypeStringify from 'rehype-stringify';
import rehypeShiki from '@shikijs/rehype';

const excerptProcessor = unified()
  .use(remarkParse)
  .use(remarkRehype, { allowDangerousHtml: true })
  .use(rehypeShiki, {
    themes: { light: 'catppuccin-latte', dark: 'catppuccin-frappe' },
    langAlias: {
      purs: 'haskell',
      reason: 'ocaml',
      mustache: 'handlebars',
      git: 'bash',
      thrift: 'protobuf',
      class: 'java',
      conf: 'ini',
      dot: 'plaintext',
      g4: 'plaintext',
    },
  })
  .use(rehypeStringify, { allowDangerousHtml: true });

export function parsePostId(id: string): {
  year: string;
  month: string;
  day: string;
  slug: string;
} | null {
  const match = id.match(/^(\d{4})-(\d{2})-(\d{2})-(.+)$/);
  if (!match) return null;
  const [, year, month, day, slug] = match;
  return { year, month, day, slug };
}

export function splitExcerpt(body: string): { excerpt: string; hasMore: boolean } {
  const idx = body.indexOf('<!--more-->');
  if (idx === -1) return { excerpt: body, hasMore: false };
  return { excerpt: body.slice(0, idx).trim(), hasMore: true };
}

export async function renderExcerpt(md: string): Promise<string> {
  const result = await excerptProcessor.process(md);
  return String(result);
}

export function sortPosts<T extends { id: string }>(posts: T[]): T[] {
  return [...posts].sort((a, b) => {
    const da = a.id.slice(0, 10);
    const db = b.id.slice(0, 10);
    return db.localeCompare(da);
  });
}

export function postUrl(id: string): string {
  const meta = parsePostId(id);
  if (!meta) return '/';
  return `/${meta.year}/${meta.month}/${meta.day}/${meta.slug}.html`;
}
