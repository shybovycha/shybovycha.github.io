import type { CollectionEntry } from 'astro:content';

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

export function splitExcerptHtml(entry: CollectionEntry<'posts'> | CollectionEntry<'drafts'>): {
  excerptHtml: string;
  hasMore: boolean;
} {
  const html = entry.rendered?.html ?? '';
  const idx = html.indexOf('<!--more-->');
  if (idx === -1) return { excerptHtml: html, hasMore: false };
  return { excerptHtml: html.slice(0, idx), hasMore: true };
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
