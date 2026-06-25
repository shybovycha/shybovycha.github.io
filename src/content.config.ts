import { defineCollection } from 'astro:content';
import { z } from 'astro/zod';
import { glob } from 'astro/loaders';

const postSchema = z.object({
  title: z.string().optional(),
  tags: z.array(z.string()).optional(),
  date: z.string().optional(),
});

export const collections = {
  posts: defineCollection({
    loader: glob({ pattern: '*.md', base: './_posts' }),
    schema: postSchema,
  }),
  drafts: defineCollection({
    loader: glob({ pattern: '*.md', base: './_drafts' }),
    schema: postSchema,
  }),
};
