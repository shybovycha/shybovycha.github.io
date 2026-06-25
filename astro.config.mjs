import { defineConfig } from 'astro/config';
import sitemap from '@astrojs/sitemap';

export default defineConfig({
  site: 'https://shybovycha.github.io',
  build: { format: 'file' },
  compressHTML: true,
  trailingSlash: 'never',
  integrations: [sitemap()],
  markdown: {
    shikiConfig: {
      themes: { light: 'catppuccin-latte', dark: 'catppuccin-frappe' },
    },
  },
});
