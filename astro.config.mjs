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
    },
  },
});
