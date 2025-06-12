import adapter from '@sveltejs/adapter-auto';
import { vitePreprocess } from '@sveltejs/vite-plugin-svelte';
import { mdsvex } from 'mdsvex';

/** @type {import('@sveltejs/kit').Config} */
const config = {
	// Consult https://svelte.dev/docs/kit/integrations
	// for more information about preprocessors
	extensions: [ '.svelte', '.md', '.svx' ],
	preprocess: [
		mdsvex({
			extensions: [ '.md', '.svx' ],
		}),

		vitePreprocess(), 
	],

	kit: {
		// adapter-auto only supports some environments, see https://svelte.dev/docs/kit/adapter-auto for a list.
		// If your environment is not supported, or you settled on a specific environment, switch out the adapter.
		// See https://svelte.dev/docs/kit/adapters for more information about adapters.
		adapter: adapter({
			pages: '_site',
			assets: '_site',
			fallback: '404.html',
			precompress: false,
			strict: true,
		}),
		paths: {
			base: process.argv.includes('dev') ? '' : process.env.BASE_PATH,
		},
	}
};

export default config;
