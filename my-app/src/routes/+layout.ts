import type { LayoutLoad } from './$types';

const posts = import.meta.glob('../posts/**/*.md', { eager: true });

export const load: LayoutLoad = async () => {
	return {
		posts: Object.values(posts).map(m => ({ component: m.default })),
	};
};

export const prerender = true;

