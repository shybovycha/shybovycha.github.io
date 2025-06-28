import type { LayoutLoad } from './$types';

import orderBy from 'lodash/orderBy';
import chunk from 'lodash/chunk';

import { sentenceCase } from 'change-case';

const postsModules = import.meta.glob('../posts/**/*.md', { eager: true });

const resolveTimestamp = (path, meta) => {
  return meta?.date ?? (path.replace(/^.*(\d{4}-\d{2}-\d{2})-.*$/, '$1T00:00:00+10:00'));
};

const resolveSlug = (path, meta) => {
  return meta?.slug ?? path.replace(/^.*\d{4}-\d{2}-\d{2}-(.+)\..+$/, '$1');
};

const resolveTitle = (path, meta) => {
  return meta?.title ?? sentenceCase(path.replace(/^\d{4}-\d{2}-\d{2}-(.+)\..+$/, '$1') || path);
};

const resolveLink = (path, meta) => {
  return (meta?.slug ?? path.replace(/^\D+(.+)$/, '$1')).replace(/^(\d{4})-(\d{2})-(\d{2})-(.+)$/, '$1/$2/$3/$4');
};

const resolveTimeFromPath = (path) => {
  return new RegExp(/^.*(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})-.*$/).exec(path).groups;
};

const fillProps = ({ path, meta }) => {
	return {
		timestamp: resolveTimestamp(path, meta),
		date: resolveTimeFromPath(path),
		slug: resolveSlug(path, meta),
		title: resolveTitle(path, meta),
		link: resolveLink(path, meta),
	};
};

export const load: LayoutLoad = async () => {
  const posts1 = Object.entries(postsModules)
		.map(([path, m]) => ({ component: m.default, path: path, meta: m.metadata }))
		.map(p => ({ ...p, ...fillProps(p) }));

	console.log('>>>', posts1);

	const posts = orderBy(posts1, 'timestamp', 'desc');
	
	const PAGE_SIZE = 10;

	const pages = chunk(posts, PAGE_SIZE);

	return {
		posts, 
		pages,
	};
};

export const prerender = true;

