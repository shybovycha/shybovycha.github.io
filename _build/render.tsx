import React from 'react';
import ReactDOMServer from 'react-dom/server';

import Sitemap from './components/Sitemap';
import RobotsTxt from './components/RobotsTxt';
import RssAtom from './components/RssAtom';
import RssFeed from './components/RssFeed';
import PostPage from './components/PostPage';
import StaticPage from './components/StaticPage';
import IndexPage from './components/IndexPage';

export interface PostContent {
    frontMatter: Record<string, any>;
    excerpt?: string;
    content: string;
}

export interface LoadPostContentOptions {
    excerptSeparator: string | undefined;
}

export interface Post {
    title: string;
    link: string;
    timestamp: Date;
    updateTimestamp: Date;
    excerpt?: string;
    content: string;
}

export type StaticPage = PostContent & {
    outputPath: string;
}

export const renderSitemap = (posts: Post[], baseUrl: string) => Promise.resolve(Sitemap(posts, baseUrl));

export const renderRobotsTxt = (posts: Post[], baseUrl: string) => Promise.resolve(RobotsTxt(posts, baseUrl));

export const renderRssAtom = (posts: Post[], baseUrl: string) => Promise.resolve(RssAtom(posts, baseUrl));

export const renderRssFeed = (posts: Post[], baseUrl: string) => Promise.resolve(RssFeed(posts, baseUrl));

export const renderPost = (post: Post) => Promise.resolve({ ...post, content: ReactDOMServer.renderToStaticMarkup(<PostPage {...post} />) });

export const renderStaticPage = (page: StaticPage) => Promise.resolve({ ...page, content: ReactDOMServer.renderToStaticMarkup(<StaticPage {...page} />) });

export const renderIndexPage = (posts: Post[], idx: number) => Promise.resolve(ReactDOMServer.renderToStaticMarkup(<IndexPage pageIndex={idx} posts={posts} />));
