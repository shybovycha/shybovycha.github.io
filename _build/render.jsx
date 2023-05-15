import React from 'react';
import ReactDOMServer from 'react-dom/server';

import Sitemap from './components/Sitemap';
import RobotsTxt from './components/RobotsTxt';
import PostPage from './components/PostPage';
import StaticPage from './components/StaticPage';
import IndexPage from './components/IndexPage';

export const renderSitemap = (posts, baseUrl) => Promise.resolve(Sitemap(posts, baseUrl));

export const renderRobotsTxt = (posts, baseUrl) => Promise.resolve(RobotsTxt(posts, baseUrl));

export const renderPost = (post) => Promise.resolve({ link: post.link, content: ReactDOMServer.renderToStaticMarkup(<PostPage {...post} />) });

export const renderStaticPage = (page) => Promise.resolve(ReactDOMServer.renderToStaticMarkup(<StaticPage {...page} />));

export const renderIndexPage = (posts, idx) => Promise.resolve(ReactDOMServer.renderToStaticMarkup(<IndexPage pageIndex={idx} posts={posts} />));
