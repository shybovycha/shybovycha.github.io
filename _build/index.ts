import fs from 'fs';
import fsPromise from 'fs/promises';
import path from 'path';

import { parse as parseDate, isValid as isValidDate } from 'date-fns';

import matter from 'gray-matter';

import rehypeStringify from 'rehype-stringify';
import remarkGridTable from '@adobe/remark-gridtables';
import { TYPE_TABLE, mdast2hastGridTablesHandler } from '@adobe/mdast-util-gridtables';
import remarkGfm from 'remark-gfm';
import remarkParse from 'remark-parse';
import remarkRehype from 'remark-rehype';
import rehypeShiki from '@shikijs/rehype';
import { unified } from 'unified';

import { chunk, isString } from 'lodash';

import {
    renderRobotsTxt,
    renderRssAtom,
    renderRssFeed,
    renderSitemap,
    renderIndexPage,
    renderPost,
    renderStaticPage,
    LoadPostContentOptions,
    Post,
    PostContent,
    StaticPage
} from './render';

import config from './config';

import logger from './logger';

const parseMarkdown = async (src: string): Promise<string> => {
    const res = await unified()
        .use(remarkParse)
        .use(remarkGridTable)
        .use(remarkGfm)
        .use(remarkRehype, {
            handlers: {
                [TYPE_TABLE]: mdast2hastGridTablesHandler(),
            },
        })
        .use(rehypeShiki, {
            themes: {
                light: 'catppuccin-latte',
                dark: 'catppuccin-frappe',
            },
        })
        .use(rehypeStringify)
        .process(src);

    return String(res);
};

const parsePostContent = async (src: string, loadOptions?: LoadPostContentOptions): Promise<PostContent> => {
    const options = loadOptions ? { excerpt: true, excerpt_separator: loadOptions.excerptSeparator } : {};

    const { data: frontMatter, excerpt, content } = matter(src, options);

    const parsedExcerpt = excerpt ? await parseMarkdown(excerpt) : undefined;
    const parsedContent = await parseMarkdown(content);

    return {
        frontMatter: frontMatter,
        excerpt: parsedExcerpt,
        content: parsedContent,
    };
};

const parsePostDate = (postPath: string, frontMatter: Record<string, any>) => {
    const fileDate = path.basename(postPath).replace(/^(\d{4}-\d{2}-\d{2}).+$/, '$1');
    const fallbackDate = parseDate(fileDate, 'yyyy-MM-dd', new Date());

    const frontMatterDate = frontMatter.date && isString(frontMatter.date) ? parseDate(frontMatter.date, 'yyyy-MM-dd HH:mm:ss XXXXX', new Date()) : null;

    if (frontMatterDate && isValidDate(frontMatterDate)) {
        return frontMatterDate;
    }

    if (fallbackDate && isValidDate(fallbackDate)) {
        return fallbackDate;
    }

    return new Date();
};

const postCache = new Map();

const loadPost = async (absoluteFilePath: string, postDir: string): Promise<Post> => {
    const postPath = postDir ? absoluteFilePath.replace(postDir, '') : absoluteFilePath;
    const fileStat = fs.statSync(absoluteFilePath);
    const updateTimestamp = fileStat.mtime;

    const cached = postCache.get(postPath);

    if (cached && updateTimestamp === cached.timestamp) {
        return cached.post;
    }

    const src = await Bun.file(absoluteFilePath, { type: 'text/plain;charset=utf-8' }).text().catch(e => { logger.error(`Could not find post ${absoluteFilePath}`); throw e; });
    const { frontMatter, excerpt, content } = await parsePostContent(src, { excerptSeparator: '<!--more-->' });

    const postLink = path.join(path.dirname(postPath), path.basename(postPath).replace(/^(\d+)-(\d+)-(\d+)-(.+)\.(md|markdown|html?)$/, '$1/$2/$3/$4.html')).replace('\\', '/').replace(/^[\\\/]+/, '').replace(/^(.+)\.(md|markdown|html?)$/, '$1.html');

    logger.log(`Processing post ${postPath} -> ${postLink}`);

    const post = {
        title: frontMatter.title,
        link: postLink,
        timestamp: parsePostDate(postPath, frontMatter),
        updateTimestamp,
        excerpt,
        content,
    };

    postCache.set(postPath, {
        timestamp: updateTimestamp,
        post
    });

    return post;
};

const getFilesRec = (dir: string): string[] => {
    if (!fs.existsSync(dir) || !fs.lstatSync(dir).isDirectory()) {
        return [];
    }

    const result: string[] = [];
    const queue: string[] = [dir];

    while (queue.length > 0) {
        const p = queue.shift() as string;

        const stat = fs.lstatSync(p);

        if (stat.isDirectory()) {
            fs.readdirSync(p).forEach(f => queue.push(path.join(p, f)));
        } else {
            result.push(p);
        }
    }

    return result;
};

const loadPosts = async (postDir: string): Promise<Post[]> => {
    if (postCache.size > 0) {
        return Object.values(postCache).sort((a, b) => b.timestamp - a.timestamp);
    }

    const files = getFilesRec(postDir);

    const posts = await Promise.all(files.map((file) => loadPost(file, postDir)));

    return posts;
};

const loadStaticPages = async (staticPages: Record<string, string>, staticPagesDir: string): Promise<StaticPage[]> =>
    Promise.all(
        Object.entries(staticPages)
            .map(([ filePath, outputPath ]) => ([ path.join(staticPagesDir, filePath), outputPath ]))
            .map(async ([ filePath, outputPath ]) => {
                try {
                    const txt = await Bun.file(filePath, { type: 'text/plain;charset=utf-8' }).text();
                    const parsedContent = await parsePostContent(txt);

                    return { ...parsedContent, outputPath };
                } catch (e) {
                    logger.error(`Could not find static page ${filePath}`);
                    throw e;
                }
            })
    );

const loadDrafts = async (draftsDir: string, buildDrafts: boolean) => {
    if (!buildDrafts) {
        return Promise.resolve([]);
    }

    const drafts = await loadPosts(draftsDir);

    return drafts.map(draft => ({
        ...draft,
        link: `_drafts/${draft.link}`,
    }));
};

// ---

const createPostDir = async (post: Post, outputDir: string) => {
    await fsPromise.mkdir(path.join(outputDir, path.dirname(post.link)), { recursive: true });
};

const writeSitemap = async (sitemap: string, outputDir: string) => {
    try {
        await Bun.write(path.join(outputDir, 'sitemap.xml'), sitemap);
    } catch (e) {
        logger.error(`Could not write sitemap`, e);
        throw e;
    }
};

const writeRobotsTxt = async (robotsTxt: string, outputDir: string) => {
    try {
        await Bun.write(path.join(outputDir, 'robots.txt'), robotsTxt)
    } catch (e) {
        logger.error(`Could not write robots.txt`, e);
        throw e;
    }
};

const writeRssAtom = async (rssAtom: string, outputDir: string) => {
    try {
        await Bun.write(path.join(outputDir, 'atom.xml'), rssAtom);
    } catch (e) {
        logger.error(`Could not write RSS atom`, e);
        throw e;
    }
};

const writeRssFeed = async (rssFeed: string, outputDir: string) => {
    try {
        await Bun.write(path.join(outputDir, 'feed.rss'), rssFeed);
    } catch (e) {
        logger.error(`Could not write RSS feed`, e);
        throw e;
    }
};

const writePosts = (renderedPosts: Post[], outputDir: string) =>
    Promise.all(renderedPosts.map(async post => {
        const filePath = path.join(outputDir, post.link);

        logger.log(`Writing post ${filePath}`);

        try {
            return await Bun.write(filePath, '<!DOCTYPE html>' + post.content);
        } catch (e) {
            logger.error(`Could not write post ${filePath}`, e); throw e;
        }
    }));

const writeStaticPages = (renderedStaticPages: StaticPage[], outputDir: string) =>
    Promise.all(renderedStaticPages.map(async ({ content, outputPath }) => {
        const filePath = path.join(outputDir, outputPath);

        logger.log(`Writing static page ${filePath}`);

        try {
            return await Bun.write(filePath, '<!DOCTYPE html>' + content);
        } catch (e) {
            logger.error(`Could not write static page ${filePath}`, e); throw e;
        }
    }));

const writeIndexPages = (renderedIndexPages: string[], outputDir: string) =>
    Promise.all(renderedIndexPages.map(async (content, index) => {
        const filePath = path.join(outputDir, index == 0 ? 'index.html' : `page${index + 1}.html`);

        logger.log(`Writing page ${filePath}`);

        try {
            return await Bun.write(filePath, '<!DOCTYPE html>' + content);
        } catch (e) {
            logger.error(`Could not write index page ${filePath}`, e); throw e;
        }
    }));

const copyStaticFiles = (staticDirs: string[], outputDir: string) =>
    Promise.all([
        staticDirs
            .flatMap(dir => getFilesRec(dir))
            .map(file => ([ file, path.join(outputDir, file) ]))
            .map(async ([ src, dst ]) => {
                logger.log(`Copying ${src} -> ${dst}`);

                try {
                    await fsPromise.mkdir(path.dirname(dst), { recursive: true });
                    return await fsPromise.copyFile(src, dst);
                } catch (e) {
                    logger.error(`Could not copy file ${src} to ${dst}`); throw e;
                }
            }),

        [
            ['builder_bundle.css', 'main.css'],
        ].map(async ([file, alias]) => {
            try {
                await fsPromise.copyFile(path.join('_build_tmp', file), path.join(outputDir, alias));
                logger.log(`Copying ${file} -> ${path.join(outputDir, alias)}`);
            } catch (e) {
                logger.error(`Could not copy file ${file}`);
                throw e;
            }
        }),
    ]);

const copyOtherFiles = (files: string[], outputDir: string) =>
    Promise.all(
        files.map(async (file) => {
            try {
                await fsPromise.copyFile(file, path.join(outputDir, path.basename(file)));

                logger.log(`Copying ${file} -> ${path.join(outputDir, path.basename(file))}`);
            } catch (e) {
                logger.error(`Could not copy file ${file}`);
                throw e;
            }
        })
    );

const clean = (outputDir: string) =>
    fsPromise.rm(outputDir, { recursive: true, force: true })
        .then(() => fsPromise.mkdir(outputDir, { recursive: true }));

const build = async () => {
    const postsDir = process.env.POSTS_DIR || config.postsDir;
    const draftsDir = process.env.DRAFTS_DIR || config.draftsDir;
    const staticPagesDir = process.env.PAGES_DIR || config.staticPagesDir;
    const staticFilesDirs = process.env.STATIC_FILES_DIRS?.split(',') || config.staticFilesDirs;
    const otherFiles = process.env.OTHER_FILES?.split(',') || config.otherFiles;
    const outputDir = process.env.OUTPUT_DIR || config.outputDir;
    const pageSize = process.env.PAGE_SIZE ? parseInt(process.env.PAGE_SIZE) : config.pageSize;
    const baseUrl = process.env.BASE_URL || config.baseUrl;
    const buildDrafts = (process.env.BUILD_DRAFTS === 'true') || config.buildDrafts;
    const staticPagesMap = config.staticPages || {};

    const [_posts, _drafts, staticPages] = await Promise.all([
        loadPosts(postsDir),
        loadDrafts(draftsDir, buildDrafts),
        loadStaticPages(staticPagesMap, staticPagesDir),
    ]);

    const posts = [..._posts, ..._drafts].sort((a, b) => b.timestamp.getTime() - a.timestamp.getTime());

    await clean(outputDir);

    const [sitemap, robotsTxt, rssAtom, rssFeed, renderedPosts, renderedIndexPages, renderedStaticPages, _1, _2] = await Promise.all([
        renderSitemap(posts, baseUrl),
        renderRobotsTxt(posts, baseUrl),
        renderRssAtom(posts, baseUrl),
        renderRssFeed(posts, baseUrl),
        Promise.all(posts.map(post => renderPost(post))),
        Promise.all(chunk(posts, pageSize).map((posts: Post[], idx: number) => renderIndexPage(posts, idx))),
        Promise.all(staticPages.map((page) => renderStaticPage(page))),
        Promise.all(posts.map(post => createPostDir(post, outputDir))),
        copyStaticFiles(staticFilesDirs, outputDir),
        copyOtherFiles(otherFiles, outputDir),
    ]);

    return await Promise.all([
        writeSitemap(sitemap, outputDir),
        writeRobotsTxt(robotsTxt, outputDir),
        writeRssAtom(rssAtom, outputDir),
        writeRssFeed(rssFeed, outputDir),
        writePosts(renderedPosts, outputDir),
        writeStaticPages(renderedStaticPages, outputDir),
        writeIndexPages(renderedIndexPages, outputDir),
    ]);
};

await build();

