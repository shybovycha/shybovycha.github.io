import fs from 'fs';
import fsPromise from 'fs/promises';
import path from 'path';

import { parse as parseDate, isValid as isValidDate } from 'date-fns';

import matter from 'gray-matter';
import { marked } from 'marked';

import { chunk } from 'lodash';

import Prism from'prismjs';

import 'prismjs/components/prism-c';
import 'prismjs/components/prism-cpp';
import 'prismjs/components/prism-clike';
import 'prismjs/components/prism-ruby';
import 'prismjs/components/prism-haskell';
import 'prismjs/components/prism-elm';
import 'prismjs/components/prism-purescript';
import 'prismjs/components/prism-antlr4';
import 'prismjs/components/prism-bash';
import 'prismjs/components/prism-cmake';
import 'prismjs/components/prism-clojure';
import 'prismjs/components/prism-csharp';
import 'prismjs/components/prism-avro-idl';
import 'prismjs/components/prism-basic';
import 'prismjs/components/prism-crystal';
import 'prismjs/components/prism-json';
import 'prismjs/components/prism-jsx';
import 'prismjs/components/prism-javascript';
import 'prismjs/components/prism-java';
import 'prismjs/components/prism-sql';
import 'prismjs/components/prism-squirrel';
import 'prismjs/components/prism-chaiscript';
import 'prismjs/components/prism-lua';
import 'prismjs/components/prism-markdown';
import 'prismjs/components/prism-nasm';
import 'prismjs/components/prism-markup-templating';
import 'prismjs/components/prism-php';
import 'prismjs/components/prism-erlang';
import 'prismjs/components/prism-scala';
import 'prismjs/components/prism-python';
import 'prismjs/components/prism-zig';
import 'prismjs/components/prism-vim';
import 'prismjs/components/prism-typescript';
import 'prismjs/components/prism-swift';
import 'prismjs/components/prism-rust';
import 'prismjs/components/prism-css';
import 'prismjs/components/prism-sass';
import 'prismjs/components/prism-less';
import 'prismjs/components/prism-regex';
import 'prismjs/components/prism-reason';
import 'prismjs/components/prism-protobuf';
import 'prismjs/components/prism-prolog';
import 'prismjs/components/prism-ini';
import 'prismjs/components/prism-go';
import 'prismjs/components/prism-groovy';
import 'prismjs/components/prism-git';
import 'prismjs/components/prism-glsl';
import 'prismjs/components/prism-flow';
import 'prismjs/components/prism-elixir';
import 'prismjs/components/prism-d';
import 'prismjs/components/prism-ocaml';
import 'prismjs/components/prism-fsharp';
import 'prismjs/components/prism-diff';
import 'prismjs/components/prism-handlebars';
import 'prismjs/components/prism-pug';
import 'prismjs/components/prism-yaml';
import 'prismjs/components/prism-dot';
import 'prismjs/components/prism-scss';
import 'prismjs/components/prism-less';
import 'prismjs/components/prism-gherkin';

import { renderRobotsTxt, renderSitemap, renderIndexPage, renderPost, renderStaticPage } from './render';

marked.setOptions({
    gfm: true, // GitHub-flavoured Markdown
    xhtml: true, // self-close single tags
    smartypants: true, // dashes and ellipses

    highlight(code, language) {
        if (!Prism.languages[language]) {
            if (language) {
                console.warn(`Can't find language "${language}" for code ${code}`);
            }

            return code;
        }

        return Prism.highlight(code, Prism.languages[language], language);
    },
});

const loadPostContent = (src, { excertpSeparator = null } = {}) => {
    const options = excertpSeparator ? { excerpt: true, excerpt_separator: '<!--more-->' } : {};

    const { data: frontMatter, excerpt, content } = matter(src, options);

    return {
        frontMatter,
        excerpt: marked.parse(excerpt),
        content: marked.parse(content),
    };
};

const parsePostDate = (postPath, frontMatter) => {
    const fileDate = path.basename(postPath).replace(/^(\d{4}-\d{2}-\d{2}).+$/, '$1');
    const fallbackDate = parseDate(fileDate, 'yyyy-MM-dd', new Date());

    const frontMatterDate = frontMatter.date ? parseDate(frontMatter.date, 'yyyy-MM-dd HH:mm:ss XXXXX', new Date()) : null;

    if (frontMatterDate && isValidDate(frontMatterDate)) {
        return frontMatterDate;
    }

    if (fallbackDate && isValidDate(fallbackDate)) {
        return fallbackDate;
    }

    return new Date();
};

const postCache = new Map();

const loadPost = (absoluteFilePath, postDir) => {
    const postPath = postDir ? absoluteFilePath.replace(postDir, '') : absoluteFilePath;
    const timestamp = fs.statSync(absoluteFilePath).mtimeMs;

    const cached = postCache.get(postPath);

    if (cached && timestamp === cached.timestamp) {
        return cached.post;
    }

    const src = fs.readFileSync(absoluteFilePath, 'utf-8');
    const { frontMatter, excerpt, content } = loadPostContent(src, { excertpSeparator: '<!--more-->' });

    const postLink = path.join(path.dirname(postPath), path.basename(postPath).replace(/^(\d+)-(\d+)-(\d+)-(.+)\.(md|html?)$/, '$1/$2/$3/$4.html')).replace('\\', '/').replace(/^[\\\/]+/, '');

    console.log('Processing post', postPath, '->', postLink);

    const post = {
        title: frontMatter.title,
        link: postLink,
        timestamp: parsePostDate(postPath, frontMatter),
        excerpt,
        content,
    };

    postCache.set(postPath, {
        timestamp,
        post
    });

    return post;
};

const getFilesRec = (dir) => {
    if (!fs.existsSync(dir) || !fs.lstatSync(dir).isDirectory()) {
        return [];
    }

    const result = [];
    const queue = [dir];

    while (queue.length > 0) {
        const p = queue.shift();

        const stat = fs.lstatSync(p);

        if (stat.isDirectory()) {
            fs.readdirSync(p).forEach(f => queue.push(path.join(p, f)));
        } else {
            result.push(p);
        }
    }

    return result;
};

const loadPosts = (postDir) => {
    if (postCache.size > 0) {
        return Object.values(postCache).sort((a, b) => b.timestamp - a.timestamp);
    }

    return getFilesRec(postDir)
        .map((file) => loadPost(file, postDir))
        .sort((a, b) => b.timestamp - a.timestamp);;
};

const loadPages = (pagesDir) => getFilesRec(pagesDir);

// ---

const createPostDir = (post, outputDir) => fsPromise.mkdir(path.join(outputDir, path.dirname(post.link)), { recursive: true });

const writeSitemap = (sitemap, outputDir) => fsPromise.writeFile(path.join(outputDir, 'sitemap.xml'), sitemap);

const writeRobotsTxt = (robotsTxt, outputDir) => fsPromise.writeFile(path.join(outputDir, 'robots.txt'), robotsTxt);

const writePosts = (renderedPosts, outputDir) =>
    Promise.all(renderedPosts.map(post => {
        const filePath = path.join(outputDir, post.link);

        console.log('Writing post', filePath);

        return fsPromise.writeFile(filePath, post.content);
    }));

const writeStaticPages = (staticPages, outputDir) => Promise.resolve(null);

const writeIndexPages = (renderedIndexPages, outputDir) =>
    Promise.all(renderedIndexPages.map((content, index) => {
        const filePath = path.join(outputDir, index == 0 ? 'index.html' : `page${index + 1}.html`);

        console.log('Writing page', filePath);

        return fsPromise.writeFile(filePath, content);
    }));

const copyStaticFiles = (outputDir) =>
    Promise.all([
        ...[
            ...getFilesRec('images'),
            ...getFilesRec('tumblr_files')
        ]
        .map(file => ([ file, path.join(outputDir, file) ]))
        .map(([ src, dst ]) => fsPromise.mkdir(path.dirname(dst), { recursive: true }).then(() => fsPromise.copyFile(src, dst))),

        fsPromise.copyFile('_build/builder_bundle.css', path.join(outputDir, 'main.css')),
        fsPromise.copyFile('_build/static.js', path.join(outputDir, 'static.js')),
    ]);

const clean = (outputDir) =>
    fsPromise.rm(outputDir, { recursive: true, force: true })
        .then(() => fsPromise.mkdir(outputDir, { recursive: true }));

const build = async () => {
    // TODO: extract config file
    const postsDir = process.env.POSTS_DIR || '_posts';
    const staticPagesDir = process.env.PAGES_DIR || '_pages';
    // const layoutsDir = process.env.LAYOUTS_DIR || '_layouts';
    const staticFilesDir = process.env.STATIC_FILES_DIR || 'public';
    const outputDir = process.env.OUTPUT_DIR || 'out';
    const pageSize = process.env.PAGE_SIZE || 10;

    const posts = loadPosts(postsDir);
    const staticPages = loadPages(staticPagesDir);

    await clean(outputDir);

    const [sitemap, robotsTxt, renderedPosts, renderedIndexPages, renderedStaticPages, _] = await Promise.all([
        renderSitemap(posts),
        renderRobotsTxt(posts),
        Promise.all(posts.map(post => renderPost(post))),
        Promise.all(chunk(posts, pageSize).map((posts, idx) => renderIndexPage(posts, idx))),
        Promise.all(staticPages.map(page => renderStaticPage(page))),
        Promise.all(posts.map(post => createPostDir(post, outputDir))),
    ]);

    return await Promise.all([
        writeSitemap(sitemap, outputDir),
        writeRobotsTxt(robotsTxt, outputDir),
        writePosts(renderedPosts, outputDir),
        writeStaticPages(renderedStaticPages, outputDir),
        writeIndexPages(renderedIndexPages, outputDir),
        copyStaticFiles(outputDir),
    ]);
};

build();
