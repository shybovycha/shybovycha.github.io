import fs from 'fs';
import fsPromise from 'fs/promises';
import path from 'path';

import { parse as parseDate, isValid as isValidDate } from 'date-fns';

import matter from 'gray-matter';

import { marked } from 'marked';
import { markedHighlight } from 'marked-highlight';
import { gfmHeadingId as markedGfmHeadingId  } from 'marked-gfm-heading-id';
import { mangle as markedMangle } from 'marked-mangle';
import { markedSmartypants } from 'marked-smartypants';
import { markedXhtml } from 'marked-xhtml';

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

import {
    renderRobotsTxt,
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

marked.use({
    gfm: true, // GitHub-flavoured Markdown
});

marked.use(markedXhtml()); // self-close single tags
marked.use(markedSmartypants()); // dashes and ellipses
marked.use(markedGfmHeadingId());
marked.use(markedMangle());

marked.use(markedHighlight({
    highlight(code: string, language: string) {
        if (!Prism.languages[language]) {
            if (language) {
                logger.warn(`Can't find language "${language}" for code ${code}`);
            }

            return code;
        }

        return Prism.highlight(code, Prism.languages[language], language);
    },
}));

class Logger {
    public stream = Bun.stdout;
    private writer = this.stream.writer();
    private dateFormatter = new Intl.DateTimeFormat('en-GB', { dateStyle: 'short', timeStyle: 'medium' });

    private write(logLevel: 'INFO' | 'WARN' | 'ERROR' | 'DEBUG', ...args: any[]) {
        const message = args.map(a => typeof(a) !== 'string' ? JSON.stringify(a) : a).join(' ');
        const timestamp = this.dateFormatter.format(new Date()); // TODO: replace with Temporal.Now.instant().toLocaleString(); once Temporal API becomes available

        this.writer.write(`[${timestamp}] [${logLevel}] ${message}\n`);
    }

    log(...args: any[]) {
        this.write('INFO', ...args);
    }

    info(...args: any[]) {
        this.write('INFO', ...args);
    }

    warn(...args: any[]) {
        this.write('WARN', ...args);
    }

    error(...args: any[]) {
        this.write('ERROR', ...args);
    }

    debug(...args: any[]) {
        this.write('DEBUG', ...args);
    }

    flush() {
        this.writer.flush();
    }
}

const logger = new Logger();

const parsePostContent = (src: string, loadOptions?: LoadPostContentOptions): PostContent => {
    const options = loadOptions ? { excerpt: true, excerpt_separator: loadOptions.excerptSeparator } : {};

    const { data: frontMatter, excerpt, content } = matter(src, options);

    return {
        frontMatter,
        excerpt: excerpt && marked.parse(excerpt),
        content: marked.parse(content),
    };
};

const parsePostDate = (postPath: string, frontMatter: Record<string, any>) => {
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

const loadPost = async (absoluteFilePath: string, postDir: string): Promise<Post> => {
    const postPath = postDir ? absoluteFilePath.replace(postDir, '') : absoluteFilePath;
    const timestamp = fs.statSync(absoluteFilePath).mtimeMs;

    const cached = postCache.get(postPath);

    if (cached && timestamp === cached.timestamp) {
        return cached.post;
    }

    const src = await fsPromise.readFile(absoluteFilePath, 'utf-8').catch(e => { logger.error(`Could not find post ${absoluteFilePath}`); throw e; });
    const { frontMatter, excerpt, content } = parsePostContent(src, { excerptSeparator: '<!--more-->' });

    const postLink = path.join(path.dirname(postPath), path.basename(postPath).replace(/^(\d+)-(\d+)-(\d+)-(.+)\.(md|html?)$/, '$1/$2/$3/$4.html')).replace('\\', '/').replace(/^[\\\/]+/, '');

    logger.log(`Processing post ${postPath} -> ${postLink}`);

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

    return posts.sort((a, b) => b.timestamp.getTime() - a.timestamp.getTime());
};

const loadStaticPages = async (staticPages: Record<string, string>, staticPagesDir: string): Promise<StaticPage[]> =>
    Promise.all(
        Object.entries(staticPages)
            .map(([ filePath, outputPath ]) => ([ path.join(staticPagesDir, filePath), outputPath ]))
            .map(([ filePath, outputPath ]) =>
                fsPromise.readFile(filePath, 'utf-8')
                    .catch(e => { logger.error(`Could not find static page ${filePath}`); throw e; })
                    .then(txt => ({ ...parsePostContent(txt), outputPath }))
            )
    );

// ---

const createPostDir = (post: Post, outputDir: string) => fsPromise.mkdir(path.join(outputDir, path.dirname(post.link)), { recursive: true });

const writeSitemap = (sitemap: string, outputDir: string) => Bun.write(path.join(outputDir, 'sitemap.xml'), sitemap).catch(e => { logger.error(`Could not write sitemap`, e); throw e; });

const writeRobotsTxt = (robotsTxt: string, outputDir: string) => Bun.write(path.join(outputDir, 'robots.txt'), robotsTxt).catch(e => { logger.error(`Could not write robots.txt`, e); throw e; });

const writePosts = (renderedPosts: Post[], outputDir: string) =>
    Promise.all(renderedPosts.map(post => {
        const filePath = path.join(outputDir, post.link);

        logger.log(`Writing post ${filePath}`);

        return Bun.write(filePath, '<!DOCTYPE html>' + post.content).catch(e => { logger.error(`Could not write post ${filePath}`, e); throw e; });
    }));

const writeStaticPages = (renderedStaticPages: StaticPage[], outputDir: string) =>
    Promise.all(renderedStaticPages.map(({ content, outputPath }) => {
        const filePath = path.join(outputDir, outputPath);

        logger.log(`Writing static page ${filePath}`);

        return Bun.write(filePath, '<!DOCTYPE html>' + content).catch(e => { logger.error(`Could not write static page ${filePath}`, e); throw e; });
    }));

const writeIndexPages = (renderedIndexPages: string[], outputDir: string) =>
    Promise.all(renderedIndexPages.map((content, index) => {
        const filePath = path.join(outputDir, index == 0 ? 'index.html' : `page${index + 1}.html`);

        logger.log(`Writing page ${filePath}`);

        return Bun.write(filePath, '<!DOCTYPE html>' + content).catch(e => { logger.error(`Could not write index page ${filePath}`, e); throw e; });
    }));

const copyStaticFiles = (staticDirs: string[], outputDir: string) =>
    Promise.all([
        staticDirs
            .flatMap(dir => getFilesRec(dir))
            .map(file => ([ file, path.join(outputDir, file) ]))
            .map(([ src, dst ]) => {
                logger.log(`Copying ${src} -> ${dst}`);

                return fsPromise.mkdir(path.dirname(dst), { recursive: true })
                    .then(() => fsPromise.copyFile(src, dst))
                    .catch(e => { logger.error(`Could not copy file ${src} to ${dst}`); throw e; });
            }),

        fsPromise.copyFile('_build/main_bundle.css', path.join(outputDir, 'main.css')).catch(e => { logger.error(`Could not copy file main_bundle.css`); throw e; }),
        fsPromise.copyFile('_build/prism.min_bundle.css', path.join(outputDir, 'prism.min.css')).catch(e => { logger.error(`Could not copy file prism.min_bundle.css`); throw e; }),
    ]);

const copyOtherFiles = (files: string[], outputDir: string) =>
    Promise.all(
        files.map(file => fsPromise.copyFile(file, path.join(outputDir, path.basename(file))).then(() => logger.log(`Copying ${file} -> ${path.join(outputDir, path.basename(file))}`)).catch(e => { logger.error(`Could not copy file ${file}`); throw e; }))
    );

const clean = (outputDir: string) =>
    fsPromise.rm(outputDir, { recursive: true, force: true })
        .then(() => fsPromise.mkdir(outputDir, { recursive: true }));

const build = async () => {
    const postsDir = process.env.POSTS_DIR || config.postsDir;
    const staticPagesDir = process.env.PAGES_DIR || config.staticPagesDir;
    const staticFilesDirs = process.env.STATIC_FILES_DIRS?.split(',') || config.staticFilesDirs;
    const otherFiles = process.env.OTHER_FILES?.split(',') || config.otherFiles;
    const outputDir = process.env.OUTPUT_DIR || config.outputDir;
    const pageSize = process.env.PAGE_SIZE ? parseInt(process.env.PAGE_SIZE) : config.pageSize;
    const baseUrl = process.env.BASE_URL || config.baseUrl;
    const staticPagesMap = config.staticPages || {};

    const [posts, staticPages] = await Promise.all([
        loadPosts(postsDir),
        loadStaticPages(staticPagesMap, staticPagesDir),
    ]);

    await clean(outputDir);

    const [sitemap, robotsTxt, renderedPosts, renderedIndexPages, renderedStaticPages, _1, _2] = await Promise.all([
        renderSitemap(posts, baseUrl),
        renderRobotsTxt(posts, baseUrl),
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
        writePosts(renderedPosts, outputDir),
        writeStaticPages(renderedStaticPages, outputDir),
        writeIndexPages(renderedIndexPages, outputDir),
    ]);
};

await build();
