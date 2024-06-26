import path from 'path';
import fsPromise from 'fs/promises';

import { parse as parseDate, isValid as isValidDate } from 'date-fns';
// import { chunk, isString } from 'lodash';

import { evaluate } from '@mdx-js/mdx';

import React from 'react';
import * as runtime from 'react/jsx-runtime';
import ReactDOMServer from 'react-dom/server';

// layouts
import PostPage from './components/PostPage';

// plugins
import rehypePrism from 'rehype-prism-plus';
import { remarkExtendedTable } from 'remark-extended-table';
import remarkGfm from 'remark-gfm';
import remarkFrontmatter from 'remark-frontmatter';

// import 'prismjs/components/prism-c';
// import 'prismjs/components/prism-coffeescript';
// import 'prismjs/components/prism-latex';
// import 'prismjs/components/prism-kotlin';
// import 'prismjs/components/prism-cpp';
// import 'prismjs/components/prism-clike';
// import 'prismjs/components/prism-ruby';
// import 'prismjs/components/prism-haskell';
// import 'prismjs/components/prism-elm';
// import 'prismjs/components/prism-purescript';
// import 'prismjs/components/prism-antlr4';
// import 'prismjs/components/prism-bash';
// import 'prismjs/components/prism-cmake';
// import 'prismjs/components/prism-clojure';
// import 'prismjs/components/prism-csharp';
// import 'prismjs/components/prism-avro-idl';
// import 'prismjs/components/prism-basic';
// import 'prismjs/components/prism-crystal';
// import 'prismjs/components/prism-json';
// import 'prismjs/components/prism-jsx';
// import 'prismjs/components/prism-javascript';
// import 'prismjs/components/prism-java';
// import 'prismjs/components/prism-sql';
// import 'prismjs/components/prism-squirrel';
// import 'prismjs/components/prism-chaiscript';
// import 'prismjs/components/prism-lua';
// import 'prismjs/components/prism-markdown';
// import 'prismjs/components/prism-nasm';
// import 'prismjs/components/prism-markup-templating';
// import 'prismjs/components/prism-php';
// import 'prismjs/components/prism-erlang';
// import 'prismjs/components/prism-scala';
// import 'prismjs/components/prism-python';
// import 'prismjs/components/prism-zig';
// import 'prismjs/components/prism-vim';
// import 'prismjs/components/prism-typescript';
// import 'prismjs/components/prism-swift';
// import 'prismjs/components/prism-rust';
// import 'prismjs/components/prism-css';
// import 'prismjs/components/prism-sass';
// import 'prismjs/components/prism-less';
// import 'prismjs/components/prism-regex';
// import 'prismjs/components/prism-reason';
// import 'prismjs/components/prism-protobuf';
// import 'prismjs/components/prism-prolog';
// import 'prismjs/components/prism-ini';
// import 'prismjs/components/prism-go';
// import 'prismjs/components/prism-groovy';
// import 'prismjs/components/prism-git';
// import 'prismjs/components/prism-glsl';
// import 'prismjs/components/prism-flow';
// import 'prismjs/components/prism-elixir';
// import 'prismjs/components/prism-d';
// import 'prismjs/components/prism-ocaml';
// import 'prismjs/components/prism-reason';
// import 'prismjs/components/prism-fsharp';
// import 'prismjs/components/prism-diff';
// import 'prismjs/components/prism-handlebars';
// import 'prismjs/components/prism-pug';
// import 'prismjs/components/prism-yaml';
// import 'prismjs/components/prism-dot';
// import 'prismjs/components/prism-scss';
// import 'prismjs/components/prism-less';
// import 'prismjs/components/prism-gherkin';
// import 'prismjs/components/prism-lisp';

const getPostDate = (postPath: string) => {
    const fileDate = path.basename(postPath).replace(/^(\d{4}-\d{2}-\d{2}).+$/, '$1');
    const fallbackDate = parseDate(fileDate, 'yyyy-MM-dd', new Date());

    if (fallbackDate && isValidDate(fallbackDate)) {
        return fallbackDate;
    }

    return new Date();
};

const filePath = '_posts/2024-01-04-how-unique-are-your-bundles.md';

const source = await fsPromise.readFile(filePath);
const { default: PostContent } = await evaluate(source, { ...runtime, rehypePlugins: [ rehypePrism ], remarkPlugins: [ remarkGfm, remarkFrontmatter, remarkExtendedTable ] });
const html = ReactDOMServer.renderToStaticMarkup(<PostContent />);
const result = ReactDOMServer.renderToStaticMarkup(<PostPage timestamp={getPostDate(filePath)} title="Test" content={html} />);

console.log(result);
