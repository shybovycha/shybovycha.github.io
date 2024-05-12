import path from 'path';

import config from './config';

const outputDir = process.env.OUTPUT_DIR || config.outputDir;

const PORT = 4000;

Bun.serve({
    port: PORT,
    async fetch(req) {
        const reqPath = new URL(req.url).pathname;
        const filePath = path.join(outputDir, reqPath.endsWith('/') ? reqPath + 'index.html' : reqPath);

        console.log('Loading file', reqPath, '->', filePath);

        const file = Bun.file(filePath);

        return new Response(file);
    },
});

console.log(`Serving site on port ${PORT}`);
