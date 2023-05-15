import React from 'react';
import { format } from 'date-fns';

const createRobotsUrl = (post, baseUrl) => `<url>
    <loc>${baseUrl}/${post.link.replaceAll(/[\/\\]+/g, '/')}</loc>
    <lastmod>${format(post.timestamp, 'yyyy-MM-dd')}</lastmod>
</url>`;

const Sitemap = (posts, baseUrl) => {
    const urls = posts.map(post => createRobotsUrl(post, baseUrl)).join('\n');

    return `<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
${urls}
</urlset>
`;
};

export default Sitemap;
