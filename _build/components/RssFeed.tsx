import { format as formatDate } from 'date-fns';
import { XMLBuilder } from 'fast-xml-parser';
import { v5 as uuidv5 } from 'uuid';

import { Post } from '../render';

const formatRFC822 = (date: Date) => formatDate(date, 'EEE, dd MMM yyyy HH:mm:ss xxxx');

const createRssItem = (post: Post, baseUrl: string) => {
    const { title } = post;

    const builder = new XMLBuilder({
        ignoreAttributes: false,
        format: true,
        preserveOrder: true,
        processEntities: true,
    });

    return builder.build([
        {
            item: [
                {
                    title: [
                        {
                            '#text': title,
                        },
                    ],
                },
                {
                    link: [
                        {
                            '#text': `${baseUrl}/${post.link.replaceAll(/[\/\\]+/g, '/')}`,
                        },
                    ],
                },
                {
                    guid: [
                        {
                            '#text': `urn:uuid:${uuidv5(post.link, uuidv5.URL)}`,
                        },
                    ],
                    ':@': {
                        '@_isPermaLink': false,
                    },
                },
                {
                    pubDate: [
                        {
                            '#text': formatRFC822(post.timestamp),
                        },
                    ],
                },
                {
                    description: [
                        {
                            '#text': title,
                        },
                    ],
                },
                {
                    author: [
                        {
                            '#text': 'shybovycha@gmail.com (Artem Shubovych)',
                        },
                    ],
                },
            ],
        },
    ]);
};

const RssFeed = (posts: Post[], baseUrl: string) => {
    const items = posts.map(post => createRssItem(post, baseUrl)).join('\n');

    return `<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
<channel>
    <title>MooFoo blogs</title>
    <description>Artem Shubovych' blog.</description>
    <link>${baseUrl}</link>
    <atom:link href="${baseUrl}/feed.rss" rel="self" type="application/rss+xml" />
    <atom:link href="${baseUrl}/atom.xml" rel="alternate" type="application/rss+xml" />
    <copyright>${new Date().getFullYear()} Artem Shubovych, All rights reserved</copyright>
    <lastBuildDate>${formatRFC822(new Date())}</lastBuildDate>
    <pubDate>${formatRFC822(new Date())}</pubDate>
    <ttl>1800</ttl>
    ${items}
</channel>
</rss>
`;
};

export default RssFeed;
