import { formatISO } from 'date-fns';
import { XMLBuilder } from 'fast-xml-parser';
import { v5 as uuidv5 } from 'uuid';

import { Post } from '../render';

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
                    link: [],
                    ':@': {
                        '@_href': `${baseUrl}/${post.link.replaceAll(/[\/\\]+/g, '/')}`,
                    },
                },
                {
                    guid: [
                        {
                            '#text': `urn:uuid:${uuidv5(post.link, uuidv5.URL)}`,
                        },
                    ],
                    ':@': {
                        '@_isPermalink': false,
                    },
                },
                {
                    pubDate: [
                        {
                            '#text': formatISO(post.timestamp),
                        },
                    ],
                },
                {
                    updated: [
                        {
                            '#text': formatISO(post.timestamp),
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
                            name: [
                                {
                                    '#text': 'Artem Shubovych',
                                },
                            ],
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
<rss version="2.0">
<channel>
    <title>MooFoo blogs</title>
    <description>Artem Shubovych' blog.</description>
    <link>${baseUrl}</link>
    <copyright>${new Date().getFullYear()} Artem Shubovych, All rights reserved</copyright>
    <id>urn:uuid:${uuidv5(baseUrl, uuidv5.URL)}</id>
    <lastBuildDate>${formatISO(new Date())}</lastBuildDate>
    <pubDate>${formatISO(new Date())}</pubDate>
    <ttl>1800</ttl>
    ${items}
</channel>
</rss>
`;
};

export default RssFeed;
