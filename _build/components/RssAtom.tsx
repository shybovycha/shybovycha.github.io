import { formatISO } from 'date-fns';
import { XMLParser, XMLBuilder } from 'fast-xml-parser';
import { v5 as uuidv5 } from 'uuid';

import { Post } from '../render';

const createRssEntry = (post: Post, baseUrl: string) => {
    const { content, title, excerpt } = post;

    const parser = new XMLParser({
        ignoreAttributes: false,
        preserveOrder: true,
        processEntities: true,
        htmlEntities: true,
        unpairedTags: ['img', 'hr', 'br'],
        stopNodes: ['*.pre', '*.code'],
        updateTag(tagName, _jsonPath, attributes) {
            if (tagName.toLowerCase() === 'script' || tagName.toLowerCase() === 'style') {
                return false;
            }

            if (attributes) {
                if (attributes['@_class']) {
                    delete attributes['@_class'];
                }

                if (attributes['@_style']) {
                    delete attributes['@_style'];
                }
            }

            return true;
        },
    });

    try {
        const contentXml = parser.parse(content);
        const excerptXml = excerpt && parser.parse(excerpt);

        const builder = new XMLBuilder({
            ignoreAttributes: false,
            format: true,
            preserveOrder: true,
            processEntities: true,
        });

        return builder.build([
            {
                entry: [
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
                        id: [
                            {
                                '#text': `urn:uuid:${uuidv5(post.link, uuidv5.URL)}`,
                            },
                        ],
                    },
                    {
                        published: [
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
                        summary:
                            excerpt
                                ? [{ div: excerptXml, ':@': { '@_xmlns': 'http://www.w3.org/1999/xhtml' } }]
                                : [{ '#text': title }],
                        ':@': {
                            '@_type': excerpt ? 'xhtml' : 'text',
                        },
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
                    {
                        content: [
                            {
                                div: contentXml,
                                ':@': {
                                    '@_xmlns': 'http://www.w3.org/1999/xhtml',
                                },
                            },
                        ],
                        ':@': {
                            '@_type': 'xhtml',
                        },
                    },
                ],
            },
        ]);
    } catch (e) {
        console.error(`Failed to create RSS atom from post`, post, `because of an error`, e);
        return '';
    }
};

const RssAtom = (posts: Post[], baseUrl: string) => {
    const entries = posts.map(post => createRssEntry(post, baseUrl)).join('\n');

    return `<?xml version="1.0" encoding="UTF-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
    <title>MooFoo blogs</title>
    <subtitle>Artem Shubovych' blog.</subtitle>
    <link href="${baseUrl}/atom.xml" rel="self" />
    <link href="${baseUrl}" />
    <id>urn:uuid:${uuidv5(baseUrl, uuidv5.URL)}</id>
    <updated>${formatISO(new Date())}</updated>
    ${entries}
</feed>
`;
};

export default RssAtom;
