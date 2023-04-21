import React from 'react';

import { format } from 'date-fns';

import Layout from './Layout';

const PostPage = ({ title, timestamp, content }) => {
    return (
        <Layout title={title}>
            <article>
                <h1>{title}</h1>

                <time>{format(timestamp, 'dd MMM yyyy')}</time>

                <content dangerouslySetInnerHTML={{ __html: content }} />
            </article>
        </Layout>
    );
};

export default PostPage;
