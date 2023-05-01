import React from 'react';

import { format } from 'date-fns';

import Header from './Header';
import Layout from './Layout';

const PostPage = ({ title, timestamp, content }) => {
    const header = <Header />;

    return (
        <Layout title={title} header={header}>
            <article>
                <h1>{title}</h1>

                <div>
                    <time>{format(timestamp, 'dd MMM yyyy')}</time>
                </div>

                <content dangerouslySetInnerHTML={{ __html: content }} />
            </article>
        </Layout>
    );
};

export default PostPage;
