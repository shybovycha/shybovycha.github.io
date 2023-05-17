import React from 'react';

import { format } from 'date-fns';

import Header from './Header';
import Layout from './Layout';

export interface PostPageProps {
    title: string;
    timestamp: Date;
    content: string;
}

const PostPage = ({ title, timestamp, content }: PostPageProps) => {
    const header = <Header />;

    return (
        <Layout title={title} header={header}>
            <article>
                <h1>{title}</h1>

                <div>
                    <time>{format(timestamp, 'dd MMM yyyy')}</time>
                </div>

                <div className="content" dangerouslySetInnerHTML={{ __html: content }}></div>
            </article>
        </Layout>
    );
};

export default PostPage;
