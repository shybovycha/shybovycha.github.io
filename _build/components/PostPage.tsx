import React from 'react';

import { format } from 'date-fns';

import Header from './Header';
import Layout from './Layout';

export interface PostPageProps {
    title: string;
    timestamp: Date;
    content: React.ReactNode | React.ReactNode[] | string;
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

                <content dangerouslySetInnerHTML={{ __html: content }} />
            </article>
        </Layout>
    );
};

export default PostPage;
