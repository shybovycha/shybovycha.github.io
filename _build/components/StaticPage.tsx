import React from 'react';

import Layout from './Layout';
import Header from './Header';

import '../styles/main.css';

export interface StaticPageProps {
    content?: React.ReactNode | React.ReactNode[] | string;
}

const StaticPage = ({ content }: StaticPageProps) => {
    return (
        <Layout header={<Header />}>
            <article>
                <content dangerouslySetInnerHTML={{ __html: content }} />
            </article>
        </Layout>
    );
};

export default StaticPage;
