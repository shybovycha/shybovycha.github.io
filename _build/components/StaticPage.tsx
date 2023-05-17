import React from 'react';

import Layout from './Layout';
import Header from './Header';

import '../styles/main.css';

export interface StaticPageProps {
    content: string;
}

const StaticPage = ({ content }: StaticPageProps) => {
    return (
        <Layout header={<Header />}>
            <article>
                <div className="content" dangerouslySetInnerHTML={{ __html: content }}></div>
            </article>
        </Layout>
    );
};

export default StaticPage;
