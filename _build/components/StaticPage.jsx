import React from 'react';

import Layout from './Layout';
import Header from './Header';

import * as style from '../styles/main.css';

const StaticPage = ({ content }) => {
    return (
        <Layout header={<Header />}>
            <article>
                <content dangerouslySetInnerHTML={{ __html: content }} />
            </article>
        </Layout>
    );
};

export default StaticPage;
