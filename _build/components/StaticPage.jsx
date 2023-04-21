import React from 'react';

import Layout from './Layout';

const StaticPage = ({ title, content }) => {
    return (
        <Layout title={title}>
            <article>
                <h1>{title}</h1>

                <content dangerouslySetInnerHTML={{ __html: content }} />
            </article>
        </Layout>
    );
};

export default StaticPage;
