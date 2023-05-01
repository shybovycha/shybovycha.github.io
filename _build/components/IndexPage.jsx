import React from 'react';

import { format } from 'date-fns';

import Layout from './Layout';
import Header from './Header';
import IndexPageFooter from './IndexPageFooter';

const PostPreview = ({ title, timestamp, excerpt, content, link }) => {
    return (
        <article>
            <h1>
                <a href={link}>{excerpt ? <a href={link}>{title}</a> : title}</a>
            </h1>

            <time>{format(timestamp, 'dd MMM yyyy')}</time>

            <content dangerouslySetInnerHTML={{ __html: excerpt || content }} />

            {excerpt ? <a role="button" className="btn btn-md btn-primary read-more" href={link}>Read more</a> : null}
        </article>
    );
};

const IndexPage = ({ posts, pageIndex }) => {
    const postPreviews = posts.map(post => (<PostPreview key={post.link} {...post} />));

    const header = <Header isHome={ pageIndex === undefined || pageIndex === 0 } />;

    const footer = <IndexPageFooter pages={ posts.length } currentPage={ pageIndex } />;

    return (
        <Layout header={header} footer={footer}>
            {postPreviews}
        </Layout>
    );
};

export default IndexPage;
