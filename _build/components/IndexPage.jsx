import React from 'react';

import { format } from 'date-fns';

import Layout from './Layout';
import Header from './Header';
import IndexPageFooter from './IndexPageFooter';

const PostPreview = ({ title, timestamp, excerpt, content, link }) => (
    <article>
        <h1>
            {excerpt ? <a href={link}>{title}</a> : title}
        </h1>

        <div>
            <time>{format(timestamp, 'dd MMM yyyy')}</time>
        </div>

        <content dangerouslySetInnerHTML={{ __html: excerpt || content }} />

        {excerpt ? <a role="button" className="btn btn-md btn-primary read-more" href={link}>Read more</a> : null}
    </article>
);

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
