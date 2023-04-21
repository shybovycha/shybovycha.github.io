import React from 'react';

import { format } from 'date-fns';

import Layout from './Layout';

const PostPreview = ({ title, timestamp, excerpt, content, link }) => {
    return (
        <article>
            <h1>{excerpt ? <a href={link}>{title}</a> : title}</h1>

            <time>{format(timestamp, 'dd MMM yyyy')}</time>

            <content dangerouslySetInnerHTML={{ __html: excerpt || content }} />

            {excerpt ? <a role="button" className="read-more" href={link}>Read more</a> : null}
        </article>
    );
};

const IndexPage = ({ posts, pageIndex }) => {
    const postPreviews = posts.map(post => (<PostPreview key={post.link} {...post} />));

    return (
        <Layout>
            <div className="content">
                <main>
                    {postPreviews}
                </main>
            </div>
        </Layout>
    );
};

export default IndexPage;
