import React from 'react';

import { format } from 'date-fns';

import Layout from './Layout';
import Header from './Header';
import IndexPageFooter from './IndexPageFooter';

import { Post } from '../render';

export interface PostPreviewProps {
    title: string;
    timestamp: Date;
    excerpt?: string;
    content: string;
    link: string;
}

export interface IndexPageProps {
    posts: Post[];
    pageIndex: number;
}

const PostPreview = ({ title, timestamp, excerpt, content, link }: PostPreviewProps) => (
    <article>
        <h1>
            <a href={link}>{title}</a>
        </h1>

        <div>
            <time>{format(timestamp, 'dd MMM yyyy')}</time>
        </div>

        <div className="content" dangerouslySetInnerHTML={{ __html: excerpt || content }}></div>

        {excerpt ? <a role="button" className="btn btn-md btn-primary read-more" href={link}>Read more</a> : null}
    </article>
);

const IndexPage = ({ posts, pageIndex }: IndexPageProps) => {
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
