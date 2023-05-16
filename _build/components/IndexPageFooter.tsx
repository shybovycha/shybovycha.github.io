import React from 'react';

const PreviousPage = ({ currentPage }: { currentPage: number }) => {
    if (currentPage === 0) {
        return null;
    }

    const previousUrl = currentPage === 1 ? '/' : `/page${ currentPage }.html`;

    return (
        <div className="prev">
            <a href={previousUrl} className="prev">&larr; Previous page</a>
        </div>
    );
};

const NextPage = ({ currentPage, pages }: { currentPage: number, pages: number }) => {
    if (currentPage === pages - 1) {
        return null;
    }

    return (
        <div className="next">
            <a href={`/page${ currentPage + 2 }.html`} className="next">Next page &rarr;</a>
        </div>
    );
};

const IndexPageFooter = ({ currentPage, pages }: { currentPage: number, pages: number }) => {
    return (
        <footer>
            <nav>
                <PreviousPage currentPage={ currentPage } />

                <div className="current">Page #{`${ currentPage + 1 }`} of {`${ pages }`}</div>

                <NextPage currentPage={ currentPage } pages={ pages } />
            </nav>
        </footer>
    );
};

export default IndexPageFooter;
