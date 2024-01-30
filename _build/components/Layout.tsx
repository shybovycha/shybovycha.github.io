import React from 'react';

import 'prismjs/themes/prism.min.css';
import '../styles/main.css';

export interface LayoutProps {
    title?: React.ReactNode | string;
    head?: React.ReactNode | React.ReactNode[];
    header?: React.ReactNode | React.ReactNode[];
    footer?: React.ReactNode | React.ReactNode[];
    children?: React.ReactNode | React.ReactNode[];
}

const Layout = ({ title, head = null, header = null, footer = null, children }: LayoutProps) => (
    <html lang="en">
    <head>
        <meta charSet="UTF-8" />
        <meta httpEquiv="X-UA-Compatible" content="IE=edge" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <title>{title}</title>

        <link rel="icon" href="/images/favicon-compressed.webp" />
        <link rel="shortcut icon" href="/images/favicon-compressed.webp" />
        <link rel="apple-touch-icon" href="/images/favicon-compressed.webp" />

        <link rel="preload" href="/main.css" as="style" />

        <link rel="stylesheet" href="/main.css" />
        <link rel="stylesheet" href="/prism.min.css" />

        {head}
    </head>
    <body>
        {header}

        <main>
            {children}
        </main>

        {footer}

        <script async src="https://www.googletagmanager.com/gtag/js?id=G-FHHSX9LYS3"></script>
        <script src="/js/google_analytics.js"></script>
        <script src="/js/additional_gtag_analytics.js"></script>
    </body>
    </html>
);

export default Layout;
