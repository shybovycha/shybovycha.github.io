import React from 'react';

import * as prismStyle from 'prismjs/themes/prism.min.css';
import * as style from '../styles/main.css';

const Layout = ({  title, head = null, header = null, footer = null, children }) => (
    <html lang="en">
    <head>
        <meta charSet="UTF-8" />
        <meta httpEquiv="X-UA-Compatible" content="IE=edge" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <title>{title}</title>

        <link rel="icon" href="/images/favicon-compressed.webp" />
        <link rel="shortcut icon" href="/images/favicon-compressed.webp" />
        <link rel="apple-touch-icon" href="/images/favicon-compressed.webp" />

        {head}
    </head>
    <body>
        <link rel="preload" href="/main.css" as="style" />
        <noscript><link rel="stylesheet" href="/main.css" /></noscript>

        {header}

        {children}

        {footer}

        <script src="/static.js"></script>
    </body>
    </html>
);

export default Layout;
