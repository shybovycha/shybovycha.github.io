import React from 'react';

import classname from 'classnames';

import '../styles/main.css';

const Header = ({ isHome, isAbout }: { isHome?: boolean, isAbout?: boolean }) => (
    <nav className="top">
        <div className="links">
            <a className={classname('nav-link', 'nav-item', { 'active': isHome })} href="/">Home</a>

            <a className={classname('nav-link', 'nav-item', { 'active': isAbout })} href="/about.html">About</a>
        </div>
    </nav>
);

export default Header;
