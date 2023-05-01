import React from 'react';

import classname from 'classnames';

import * as style from '../styles/main.css';

const Header = ({ isHome, isAbout }) => (
    <nav className="top">
        <div className="links">
            <a className={classname('nav-link', 'nav-item', { 'active': isHome })} href="/">Home</a>

            <a className={classname('nav-link', 'nav-item', { 'active': isAbout })} href="/about/">About</a>
        </div>
    </nav>
);

export default Header;
