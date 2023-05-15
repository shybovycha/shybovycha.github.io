import React from 'react';

const RobotsTxt = (_posts, baseUrl) => `User-agent: Googlebot
Disallow: /nogooglebot/

User-agent: *
Allow: /

Sitemap: ${baseUrl}/sitemap.xml
`;

export default RobotsTxt;
