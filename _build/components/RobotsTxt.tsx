const RobotsTxt = (_posts: any[], baseUrl: string) => `User-agent: Googlebot
Disallow: /nogooglebot/

User-agent: *
Allow: /

Sitemap: ${baseUrl}/sitemap.xml
`;

export default RobotsTxt;
