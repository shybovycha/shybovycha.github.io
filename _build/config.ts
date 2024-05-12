export interface Config {
    baseUrl: string;
    outputDir: string;
    pageSize: number;
    postsDir: string;
    draftsDir: string;
    buildDrafts: boolean;
    staticFilesDirs: string[];
    staticPagesDir: string;
    staticPages: Record<string, string>;
    otherFiles: string[];
}

const config: Config = {
    postsDir: '_posts',
    draftsDir: '_drafts',
    buildDrafts: false,
    staticPagesDir: '.',
    staticFilesDirs: [
        'images',
        'tumblr_files',
        'js',
    ],
    otherFiles: [
        'google92735c11764b132a.html',
    ],
    staticPages: {
        'about.md': '/about.html',
    },
    outputDir: '_site',
    pageSize: 10,
    baseUrl: 'https://shybovycha.github.io',
};

export default config;
