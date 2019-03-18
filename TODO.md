## Design

* Header - 64px high
* Font-size - 21px, header - 42px
* Main content - 780px wide
* Sticky side navbar (tags? post info? "about me"? notes?)

```scss
.posts {
    display: flex;
    justify-content: center;

    .posts-wrapper {
        max-width: 780px;
    }
}

.post {
    font-size: 21px;

    h1 {
        font-size: 42px;
        font-weight: 400;
    }

    p {
        margin: 1em;
    }
}

nav.header {
    height: 64px;
    font-size: 18px;
}
```

## Features

* Markdown
* HTML
* Syntax highlighting
* Tags
* Timestamps
* Images (responsive, retina, GIF / WEBP)
* Links to other pages, table of contents
* Drafts
* Optimized CSS & JS
* Lazy loading images
* Optimize images for fast loading (progressive JPEG)

## Build

```ruby
require 'redcarpet'

renderer = Redcarpet::Render::HTML.new
markdown = Redcarpet::Markdown.new(renderer, fenced_code_blocks: true, no_intra_emphasis: true, disable_indented_code_blocks: true)

Dir['./*.md'].reject(['.', '..']).each do |filename|
    content = File.read(filename)
end

Dir['./_posts/**/*'].reject(['.', '..']).each do |filename|
    file_content = File.readlines(filename)

    if file_content[0] == '---'
        front_matter = file_content.drop(1).take_while { |line| line.strip != '---' }
        content = file_content.drop(1 + front_matter.length)
    end

    excerpt = file_content.take_while { |line| line.strip != '<!--more-->' }

    filename_re = /^((\d{4})-(\d{2})-(\d{2}))-(.+)\.(\w+)$/

    basename = File.basename(filename)
    extension = File.extension(filename)

    timestamp = filename.gsub filename_re, '\1'
    slug = filename.gsub filename_re, '\5'

    if ['md', 'markdown'].include? extension
        body = markdown.render(content)
        short_body = markdown.render(excerpt)
    else
        body = content
        short_body = excerpt
    end
end
```
