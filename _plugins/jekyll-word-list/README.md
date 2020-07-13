## Overview

## Building

```sh
$ gem build jekyll-word-list.gemspec
```

## Using

1. Install gem:

    ```
    $ gem install jekyll-word-list-0.0.1.gem
    ```

2. Add plugin to the Jekyll's `_config.yml` file:

    ```
    plugins_dir: ./_plugins

    plugins:
      - jekyll-word-list
    ```

    Also, ignore the indexes:

    ```
    exclude:
      - _data
    ```

3. Build (or serve) your website with Jekyll

    ```
    jekyll build
    ```

4. Use the provided `_data/word_list_compact.json` together with the JS script provided to embedd search on your website. Alternatively, use `_data/word_list_full.json` for debugging.