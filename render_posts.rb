require 'liquid'
require 'rouge'
require 'redcarpet'
require 'yaml'

POST_FILE_EXTENSIONS = %w(htm html md markdown)

class Processor
  def initialize
    @post_files = find_posts('_posts')
    @draft_files = find_posts('_drafts')
    # @include_files = find_posts('_includes')

    @renderer = UberRenderer.new
  end

  def render_all(output_dir, options = { drafts: false })
    Dir.mkdir output_dir unless Dir.exists? output_dir

    to_render = @post_files
    to_render += @draft_files if options[:drafts]

    to_render.map do |f|
      output_filename = "#{output_dir}/#{File.basename(f, File.extname(f))}.html"

      File.write(output_filename, @renderer.render(f))

      output_filename
    end
  end

  private

  def find_posts(path)
    path_filters = POST_FILE_EXTENSIONS.map { |e| "**/*.#{e}" }

    Dir[ *path_filters.map { |f| "#{path}/#{f}" } ]
  end
end

class UberRenderer
  def initialize
    @markdown_renderer = Redcarpet::Markdown.new(MarkdownRenderer, autolink: true, tables: true, strikethrough: true, underline: true, disable_indented_code_blocks: true, fenced_code_blocks: true)
  end

  def render(path)
    lines = File.readlines path

    lines = lines.drop_while { |l| l.nil? or l.strip.empty? }

    properties = {}

    if lines[0].strip == '---'
      front_matter = ''
      lines.shift

      until lines.empty? or lines[0].strip == '---' do
        front_matter += lines.shift
      end

      lines.shift

      properties = YAML.load front_matter
    end

    source = lines.join

    extension = File.extname path

    template = Liquid::Template.parse source

    unless template.errors.empty?
      puts "Could not parse Liquid template #{path}"
      puts template.errors.inspect
      return nil
    end

    processed_source = template.render

    processed_source = render_markdown(processed_source) if extension == '.md' or extension == '.markdown'

    meta = properties.entries.map do |key, value|
      value_str = value

      value_str = value.join(',') if value.is_a? Array

      "<meta name=\"#{key}\" content=\"#{value_str}\">\n"
    end.join

    meta + processed_source
  end

  private

  def render_markdown(source)
    @markdown_renderer.render source
  end
end

class MarkdownRenderer < Redcarpet::Render::HTML
  def block_code(code, language)
    formatter = Rouge::Formatters::HTML.new
    lexer = Rouge::Lexer.find language

    lexer = Rouge::Lexer.guess(source: code) if lexer.nil?

    if lexer.nil?
      puts "Could not find Rouge lexer for '#{language}' language and code\n===BEGIN CODE===\n#{code}\n===END CODE===\n\n"
      lexer = Rouge::Lexers::PlainText
    end

    formatter.format(lexer.lex(code))
  end
end

class IncludeTag < Liquid::Tag
  def initialize(name, arg, options)
    super(name, arg, options)

    @include_path = arg
  end

  def render(context)
    r = UberRenderer.new

    r.render @include_path
  end
end

class PostUrlTag < Liquid::Tag
  def initialize(name, arg, options)
    super(name, arg, options)

    @post_url = arg
  end

  def render(context)
    "/#{@post_url}"
  end
end

Liquid::Template.register_tag('include', IncludeTag)
Liquid::Template.register_tag('post_url', PostUrlTag)

if ARGV.size < 1
  puts "Expected the target directory to be passed"
  exit 1
end

processor = Processor.new
processor.render_all ARGV[0]
