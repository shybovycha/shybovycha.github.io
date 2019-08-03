# lib = File.expand_path('lib', __dir__)
# $LOAD_PATH << lib unless $LOAD_PATH.include? lib

Gem::Specification.new do |spec|
  spec.name = 'jekyll-word-list'
  spec.version = '0.0.1'
  spec.authors = ['Artem Shubovych']
  spec.email = ['shybovycha@gmail.com']
  spec.summary = 'A Jekyll plugin which generates a list of words used on a site. Used for full-text search'
  spec.license = 'MIT'

  spec.files = ['lib/jekyll-word-list.rb']
  spec.test_files = []
  # spec.require_paths = ['lib']

  spec.required_ruby_version = '>= 2.3.0'

  spec.add_dependency 'jekyll', '~> 3.7'

  spec.add_development_dependency 'bundler', '~> 2.0'
  spec.add_development_dependency 'nokogiri', '~> 1.10'
end
