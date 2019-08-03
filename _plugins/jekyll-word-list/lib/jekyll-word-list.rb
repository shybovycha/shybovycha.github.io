require 'jekyll'

require 'nokogiri'

require 'securerandom'
require 'json'

module JekyllWordList
  class Index
    def initialize
      @docs = {}
      @words = {}
    end

    def add(doc, doc_reference)
      doc_id = generate_doc_id
      
      @docs[doc_id] = doc_reference

      doc_words = extract_words doc

      @words = doc_words.each_with_index.reduce(@words) do |acc, pair|
        word, position = pair[0], pair[1]

        unless acc[word]
          acc[word] = { count: 0, occurrences: {} }
        end

        unless acc[word][:occurrences][doc_id]
          acc[word][:occurrences][doc_id] = { positions: [], count: 0 }
        end

        acc[word][:occurrences][doc_id][:positions] << position
        acc[word][:occurrences][doc_id][:count] += 1
        acc[word][:count] += 1

        acc
      end
    end

    def to_json(format = :compact)
      case format
      when :compact
        to_json_compact
      when :full
        to_json_full
      else
        raise "Unsupported JSON format `#{format.inspect}`"
      end
    end

    private

    ##
    # Generates compact representation of the index in form of
    #
    #  { words: { <word>: { <doc_id>: [ <positions_in_doc> ] } }, documents: { <doc_id>: <doc_path> } }
    #
    def to_json_compact
      compact_words = @words.to_a.map do |word, word_data|
        new_data = word_data[:occurrences].to_a.map { |doc_id, occurrence_data| [ doc_id, occurrence_data[:positions] ]  }

        [ word, Hash[new_data] ]
      end
                                          
      { words: Hash[compact_words], documents: @docs }.to_json
    end

    ##
    # Generates descriptive representation of the index in form of
    #
    # { words: { <word>: { occurrences: { <doc_id>: { positions: [ <positions_in_doc> ], count: <word_occurrences_count_in_doc> } }, count: <total_word_occurrences_count> } }, documents: { <doc_id>: <doc_path> } }
    #
    def to_json_full
=begin
      schema = {
        type: 'object',
        required: [ 'words', 'documents' ],
        properties: {
          words: {
            type: 'object',
            properties: {
              count: {
                type: 'integer',
                minimum: 1
              },
              occurrences: {
                type: 'object',
                additionalProperties: {
                  type: 'object',
                  required: [ 'positions', 'count' ],
                  properties: {
                    positions: {
                    type: 'array',
                    items: {
                      type: 'integer',
                      minimum: 0
                    }
                  },

                  count: {
                    type: 'integer',
                    minimum: 1
                  }
                }
              }
            }
          }
        }
        }
      }
=end

      schema = {}
      
      { words: @words, documents: @docs, schema: schema }.to_json
    end

    def generate_doc_id
      # SecureRandom.uuid
      @docs.size + 1
    end

    def extract_words(text)
      # \W - all non-word chars; including dash ('-')
      # classes in regexes could have subtractions: [baseClass-[exclusions]], but not in Ruby; in Ruby it works by using a negative union: [baseClass&&[^exclusion]]
      text.split(/[!?,;'"\/\\|=(){}\[\]<>@#%\^&`*\s]+/).map { |word| word.strip.downcase }.reject { |word| word.empty? or /^(\W+|\d+)$/.match? word }
    end
  end
  
  class Parser
    def initialize
      @index = JekyllWordList::Index.new
    end
    
    def parse(page_html, page_id)
      doc = Nokogiri::HTML.parse(page_html)

      body = doc.at_css('body')

      body_text = traverse(body).join(' ')

      @index.add body_text, page_id
    end

    def index_json
      @index.to_json
    end

    private

    def traverse(node, acc = [])
      return acc if ['script', 'style'].include? node.name or (node.name == 'pre' and node.parent.name == 'code') or (node.name == 'code' and node.parent.name == 'pre')

      return acc + [node.text] if node.text?

      node.children.reduce(acc) { |acc, child| traverse(child, acc) }
    end
  end
end

Jekyll::Hooks.register :site, :post_render do |site|
  # puts ">>> site.dest = #{site.dest}"
  # puts ">>> site.source = #{site.source}"
  # puts ">>> site.config = #{site.config}"
  
  parser = JekyllWordList::Parser.new

  documents = (site.pages + site.posts.docs).reject { |e| e.url =~ /(css|js|xml)$/ }
  
  documents.each { |page| parser.parse(page.content, page.url) }

  # puts parser.index_json
  index_filename = File.expand_path('word_list.json', site.source)
  File.write(index_filename, parser.index_json)
end
