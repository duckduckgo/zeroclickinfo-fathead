#!/usr/bin/env ruby
# frozen_string_literal: true

# Holds database column values to be output as tab-separated values.
class OutputRow
  HEADERS = [
    :title,
    :type,
    :redirect_title,
    nil,
    :categories,
    nil,
    :related,
    nil,
    :links,
    :disambiguation_content,
    :image,
    :abstract,
    :url
  ].freeze

  ARTICLE_TYPES = {
    article: 'A',
    disambiguation: 'D',
    redirect: 'R'
  }.freeze

  attr_reader(*HEADERS.compact - [:abstract])
  attr_writer(*HEADERS.compact - [:type])

  def initialize
    yield self
    freeze
  end

  def type=(value)
    @type = ARTICLE_TYPES.fetch(value)
  end

  def abstract
    @abstract.to_s.gsub("\n", '\n')
  end

  def to_s
    HEADERS.map { |header| public_send header if header } * "\t" << "\n"
  end
end

# Abstract description of documentation.
class Docs
  attr_accessor :name, :url, :description

  def initialize
    yield self
    freeze
  end

  def abstract
    description.css('p,pre.ruby').chunk(&:name).map do |name, elements|
      if name == 'pre'
        elements.map { |e| "<pre><code>#{e.text}</code></pre>" }.join
      else
        elements.map { |e| e.text.gsub(/\s+/, ' ') }.join('<br>')
      end
    end.join
  end

  def to_row
    OutputRow.new do |row|
      row.title = name
      row.type = :article
      row.categories = categories
      row.abstract = abstract
      row.url = url
    end
  end

  # Class or module documentation.
  class Class < self
    attr_accessor :type

    def categories
      type.to_s == 'module' ? 'modules' : 'classes'
    end
  end

  # Class method or instance method documentation.
  class Method < Docs
    def categories
      "#{name.to_s.include?('#') ? 'instance' : 'class'} methods"
    end

    def abstract
      usage + super
    end

    private

    def usage
      text = description.css('.method-callseq').map(&:text).join("\n")
      text.nil? || text.empty? ? '' : "<pre><code>#{text}</code></pre>"
    end
  end
end

# Prints class, module, and method IA entries from ruby-doc source directory.
#
#   Usage: ./parse.rb <path/to/ruby-doc/index.html>
#
if $PROGRAM_NAME == __FILE__
  trap 'PIPE', 'EXIT'

  require 'pathname'
  require 'uri'
  require 'nokogiri'

  base_dir = Pathname(ARGF.path).dirname
  base_url = URI('http://ruby-doc.org/core/')

  Nokogiri(ARGF).css('#class-index .entries p:not(.nodoc)').each do |entry|
    rel_path = entry.at('a')['href']
    page = Nokogiri(base_dir.join(rel_path).read)

    class_docs = Docs::Class.new do |docs|
      docs.name = page.at('h1').text
      docs.url = base_url + rel_path
      docs.type = entry['class']
      docs.description = page.at('#description')

      docs.to_row.display
    end

    page.css('#method-list-section .link-list a').each do |link|
      Docs::Method.new do |docs|
        docs.name = class_docs.name + link.text
        docs.url = class_docs.url + link['href']
        docs.description = page.at(":has([name=#{docs.url.fragment}])")

        docs.to_row.display
      end
    end
  end
end
