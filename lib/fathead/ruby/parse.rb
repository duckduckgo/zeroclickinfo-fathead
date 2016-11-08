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

  attr_accessor(*HEADERS.compact)

  def initialize
    yield self
    freeze
  end

  def to_s
    HEADERS.map { |header| escape public_send header if header } * "\t" << "\n"
  end

  private

  def escape(text)
    String(text).gsub(/[\\\n\t]/, '\\' => '\\\\', "\n" => '\n', "\t" => '\t')
  end
end

# Abstract description of documentation.
class Documentation
  attr_accessor :title, :url, :description, :categories, :related, :alt_titles

  def initialize
    yield self
    freeze
  end

  def abstract
    prog_container intro.map { |elt| element_to_s elt }.unshift(usage).join
  end

  def to_s
    [to_row, to_redirect_rows].join
  end

  def breaks_category_page?
    title.include?('[') || abstract.include?('[')
  end

  private

  def to_row
    OutputRow.new do |row|
      row.title = title
      row.type = 'A' # article
      row.categories = categories.join("\n")
      row.related = related.join("\n")
      row.abstract = abstract
      row.url = url
    end
  end

  def to_redirect_rows
    (alt_titles - [title]).map do |alt_title|
      OutputRow.new do |row|
        row.title = alt_title
        row.type = 'R' # redirect
        row.redirect_title = title
      end
    end
  end

  def intro
    description.css('p,pre.ruby,h2').take_while { |e| e.name != 'h2' }.first(3)
  end

  def prog_container(string)
    %(<section class="prog__container">#{string}</section>)
  end

  def element_to_s(element)
    if element.name == 'pre'
      code_block element.text
    else
      paragraph element.text
    end
  end

  def code_block(string)
    "<pre><code>#{escape string}</code></pre>" unless string.empty?
  end

  def paragraph(string)
    "<p>#{escape string.gsub(/[[:space:]]+/, ' ').strip}</p>"
  end

  def escape(string)
    string.gsub(/[&<]/, '&' => '&amp;', '<' => '&lt;')
  end

  def usage
    code_block description.css('.method-callseq').map(&:text).join("\n")
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
  index_page = Nokogiri(ARGF)

  index_page.css('#class-index .entries p:not(.nodoc)').each do |entry|
    rel_path = entry.at('a')['href']
    path = base_dir + rel_path
    page = Nokogiri(path.read)

    class_docs = Documentation.new do |docs|
      docs.title = page.at('h1').text
      docs.url = base_url + rel_path
      docs.description = page.at('#description')
      docs.categories = [
        entry['class'] == 'module' ? 'Ruby modules' : 'Ruby classes'
      ]
      docs.related = [
        "[[Ruby #{docs.title} methods]]"
      ]
      docs.alt_titles = [
        docs.title.gsub('::', ' ')
      ]

      docs.categories = [] if docs.breaks_category_page?

      docs.display
    end

    page.css('#method-list-section .link-list a').each do |link|
      Documentation.new do |docs|
        docs.title = (class_docs.title + link.text).gsub(/(?:::|#)/, ' ')
        docs.url = class_docs.url + link['href']
        docs.description = page.at(":has([name=#{docs.url.fragment}])")
        docs.categories = [
          "Ruby #{link.text.start_with?('#') ? 'instance' : 'class'} methods",
          "Ruby #{class_docs.title} methods"
        ]
        docs.related = [
          "[[Ruby #{class_docs.title}]]"
        ]
        docs.alt_titles = [
          class_docs.title + link.text.sub(/\A(?:::|#)/, '.'),
          class_docs.title + link.text
        ]

        docs.categories = [] if docs.breaks_category_page?

        docs.display
      end
    end
  end
end
