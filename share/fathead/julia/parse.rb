#!/usr/bin/env ruby

require 'nokogiri'

doc = Nokogiri::HTML(open("download/packages.html"))
packages = doc.css("div.pkglisting")

class JuliaPackage
  def initialize(package)
    @name = package.css("a")[0].text
    @href = package.css("a").first["href"]
    @description = package.css("h4").text
    @version = package.css("p a")[0].text
    @licence = package.css("p a")[1].text
    @author = package.css("p a")[2].text
    @stars = package.css("span[title='GitHub stars']").text
  end

  def to_s
    [
      @name,
      "A",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      [
        "Description: #{@description}",
        "Version: #{@version}",
        "Author: #{@author}",
        "Licence: #{@licence}",
        "Github Stars: #{@stars}"
      ].join("<br>"),
      @href
    ].join("\t")
  end

  def redirect?
   split_camel_case.size > 1
  end

  def split_camel_case
    @name.split(/(?=[A-Z])/).map {|w| w.downcase}
  end

  def redirect_to_s
    [split_camel_case.join(" "),"R","","","","","","","","","","",""].join("\t")
  end
end

output = File.open("output.txt", "w")

packages.each do |p|
  package = JuliaPackage.new(p)
  output.puts package
  output.puts(package.redirect_to_s) if package.redirect?
end
