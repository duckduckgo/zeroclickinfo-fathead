
require 'nokogiri'

BASE_URL = "http://www.opengl.org/sdk/docs/man/html/"

# An entry in the output database
class Entry
  attr_accessor :title, :type, :redirect, :categories, :seeAlso, :externalLinks,
    :images, :abstract, :sourceUrl

  def initialize( type = "A" )
    @title = ""
    @type = type
    @redirect = ""
    @categories = []
    @seeAlso = ""
    @externalLinks = ""
    @images = ""
    @abstract = ""
    @sourceUrl = ""
  end

  def to_s()
    return "#{@title}\t#{@type}\t#{@redirect}\t\t#{@categories.join(" ")}\t\t#{@seeAlso}\t\t#{@externalLinks}\t\t#{@images}\t#{@abstract}\t#{@sourceUrl}"
  end
end

# An entry of type article
class Article < Entry
end

# An entry of type redirect
class Redirect < Entry
  def initialize()
    super("R")
  end
end

# Parse an OpenGL documentation XML file.  Return value is an array of entry objects
# representing the function(s) that are documented within the file.
def parseFile( f )

  # Open the file and process
  File.open(f) { |file|

    # Parse file with Nokogiri
    doc = Nokogiri::XML(file)

    # Every input file should generate an article entry and possibly several
    # redirects (depending on contents).  Start with the article
    article = Article.new

    # Get the title
    article.title = doc.css("refmeta refentrytitle").first().inner_text();
    # The purpose (refpurpose) is a good summary of the function
    purpose = doc.css("refnamediv refpurpose").first().inner_text();
    # The source URL should have the same name with extension .xhtml
    article.sourceUrl = BASE_URL + File.basename(file, ".xml") + ".xhtml"

    # Find the function prototypes
    protos = getFunctionPrototypes(doc)

    # Ready an array of entries to return
    ent = []

    # Occasionally, the title (refentrytitle) is a comma separated list of function
    # names.  In which case, we duplicate, and attempt to get the appropriate
    # prototype for each.
    if article.title.include? ","
      funcs = article.title.split(/\s*,\s*/)
      for f in funcs
        if protos.has_key? f
          codeBlock = "<pre><code>#{protos[f]}</code></pre>"
          dupArticle = article.dup
          dupArticle.title = f
          dupArticle.abstract = sanitize("#{codeBlock} #{purpose}")
          ent << dupArticle
        end
      end
    else
      # Grab the first 3 function prototypes (there can be many), and put
      # them in a code block
      codeBlock = ""
      if protos.size > 0
        codeBlock = "<pre><code>#{protos.values.take(3).join("<br>")}"
        # if there's more, add elipsis
        codeBlock << "<br>..." if protos.size > 3
        # finish with the closing tags
        codeBlock << "</code></pre>"
      end

      # The abstract is the first three function prototypes followed by the
      # brief summary (purpose)
      article.abstract = sanitize("#{codeBlock} #{purpose}")

      # Add the article to our list of entries
      ent << article

      # If there is more than one function, then we make redirects for each
      # variation
      for p in protos.keys
        if p != article.title
          redirect = Redirect.new
          redirect.title = p
          redirect.redirect = article.title
          ent << redirect
        end
      end
    end

    return ent
  }

end

# Remove new-lines and tabs
def sanitize( str )
  return str.gsub(/[\n\r\t]/, ' ')
end

# Gather the function prototypes in the document.  The return is a hash
# with the key being the function name, and the value being a string containing
# the function prototype.
def getFunctionPrototypes( doc )
  synopsis = doc.css("refsynopsisdiv funcprototype")

  funcs = {}
  if synopsis.length > 0
    for func in synopsis
      funcName = func.css("function").first().inner_text()
      funcs[funcName] = getFunctionProto(func)
    end
  end

  return funcs
end

# Grab the function prototype as a string from the given function node
def getFunctionProto( funcProtoNode )
  funcNameAndReturnType = funcProtoNode.css("funcdef").first().inner_text()
  params = []
  paramNodes = funcProtoNode.css("paramdef")
  for p in paramNodes
    params << p.inner_text()
  end
  return "#{funcNameAndReturnType}(#{params.join(", ")})"
end

# Open the output file for writing
File.open("output.txt", "wt") { |outFile|
  # Process each XML file in the download directory beginning with gl, but
  # not beginning with "gl_" (the latter are for GLSL and have a different
  # format).
  Dir.glob("download/gl*.xml") { |f|
    fname = File.basename(f)
    if !fname.start_with?("gl_")
      entries = parseFile(f)
      entries.each { |e| outFile.puts e }
    end
  }
}
