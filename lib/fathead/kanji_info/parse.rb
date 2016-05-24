# Fathead kanji_strokeorder for DuckDuckGo
# Mark McCaskey 2016
# See license for terms of use/redistribution
require 'nokogiri'
require 'thread'

# Thirteen fields:
# 1.  Character
# 2.  Unclear. Info = Type of article. A for artciles, D for disambiguation, R for redirects.  Required
# 3.  empty
# 4.  empty
# 5.  unsure. info = Categories
# 6.  empty
# 7.  unsure. info = Related topics.  Will be turned into link in zero-click info box. 
# 8.  empty
# 9.  unsure. info = External links
# 10. empty (maybe used later to disambiguate Chinese and Japanese?)
# 11. Should be local image. need to figure this out later. info = Image.  Can reference an external image [[Image:$url]]
# 12. bulk of info. info = Abstract. Generally one readable sentence ending in a period.
# 13. wiktionary URL. info = URL.  full URL for source

def generate_entry(character, information)
  animated_file = "svgs/" + character.ord.to_s + "_animated.svg"

  return character + "\tA\t\t\t\t\t\t\t\t\t" + animated_file + "\t" + information + "\thttps://en.wiktionary.org/wiki/" + character
end

# To be displayed in the info box
def format_paragraph(misc, rmgroup)
  reading_type     = Array["Pinyin", "Korean", "Onyomi", "Kunyomi"]
  pinyin_readings = Array[]
  korean_readings = Array[]
  korean_roman_readings = Array[]
  onyomi_readings = Array[]
  kunyomi_readings = Array[]
  reading_lookup = { "pinyin" => pinyin_readings, "korean_r" => korean_roman_readings, "korean_h" => korean_readings, "ja_on" => onyomi_readings, "ja_kun" => kunyomi_readings }
  output = "<pre><code>"
  
  output << "Stroke count: " + misc.at_xpath('stroke_count').content + "\\n"
  
  # Nicely format and handle potential non-existence and duplicates
  # of readings in pinyin, hangul, Japanese onyomi and kunyomi
  readings = rmgroup.xpath("reading")
  if readings != nil
    for i in 0 ... readings.size 
      reading_lookup[readings[i].attributes["r_type"].value].push(readings[i].content)
    end
    
    # writing done here to avoid writing blank entries
    if pinyin_readings.size > 0
      output << "Pinyin: " + pinyin_readings.join(',') + "\\n"
    end
    if korean_readings.size > 0
      output << "Korean: " + korean_readings.join(',') + "\\n"
    end
    if onyomi_readings.size > 0
      output << "Onyomi: " + onyomi_readings.join(',') + "\\n"
    end
    if kunyomi_readings.size > 0
      output << "Kunyomi: " + kunyomi_readings.join(',') + "\\n"
    end
  end
  
  # Nicely format and handle potential non-existence and duplicates
  # of meanings in English
  meanings = rmgroup.xpath("meaning")
  if not meanings.nil? 
    meanings_output = Array[]
    for meaning in meanings
      if meaning.attributes["m_lang"].nil? # ignore French, Spanish, and Portugese for now
        meanings_output.push(meaning.content)
      end
    end
    
    # writing done here to avoid having "Meanings: " and no meanings
    if meanings_output.size > 0
      output << "Meanings: " + meanings_output.join(", ")
    end
  end
  
  
  output << "</code></pre>"
  return output
end

def format_output(character_xml)
  if character_xml.nil?
    return ""
  end

  character   = character_xml.at_xpath('literal').content
  misc        = character_xml.at_xpath('misc')
  rmgroup     = character_xml.at_xpath('//rmgroup')
  information = format_paragraph(misc, rmgroup)
  
  return generate_entry(character, information)
end

def main()
  if ARGV.size < 1
    abort("Enter a correctly formatted XML file as input")
  elsif not File.exist?(ARGV[0])
    abort("file " + ARGV[0] + " could not be found")
  end
  
  xml_doc  = File.open(ARGV[0]) { |f| Nokogiri::XML(f) }
  out_file = File.open('output.txt', 'w')
  
  character_queue = Queue.new
  done_signal = 0
  
  # entry generator
  producer = Thread.new {
    xml_char = xml_doc.xpath('//character')
    while not xml_char.empty?
      character_queue << format_output(xml_char.shift)
    end
    done_signal = 1
  }
  
  
  # entry writer
  consumer = Thread.new {
    out_file.print character_queue.pop
    while done_signal == 0 || (not character_queue.empty?)
      store = "\n" + character_queue.pop
      out_file.print store
    end
  }

  consumer.join
  
  out_file.flush
  out_file.close
end

main()
