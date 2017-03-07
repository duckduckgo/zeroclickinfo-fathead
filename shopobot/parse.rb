require 'json'

DOWNLOAD_DIR='download'

def open(file)
  file = File.new(file, "r")
  json = file.readlines.to_s
  all = JSON.parse(json)
  prodList = JSON.parse(all[0])
  puts prodList[0]
  prodList.each do |product|
    title = product['title']
    if product['offers']
      external_links = ''
      product['offers'].each do |offer|
        offer.each do |key, value|
          if value != ""
            external_links += "[#{value} #{key}]\\n"
          end
        end
      end
    end
    image = product['image']
    url = product['url']
    description = "30 Day Price History: $#{product['high']}-$#{product['low']}. "
    if product['description']
      description += product['description']
    end
    puts "#{title}\tA\t\t\t\t\t\t\t#{external_links}\t\t#{image}\t#{description}\t#{url}"
  end
end


Dir.new(DOWNLOAD_DIR).entries.each do |file|
  open(DOWNLOAD_DIR+'/'+file) if file=~/\.json/
end