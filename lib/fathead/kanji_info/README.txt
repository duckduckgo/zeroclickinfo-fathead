# kanji_info
Information about Chinese characters with stroke order diagrams.

## Dependencies
main work:
* wget
* ruby (tested with ruby 1.9.3p484)
  * nokogiri
  * thread
generating svgs:
* wget
* git
* ruby
  * rubygems
  * nokogiri
  * pp

## Notes
To generate the svgs referenced in output.txt do
```
git clone https://github.com/MarkMcCaskey/kanjivg2svg
cd kanjivg2svg
wget "https://github.com/KanjiVG/kanjivg/releases/download/r20160426/kanjivg-20160426-all.zip"
unzip kanjivg-20160426-all.zip
ruby kanjivg2svg.rb kanji/ animated
ruby kanjivg2svg.rb kanji/ frames
```
After this there should be an svgs directory inside of kanjivg2svg.
Alternatively, it may be possible to inline the SVGs directly into the <code> section of output.txt.

Note: currently only the animated frames are referenced in output.txt, however I would also like to include the frames in an infobox if possible.



