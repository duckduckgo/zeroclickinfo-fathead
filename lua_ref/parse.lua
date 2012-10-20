#!/usr/bin/env lua
----
-- Lua manual parser for DuckDuckGoGo fathead plugin
----
-- by Severak 2012
-- license WTFPL (on this script)
----

MANUAL_VERSION="5.1"
MANUAL_URL='http://www.lua.org/manual/'..MANUAL_VERSION..'/manual.html'
REF_REGEX='<hr><h3><a name="pdf%-(.+)"><code>(.+)</code></a></h3>'
TEST_REF='<hr><h3><a name="pdf-assert"><code>assert (v [, message])</code></a></h3>'
EOL="\\n"

function parseManual()
	local refs={}
	local buffer={}
	local titles={}
	local func=false
	for line in io.lines("manual.html") do
		if line:match(REF_REGEX) then
			local fce,synopsis=line:match(REF_REGEX)
			titles[fce]=synopsis
			if func then
				refs[func]=table.concat(buffer, EOL)
			end
			func=fce
			buffer={}
		elseif line:match("<h2>") then
			if func then
				refs[func]=table.concat(buffer, EOL)
				func=false
			end
		elseif line:match("<h1>") then
			func=false
		elseif line:match("<h3>") then
			func=false
		else
			if func then
				line=string.gsub(line, '<a href="#(.-)"', '<a href="'..MANUAL_URL..'#%1"')
				buffer[#buffer+1]=line
			end
		end
	end
	return refs, titles
end

function alphaSortKeys(t)
	local ret={}
	for k,v in pairs(t) do
		ret[#ret+1]=k
	end
	table.sort(ret)
	return ret
end

function writeOutput(refs,synopsis)
	local keys=alphaSortKeys(refs)
	for _,topic in pairs(keys) do
		local line={}
		--for those who don't use lua:
		--lua starts array index at 1
		line[1]=topic
		line[2]="A"
		line[3]="" --not yet
		line[4]="" --not yet
		line[5]="" --not yet
		line[6]="" --not yet
		line[7]="" --not yet
		line[8]="" --not yet
		line[9]=MANUAL_URL
		line[10]="" --not yet
		line[11]="" --not yet
		line[12]="<pre><code>"..synopsis[topic].."</code></pre>"..EOL..refs[topic]
		line[13]=MANUAL_URL.."#pdf-"..topic
		print(table.concat(line,"\t"))
	end
end

refs, synopsis=parseManual()
writeOutput(refs,synopsis)