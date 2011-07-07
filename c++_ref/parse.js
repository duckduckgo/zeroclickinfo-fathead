var fs    = require('fs');
var jsdom = require('jsdom');
var path  = require('path');
var us    = require('underscore');


var PATH_BASE = './data/www.cplusplus.com/reference/';
var URL_BASE  = 'http://www.cplusplus.com/reference/';
var JQUERY_URL = './jquery-1.5.min.js'; // 'http://code.jquery.com/jquery-1.5.min.js'; 

/*
function create_window(cb) {
    var document = jsdom.jsdom('<html><body>');
    var window = document.createWindow();

    jsdom.jQueryify(window, JQUERY_URL, function() {
	cb(window, document);
    });
}
*/

var escaped_one_to_xml_special_map = {
    '&amp;': '&',
    '&quot;': '"',
    '&apos;': "'", 
    '&lt;': '<',
    '&gt;': '>'
};

function decodeXml(string) {
    return string.replace(/(&quot;|&apos;|&lt;|&gt;|&amp;)/g,
			  function(str, item) {
			      return escaped_one_to_xml_special_map[item];
			  });
}


function create_window(cb) {
    jsdom.env('<html><body>', [ JQUERY_URL ], function(errors, window) {
	if (errors) {
	    throw new Error(errors[0]);
	}
	cb(window, window.document);
    });
}

function fetch_html(path, cb) {
    var html = fs.readFileSync(path).toString();
    // console.error("html:", html);
    // process.nextTick(function() {
	cb(html);
    // });
}

function get_all_file_paths() {
    function is_a_directory(dir) {
	return dir !== 'algorithm' && fs.statSync(path.join(PATH_BASE, dir)).isDirectory();
    }

    function get_all_subdirs(sdirs) {
	sdirs = us.flatten(sdirs.map(function(dir) {
	    return fs.readdirSync(path.join(PATH_BASE, dir)).map(function(sdir) {
		return path.join(dir, sdir);
	    });
	})).filter(is_a_directory);
	return sdirs;
    }

    var sdirs = get_all_subdirs(get_all_subdirs(get_all_subdirs(['.'])));
    sdirs = sdirs.concat(get_all_subdirs(['algorithm']));
    return sdirs;
}

function start_parsing(dirs, window, document, cb) {
    console.error("start_parsing: dirs:", dirs);

    var data = [ ];
    function parse_file(dirs, i, data) {

	var _p  = path.join(PATH_BASE, dirs[i], 'index.html');
	fetch_html(_p, function(html) {
	    console.error("Parsing file:", _p);

	    document.innerHTML = html;
	    var $ = window.$;

	    $("#CH_main").html(
		$("#CH_main").html().replace("</p>", "</p><p>")
	    );
	    var dst = $(".docsubtop");
	    var module = path.basename(path.dirname(dirs[i]));
	    var proc = path.basename(dirs[i]);
	    var desc_short = dst.next().text().trim().replace(/\n/g, "\\n");
	    var desc_long  = dst.next().next().text().trim().replace(/\n/g, "\\n");

	    var fq_name = $('.C_doctop').text();
	    var signature = $('.prototype').text().split(';')[0].trim();
	    var description = desc_short + '. ' + desc_long;
	    var title = module + ' ' + proc;
	    var url = URL_BASE + dirs[i];

	    var entry = {
		page: title, 
		synopsis: fq_name + "\n" + signature, 
		description: description, 
		url: url
	    };

	    data.push(entry);

	    var global_modules = {
		algorithm: 1, 
		functional: 1, 
		numeric: 1, 
		iterator: 1, 
		locale: 1, 
		memory: 1, 
		stdexcept: 1, 
		utility: 1, 
		typeinfo: 1, 
		"new": 1, 
		limits: 1, 
		exception: 1
	    };

	    if ((module[0] == 'c' && module !== 'char_traits') || 
		global_modules.hasOwnProperty(global_modules)) {
		entry = {
		    page: proc, 
		    synopsis: fq_name + "\n" + signature, 
		    description: description, 
		    url: url
		};
		data.push(entry);
	    }

	    // console.error("entry:", entry);

	    // next_cb(dirs, i+1, data, next_cb, end_cb);
	});
    }

    for (var i = 0; i < dirs.length; ++i) {
	parse_file(dirs, i, data);
    }
    cb(data);
}


function parse_all_docs(window, document, cb) {
    start_parsing(get_all_file_paths(), window, document, cb);
}


function dump_to_file(docs) {
    var _d = docs.map(function(d) {
	return decodeXml([ d.page, '', d.url, d.description, 
			   d.synopsis, '', '', 'en' ].join('\t').replace(/\n/g, ' '));
    });
    fs.writeFileSync('c++.docs.txt', _d.join('\n'));
}

function main() {
    create_window(function(window, document) {
	parse_all_docs(window, document, function(data) {
	    dump_to_file(data);
	    console.error("DONE!!");
	});
    });
}

main();


