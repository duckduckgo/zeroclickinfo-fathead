var fs    = require('fs');
var jsdom = require('jsdom');

var FILE_NAME = './all.html';
var URL_BASE  = 'http://nodejs.org/api/all.html';
var JQUERY_URL = './jquery-1.5.min.js'; // 'http://code.jquery.com/jquery-1.5.min.js'; 

function fetch_html(local, cb) {
    var html = fs.readFileSync(FILE_NAME).toString();
    // console.error("html:", html);
    cb(html);
}

// create table docs(page text not null, namespace text not null, url text not null, description text not null, synopsis text not null, details text not null, type text not null, lang text not null);

function parse_out_docs(html, cb) {
    jsdom.env(html, [ JQUERY_URL ], function(errors, window) {
	if (!window) {
	    throw new Error("Arghh!!");
	}

	console.error("Start parsing");

	var $ = window.$;
	var docs = [ ];
	function process_element(i, elem) {
	    elem = $(elem);
	    var next = elem.next();
	    var description = '';
	    var id = elem.attr('id') || ''
	    var url = URL_BASE + '#' + id;
	    var namespace = '';
	    var page = id;
	    var synopsis = elem.text();
	    var is_event = false;

	    var m = id.match(/^([^\.]+)\.(.+)$/);
	    if (m && m.length === 3) {
		namespace = m[1];
		page = m[2];
	    }
	    else {
		m = id.match(/^event_([^_]+)_/);
		if (m && m.length === 2) {
		    is_event = true;
		    page = m[1];
		    var _p = elem.prevAll('h2').first();
		    if (elem.is('h3') && _p) {
			namespace = _p.attr('id');
		    }
		}
	    }

	    if (next.is('p')) {
		description = next.text();
	    }

	    var ret = {
		page: page, 
		description: description, 
		url: url, 
		synopsis: synopsis, 
		namespace: namespace
	    };

	    console.error("ret:", ret);
	    // return ret;
	    docs.push(ret);

	    if (namespace) {
		var ret2 = {
		    page: namespace + '.' + page, 
		    description: description, 
		    url: url, 
		    synopsis: synopsis, 
		    namespace: ''
		};

		if (is_event) {
		    ret2.page = namespace + ' ' + page + ' event';
		}

		docs.push(ret2);
	    }
	}

	$('h3').each(process_element);
	$('h2').each(process_element);

	cb(docs);
    });
}

function dump_to_file(docs) {
    var _d = docs.map(function(d) {
	return [ d.page, d.namespace, d.url, d.description, 
		 d.synopsis, '', '', 'en' ].join('\t').replace(/\n/g, ' ');
    });
    fs.writeFileSync('output.txt', _d.join('\n'));
}

function main() {
    fetch_html(true, function(html) {
	parse_out_docs(html, /*dump_to_db*/ dump_to_file);
    });
}

main();


