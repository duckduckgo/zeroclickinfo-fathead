var fs    = require('fs');
var jsdom = require('jsdom');

var FILE_NAME = './download/all.html';
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
        function prettify_id(id) {
            return id.replace(/^all_/, '').replace(/\./g, ' ');
        }

	function process_element(i, elem) {
	    elem = $(elem);
	    var next = elem.next();
	    var description = '';
            var inner_span_a = elem.find("span:first-child a");
	    var id = inner_span_a.length == 1 ? inner_span_a.attr('id') : '';
	    var url = URL_BASE + '#' + id;
	    var namespace = '';
	    var page = prettify_id(id);
            var signature = elem.text().replace(/\s*#$/, '');
            var signature_text = signature.replace(/\([^\)]*\)/, '');
	    var synopsis = signature;
	    var is_event = false;

	    var m1 = signature_text.match(/^([^\.]+)\.(.+)$/); // member function
            var m2 = signature_text.match(/^Event:\ \'([^\']+)\'/); // event
            var m3 = signature_text.match(/^([^\.]+)$/); // global free-standing function

	    if (m1 && m1.length === 3) {
		namespace = m1[1];
		page = m1[2];
            } else if (m2 && m2.length == 2) {
		is_event = true;
		page = m2[1];
		var _p = elem.prevAll('h2').first();
		if (elem.is('h3') && _p) {
                    var nsm = _p.text().match(/Class:\ ([^#]+)/);
                    if (nsm && nsm.length == 2) {
			namespace = nsm[1].split('.').reverse()[0];
		    }
		}
	    } else if (m3 && m3.length == 2) {
                page = m3[1];
	    }

	    if (next.is('p')) {
		description = next.text();
	    } else if (next.next().is('p')) {
                description = next.next().text();
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
    fs.writeFileSync('output.txt', _d.join('\n') + "\n");
}

function main() {
    fetch_html(true, function(html) {
	parse_out_docs(html, /*dump_to_db*/ dump_to_file);
    });
}

main();


