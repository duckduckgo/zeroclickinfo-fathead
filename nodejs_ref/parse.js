var fs    = require('fs');
var jsdom = require('jsdom');
var sqlite = require('sqlite');

var FILE_NAME = './all-0.4.7.html';
var URL_BASE  = 'http://nodejs.org/docs/v0.4.7/api/all.html';

function fetch_html(local, cb) {
    var html = fs.readFileSync(FILE_NAME).toString();
    // console.error("html:", html);
    cb(html);
}

// create table docs(page text not null, namespace text not null, url text not null, description text not null, synopsis text not null, details text not null, type text not null, lang text not null);

function parse_out_docs(html, cb) {
    jsdom.env(html, [ 'http://code.jquery.com/jquery-1.5.min.js' ], function(errors, window) {
	if (!window) {
	    throw new Error("Arghh!!");
	}

	console.error("Start parsing");

	var $ = window.$;
	var docs = $('h3').map(function(i, h3) {
	    h3 = $(h3);
	    var next = h3.next();
	    var description = '';
	    var id = h3.attr('id') || ''
	    var url = URL_BASE + '#' + id;
	    var namespace = '';
	    var page = id;
	    var synopsis = h3.text();

	    var m = id.match(/^([^\.]+)\.(.+)$/);
	    if (m && m.length === 3) {
		namespace = m[1];
		page = m[2];
	    }
	    else {
		m = id.match(/^event_([^_]+)_/);
		if (m && m.length === 2) {
		    page = m[1];
		    var _p = h3.prevAll('h2').first();
		    if (_p) {
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
	    return ret;
	});
	cb(docs.toArray());
    });
}

function insert_next(db, docs, cb) {
    if (docs.length === 0) {
	db.close(function() { });
	return;
    }
    var d = docs[0];

    var sql = "INSERT INTO docs(page, namespace, url, description, " + 
	"synopsis, details, type, lang) " + 
	"VALUES (?, ?, ?, ?, ?, ?, ?, ?)";
    var args = [ d.page, d.namespace, d.url, d.description, 
		 d.synopsis, '', '', 'en' 
	       ];
    console.error("args:", args);

    db.execute(sql, args, function (error, rows) {
	if (error) {
	    throw error;
	}
	docs.shift();
	cb(db, docs, insert_next);
    });

}

function dump_to_db(docs) {
    var db = new sqlite.Database();

    db.open("nodejs.sqlite3", function (error) {
	if (error) {
	    throw error;
	}
	insert_next(db, docs, insert_next);
    });
}

function dump_to_file(docs) {
    var _d = docs.map(function(d) {
	return [ d.page, d.namespace, d.url, d.description, 
		 d.synopsis, '', '', 'en' ].join('\t').replace(/\n/g, ' ');
    });
    fs.writeFileSync('nodejs.docs.txt', _d.join('\n'));
}

function main() {
    fetch_html(true, function(html) {
	parse_out_docs(html, /*dump_to_db*/ dump_to_file);
    });
}

main();


