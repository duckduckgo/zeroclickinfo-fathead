package DDG::Fathead::Investopedia;

use DDG::Fathead;

primary_example_queries "financial accounting term";

secondary_example_queries
	"term finacial accounting",
    "financial accounting definition";

name "Investopedia Terms";

description "Finacial's name definition/meating/dictionary";

icon_url "/i/www.investopedia.com.ico";

source "Investopedia";

code_url "http://www.investopedia.com/terms/f/financialaccounting.asp";

topics "economy_and_finance";

category "finance";

attribution
    Email => ['dwashburn@valueclickbrands.com', 'Daniel Washburn'],
    web => ['http://www.jperla.com/blog', 'Joseph Perla'];

1;
