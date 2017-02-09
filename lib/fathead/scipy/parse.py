# -*- coding: utf-8 -*-
import argparse
import glob
from multiprocessing import Pool, cpu_count
import os.path as op

from bs4 import BeautifulSoup

DOCS_URL = "https://docs.scipy.org/doc/scipy/reference/generated/"
PACKAGE_NAME = "scipy"

OUTPUT_FILE = "output.txt"

# Be careful editing these templates. They are (leading) whitespace sensitive.

OUTPUT_TEMPLATE = """\
{title}\t
{entry_type}\t
{redirect_title}\t
{empty_field}\t
{categories}\t
{empty_field}\t
{related_topics}\t
{empty_field}\t
{external_links}\t
{disambiguation}\t
{image}\t
{abstract}\t
{url}
""".replace("\n", "")

ABSTRACT_TEMPLATE = """\
<section class="prog__container">
{rendered_information}
{rendered_signature}
{rendered_parameters}
{rendered_returns}
{rendered_examples}
</section>
""".replace("\n", "")

INFORMATION_TEMPLATE = """<p>{information}</p>"""

SIGNATURE_TEMPLATE = """<pre><code>{signature}</code></pre>"""

PARAMETERS_TEMPLATE = """
<span class="prog__sub">Parameters:</span>
<pre><code>{parameters}</code></pre>
""".replace("\n", "")

RETURNS_TEMPLATE = """\
<span class="prog__sub">Returns:</span>
<pre><code>{returns}</code></pre>
""".replace("\n", "")

EXAMPLES_TEMPLATE = """
<span class="prog__sub">Examples:</span>
<pre><code>{examples}</code></pre>
""".replace("\n", "")

CPU_COUNT = cpu_count()


def generate_fathead(folder, processes=CPU_COUNT):
    """Process documentation and write fathead file.

    Parameters
    ----------
    folder : str
        Path to the folder containing the downloaded html docs.

    processes : int
        Number of parallel processes to use to process the documentation.
    """

    docs_folder = op.abspath(folder)
    files_to_parse = glob.glob(op.join(docs_folder, "*.html"))

    pool = Pool(processes)
    output = pool.map(parse_file, files_to_parse)

    with open(OUTPUT_FILE, "w") as fp:
        fp.writelines(output)


def parse_file(doc_file):
    """Generate fathead entries for a file.

    Parameters
    ----------
    doc_file : str
        Path to html file to parse.

    Returns
    -------
    output_entries : str
        String containing the fathead entries for the file being processed.
        There may be more than one line (i.e. entries) per file processed.

    """
    output_entries = ""

    soup = BeautifulSoup(open(doc_file), "html.parser")

    section_div = soup.find("div", attrs={"class": "section"})

    title = get_title(section_div)

    if not title:
        return ""

    abstract = get_abstract(section_div)

    if not abstract:
        return ""

    related_topics = get_related_topics(section_div)

    redirects = get_redirects(title)

    url = DOCS_URL + op.basename(doc_file)

    output_entries = OUTPUT_TEMPLATE.format(title=title,
                                            entry_type="A",
                                            redirect_title="",
                                            empty_field="",
                                            categories="",
                                            related_topics=related_topics,
                                            external_links="",
                                            disambiguation="",
                                            image="",
                                            abstract=abstract,
                                            url=url)

    for redirect in redirects:
        output_entries = output_entries \
                + "\n" \
                + OUTPUT_TEMPLATE.format(title=redirect,
                                         entry_type="R",
                                         redirect_title=title,
                                         empty_field="",
                                         categories="",
                                         related_topics="",
                                         external_links="",
                                         disambiguation="",
                                         image="",
                                         abstract="",
                                         url="")

    return output_entries + "\n"


def get_title(section_div):
    """Return the fathead entry title.

    Parameters
    ----------
    section_div : bs4.BeautifulSoup
        The BeautifulSoup object corresponding to the div with the "class"
        attribute equal to "section" in the html doc file.

    Returns
    -------
    title : Str
        The fathead entry title.
    """
    title = section_div.h1.text[:-1]
    return title


def get_abstract(section_div):
    """Return the fathead entry abstract.

    Parameters
    ----------
    section_div : bs4.BeautifulSoup
        The BeautifulSoup object corresponding to the div with the "class"
        attribute equal to "section" in the html doc file.

    Returns
    -------
    abstract : Str
        The fathead entry abstract.
    """
    try:
        information = section_div.dd.p.text
    except AttributeError:
        information = ""
    if not information:
        return ""

    rendered_information = INFORMATION_TEMPLATE.format(information=information)

    raw_signature = [each.string for each in section_div.dt.children]
    signature = ''.join(raw_signature).strip().replace("¶", "")
    # There should a nice way to not parse "[source]" in the first place.
    if signature.endswith("[source]"):
        signature = signature[:-len("[source]")]
    if signature:
        rendered_signature = SIGNATURE_TEMPLATE.format(signature=signature)
    else:
        rendered_signature = ""

    parameters = get_params(section_div, "field-odd field")
    if parameters:
        rendered_parameters = PARAMETERS_TEMPLATE.format(parameters=parameters)
    else:
        rendered_parameters = ""

    returns = get_params(section_div, "field-even field")
    if returns:
        rendered_returns = RETURNS_TEMPLATE.format(returns=returns)
    else:
        rendered_returns = ""

    examples = get_examples(section_div, "highlight-python")
    if examples:
        rendered_examples = EXAMPLES_TEMPLATE.format(examples=examples)
    else:
        rendered_examples = ""

    abstract = ABSTRACT_TEMPLATE.format(
        rendered_information=scrub_text(rendered_information),
        rendered_signature=scrub_text(rendered_signature),
        rendered_parameters=scrub_text(rendered_parameters),
        rendered_returns=scrub_text(rendered_returns),
        rendered_examples=scrub_text(rendered_examples)
    )
    return abstract


def get_examples(section_div, examples_class):
    """Parse and return the examples of the documentation topic.

    Parameters
    ----------
    section_div : bs4.BeautifulSoup
        The BeautifulSoup object corresponding to the div with the "class"
        attribute equal to "section" in the html doc file.

    examples_class: Str
        The value of the "class" attribute of the <div> tag within section_div.
        "highlight-python" is the class which are used for examples in the
        webpage.

    Returns
    -------
    Str:
        The examples for the topic.
    """
    example = section_div.find("div", attrs={"class": examples_class})
    if example:
        return example.text.strip()
    return


def get_params(section_div, params_class):
    """Parse and return the parameters or returns of the documentation topic.

    Parameters
    ----------
    section_div : bs4.BeautifulSoup
        The BeautifulSoup object corresponding to the div with the "class"
        attribute equal to "section" in the html doc file.

    params_class: Str
        The value of the "class" attribute of the table row within
        `section_div` that contains the parameters or the returns.
        "field-odd field" for parameters. "field-even field" for returns.

    Returns
    -------
    Str:
        The parameters or returns of the topic.
    """

    parameters = []
    params_row = section_div.find("tr", attrs={"class": params_class})
    if not params_row:
        return
    params_cells = params_row.find("td", {"class": "field-body"})
    if not params_cells:
        return
    for tag in params_cells.children:
        try:
            t = tag.text
            t.strip()
            if t.startswith("\n"):
                t = t.lstrip()
                # The DDG system doesn't like "\\t" in abstract so using 4
                # spaces instead of "\t".
                # noqa See https://github.com/duckduckgo/zeroclickinfo-fathead/pull/584#discussion_r90515652
                t = " "*4 + t
            t = t.replace("\n", " ")
            t.rstrip()
            t = t + "\n"
            parameters.append(t)
        except AttributeError:
            pass

    return ''.join(parameters).rstrip()


def get_related_topics(section_div):
    """Get topics related to the current topic.

    Parameters
    ----------
    section_div : bs4.BeautifulSoup
        The BeautifulSoup object corresponding to the div with the "class"
        attribute equal to "section" in the html doc file.

    Returns
    -------
    Str:
        The related topics in the format expected by the fathead.
    """

    related_topics = []
    try:
        seealso_div = section_div.find("div",
                                       attrs={"class": "admonition seealso"})
        seealso_p = seealso_div.find("p", attrs={"class": "last"})

        related_topics_a = seealso_p.find_all("a")
        for topic in related_topics_a:
            related_topics.append("[[" + topic["title"] + "]]")
    except AttributeError:
        pass

    return "\\\\n".join(related_topics)


def get_redirects(title):
    """Return fathead redirect entries for a fathead title.

    Two redirect entries are generated with for every title. The first removing
    all delimiters(".") in the title, and the second only removing the first
    delimiter.
    e.g. for title "scipy.chararray.capitalize" the redirects created are
    "scipy chararray capitalize" and "scipy chararray.capitalize".

    Parameters
    ----------
    title : str
        The fathead entry title.

    Returns
    -------
    redirects : set
        set containing the redirects for `title`
    """
    redirects = set()
    title_split = title.split(".")
    if title_split[0] == PACKAGE_NAME:
        redirects.add(PACKAGE_NAME + " " + ".".join(title_split[1:]))
    else:
        redirects.add(PACKAGE_NAME + "." + title)
    redirects.add(" ".join(title_split))
    return redirects


def scrub_text(text):
    """Cleans up text.

    Escapes newlines and tabs.

    Parameters
    ----------
    text : str
        Text to clean up.
    """
    scrubbed_text = text.rstrip()
    scrubbed_text = scrubbed_text.replace("\\x", "\\\\x")
    scrubbed_text = scrubbed_text.replace("\0", "\\0")
    scrubbed_text = scrubbed_text.replace("\n", "\\n")
    scrubbed_text = scrubbed_text.replace("\t", " "*4)
    return scrubbed_text


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("docs_folder",
                        help="The folder containing the html docs downloaded using the fetcher script")  # noqa

    parser.add_argument("-p", "--processes",
                        help="Number of parrallel processes to use to parse the documentation (Default: {})".format(CPU_COUNT),  # noqa
                        type=int,
                        default=CPU_COUNT)

    args = parser.parse_args()

    generate_fathead(args.docs_folder, args.processes)
