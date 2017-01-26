#!/usr/bin/python
from BeautifulSoup import BeautifulSoup
import json
import markdown
import os
import re


def main():
    cwd = os.path.dirname(__file__)
    data_path = os.path.join(cwd, 'download', 'data.json')
    output_path = os.path.join(cwd, 'output.txt')
    with open(data_path, 'r') as f:
        data = json.loads(f.read())

    answers = generate_answers(data)
    csv = u'\n'.join(u'\t'.join(line) for line in answers)
    csv = csv.encode('utf-8') + '\n'
    with open(output_path, 'w') as f:
        f.write(csv)


def generate_answers(data):
    answers = []
    for feature, feature_data in data['data'].items():
        # Generate titles of possible search terms
        title = feature_data['title'].lower().strip()
        removePlural = re.sub(r'[s]$', '', feature)
        titles = set([
            feature,
            feature.replace('-', ' '),
            removePlural.replace('-', ' '),
            title,
            u' '.join(re.split('[ -]', title))
        ])
        print u','.join(titles)

        # Commented out for now -- we can revive if we ever have a way to display
        # external links in the Related Topics Infobox
        ## Generate Related Topics
        #links = feature_data.get('links', [])
        #related = u' '.join([u'[[{} {}]]'.format(link['title'], link['url']) for link in links])
        related = ''

        # Generate abstract
        abstract = u'<p>{}</p>'.format(feature_data['description'])

        for browser in ['ie', 'firefox', 'chrome', 'safari', 'ios_saf', 'android']:
            agent = data['agents'][browser]
            out = browser_support(
                browser=agent['browser'],
                prefix=agent['prefix'],
                stats=feature_data['stats'][browser])
            abstract += u'<br>' + out

        # Add notes to abstract, if there are any
        notes = feature_data.get('notes', '')
        if notes:
            notes_in_html = markdown.markdown(notes)
            bs = BeautifulSoup(notes_in_html)
            for a in bs.findAll('a'):
                a.replaceWithChildren()
            contents = bs.renderContents()
            abstract += u'<p><b>Notes:</b> {}</p>'.format(contents.decode('utf-8'))

        abstract = '<section class="prog__container">' + abstract.replace('\n', '').replace('\r', '') + '</section>'
        print abstract
        print '------------------------------------------'

        source_url = u'http://caniuse.com/' + feature
        for title in titles:
            answers.append([
                title,      # Title
                'A',        # Type
                '',         # Redirect
                '',         # Other uses
                '',         # Categories
                '',         # References
                related,    # See also
                '',         # Further reading
                '',         # External links
                '',         # Disambiguation
                '',         # Images
                abstract,   # Abstract
                source_url  # Source URL
            ])
    return answers


def browser_support(browser, prefix, stats):
    """
    Returns a string describing browser support for a feature.

    E.g. "Firefox 22+ x"

    Stats:
    y - (Y)es, supported by default
    a - (A)lmost supported (aka Partial support)
    n - (N)o support, or disabled by default
    p - No support, but has (P)olyfill
    u - Support (u)nknown
    x - Requires prefi(x) to work

    https://github.com/Fyrd/caniuse/blob/master/Contributing.md
    """
    # Browser versions, sorted newest to oldest
    versions = sorted(stats.items(), key=lambda v: v[0], cmp=version_cmp)

    # Find the earliest version which has equal support to latest
    scores = {'y': 3, 'a': 2, 'x': -1, 'p': -2, 'n': -3}
    current = versions[0]
    for version in versions:
        current_score = sum(scores.get(c, 0) for c in current[1])
        score = sum(scores.get(c, 0) for c in version[1])
        if score > 0 and score >= current_score:
            current = version
        else:
            break

    v, stats = current
    out = browser + ' ' + v
    if 'y' in stats:
        # html entity, unicode checkmark
        out += '+ &#10003;'
    elif 'a' in stats:
        out += '+ partial'
    elif 'n' in stats:
        # html entity, unicode x
        out += ' &#10007;'
    if 'x' in stats:
        out += ' (-' + prefix + ')'
    if 'p' in stats:
        out += ' polyfill'
    return out


def version_cmp(a, b):
    """Sort versions from newest to oldest"""
    # "TP" is the Technology Preview version, and this version is by definition
    # the latest version
    if a == b == 'TP':
        return 0
    if a == 'TP':
        return -1
    elif b == 'TP':
        return 1
    d = version2float(b) - version2float(a)
    return 1 if d > 0 else -1 if d < 0 else 0


def version2float(s):
    """Converts a version string to a float suitable for sorting"""
    # Remove 2nd version: '4.1.1-4.5.5' --> '4.1.1'
    s = s.split('-')[0]
    # Max one point: '4.1.1' --> '4.1'
    s = '.'.join(s.split('.')[:2])
    # Remove non numeric characters
    s = ''.join(c for c in s if c.isdigit() or c == '.')
    return float(s)


if __name__ == '__main__':
    main()
