import re, urllib.parse

SOURCE_URL = 'http://physics.nist.gov/cgi-bin/cuu/Category?view=pdf&All+values.x=101&All+values.y=17'

def parse_file():
    f = open('download/constants.txt', 'r', encoding='utf-8')
    constants = []
    for line in f:
        parts = re.split(r'\s{2,}', line)
        if len(parts) == 4:
            #tuple of the form (quantity, value, uncertainty, unit)
            constants.append((parts[0], parts[1], parts[2], parts[3]))
    f.close()
    return constants

def gen_fathead(constant):
    return ([
            constant[0],                     #Title
            'A',                             #Type
            '',                              #Redirect
            '',                              #Other uses
            '',                              #Categories
            '',                              #References
            '',                              #See also
            '',                              #Further reading
            '',                              #External links
            '',                              #Disambiguation
            '',                              #Images
            constant[2] + ' ' + constant[3], #Abstract
            SOURCE_URL,                      #Source url
        ])
        
        #use the line below for a link to website with search results
        #'http://physics.nist.gov/cgi-bin/cuu/Results?search_for' + urllib.parse.urlencode({'': constant[0]}))

def main():
    constants = parse_file()
    f = open('output.txt', 'w', encoding='utf-8')

    txt = '';
    for constant in constants:
        for elem in gen_fathead(constant):
            txt += elem + '\t'
        txt += '\n'

    f.write(txt)
    f.close()

if __name__ == "__main__":
    main()
