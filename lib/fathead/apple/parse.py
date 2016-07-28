import sqlite3
import re

# These contains all of the API documentation for iOS 7.1. It was installed by Xcode.
# It only has classes and methods--it doesn't have actual tutorials.
ios = "/Applications/Xcode.app/Contents/Developer/Documentation/DocSets/com.apple.adc.documentation.iOS.docset/Contents/Resources/docSet.dsidx"
osx = "/Applications/Xcode.app/Contents/Developer/Documentation/DocSets/com.apple.adc.documentation.OSX.docset/Contents/Resources/docSet.dsidx"

# This is the link to the docs.
urls = {
    'iOS': "https://developer.apple.com/library/ios/",
    'Mac': "https://developer.apple.com/library/mac/",
}

# The first capture group is the class, and the second one is the method.
def separate(anchor):
    if(anchor):
        p = re.compile(r"//apple_ref/[a-z]+/[a-z]+/(.+?)/?([a-z0-9\:]*)", re.IGNORECASE);
        return p.findall(anchor)[0]

# This formats our disambig list to match the one that is required by a Fathead.
def get_disambig(m, disambig, metadata):
    el = []
    for d in disambig:
        el.append("*[[{}]] {}".format(d, metadata[d + "/" + m]['original']))

    return "\\n".join(el)

# Format the output as specified in https://duck.co/duckduckhack/fathead_overview
def generate_output(result, metadata, inverted, printed):
    abstract_format = "{name}\tA\t\t\t\t\t\t\t\t\t\t{abstract}\t{path}\n"
    disambig_format = "{name}\tD\t\t\t\t\t\t\t\t{disambig}\t\t\t\n"

    f = open('output.txt', 'a')

    for r in result:
        # All classes are unique.
        # They all get the 'A' type.
        if r in metadata:
            f.write(abstract_format.format(**metadata[r]))

        # Let's go through each module in the class.
        for module in result[r]:
            if module not in printed:
                # If we encounter the same module again, don't print it.
                # Why? Since we printed it already before. Printing it again would make a duplicate.
                printed[module] = True

                # Disambiguate if the module appears in different classes.
                if len(inverted[module]) > 1:
                    f.write(disambig_format.format(name=metadata[r + "/" + module]['name'], disambig=get_disambig(module, inverted[module], metadata)))
                # Otherwise, just print it as an abstract since it doesn't appear anywhere else.
                else:
                    f.write(abstract_format.format(name=module, abstract=metadata[r + "/" + module]['abstract'], path=metadata[r + "/" + module]['path']))

    f.close()

def create_fathead(database, platform):
    # Connect to the documentation's sqlite database.
    conn = sqlite3.connect(database)
    c = conn.cursor()

    # Variables that we need for later.
    result, metadata, inverted, printed = {}, {}, {}, {}

    # This long SQL query just gets the details about each class and method.
    for row in c.execute('''select ZTOKENNAME,ZABSTRACT,ZPATH,ZDECLARATION,ZTOKENMETAINFORMATION.ZANCHOR
                            from ZTOKEN,ZTOKENMETAINFORMATION,ZNODEURL
                            where ZTOKEN.ZLANGUAGE=2 and ZTOKENMETAINFORMATION.ZTOKEN=ZTOKEN.Z_PK and ZNODEURL.ZNODE=ZTOKEN.ZPARENTNODE'''):
        name, abstract, path, snippet, anchor = row

        # Skip items that are missing some data.
        if not name or not anchor:
            continue

        # This is the meta data that we're going to attach later.
        pack = {
            "name": name,
            "abstract": abstract or "",
            "path": urls[platform] + path + "#" + anchor,
            "original": abstract or "",
            "platform": platform,
            "snippet": snippet or "",
        }

        # Make sure we encode everything in UTF-8
        for k,v in pack.items():
            pack[k] = v.encode('utf-8')

        if snippet:
            pack['abstract'] = pack['snippet'] + " " + pack['abstract']

        pack['abstract'] = pack['abstract'].replace("\n", "\\n")

        # This variable gets an array.
        # First element is the class, and the second one is the method.
        class_method = separate(anchor)
        c, method = class_method

        # Key: Class, Value: Methods
        # Create a dictionary of classes whose value is a list of modules.
        if c not in result:
            if method:
                result[c] = [method]
                metadata[c + "/" + method] = pack
            else:
                result[c] = []
                metadata[c] = pack
        elif method:
            if method not in result[c]:
                result[c].append(method)
                metadata[c + "/" + method] = pack
        else:
            metadata[c] = pack

        # Key: method, Value: Classes
        # Create a dictionary of methods whose value is a list of classes.
        if method:
            if method not in inverted:
                inverted[method] = [c]
            elif c not in inverted[method]:
                inverted[method].append(c)

    conn.close()
    generate_output(result, metadata, inverted, printed)

create_fathead(ios, 'iOS')
create_fathead(osx, 'Mac')
