#!/usr/bin/env python
# -*- coding: utf-8 -*-
import copy
import itertools
import re

built_in = ['abs','dict','help','min','setattr','all','dir','hex','next',
'slice','any','divmod','id','object','sorted','ascii','enumerate','input',
'oct','staticmethod','bin','eval','int','open','str','bool','exec','isinstance',
'ord','sum','bytearray','filter','issubclass','pow','super','bytes','float',
'iter','print','tuple','callable','format','len','property','type','chr',
'frozenset','list','range','vars','classmethod','getattr','locals','repr',
'zip','compile','globals','map','reversed','import','complex','hasattr',
'max','round','delattr','hash','memoryview','set']

ignore_keys = ['The Python Tutorial']

bad_strings = ['\\000', '\\xe0']

class BadEntryException(Exception):
    """
    Thrown when entry data is invalid
    """
    pass


class Entry(object):
    """
    Represents a single entry in fathead output.txt datafile
    """
    def __init__(self, obj):
        # Type of entry. R = Redirect, A = Article, D = Disambiguation
        self.entry_type = ''

        # Unique key
        self.key = ''

        # Referenced entry key (only for redirects)
        # i.e. the redirect target
        self.reference = ''
        self.abstract = ''
        self.anchor = ''

        self.category = ''
        self.related = ''

        # Alternative keys used for redirects
        self.alternative_keys = []
        self.parse(obj)

    def __str__(self, obj):
        return self.alternative_keys

    def parse(self, input_obj):
        """
        Try to parse given input into a valid entry.
        Args:
            input_obj: TSV string or list of data.

        Returns:
            List of data
        Raises:
            Throws BadEntryException if data is invalid.
        """
        if isinstance(input_obj, str):
            processed = input_obj.split('\t')
            self.data = processed
        elif isinstance(input_obj, list):
            self.data = input_obj
        try:
            self.key = self.data[0].strip()
            self.entry_type = self.data[1].strip()
            self.reference = self.data[2].strip()
            if len(self.data) > 3:
                self.category = self.data[4].strip()
                self.related = self.data[6].strip()
                self.abstract = self.data[11].strip()
                self.anchor = self.data[12].strip()
            elif self.entry_type == 13 and len(self.data) != 13:
                raise BadEntryException
        except Exception as e:
            raise BadEntryException('Article had invalid number of elements.')

        if self.entry_type == 'A':
            self.parse_alternative_keys()

        return self.data

    def parse_alternative_keys(self):
        """
        Find alternative keys to use in generated redirects

        Returns:
            Set of possible redirect entries
        """
        self.alternative_keys = set()

        if '.' in self.key and self.entry_type == 'A':
            key_arr = self.key.split('.')
            method_name = key_arr[-1]
            key_arr_len = len(key_arr)

            # always add method name as a key
            self.alternative_keys.add(method_name)

            # add all permutations of package and class
            if key_arr_len >= 3:

                for l in range(key_arr_len-1):
                    permutations = itertools.permutations(key_arr[:key_arr_len-1], l+1)

                    for k in permutations:
                        new_key = "{} {}".format(' '.join(k), method_name)
                        self.alternative_keys.add(new_key)
            else:
                package_name = key_arr[0]
                new_key = "{} {}".format(package_name, method_name)
                self.alternative_keys.add(new_key)

        return self.alternative_keys

    def get_data(self):
        return self.data

    def get_key(self):
        return self.key

    def set_entry_type(self, new_type):
        self.entry_type = new_type
        
    def get_related(self):
        return self.related
    
    def set_related(self, new_related):
        self.related = new_related

    def get_type(self):
        return self.entry_type

    def get_redirects(self):
        redirs = []
        for alt_key in list(self.alternative_keys):
            if alt_key in built_in:
                continue
            entry = Entry([
                    alt_key,
                    'R',
                    self.key
                ])
            redirs.append(entry)

        return redirs

    def get_alternatives(self):
        return self.alternative_keys

    def get_abstract(self):
        return self.abstract
    
    def get_reference(self):
        return self.reference

    def get_entry(self):
        return '\t'.join([
            self.key,             # title / key
            self.entry_type,      # entry type
            self.reference,       # no redirect data
            '',                   # ignore
            self.category,        # categories
            '',                   # ignore
            self.related,         # related topics
            '',                   # ignore
            '',                   # add an external link back to page
            '',                   # no disambiguation
            '',                   # images
            self.abstract,        # abstract
            self.anchor           # anchor to specific section
        ])

    def __str__(self):
        return self.get_entry()

def generate_redirects(f):
    output = dict()

    # For debugging purposes
    duplicate_count = 0
    nbr_of_disambiguations = 0
    # First, add all articles to output, so that package names are 
    # already in output to be able to correctly generate related articles
    for line in f.readlines():
        try:
            # Parse entry
            entry = Entry(line)
            key = entry.get_key()
                
            # Ignore redirects from parse.py, generate them in 
            # get_redirects instead.
            if entry.get_type() == 'R' or entry.get_key() in ignore_keys:
                continue               

            # Do we have the entry yet?
            if key not in output:
                output[key] = str(entry)
                
        except BadEntryException as e:
            pass  # Continue execution entry data is invalid.
    
    # Now, go through all articles we have in output, and generate related 
    # fields, redirects etc. Store them in sets, so that any 
    # redirect/disambiguation only appear once in the field
    related_fields = dict()
    disambiguations = dict()
    # Need to deepcopy the iterator items, because output will change in 
    # the loop
    iterator_items = copy.deepcopy(output).keys()
    for key in iterator_items:
        entry = Entry(output[key])
        split_key = key.split('.')
        package_name = '.'.join(split_key[0:len(split_key)-1])

        if package_name in output \
            and output[package_name].startswith(package_name + '\t' + 'A'):
            #Add related links to this entry to the package entry
            if package_name not in related_fields:
                related_fields_for_article = set()
                related_fields_for_article.add(key)
                related_fields[package_name] = related_fields_for_article
            else:
                related_fields[package_name].add(key)

        # Get all possible redirect entries
        key_length = len((re.findall(r"[\w']+", key)))
        if key_length > 1:
            redirects = entry.get_redirects()
            for redirect in redirects:
                redirect_key = redirect.get_key()
                redirect_entry = redirect.get_entry()
                if redirect_key not in output:
                    output[redirect_key] = str(redirect_entry)
                elif str(output[redirect_key]) != str(redirect_entry):
                    #Create a disambiguation
                    if output[redirect_key].startswith(redirect_key + '\tR'):
                        current_entry = Entry(output[redirect_key])
                        #Replace the existing redirect with a disambiguation
                        output[redirect_key] = str(redirect_key) + '\t' + 'D' +'\t\t\t\t\t\t\t\t'
                        # Add the redirect currently in output to the disambigutions
                        current_entry_article_redirect_target = Entry(output[current_entry.get_reference()])
                        disambiguation_for_this_key = set()
                        disambiguation_for_this_key.add((str(current_entry.get_reference()),
                                                         str(current_entry_article_redirect_target.get_abstract())))
                        # Add the redirect this line wanted to add to the disambigutions
                        article_redirect_target = Entry(output[redirect.get_reference()])
                        disambiguation_for_this_key.add((str(redirect.get_reference()),
                                                        str(article_redirect_target.get_abstract())))
                        disambiguations[redirect_key] = disambiguation_for_this_key
                        duplicate_count += 1
                        nbr_of_disambiguations += 1
                    elif output[redirect_key].startswith(redirect_key + '\tD'):
                        # Another disambiguation detected, append it to the entry
                        disambiguations[redirect_key].add((str(redirect.get_reference()),
                                                        str(article_redirect_target.get_abstract())))
                        duplicate_count += 1

    # Now, we add the related fields and disambiguations to output
    for key, related_fields_set in related_fields.items():
        output_entry = Entry(output[key])
        related = ''
        for r in related_fields_set:
            if related != '':
                related += '\\\\n'
            related += '[[' + str(r) + ']]'
        output_entry.set_related(related)
        output[key] = output_entry.get_entry()
        
    for key, disambiguations_set in disambiguations.items():
        disambiguation_str = ''
        for disambiguation_key, abstract in disambiguations_set:
            disambiguation_entry_str = ''
            if disambiguation_str != '':
                disambiguation_entry_str += '\\n'
            disambiguation_entry_str += '*' + '[['+disambiguation_key+']] '
            disambiguation_entry_str += abstract
            disambiguation_str += disambiguation_entry_str
        # Disambiguation field is third last, need to add tabs to the end to
        # get a valid entry
        output[key] += disambiguation_str + "\t\t\t"
        
    print("Duplicates: %s" % duplicate_count)
    print("Disambiguations: %s" % nbr_of_disambiguations)

    # Adds python versions as subtitle.
    for line in output:
        try:
            entry = Entry(output[line])
            if entry.get_type() == 'A':
                if output[line].startswith('python2'):
                    output[line] = output[line][:output[line].index('\t')] + ' (python2)' + output[line][output[line].index('\t'):]
                else:
                    output[line] = output[line][:output[line].index('\t')] + ' (python3)' + output[line][output[line].index('\t'):]
        except:
            pass

    with open('output2.txt', 'w') as output_file:
        for key, line in output.items():
            if line.endswith('\\n'):
                line = line[:-2]
                line += '\t\t\t'
            line = bad_string_check(line)
            tsv = '{}\n'.format(line)
            output_file.write(tsv)

def bad_string_check(line):
    for string in bad_strings:
        if string in line:
            replacement = '\\' + string
            line = line.replace(string, replacement)
    return line

if __name__ == "__main__":
    # Open output file for reading and writing.
    with open('output.txt', 'r+') as output_file:
        generate_redirects(output_file)
