#!/usr/bin/env python
# -*- coding: utf-8 -*-
import itertools

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
        # Type of entry. R = Redirect, A = Article.
        self.entry_type = ''

        # Unique key
        self.key = ''

        # Referenced entry key (only for redirects)
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

    def get_type(self):
        return self.entry_type

    def get_redirects(self):
        redirs = []

        for alt_key in list(self.alternative_keys):
            entry = Entry([
                alt_key,
                'R',
                self.key
            ])
            redirs.append(entry)

        return redirs

    def get_alternatives(self):
        return self.alternative_keys

    def get_entry(self):
        return '\t'.join([
            self.key,             # title / key
            self.entry_type,      # entry type
            self.reference,       # no redirect data
            '',                   # ignore
            self.category,        # categories
            '',                   # ignore
            self.related,                   # no related topics
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

    for line in f.readlines():
        try:
            # Parse entry
            entry = Entry(line)

            if entry.get_type() == 'R':
                continue

            key = "'" + entry.get_key() + "'"

            # Do we have the entry yet?
            if key not in output or '3.5/library/functions.html' in str(entry):
                output[key] = str(entry)
            elif '3.5/library/functions.html' not in str(entry):
                del output[key]
                duplicate_count += 1

            # Get all possible redirect entries
            redirects = entry.get_redirects()
            for redirect in redirects:
                key = redirect.get_key()
                if key not in output or '3.5/library/functions.html' in str(redirect.get_entry()):
                    output[key] = str(redirect.get_entry())
                else:
                    del output[key]
                    duplicate_count += 1
        except BadEntryException as e:
            pass  # Continue execution entry data is invalid.

    print(duplicate_count)

    with open('output2.txt', 'w') as output_file:
        for key, line in output.items():
            tsv = '{}\n'.format(line)
            output_file.write(tsv)

if __name__ == "__main__":
    # Open output file for reading and writing.
    with open('output.txt', 'r+') as output_file:
        generate_redirects(output_file)
