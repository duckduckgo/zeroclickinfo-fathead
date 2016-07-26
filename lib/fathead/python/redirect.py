#!/usr/bin/env python
# -*- coding: utf-8 -*-


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
        self.type = None

        # Unique key
        self.key = None

        # Alternative keys used for redirects
        self.alternative_keys = []
        self.parse(obj)

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
            processed = input_obj.split('\t', 13)
            self.data = processed
        elif isinstance(input_obj, list):
            self.data = input_obj

        # Entry must contain 13 elements.
        if len(self.data) != 13:
            raise BadEntryException('Bad entry format')

        self.key = self.data[0]
        self.type = self.data[1]
        self.parse_alternative_keys()

        return self.data

    def parse_alternative_keys(self):
        """
        Find alternative keys to use in generated redirects

        Returns:
            Set of possible redirect entries
        """
        self.alternative_keys = set()

        if self.key.count('.') > 0:
            key_arr = self.key.split('.')
            method_name = key_arr[-1]
            package_name = key_arr[0]
            new_key = "{} {}".format(package_name, method_name)
            self.alternative_keys.add(new_key)
            self.alternative_keys.add(method_name)
        return self.alternative_keys

    def get_data(self):
        return self.data

    def get_key(self):
        return self.key

    def get_type(self):
        return self.type

    def get_redirects(self):
        # @TODO: Generate R entries
        return []

    def get_alternatives(self):
        return self.alternative_keys


def generate_redirects(f):
    # @TODO: work in progress
    possible_redirects = set()
    for line in f.readlines():
        try:
            # Parse entry
            entry = Entry(line)
            # Get all possible redirect entries
            possible_redirects = possible_redirects | entry.get_alternatives()
        except BadEntryException as e:
            pass  # Continue execution entry data is invalid.


if __name__ == "__main__":
    # Open output file for reading and writing.
    with open('output.txt', 'r+') as output_file:
        generate_redirects(output_file)
