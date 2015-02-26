#!/usr/bin/python
import unittest
import parse

class TestParse(unittest.TestCase):

  def setUp(self):
    pass

  def test_read_files_with_documentation(self):
    # test number of directories
    self.assertEqual(2272, len(parse.collectDocFilesFrom("./docs/api/java")))
    self.assertEqual(2892, len(parse.collectDocFilesFrom("./docs/api/javax")))

  def test_read_documentation_directory_and_file(self):
    (clazz, description) = parse.getClass('./docs/api', 'java/lang/String.html')
    self.assertEqual(u'Class String', clazz)
    # check for an almost random piece in the documentation
    self.assertTrue("String concatenation is implemented" in description)

if __name__ == '__main__':
    unittest.main()

