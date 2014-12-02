#!/usr/bin/python
import unittest
import parse_utils

class TestParse(unittest.TestCase):

  def setUp(self):
    pass

  def test_remove_keywords(self):
    self.assertEqual("SQLWarning", parse_utils.remove_keywords("Class SQLWarning"))
    self.assertEqual("StandardCopyOption", parse_utils.remove_keywords("Enum StandardCopyOption"))
    self.assertEqual("DriverAction", parse_utils.remove_keywords("Interface DriverAction"))
    self.assertEqual("Native", parse_utils.remove_keywords("Annotation Type Native"))

  def test_read_files_with_documentation(self):
    # test number of directories
    self.assertEqual(2272, len(parse_utils.collectDocFilesFrom("./docs/api/java")))
    self.assertEqual(2892, len(parse_utils.collectDocFilesFrom("./docs/api/javax")))

  def test_description_is_cut_down(self):
    self.assertEqual("1234567890.", 
	parse_utils.cutlength("1234567890.1234567890"))

  def test_description_is_not_cut_when_it_is_short(self):
    self.assertEqual("This is a short description. It has multiple sentences.", parse_utils.cutlength("This is a short description. It has multiple sentences. 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"))

  def test_read_documentation_directory_and_file(self):
    (clazz, description, url) = parse_utils.getClass('./docs/api', 'java/lang/String.html')
    self.assertEqual(u'Class String', clazz)
    # check for an almost random piece in the documentation
    self.assertEqual(u'The String class represents character strings.', description)

  def test_do_not_concat_if_no_class_found(self):
    line = parse_utils.concat_list(["", "", ""])
    self.assertTrue(line.startswith("No class found"))

if __name__ == '__main__':
    unittest.main()
