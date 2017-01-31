import string
import sys
import cProfile

class Word:

	def __init__(self, word):
		self.real = word
		self.frequency = [0 for i in range(26)]
		for c in word:
			self.frequency[ord(c) - ord("a")] += 1

	@staticmethod
	def valid(word):
		for c in word:
			if ord(c) - ord('a') >= 26:
				return False
		return True

	@staticmethod
	def validate(word):
		valid = ""
		for c in word:
			if ord(c) - ord('a') < 26 and ord(c) - ord('a') >= 0:
				valid += c
		return valid

	def contains(self, word):
		for i in range(26):
			if self.frequency[i] < word.frequency[i]:
				return False
		return True

class FrequencyList:

	def __init__(self):
		# set a maximum of 10000 for the length of a word
		self.words = [[] for i in range(10000)]

	def add(self, word):
		if Word.valid(word):
			self.words[len(word)].append(
				Word(word)
			)

class Anagram:

	def __init__(self, needle, min_length, max_length):
		self.frequency_list = FrequencyList()

		# save the words in the linear solver
		self.load_data(self.frequency_list)

		# save the 'needle'
		self.needle = string.lower(needle)
		self.min_length = min_length
		self.max_length = max_length

	def load_data(self, structure):
		dictionary = open("dictionary", "r")
		for line in dictionary:
			structure.add(line[:-1])

	def solve(self):
		self.results = []
		self.execute_linear_solver()

	def execute_linear_solver(self):
		needle = Word(Word.validate(self.needle))
		for j in range(min_length, max_length + 1):
			words = self.frequency_list.words[j]
			total = 0
			results = []
			for i in range(len(words)):
				if needle.contains(words[i]):
					results.append(words[i].real)
					total += 1

			# j is the length to be checked (min_length <= j <= max_length)
			# results is a sorted list with all the valid words of length j
			# total is the total number of anagrams found with length j 
			results.sort()
			self.results.append(
				"%d words of length (%d) found: %s" % (total, j, results)
			)

if __name__ == '__main__':
	# if len(sys.argv) < 3:
	# 	raise NameError('A word is needed')
	
	# write file
	output = open('output.txt', 'wb');

	# catch args
	# anagram is sys.argv[1]
	word = sys.argv[2]
	min_length = 5
	max_length = 5
	
	if (len(sys.argv) > 3):
		min_length = max_length = int(sys.argv[3])
	if (len(sys.argv) > 4):
		max_length = max(int(sys.argv[4]), min_length)
		
	# cProfile.run('anagram = Anagram(word, min_length, max_length)')
	# cProfile.run('anagram.solve()')	
	anagram = Anagram(word, min_length, max_length)	
	anagram.solve()
	output.write('\n'.join(anagram.results))
