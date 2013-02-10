class Node:

	def __init__(self):
		self.word_number = -1
		self.visited = False
		self.cnt = [0 for i in range(26)]
		self.next = {}

class Trie:

	def __init__(self):
		self.root = Node()
		self.word_index = 0
		self.words = []

	def add(self, word):
		iterator = self.root

		# save the word in self.words
		self.words.append(word)

		# save the word in the trie
		for char in word:
			if char not in iterator.next:
				iterator.next[char] = Node()
			iterator = iterator.next[char]
		iterator.word_number = self.word_index
		self.word_index += 1

	def find(self, word):
		iterator = self.root
		for char in word:
			if char not in iterator.next:
				return False
			iterator = iterator.next[char]
		return iterator.is_word