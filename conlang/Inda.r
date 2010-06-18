REBOL [
	needs: [
		; http://r3.revolucent.net/net.revolucent.conlang.v1.r
		%/Users/gregory/Projects/rebol/net.revolucent.conlang.v1.r
	]
]

mingle2: funct [
	series [string!]
	/except
		exceptions [block!]
][
	result: copy []
	foreach item1 series [
		foreach item2 series [
			unless find exceptions item: rejoin [item1 item2] [
				append/only result item
			]
		]
	]
	result
]

rules: [
	
	onset: rand [
		20 rand "ktpdbhsfrlnm"
		 1 "'"
	]
	
	vowel: rand [
		
		5 rand [
			10 "a"
			 5 "e"
			 2 "o"
		]
		
		1 rand [
			
			5 rand [
				
				rand [
					10 "á"
					 5 "é"
					 2 "ó"
				]

				rand [
					10 "aa"
					 5 "ee"
					 2 "oo"
				]				
				
			]
			
			1 rand [
				
				rand [
					10 "áá"
					 5 "éé"
					 2 "óó"
				]

				rand [
					10 "áa"
					 5 "ée"
					 2 "óo"
				]

				rand [
					10 "aá"
					 5 "eé"
					 2 "oó"
				]							
				
			]
						
		]
				
	]
	
	nucleus: join [
		rand [
			9 ""
			2 "i"
			1 "u"
		]
		vowel
		rand [
			9 ""
			rand "iu"
		]
	]
	
	medial-consonant: rand [
		2 rand [ 
			join ["k" rand "kthsrlnm"]
			join ["t" rand "kthrlm"]
			join ["p" rand "tphsrln"]
			join ["h" rand "ktpsfdbrlnm"]
			join ["s" rand "ktpsfdbrlnm"]
			join ["f" rand "tsfrln"]
			join ["d" rand "sdbrlnm"]
			join ["b" rand "sdbrln"]
			join [rand "rl" rand "ktphsfdbrlnm"]
			join ["n" rand "kthsfdrlnm"]
			join ["m" rand "phsbrlnm"]
		]
		5 rand "ktphsfrlnmdb"		
	]
	
	medial: join [
		nucleus
		medial-consonant
	]
	
	final-consonant: rand [
		""
		rand "ktphsfrlnm"
	]
	
	main: join [
		onset 
		rept 1 3 medial
		nucleus
		final-consonant
	]
]

random/seed now
e: parse-conlang rules
words: copy []
while [20 > length? words] [
	unless find words word: e/eval [append words word]
]
print words
