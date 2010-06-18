REBOL [
	needs: [
		%/Users/gregory/Projects/rebol/net.revolucent.conlang.v1.r
	]
]

; q k t ' h s r l n a i u ay aw al

rules: [

	onset: rand [
		7 rand "kt'hsln"
		2 rand [
			join ["k" rand "thsrn"]
			join ["t" rand "qkhsln"]
		]
	]
	
	nucleus: rand [
		7 rand [
			5 "a"
			2 rand "iu"
		]
		2 rand [
			2 rand [ "ay" "al" ]
			1 "aw"
		]
	]
	
	main: join [
		rept 1 5 join [onset nucleus]
		rand [
			""
			":"
		]
		rand [
			""
			"/"
		]
	]
]

fix: funct [word [string!]] [
	if any [find word "q" find word "k" find word "'"] [
		replace/all word "l" "r"
	]
	if find word "/" [
		replace/all word "k" "q"
		replace/all word "/" ""
	]
	if find word ":" [
		replace/all word "s" "c"
		replace/all word ":" ""
	]
	word
]

random/seed now
e: parse-conlang rules
words: copy []
while [20 > length? words] [
	unless find words word: fix e/eval [append words word]
]
print words