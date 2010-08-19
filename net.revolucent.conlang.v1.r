{Copyright (c) 2010 Gregory Higley

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.}

REBOL [
	title: "Revolucent Conlang Module"
	name: net.revolucent.conlang	
	type: module
	file: %net.revolucent.conlang.v1.r
	author: "Gregory Higley"
	date: 2010-08-01
	version: 1.0.1
	needs: [
		2.100.99
		http://r3.revolucent.net/net.revolucent.core.v1.r 1.3.4
	]
	exports: [parse-conlang]
	history: [
		1.0.1 {Added a dependency to NET.REVOLUCENT.CORE/PROTECT-MODULE.}
	]
	purpose: {
		The Revolucent Conlang Module provides a way for conlang authors
		to generate random words in a language whose idealized phonemic
		structure is specified in the Conlang dialect.
	}
	license: 'mit
]

conlang!: object [
	
	expression!: object [
		parent: none
		weight: 1
	]
	
	names: make map! []

	make-rand: func [
		/string
			s [string!]
		/local
			spec
	][
		spec: copy []
		either string [
			append spec compose/deep [
				eval: does [to string! random/only (s)]
				tree: does [
					copy ['rand (s)]
				]
			]
		][
			append spec [
				expressions: copy []
				eval: has [e list] [
					list: copy []
					foreach e expressions [
						append/dup list e e/weight
					]
					e: random/only list
					e/eval
				]
				tree: has [list] [
					list: copy []
					foreach e expressions [
						repend list [e/weight e/tree]
					]
					reduce ['rand list]
				]
			]
		]
		make expression! spec
	]
	
	make-join: has [spec] [
		make expression! [
			expressions: copy []
			eval: has [result] [
				result: copy ""
				foreach e expressions [
					append result e/eval
				]
				result
			]
			tree: has [list] [
				list: copy []
				foreach e expressions [
					append list e/tree
				]
				reduce ['join list]
			]
		]
	]
	
	make-rept: func [
		mn [integer!]
		mx [integer!]
	][
		make expression! [
			expressions: copy []
			min-repeat: mn
			max-repeat: mx
			eval: has [e r result] [
				result: copy ""
				e: expressions/1
				repeat n min-repeat [
					append result e/eval
				]
				r: (random (max-repeat - min-repeat + 1)) - 1
				repeat n r [
					append result e/eval
				]
				result
			]
			tree: does [
				reduce ['rept min-repeat max-repeat expressions/1/tree]
			]
		]
	]
	
	make-string: func [
		value [string!]
	][
		make expression! [
			eval: value
			tree: value
		]
	]
	
	make-ref: func [
		value [word!]
	][
		make expression! [
			ref: value
			eval: does [
				names/:ref/eval
			]
			tree: does [
				names/:ref/tree
			]
		]
	]
	
	current: none
	
	push: func [
		o [object!]
	][
		append current/expressions o
		o/parent: current
		o/weight: weight
		weight: 1
		current: o
	]
	
	pop: does [
		current: current/parent
	]
	
	; Some temporary variables for parsing
	e: weight: mn: mx: name: none
		
	expression: [
		'rand set e string! (push make-rand/string e pop)
	|	'rand (push make-rand) into [some [(weight: 1) opt [set weight integer!] expression]] (pop)
	|	'rept set mn integer! set mx integer! (push make-rept mn mx) expression (pop)
	|	'join (push make-join) into [some expression] (pop)
	|	set e string! (push make-string e pop)
	|	set e word! (push make-ref e pop)
	]
	
	conlang: [
		some [
			set name set-word! (current: names/:name: make-join)
			expression
			(current: none)
		]
	]
	
]   

parse-conlang: funct [
	{Parses a block of rules in the CONLANG dialect and returns an object which can be repeatedly
	evaluated to produce a random word according to the given rules.
	
	Example:
	
	; Given the following declaration in the CONLANG dialect:
	
	^-conlang: [
	^-^-main: rept 1 4 join [rand "ktp" rand "aeo"]
	^-]
	
	; We can then do the following:
	
	^->> word-generator: parse-conlang conlang
	^->> random/seed now ; Don't do this before every call to eval. Once per session is enough.
	^->> word-generator/eval
	^-== "ka"
	}
	arg [file! string! block!]
][
	arg: either block? arg [arg] [load/unbound arg]
	conlang: make conlang! []
	either parse arg conlang/conlang [conlang/names/main] [none]
]

protect-module self