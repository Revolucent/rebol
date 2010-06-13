REBOL [
	type: module
	name: net.revolucent.conlang
	exports: [parse-conlang]
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
		]
	]
	
	make-string: func [
		value [string!]
	][
		make expression! [
			eval: value
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
		]
	]
	
	current: none
	
	push: func [
		o [object!]
	][
		append current/expressions o
		o/parent: current
		current: o
	]
	
	pop: does [
		current: current/parent
	]
	
	; Some temporary variables for parsing
	e: n: mn: mx: name: none
		
	expression: [
		'rand set e string! (push make-rand/string e pop)
	|	'rand (push make-rand) into [some [(n: 1) opt [set n integer!] expression (set in last current/expressions 'weight n)]] (pop)
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
