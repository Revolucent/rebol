REBOL [
	type: module
	name: net.revolucent.conlang
	exports: [conlang]
]

expression!: object [
	parent: none
	verb: 'join ; default verb for all expressions
]

conlang: object [
		
	parse: funct [
		file [file!]
	][			
		
		current: none
		
		push: func [
			'spec [word! block!]
		]
				
		rand-arg: [
			string!
		|	into [some [opt integer! expression]]
		]		
		
		expression: [
			'rand rand-arg
		|	'rept integer! integer! expression
		|	'join into [some expression]
		|	word!
		|	string!
		]
		
		rules: [
			some [
				set-word!
				expression
			]
		]
		
		system/contexts/system/parse load/unbound file rules
	]
	
]

