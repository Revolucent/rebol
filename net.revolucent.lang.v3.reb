REBOL [
	Type: module
	Exports: [make-lang]
	Needs: [net.revolucent.series.v3]
]                                                        

current-action: none
current-word: none
current-weight: 1

push: func [
	action [object!]
][
	current-action/add-action action
	action
]
pop: does [current-action: current-action/parent]

make-literal: funct [
	value [string!]
][
	object compose/deep [
		parent: none
		resolve: does [(value)]
	]
]

make-get: funct [
	root-action [object!]
	word [word!]
][
	object compose/deep [
		parent: none
		resolve: does [
			use [word root-action] [
				word: (to lit-word! word)
				root-action: (root-action)
				root-action/sub-actions/:word/resolve
			]
		]
	]
]

make-choice: does [
	object [
		parent: none
		sub-actions: copy []
                add-action: func [
			action [object!]
		][
			action/parent: self
			loop current-weight [
				append sub-actions action
			]
		]
		resolve: does [
			use [action] [
				action: random/only sub-actions
				action/resolve
			]
		]
	]
]

make-concat: does [
	object compose/deep [
		parent: none
		sub-actions: copy []
		add-action: func [
			action [object!]
		][
			action/parent: self
			append sub-actions action
		]
		resolve: does [
			use [result] [
				result: copy {}
				foreach action sub-actions [
					result: ajoin [result action/resolve]
				]
				result
			]
		]
	]
]

make-repeat: does [
	object compose/deep [
		parent: none
		sub-action: none
		add-action: func [
			action [object!]
		][
			action/parent: self
			sub-action: action
		]
		resolve: does [
			use [result] [
				result: copy {}
				loop random/only range [(min-repeat) - (max-repeat)] [
					result: ajoin [result sub-action/resolve]
				]
				result
			]
		]
	]
]

make-root: does [
	object [
		sub-actions: map []
		add-action: func [
			action [object!]
		][
			action/parent: self
			sub-actions/:current-word: action
		]
		resolve: does [
			sub-actions/main/resolve
		]
	]
]

value: none
item-rule: [
	set value word! (push make-literal to string! value)
|	set value string! (push make-literal value)
	; Note that root-action gets explicitly bound inside the make-lang function.
|	set value get-word! (push make-get root-action to word! value)
|	into concat-rule
|	into choice-rule
|	into repeat-rule
]

concat-rule: [
	'+
	(current-action: push make-concat)
	some item-rule
	(pop)
]

choice-rule: [
	'|
	(current-action: push make-choice)
	some [
		(current-weight: 1)
		opt [set current-weight integer!]
		item-rule
	]
	(pop)
]

min-repeat: max-repeat: none
repeat-rule: [
	set min-repeat integer!
	set max-repeat integer!
	'*
	(current-action: push make-repeat)
	item-rule
	(pop)
]

lang-rule: [
	some [
		set current-word set-word!
		item-rule
	]
]

make-lang: func [
	lang-def [block!]
	/debug
	/local
		root-action
][
	current-action: root-action: make-root
	current-word: none
	current-weight: 1
	bind item-rule 'root-action
	parse lang-def lang-rule
	either debug [root-action] [:root-action/resolve]
]


