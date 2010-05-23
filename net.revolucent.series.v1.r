REBOL [
	author: "Gregory Higley"
	title: "Revolucent Series Module"
	name: net.revolucent.series
	type: module
	version: 1.8.0
	history: [
		1.8.0 {Added CHANGE-EACH.}
		1.7.0 {Added DELIMIT and ENCOMMA.}
		1.6.1 {Improved the implementation of DROP-WHILE.}
		1.6.0 {Added DROP-WHILE.}
		1.5.0 {Added PERMUTE. Removed the /AT refinement from INTERPOLATE.}
		1.4.4 {Changed INTERPOLATE to be more flexible.}
		1.4.3 {Changed FILTER to use SIFT internally.}
		1.4.2 {Upgraded to 2.100.90.}
		1.4.1 {Fixed RANGE for 2.100.87.}
		1.4.0 {Added CARTESIAN and COMP.}
		1.3.0 {Added FILTER and SIFT.}
		1.2.0 {Added EACH.}
		1.1.1 {Gave the module a name. D'oh!}
		1.1.0 {Added the FOLD function.}
		1.0.0 {Added INTERPOLATE and RANGE functions.}
	]	
	needs: [2.100.94]	
	exports: [
		cartesian
		change-each
		comp
		delimit
		drop-while
		each
		encomma
		filter
		fold
		interpolate
		permute
		range
		sift
	]
]

change-each: funct [
	{Changes each value in a series. (Modifies)}
	'word [word!]
	series [series!]
	expression [block!]
	/only
	/dup
		count [number! pair!]
][
	start: series
	fn: func reduce [word] expression
	while [! tail? series] [
		series: apply :change [
			series
			fn first series
			false none ; /part length
			only ; /only
			dup count ; /dup count
		]
	]
	start
]

interpolate: funct [
	{Interpolates a value into a series. (Modifies)}
	series [series!] {Series at point to interpolate}
	value [any-type!] {Value to interpolate into the series}
	/only {Interpolates a block as a block}
	/append {Adds the value at the end also}
][
	current: series
	while [! tail? current] [
		current: next apply :insert [current :value false none only]
	]
	if append [
		; At this point we can overwrite the value of the local append
		append: get in system/contexts/system 'append
		apply :append [current :value false none only]
	]
	series
]

range: funct [
	{Generates a range of integers using a very small DSL, to wit:
	
^-^-range [1 - 3 7 2 - 4] generates the block [1 2 3 7 2 3 4].
}
	spec [block!] {Specification block written in the range DSL}
	/vector {Specify this refinement to return the result as a vector}
		bytes [integer!] {The byte size of the vector to return (16, 32, 64)}
	/local n p1 p2
][
	rules: [
		some [
			set p1 integer! '- set p2 integer! (append values make pair! reduce [p1 p2])
		|	set n integer! (append values n)
		]
	]	
	values: copy []
	unless parse spec rules [
		do make error! "The range DSL was not syntactically valid."
	]
	result: copy []
	foreach value values [
		either pair? value [
			bump: either (first value) > (second value) [-1] [1]
			for n first value second value bump [
				append result n
			]
		][append result value]
	]
	either vector [
		make vector! reduce ['integer! bytes result]
	][result]
]

filter: funct [
	{Filters a series and returns the series thus filtered. (Modifies)}
	'word [word!] {Word to set each time}
	series [series!] {The series to filter}
	body [block!] {The block to execute each time}
][
	fun: func reduce [word] body
	sift series :fun
]

sift: func [
	{Filters a series using a function and returns the series thus filtered. (Modifies)}
	series [series!] {The series to filter}
	fun [any-function!] {The function to execute each time}
][
	remove-each item series [
		! fun item
	]
	series
]

fold: func [
	{Reduces a series to a single value using the operation specified in the given block. E.g.,
	
^-^-fold x y [1 2 3] [x + y]

^-This results in the value 6, which is 1 + 2 + 3.
}
	'arg1 [word!] {First word to set each time. (local)}
	'arg2 [word!] {Second word to set each time. (local)}
	series [series!] {The series over which to iterate}
	body [block!] {The block of code to execute}
	/local
		fun
		result
][
	fun: func reduce [arg1 arg2] body
	result: first series
	foreach elem next series [
		result: fun result elem
	]
	result
]

each: func [
	{Applies a function to each element of a series.}
	series [series!]
	fun [any-function!] {The function to apply to each element}
	/into {When this refinement is specified, the results of the operation are placed into the result series, which is then returned}
		result [series!] {The series into which to place the mapped elements}
][
	either into [
		foreach element series [append/only result fun :element]
		result
	][
		foreach element series [fun :element]
		exit
	]
]

cartesian: funct [
	{Given a block of blocks, calculates the cartesian product. E.g.,

^-^-cartesian [[1 2 3] [4 5]]
^-^-== [[1 4] [1 5] [2 4] [2 5] [3 4] [3 5]]

^-My thanks to Sunanda for this excellent code.
}
	[throw]
	lists [block!] {A block of blocks}
][
	unless parse lists [some block!] [
		do make error! "The 'lists' argument must consist of a block of blocks."
	]
	lists: copy lists
	len: 1
	result: make block! foreach list lists [len: len * length? list]
	len: length? lists
	until [
		list: clear []
		loop i: len [
			insert list lists/:i/1
			i: i - 1
		]
		result: change/only result copy list
		loop i: len [
			unless tail? lists/:i: next lists/:i [break]
			if i = 1 [break]
			lists/:i: head lists/:i
			i: i - 1
		]
		tail? lists/1
	]
	head result
]

comp: funct [
	{Given a block in the list comprehesion DSL, calculates the comprehension. E.g.,
	
^-^-comp [[x * y] for x in [1 2 3] for y in [4 5 6] where [odd? x]]
^-^-== [4 5 6 8 10 12 12 15 18]
}
	[throw]
	comprehension [block!]
	/local
		body
		word
		list
][
	lists: make map! []
	where: none
	rules: [
		set body block!
		some [
			'for
			set word word!
			'in
			set list block!
			(lists/:word: list)
		]
		opt [
			'where
			set where block!
		]
	]
	unless parse comprehension rules [
		do make error! "The syntax of the list comprehension was invalid."
	]
	args: words-of lists
	fn: func args body
	where: either where [func args where] [func args [true]]
	result: copy []
	foreach list cartesian reflect lists 'values [
		if apply :where list [
			append result apply :fn list
		]
	]
	result
]

permute: funct [
	{Returns a block containing all the permutations of the given series}
	series [series!]
][
	if equal? 1 length? series [
		return reduce [copy series]
	]
	if equal? 2 length? series [
		return reduce [copy series reverse copy series]
	]
	result: reduce [
		two: copy/part series 2
		reverse copy two
	]
	for n 3 length? series 1 [
		intermediate: copy []
		for r 1 length? result 1 [
			for i 1 n 1 [
				p: either odd? r [abs ((n - i) + 1)] [i]
				append/only intermediate head insert/only at (copy first at result r) p series/:n
			]
		]
		result: copy intermediate
	]
	result
]

drop-while: funct [
	{Removes items from the beginning of a series as long as a condition is met. (Modifies)}
	'word [word!]
	data [series!]
	body [block!]
][
	removing: true
	cond: func reduce [word] body
	remove-each item data [
		removing: all [removing cond item]
	]
	data
]

delimit: funct [
	{Returns a string created by putting the specified separator between each of the items in the series.}
	series [series!]
	separator [string! char!]
][	
	switch/default type?/word interpolated: head interpolate/only at copy series 2 separator [
		string! [interpolated]
		block! [ajoin interpolated]
	][
		result: copy []
		foreach item interpolated [
			append/only result item
		]
		ajoin result
	]
]

encomma: funct [
	{Returns a string with the each item of series separated by a comma.}
	series [series!]
][
	delimit series ", "
]