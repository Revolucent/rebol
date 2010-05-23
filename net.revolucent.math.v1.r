REBOL [
	author: "Gregory Higley"
	type: module
	name: net.revolucent.math
	version: 1.9.0
	history: [
		1.9.0 {Added FROM-BASE and TO-BASE.}
		1.8.0 {Added INTEGER-SEQUENCE.}
		1.7.0 {Added AVERAGE. Added NUMBERS! Fixed FACTORS. Fixed SUM and PRODUCT for empty lists. Losened some type signatures.}
		1.6.1 {Fixed a bug in FACTORIAL due to a change in how INTERPOLATE works.}
		1.6.0 {Added LONG-DIVIDE and REPEAT-LENGTH.}
		1.5.0 {Added DIVISORS, FACTORS, AND FACTORIAL.}
		1.4.1 {Updated FIBONACCI and ROWLAND.}
		1.4.0 {Added ROWLAND.}
		1.3.0 {Added GCD.}
		1.2.0 {Added FIBONACCI.}
		1.1.1 {Added /RECURSE refinement to DIGITAL-PRODUCT and DIGITAL-SUM functions. Added /SKIP0 refinement to DIGITAL-PRODUCT.}
		1.1.0 {Added DIGITAL-PRODUCT and DIGITAL-SUM functions.}
		1.0.0 {Added SUM and PRODUCT functions.}
	]
	needs: [
		2.100.95
		http://r3.revolucent.net/net.revolucent.series.v1.r
	]
	exports: [
		average
		digital-product
		digital-sum
		divisors
		factorial
		factors
		fibonacci
		from-base
		integer-sequence
		long-divide
		gcd
		numbers!
		permute
		product
		repeat-length
		rowland
		sum
		to-base
	]
]

numbers!: make typeset! [block! paren! vector!]

factorial: funct [
	{Computes the factorial of a number.}
	n [integer!]
][
	if n < 0 [
		do make error! "Cannot calculate the factorial of a negative number."
	]
	either any [equal? n 0 equal? n 1] [1] [
		interpolate at expression: range compose [2 - (n)] 2 '*
		do expression
	]
]

factors: funct [
	{Computes the prime factors of an integer.}
	n [integer!]
][
	; This is an incredibly ugly function. I'd love to clean it up.
	if n < 1 [
		do make error! "Cannot find factors of a number less than 1."
	]
	a: copy []
	if equal? 1 n [
		return a
	]
	if equal? 2 n [
		append a 2
		return a
	]
	m: 2
	s: 1
	until [
		either equal? 0 remainder n m [
			n: divide n m
			append a m
		][
			m: m + s
			s: 2
		]
		if (1.0 * m * m) > n [
			append a n
			n: 1
		]
		equal? n 1
	]
	a
]

product: funct [
	{Calculates the product of a block or vector of numbers.}
	numbers [numbers!] {A block or vector of numbers}
][
	if equal? 0 length? numbers [return 0]	
	fold x y numbers [x * y]
]

sum: funct [
	{Calculates the sum of a block or vector of numbers.}
	numbers [numbers!] {A block or vector of numbers}
][
	if equal? 0 length? numbers [return 0]
	fold x y numbers [x + y]
]

digital: funct [
	operation [any-function!]	
	number [integer!]
	/skip0
][
	digits: copy []
	foreach digit to-string number [
		if any [not skip0 not-equal? digit #"0"] [
			append digits to-integer to-string digit
		]
	]
	operation digits
]

digital-sum: funct [
	{Calculates the digital sum of an integer.}
	number [integer!]
	/recurse {Recurses until a single digit is reached}
][
	either recurse [op: :greater-or-equal? bound: 9] [op: :lesser-or-equal? bound: 0]
	until [op bound number: digital :sum number]
	number
]

digital-product: funct [
	{Calculates the digital product of an integer.}
	number [integer!]
	/recurse {Recurses until a single digit is reached}
	/skip0 {Skips 0s when multiplying.}
][
	either recurse [op: :greater-or-equal? bound: 9] [op: :lesser-or-equal? bound: 0]
	until [op bound number: apply :digital [:product number skip0]]
	number
]

fibonacci: funct [
	limit [integer!]
][
	sequence: [0 1]
	while [greater? limit length? sequence] [
		append sequence sum at tail sequence -2
	]
	copy/part sequence limit
]

gcd: func [
    {Returns the greatest common denominator of m and n.}
    m [integer!]
    n [integer!]
][
    abs either zero? n [0] [either zero? (m // n) [n] [gcd n (m // n)]]
]

divisors: funct [
	{Gets all the divisors of a number}
	num [integer!]
	/as 
		type [datatype!] {Valid values are block! and pair!}
][
	result: copy []
	repeat n to integer! square-root num [
		if equal? 0 mod num n [ append result n ]
	]
	either as [
		unless any [equal? type block! equal? type pair!] [
			do make error! "Valid values for the /AS refinement are BLOCK! and PAIR!"
		]
		pairs: copy []
		forall result [
			append/only pairs make type reduce [first result divide num first result]
		]
		sort/compare pairs func [a b] [
			lesser? first a first b
		]
	][
		complement: copy []
		forall result [
			append complement divide num first result
		]
		sort append result complement
	]
]

rowland: none

rowland-cache: context [
	
	cache: make map! []

	set 'rowland funct [
		{Calculates a Rowland sequence.}
		limit [integer!] {The number of entries to calculate}
		/start
			value [integer!] {The starting value, defaulting to 7}
	][
		value: either start [value] [7]
		if none? result: cache/:value [
			cache/:value: result: reduce [value]
		]
		while [greater? limit length? result] [
			n: add 1 length? result
	 		append result add p: result/(n - 1) gcd n p
		]
		result
	]

]

long-divide: funct [
	{Performs manual long division. Yes, really.}
	dividend [integer!]
	divisor [integer!]
	/limit {Specifies that a limit other than the default of 1000 should be used}
		limitation [integer!] {Maximum possible length of quotient, to prevent an infinite loop}
][
	default limitation 1000
	
	decimal: none		
	quotient: copy ""
	numbers: to string! dividend
	divident: to integer! to string! first numbers
	
	while [all [! tail? numbers any [none? decimal greater? limitation length? decimal]]][
		subquotient: to integer! divide divident divisor
		append quotient to string! subquotient
		numbers: next numbers						
		if all [tail? numbers any [divident > 0 subquotient > 0]][
			default decimal tail quotient
			append numbers #"0"
			divident: to integer! rejoin [subtract divident multiply divisor subquotient first numbers]			
		]
	]
	
	if decimal [insert decimal #"."]
	while [equal? #"0" first quotient][
		quotient: remove quotient
	]		
	while [equal? #"0" last quotient][
		remove back tail quotient
	]
	if equal? #"." first quotient: head quotient [
		insert quotient #"0"
	]
	
	head quotient
]

repeat-length: funct [
	{Finds the length of the longest repeating string within another string.}
	string [any-string!] {The string to search}
][
	while [! tail? string] [
	
		if all [
			lesser? 1 length? string
			equal? 1 length? unique copy string
		][return 1]
			
		len: 1
		limit: to integer! divide length? string 2
		longest: 0
		while [lesser-or-equal? len limit] [
			matched: false
			slice: copy/part string len
			current: string
			while [! tail? current][
				current: at current (len + 1)			
				if greater? length? current length? slice [
					either equal? slice copy/part current len
						[matched: true] [matched: false break]
				]
			]
			if all [matched tail? current] [
				return length? slice
			]
			++ len
		]
		string: next string
	]
	
	0
]

average: funct [
	{Calculates the average of a list of numbers.}
	numbers [numbers!]
][
	divide sum numbers length? numbers
]

integer-sequence: funct [
	{A convenience function for generating integer sequences. E.g.,

	integer-sequence 100 [mod sum result length? result]
}
	limit [integer!] {The number of entries in the sequence to generate.}
	expression [block!] {The expression used to generate the entries in the sequence.}
	/from
		start [integer! block!] {The number at which to start. Default: 1.}
	/var
		'word [word!] {The name of the variable which stores the sequence. Default: SEQUENCE.}
][
	default word 'sequence
	sequence: either ! from [copy [1]] [
		switch type?/word start [
			integer! [reduce [start]]
			block! [copy start]
		]
	]
	fn: funct reduce [word] expression
	while [greater-or-equal? limit length? sequence] [
		append sequence fn sequence
	]
	sequence
]

base-digits: "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

to-base: funct [
	{Converts an integer to the specified base. (The result is a string.)}
	b [integer!] {The base to which to convert}
	n [integer!]
][
	if any [b < 2 b > 37] [
		do make error! "Bases must be between 2 and 37."
	]
	result: copy {}
	until [
		d: n // b + 1
		insert result base-digits/:d
		0 = n: to integer! n / b
	]
	result
]

from-base: funct [
	{Converts a number from the specified base.}
	b [integer!] {The base from which to convert}
	s [string! integer!]
][
	if any [b < 2 b > 37] [
		do make error! "Bases must be between 2 and 37."
	]	
	result: 0
	p: 0
	foreach c reverse copy to string! s [
		i: -1 + index? find base-digits c
		result: result + to integer! b ** p * i
		++ p
	]
	result
]


