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
	title: "Revolucent Series Library"
	author: "Gregory Higley"
	license: MIT
	name: net.revolucent.series
	type: module
	version: 2.0.0
	needs: [
		2.100.111
		http://r3.revolucent.net/net.revolucent.core.v2.r 2.0.0
	]
	exports: [
		accum
		accum-each
		first-
		range
;		dummy1
		dummy2
;		dummy3
	]
	history: [
		2.2.0 "Added FIRST-"
		2.1.1 "Changed ACCUM and ACCUM-EACH to allow a right fold through the /RIGHT argument."
		2.1.0 "Added ACCUM and ACCUM-EACH."		
		2.0.0 "Added RANGE."
	]
]

; These exist because of a strange bug in the 2.100.111 module system.
dummy1: dummy2: dummy3: 44

accum: funct [
	{Iteratively performs an operation on a series of values to produce a single value.}
	action [any-function!]
	series [series!]
	/right {Start at the right instead of the left.}
	/start
		start-arg [any-type!]
][
	fun: either op? :action [func [x y] [x action y]] [:action]
	either right [
		series: tail series		
		result: either start [:start-arg] [first- series]
		condition: [! head? series]
		while-body: [result: fun first- series :result]
	][
		result: either start [:start-arg] [first+ series]		
		condition: [! tail? series]
		while-body: [result: fun :result first+ series]
	]
	while condition while-body
	:result
]

accum-each: funct [
	{Iteratively performs an operation on a series of values to produce a single value.}
	'arg1 [word!]
	'arg2 [word!]
	series [series!]
	body [block!]
	/right
	/start
		start-arg [any-type!]
][
	action: func reduce [arg1 arg2] body
	apply :accum [:action series right start :start-arg]
]

first-: funct [
	'word [word!]
][
	series: get word
	set word series: back series
	first series
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
			set p1 integer! '- set p2 integer! (append/only values reduce [p1 p2])
		|	set n integer! (append values n)
		]
	]	
	values: copy []
	unless parse spec rules [
		do make error! "The range DSL was not syntactically valid."
	]
	result: copy []
	foreach value values [
		either block? value [
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
