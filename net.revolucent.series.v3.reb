; Copyright (c) 2013 Gregory Higley

; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation
; files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,
; modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software
; is furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

REBOL [
	Title: "Revolucent CSV Library"	
	Author: "Gregory Higley"
	Date: 2013-08-05
	Name: net.revolucent.parse.csv
	Version: 3.0.0
	Type: module
	Exports: [enumerator fmap intersperse range filter fold proper-case transform]
	Needs: [
		2.101.0 
		net.revolucent.core.v3
	]	
	License: MIT
]

intersperse: funct [
	"Intersperses a value between the elements of a series"
	series [series!]
	value [any-type!]
	/only "Inserts a block as a block"	
	/into
		result [series!] "A series to use as the result"
][
	default result make type? series []
	foreach elem series [
		if (length? result) > 0 [apply :append [result :value false none only]]
		append/only result :elem
	]
	result
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

transform: funct [
	"Performs map using a function, get-path, or lambda block with implicit _."
	f [any-function! get-path! block!]
	series [series! integer!]
	/only
	/into
		result [series!]
][
  if integer? series [
    series: either series > 0 [range compose [1 - (series)]] [copy []]
  ]
	default result make type? series []
  case [
    get-path? :f [f: ^ compose [ arg | (f) arg ]]
    block? :f [f: ^_ f]
  ]
	foreach elem series [
		apply :append [result f :elem false none only]
	]
	result
]

; Deprecated alias for transform
fmap: :transform

filter: funct [
	"Filters a series using the TEST function. (Modifies)"
	test [any-function! block!]
	series [series!]
][
  if block? :test [test: ^_ test]
	remove-each elem series [
		! test elem
	]
	series
]

fold: funct [
  f [any-function! block!]
  series [series!]
  /start
    accum
][
  if block? :f [f: ^_2 f]
  unless empty? series [
    default accum first+ series
    while [! tail? series] [
      accum: f accum first+ series
    ]
  ]
  :accum
]

proper-case: funct [
  string [string!]
][
  form transform [uppercase/part lowercase _ 1] parse string none
]

; Usage:
; fibonacci: enumerator [i j] [
;   set [i: j:] [0 1]
;   forever [
;     set [i: j:] reduce [j i + j]
;     yield i
;   ]
; ]
;
; All fibs less than 4 million:
; print fibonacci n [n < 4'000'000]
enumerator: closure [
  "Returns a lazy enumerator."
  enumerator-locals [block!] "Local words"
  enumerator-body [block!] "Enumeration block"
][
  enumerator-body: copy/deep enumerator-body
  func [
    'enumerate-var [word!]
    enumerate-body [block!]
    /local
      enum-callback
      result
  ][     
    result: copy []
    enum-callback: func reduce [enumerate-var] enumerate-body
    catch/name [ 
      use [yield] [
        yield: func [
          value      
        ][
          either enum-callback :value [
            append/only result :value
          ][
            throw/name result 'end-enumeration
          ]
        ]
        default enumerator-locals []
        bind enumerator-body 'yield
        do func compose [/local (enumerator-locals)] enumerator-body
      ]   
    ] 'end-enumeration
    result
  ]
]
