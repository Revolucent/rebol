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
; print fibonacci n [take [n < 4'000'000]]
; First 20 fibs
; print fibonacci n [take 20]
; First 20 even fibs
; print fibonacci n [where [even? n] take 20]
; 
; The enumerator DSL consists of three "functions":
; where, take, and drop. The final one must always
; be take, otherwise the results are undefined.
; Any number of where clauses can precede a take
; or drop, but once a take or drop has been "satisfied",
; the where clauses before it are ignored, e.g.;
;
; fibonacci n [where [even? n] drop 4 where [odd? n] take 4]
; This does NOT drop the first 4 even fibs. Instead, it
; allows only even fibs through, and drops the first 4 of those.
; Once 4 have been dropped, the drop is "satisfied" and control
; moves on to the next where expression.
enumerator: closure [
  "Defines an enumerator"
  enumerator-locals [block!] "Block of words"
  enumerator-body [block!] "Body of enumerator"
  /local
    enumeration-predicate!
][
  func [
    'enumeration-var [word!]
    enumeration-predicates [block!]
    /local
      enumeration [block!]
      enumerate [function!]
      enumeration-predicate! [object!]
      where-predicate! [object!]
      take-predicate! [object!]
      drop-predicate! [object!]
      predicate [object!]
      current-predicate [object!]
      temp-predicate [object!]
      assign-predicate [block!]
      where-body
      take-body
      take-count
  ][
    enumeration-predicate!: object [
      condition: none
      next-predicate: none
    ]
    where-predicate!: make enumeration-predicate! [
      eval: func [value] [
        if condition :value [
          next-predicate/eval :value
        ]
      ]
    ]
    take-predicate!: make enumeration-predicate! [
      eval: func [value] [
        either condition :value [
          append enumeration :value
        ][
          either next-predicate [
            predicate: next-predicate
            predicate/eval :value
          ][
            throw/name enumeration 'end-enumeration
          ]
        ]
      ]
    ]
    drop-predicate!: make enumeration-predicate! [
      count: 0
      eval: func [value] [
        case [
          :condition [
            unless condition :value [
              predicate: next-predicate
              predicate/eval :value
            ]
          ]
          count [
            either count == 0 [
              predicate: next-predicate
              predicate/eval :value
            ][
              -- count
            ]
          ]
        ]
      ]
    ]
    assign-predicate: [
      either current-predicate [
        current-predicate/next-predicate: temp-predicate
        current-predicate: temp-predicate
      ][
        predicate: current-predicate: temp-predicate
      ]
    ]
    parse enumeration-predicates [
      any [
        'where set where-body block! (
          temp-predicate: make where-predicate! [condition: func reduce [enumeration-var] where-body]
          do assign-predicate
        )
      | 'take set take-body block! (
          temp-predicate: make take-predicate! [condition: func reduce [enumeration-var] take-body]
          probe :temp-predicate/condition
          do assign-predicate
        )
      | 'take set take-count integer! (
          temp-predicate: make take-predicate! [condition: func reduce [enumeration-var] compose [(make paren! [length? enumeration]) < (take-count)]]
          do assign-predicate
        )
      | 'drop set drop-body block! (
          temp-predicate: make drop-predicate! [condition: func reduce [enumeration-var] drop-body]
          do assign-predicate
        )
      | 'drop set drop-count integer! (
          temp-predicate: make drop-predicate! [count: drop-count]
          do assign-predicate
        )
      ]
      end
    ]
    enumeration: copy []
    use [yield] [
      enumerate: func compose [/local (enumerator-locals)] bind enumerator-body 'yield
      yield: func [
        yield-var
      ][
        predicate/eval :yield-var
      ]
      catch/name [ enumerate ] 'end-enumeration
    ]
    enumeration
  ]
]
