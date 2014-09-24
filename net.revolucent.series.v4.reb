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
	Title: "Revolucent Series Library"	
	Author: "Gregory Higley"
	Date: 2014-09-23
	Name: net.revolucent.series.v4
	Version: 4.0.0
	Type: module
	Exports: [^each ^filter ^fold ^map ^map-each ^remove-each ^where enumerator range]
	Needs: [
		2.101.0 
		net.revolucent.core.v4
	]	
	License: MIT
]

^each: funct [
  "Functional FOREACH."
  lambda [any-function! block!]
  data [series!]
][
  lambda: ^ 1 :lambda
  foreach elem data [
    lambda elem 
  ]
  exit
]

^filter: funct [
  "Filters the given series. (Modifies)"
  lambda [any-function! block!]
  data [series!]
][
  lambda: ^ 1 :lambda 
  ^remove-each [! lambda _] data
  data
]

^fold: funct [
  "E.g., ^^FOLD [_1 + _2] RANGE [1 - 10]"
  lambda [any-function! block!]
  data [series!]
  /default
    value
][
  lambda: ^ 2 :lambda 
  if ! empty? data [
    value: first+ data
    while [! tail? data] [
      value: lambda :value first+ data
    ]
  ]
  :value
]

^map-each: funct [
  "Function equivalent of MAP-EACH."
  lambda [any-function! block!]
  data [block! vector!]
][
  body: ^ 1 :lambda
  map-each elem data [body elem]
]

^map: :^map-each

^remove-each: funct [
  "Functional version of REMOVE-EACH."
  lambda [block! any-function!]
  data [series!]
][
  lambda: ^ 1 :lambda
  remove-each elem data [lambda elem]
]

^where: :^filter

enumerator: closure [
  locals [block!]
  definition [block!]
  /local
    enumerate
][
  enumerate: func compose [yield /local (locals)] definition
  closure [
    'word [word!]
    body [block!]
    /local  
      results
      value
  ][
    results: copy []
    use [give stop pass] [
      give: func [value /stop /limit result-limit] [
        case [
          all [stop not limit] [throw/name :value 'stop-enumeration]
          all [limit lesser-or-equal? result-limit length? results] [
            throw/name #[unset!] 'stop-enumeration
          ]
          'else [throw/name :value 'give-enumeration]
        ]
      ]
      stop: has [/limit result-limit] [
        if any [not limit lesser-or-equal? result-limit length? results] [
          throw/name #[unset!] 'stop-enumeration
        ]
      ]
      pass: does [throw/name #[unset!] 'pass-enumeration]
      set/any 'value catch/name [
        enumerate func [value] [
          yield: func reduce [word] bind/copy body 'give
          catch/name [
            set/any 'value catch/name [
              yield :value
              throw/name #[unset!] 'give-enumeration
            ] 'give-enumeration
            if value? 'value [append/only results :value]
          ] 'pass-enumeration
        ]
      ] 'stop-enumeration
      if value? 'value [append/only results :value]
    ]
    results
  ]
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

