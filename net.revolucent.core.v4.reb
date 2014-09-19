; Copyright (c) 2014 Gregory Higley

; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation
; files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,
; modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software
; is furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE

REBOL [
	Author: "Gregory Higley"
	Title: "Revolucent Core Library"
	Date: 2014-09-05
	Name: net.revolucent.core
	Version: 4.0.0
	Type: module
	Exports: [
    ^ ^1 ^2 ^3 ^each ^filter ^fold ^map ^map-each ^where ~ arity range refinements-of ^remove-each symbol
  ]
	Needs: [2.101.0]	
	License: MIT
]

symbol: funct [
  'words [word! block!] 
][
  set words words
]

parse-lambda: funct [
  lambda [block!]
][
  spec: copy []
  body: copy []
  current: spec
  foreach elem lambda [
    either all [same? current spec equal? '| :elem] [
      current: body
    ][
      append/only current :elem  
    ]
  ]
  reduce [spec body]
]

arity: funct [
  f [any-function!]
  /with
    refinements [any-word! block!]
  /local
    word
    arg-rule
    refinement
    count-arg
][
  count: 0
  count-arg: true
  arg-rule: [
    any [
      [[word! | lit-word! | get-word!] (if count-arg [++ count])]
      opt block!
      opt string!
    ]
  ]
  default refinements copy []
  if any-word? refinements [refinements: reduce [refinements]]
  parse spec-of :f [
    opt block! ; e.g., [catch]
    opt string!
    arg-rule
    any [
      (count-arg: false)
      set refinement refinement! (if all [! equal? /local refinement find refinements refinement] [count-arg: true])
      arg-rule
    ]
    end
  ]
  count
]

^: funct [
  arg-count [integer!]
  body [block! any-function!]
][
  assert [arg-count >= 0]
  make-spec: [
    spec: copy []
    for a 1 arg-count 1 [
      append spec to word! rejoin ["_" a]
    ]
    spec
  ]
  either block? :body [
    either arg-count = 1 [
      func [_ /local _1] compose [_1: _ (body)]
    ][
      func do make-spec body
    ]
  ][
    either op? :body [
      do make-spec
      op-caller: closure [
        op [op!]
      ] compose/only/deep [
        func (spec) [apply :op (spec)] 
      ]
      op-caller :body
    ][
      :body
    ]
  ]
]

for n 1 3 1 [
  do bind/set/copy compose/deep [(to set-word! ajoin ["^^" n]) func [body [block!]] [^ (n) body]] self
]

^each: funct [
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
  lambda [any-function! block!]
  data [series!]
][
  lambda: ^ 1 :lambda 
  ^remove-each [! lambda _] data
  data
]

^where: :^filter

^map-each: funct [
  lambda [any-function! block!]
  data [block! vector!]
][
  body: ^ 1 :lambda
  map-each elem data [body elem]
]

^map: :^map-each

^fold: funct [
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

~: funct [
  lambda [block!]
][
  spec: copy []
  body: copy []
  current: spec
  foreach elem lambda [
    either all [! equal? current body equal? '| :elem] [
      current: body
    ][
      append/only current :elem 
    ]
  ]
  func spec body
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

refinements-of: funct [
  f [any-function!]
][
  ^filter [all [refinement? _ not-equal? /local _]] copy spec-of :f
]

^remove-each: funct [
  lambda [block! any-function!]
  data [series!]
][
  lambda: ^ 1 :lambda
  remove-each elem data [lambda elem]
]

