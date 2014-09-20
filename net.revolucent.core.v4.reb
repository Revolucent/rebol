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
    ^ ^1 ^2 ^3 ^each ^filter ^fold ^map ^map-each ^where ~ arity init range refinements-of ^remove-each symbol
  ]
	Needs: [2.101.0]	
	License: MIT
]

symbol: funct [
  "Creates self-referential words"
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
  "Number of arguments of a function, with or without refinements."
  f [any-function!]
  /refined "Whether to consider refinements. (Default: FALSE)"
    refinements [logic! any-word! block!] "TRUE = all refinements, otherwise word or block of refinements"
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
  refinements: case [
    any-word? refinements [reduce [refinements]]
    block? refinements [refinements]
    refinements [refinements-of :f]
    'else [copy []]
  ]
  parse spec-of :f [
    0 2 [block! | string!]
    arg-rule
    any [
      (count-arg: false)
      set refinement refinement! (if all [! equal? /local refinement find refinements refinement] [count-arg: true])
      opt string!
      arg-rule
    ]
    end
  ]
  count
]

; If the number of arguments is 1, the argument can be referenced as _
; in the body of the function, but _1 can also be used. For functions with
; two or more arguments, _1, _2, _3, etc. must be used.
^: funct [
  "Creates an anonymous function with the given number of arguments."
  arg-count [integer!] "Number of arguments"
  body [block! any-function!] "Function or body of function"
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
      func [_ /local _1] compose [_1: :_ (body)]
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

^where: :^filter

^map-each: funct [
  "Function equivalent of MAP-EACH."
  lambda [any-function! block!]
  data [block! vector!]
][
  body: ^ 1 :lambda
  map-each elem data [body elem]
]

^map: :^map-each

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
  "Returns a block with the refinements of the given function."
  f [any-function!]
][
  ^filter [all [refinement? _ not-equal? /local _]] spec-of :f
]

^remove-each: funct [
  "Functional version of REMOVE-EACH."
  lambda [block! any-function!]
  data [series!]
][
  lambda: ^ 1 :lambda
  remove-each elem data [lambda elem]
]

init: func [
  "Set word or path (or block of the same) if NONE or UNSET."
  'settee [word! path! block!]
  new-value
  /only "Treat NEW-VALUE as single value"
  /extend "If SETTEE is BLOCK, set all values to (last value of) NEW-VALUE"
][
  init-one: func [
    settee [any-word! any-path!]
    new-value
    /local
      value
  ][
    either all [! unset? get/any settee ! unset? get/any :settee ! none? value: get :settee] [
      :value
    ][
      do compose [(to either any-path? settee [set-path!] [set-word!] settee) :new-value]
    ]
  ]
  case [
    all [! block? settee any [only ! block? :new-value]] [
      init-one settee :new-value
    ]
    all [! block? settee block? :new-value] [
      init-one settee first new-value 
    ]
    all [block? settee any [only ! block? :new-value]] [
      either extend [
        result: array/initial length? settee does [:new-value] ; Prevent ARRAY/INITIAL from calling NEW-VALUE if it's a function.
        foreach settee settee [init-one settee :new-value]
      ][
        result: array length? settee
        result/1: init-one settee/1 :new-value
      ]
      result
    ]
    'else [ ; SETTEE and NEW-VALUE are both blocks and ONLY is not set.
      result: copy []
      for i 1 length? settee 1 [
        append/only result init-one settee/:i either all [extend greater? i length? new-value] [last new-value] [:new-value/:i]
      ]
      result
    ]
  ]
]
