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
    ^ ^1 ^2 ^3 ^tap ~ arity init refinements-of symbol tap
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
  f [any-function! word! path!]
  /with "Whether to consider refinements. (Default: FALSE)"
    refinements [logic! any-word! block!] "TRUE = all refinements, otherwise word or block of refinements"
  /local
    word
    arg-rule
    parts
    refinement
    count-arg
][
  switch/default type?/word :f [
    word! [
      apply :arity [get :f with refinements]
    ]
    path! [
      parts: to block! f
      apply :arity [first+ parts true parts]
    ]
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

for n 1 3 1 [
  extend self to word! ajoin ["^^" n] func [body [any-function! block!]] compose [^ (n) body]
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

refinements-of: funct [
  "Returns a block with the refinements of the given function."
  f [any-function!]
][
  refinements: copy []
  foreach item spec-of :f [
    if all [refinement? item not-equal? /local item] [
      append refinements item
    ]
  ]
  refinements
]

init: funct [
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

^tap: func [
  "Applies an expression to a single argument"
  f [block! any-function!]
  arg
][
  f: ^ 1 :f
  f :arg
]

tap: :^tap
