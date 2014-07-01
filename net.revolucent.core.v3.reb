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
	Author: "Gregory Higley"
	Title: "Revolucent Core Library"
	Date: 2013-04-23
	Name: net.revolucent.core
	Version: 0.9.0
	Type: module
	Exports: [^ ^^ ^~ ^_ ^_2 ^_3 flip attempt-to identity none-if-empty symbol transform-unless-empty ensure strive log rcurry curry lcurry l^ r^ . do. test-any test-all using]
	Needs: [2.101.0]	
	License: MIT
]

identity: func [o] [:o]

symbol: func [
	"Turns the given word(s) into symbol(s), i.e., self-referential word(s)"
	'words [word! block!] "Word or block of words"
][
	either word? words [
		set words words
	][
		foreach word words [
			symbol :word
		]
	]
	words
]

flip: funct [
  "Flips a two-argument function."
  f [any-function!]
  arg1
  arg2
][
  f arg2 arg1
]

attempt-to: closure [
	"Returns a fuction that attempts to convert a value to the given type"
	datatype [datatype!]
][
	func [val] [attempt [to datatype :val]]
]

none-if-empty: func [
	"If the series is empty, return NONE, else the series."
	series [series!]
] [
	either empty? series [none] [series]
]

lambda: func [
	"Lambda, e.g., ^[ x | x + 1 ]."
	spec [block!]
	/with
		func-maker "E.g., func, funct, closure"
	/local
		f-spec
		f-body
][
	default func-maker :func
	unless parse spec [copy f-spec to '| '| copy f-body to end] [
		do make error! rejoin ["Invalid lambda: " mold/flat spec]
	]
	func-maker f-spec f-body
]

^: func [
	"Lambda, e.g., ^^[ x | x + 1 ]"
	spec [block!]
][
	lambda spec
]

^~: func [
	"Lambda in which ~ represents the bound variable."
	body [block!]
][
	func [~] body
]

^_: func [
  "Lambda in which _ represents the bound variable."
  body [block!]
  /args
    arg-count [integer!]
  /local
    spec
][
  default arg-count 1
  assert [arg-count >= 0]
  spec: copy []
  case [
    (arg-count = 1) [
      append spec '_
    ]
    'else [
      for a 1 arg-count 1 [
        append spec to word! ajoin ["_" a]
      ]
    ]
  ]
  func spec body
]

^_2: func [body [block!]] [^_/args body 2]
^_3: func [body [block!]] [^_/args body 3]

^^: func [
	"Lambda, e.g., ^^[ x | x + 1 ]"
	spec [block!]
][
	lambda/with spec :funct
]

rcurry: funct [
	body [block!]
	/with
		'word [word!]
][
	default word to word! random "&*^^!mxyz"
	func reduce [word] append copy body word
]       

curry: :rcurry
r^: :rcurry

lcurry: funct [
	body [block!]
	/with
		'word [word!]
][
	default word to word! random "&*^^!mxyz"
	func reduce [word] insert copy body word
]       

l^: :lcurry

test-any: funct [
	test [any-function! block!]
	items [series!]
	/only
][
  if block? :test [test: ^_ test]
	unless only [items: reduce items]
	foreach item items [
		if test item [return true]
	]
	false
]

test-all: funct [
	test [any-function!]
	items [series!]
	/only
][                     
  if block? :test [test: ^_ test]
	unless only [items: reduce items]
	result: true
	foreach item items [unless result: result and test item [return result]]
	result
]

.: closure [
	fs [block!]
	/only
][
	unless only [fs: reduce fs]
	fs: reverse copy fs
	func [arg] [
		foreach f fs [
			arg: case [
				any-function? :f [f :arg]
				block? :f [do curry f :arg]
			]
		]
		:arg
	]
]

do.: funct [
	fs [block!]
	arg
	/only
][
	do apply :. [fs only] :arg
]


; Necessitated by net.revolucent.parse.csv but could be more generally useful.
transform-unless-empty: closure [
	"Returns a function that applies TRANSFORM to any non-empty series."
	transform [any-function!]
	/with-default
		default-value "Default value if series is empty"
][
	func [series [series!]] [
		either empty? series [
			does [:default-value]
		][
			transform series
		]
	]
]

ensure: funct [
	body [block!]
	finally [block!]
	/local
		result
][
	error: none
	set/any 'result try/except body func [e] [
		error: e
	]
	do finally ; If an error happens here, all bets are off
	if error [do error]
	either value? 'result [:result] [exit]
]

comment [
	{REBOL errors have a TYPE and an ID which serves
	to distinguish them from other errors, but all
	are of the common type ERROR!. Error handling
	with REBOL's TRY/EXCEPT forces the programmer
	to deal with TYPEs and IDs inside of the single
	exception handling block. This is fine in most
	cases. In cases where it's not, this function
	cleanly handles REBOL errors by TYPE and ID,
	as follows:}

	strive [
		to pair! "bob" ; Generates an error of type SCRIPT with id BAD-MAKE-ARGS.
	] e [
		script bad-make-args [ ; Handles errors of type SCRIPT id BAD-MAKE-ARGS.
			print "Uh-oh!"
		]
		script [ ; Handles errors of type SCRIPT regardless of id.
			print "A script error occurred!"
		]
		[ ; Handles anything not already handled.
			print e
		]
	]
	
	{If the /ALL refinement is used, all matching handlers
	are run, not just the first one.}

	{Why is it called STRIVE? Because no other good synonyms for TRY
	were left. REBOL already uses TRY and ATTEMPT. My third choice,
	CATCH, is also already used. I thought about EXCEPT but it emphasizes
	the exception handling portion, though really the BODY block is
	what we're trying to get done.}
]

strive: funct [
	"Handles exceptions by type and id."
	body [block!]
	'e [word!] "Word used to represent the error"
	handlers [block!] "Block of handlers"
	/all
	/finally
		finally-block [block!]
][
	try-body: either finally [[ensure body finally-block]] [body]
	try/except try-body func [err] [
		error-handlers: copy []
		assert [parse handlers [
			any [                  
				(type: id: handler: none)
				opt [set type word!]
				opt [set id word!]
				set handler block! (
					type: any [type err/type]
					id: any [id err/id]
					if lib/all [
						equal? type err/type
						equal? id err/id
					][
						if any [all empty? error-handlers] [
							append/only error-handlers handler
						]
					]
				)
			]
			end
		]]
		if empty? error-handlers [do err]
		foreach error-handler error-handlers [
			do func reduce [e] error-handler err
		]
	]
]

logging: off

log: func [
	"Alias for PRINT used for logging. Enable with log/set on."
	value
	/set "Use to turn logging on or off"
	/with
		logger "Function to use for logging. Defaults to PRINT."
][
	default logger :print
	assert [any-function? :logger]
	case [
		set [
			logging: true? :value
		]
		logging [
			; When stderr becomes available, we'll use that instead
			logger :value
		]
	]
]

protect/hide 'logging

using: funct [
	"Resource protection, analogous to C#'s USING construct."
	'word [word! block!]
	body [block!]
	/close-by
		'closer [word! block!]
	/local
		result
][
	either word? word [
		; word is a word, and its value is our object.
		o: get :word
	][
		; word is a block, assumed to consist of any-word! followed by
		; an expression which will evaluate to our object.
		o: first reduce next word
		word: first word
		; If word is a get-word!, we dereference it before assigning it.
		word: either get-word? word [get :word] [to word! word]
	]
	default closer 'close
	fclose: either word? closer [
		close: any [get in o closer get in o 'close get in o 'dispose]
		if :close [func reduce [word] [close]]
	][
		func reduce [word] closer
	]
	do-close: does [if :fclose [fclose o]]
	f: func reduce [word] body
	set/any 'result try/except [f o] func [e] [
		do-close
		do e
	]
	do-close
	either value? 'result [:result] [exit]
]

