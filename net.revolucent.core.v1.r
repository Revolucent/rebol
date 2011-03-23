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
	title: "Revolucent Core Module"
	name: net.revolucent.core
	type: module
	file: %net.revolucent.core.v1.r	
	author: "Gregory Higley"
	date: 2010-08-01
	version: 1.3.6
	history: [
		1.3.7 {Added LAMBDA.}
		1.3.6 {Added FALSE?.}
		1.3.5 [
			{Added NIL?.}
			{Added conditional debugging to the various DEBUG- functions.}
		]
		1.3.4 {Added PROTECT-MODULE.}
		1.3.3 {Added DEBUG-ASSERT.}
		1.3.2 {Added ENABLE-DEBUG.}
		1.3.1 {Added DEBUG-PROBE.}
		1.3.0 {Added DEBUG, DEBUG-DO and DEBUG-PRINT.}
		1.2.0 {Added CONST.}
		1.1.0 {Added TEST.}
		1.0.0 {Added MODULE-CHECKSUM.}
	]
	needs: [2.100.99]
	exports: [
		args
		id
		const
		debug
		debug-assert
		debug-do
		debug-print
		debug-probe
		enable-debug
		false?
		lambda
		module-checksum
		nil?
		protect-module
		test
	]
	purpose: "Common utilities"
	license: 'mit
]

false?: funct [
	{Inverse of TRUE?.}
	value
][
	! true? :value
]

nil?: funct [
	{If VALUE is a series and is empty, returns true, otherwise returns the value of FALSE?.}
	value
][
	either series? :value [empty? value] [false? :value]
]

debug: false
debug-words: none

enable-debug: func [
	{Enables or disables debugging.
	If FALSE or [] are passed, debugging is disabled.
	If TRUE is passed, debugging is enabled.
	If a block of words is passed, debugging is enabled for those words, e.g.,

		enable-debug [awesome]
		debug-do/for [print "ok"] [awesome whatever] ; prints "ok" because AWESOME was specified
		debug-do/for [print "nope"] [whatever] ; does nothing, because AWESOME was not specified
		debug-do [print "cool!"] ; prints "cool!" because debugging is enabled and no words were specified
}
	words [logic! block!]
][
	debug: ! nil? words
	if all [debug block? words] [
		foreach word words [assert [any-word? word]]
		debug-words: copy words
	]
	debug
]

debug-do: funct [
	{If debugging is enabled and any one of the conditional words is matched (if specified), the block is executed.}
	body [block!]
	/for
		words [block!] {Conditional words}
][
	if ! nil? words [foreach word words [assert [any-word? word]]]	
	if all [
		debug
		any [
			nil? words
			nil? debug-words
			! nil? intersect words debug-words
		]
	][
		do body
	]
]

debug-print: funct [
	{If debugging is enabled and any one of the conditional words is matched (if specified), the value is printed.}	
	value
	/for
		words [block!] {Conditional words}
][
	apply :debug-do [[print :value] for words]
	exit
]

debug-probe: funct [
	{If debugging is enabled and any one of the conditional words is matched (if specified), the value is printed.}		
	value
	/for
		words [block!] {Conditional words}
][
	apply :debug-do [[probe :value] for words]
	exit
]

debug-assert: funct [
    {Assert that condition is true, else throw an assertion error.}
    conditions [block!]
    /type {Safely check datatypes of variables (words and paths)}
	/for
		words [block! none!]
][
	debug-do/for [apply :assert conditions type] words
]

const: funct [
	{Creates a constant}
	'word [word!]
	value
][
	set :word :value
	protect word
	:value
]

id: funct [
	{The identity function. Returns the value passed.}
	a
][
	:a
]

lambda: funct [
	{Creates a lambda, as follows:
	
^-^-lambda [ x [integer!] y [integer!] | x + y ]
}
	def [block!]
][
	spec: copy []
	body: copy []
	current: spec
	seen: false
	while [! tail? def] [
		item: first+ def
		either all [! seen equal? '| item] [
			seen: true
			current: body
		][
			append/only current item
		]
	]
	funct spec body
]

module-checksum: funct [
	{Calculates a module checksum}
	source [url! file!]
][
	checksum/secure to-binary mold/flat load/unbound/header source
]

function-test-case: [test :value]
block-test-case: [do head append copy test :value]
test-cases: reduce [
	word! [do get test :value]
	block! block-test-case
	paren! block-test-case
	native! function-test-case
	action! function-test-case
	function! function-test-case
	closure! function-test-case				
	rebcode! function-test-case
	command! function-test-case
	op! function-test-case
]

test: funct [
	{Applies a series of tests to a value. The tests are either words which
	point to functions that take a single argument and return a LOGIC! result,
	or functions. If at least one of the tests return TRUE, the test itself is 
	returned. If the /ALL refinement is used, all of the tests must return true.
	
		test http://www.google.com [string? url?]
		test/all foo [bar? baz?]
	}
	value [any-type!]
	tests [block!]
	/all {All cases must match. (Default is at least one.) In this case a simple TRUE is returned.}
	/index {Return the index of the matching test, not the test itself}
][
	p: 1	
	test-all: all ; avoids confusion
	error: make error! "Invalid test."
	while [! tail? tests] [
		test: first+ tests
		result: switch/default type? :test bind/copy test-cases 'value error
		debug-probe/for result [net.revolucent.core]
		if error? :result [do error]
		either test-all [
			if ! :result [return false]
		][
			if :result [return either index [p] [:test]]
		]
		++ p
	]
	either test-all [true] [none]
]

protect-module: funct [
	{Protect the words of a module from modification. Words which are not exported are hidden by default.}
	m [object!]
	/expose {Expose all unexported words.}
	/exclude
		words [block!] {A block of words that will not be touched either way.}
][
	unless exclude [words: copy []]
	exclude: :lib/exclude
	module-words: exclude words-of m words
	protect/words module-words
	unless expose [
		exported-words: exclude select spec-of m 'exports words
		protect/hide/words exclude module-words exported-words
	]
]

; protect-module/exclude self [debug debug-words]