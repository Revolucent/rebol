REBOL [
	author: "Gregory Higley"
	name: net.revolucent.core
	version: 1.2.0
	history: [
		1.2.0 {Added CONST.}
		1.1.0 {Added TEST and FIND-TYPE.}
		1.0.0 {Added MODULE-CHECKSUM.}
	]
	exports: [
		id
		const
		module-checksum
		test
		find-type
	]
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

module-checksum: funct [
	source [url! file!]
][
	checksum/secure to-binary mold/flat load/unbound/header source
]

function-test-case: [test :value]
block-test-case: [eval head append copy test :value]
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
	or functions (if the /ONLY refinement is used). If at least one of the
	tests return TRUE, the test itself is returned. If the /ALL refinement is
	used, all of the tests must return true.
	
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

find-type: funct [
	{Returns the first type that matches the type of the VALUE parameter, or NONE.}
	type [datatype! typeset! block!]
	value [any-type!]	
][
	types: switch type?/word type [
		datatype! [reduce [type]]
		typeset! [to block! type]
		block! [reduce type]
	]
	foreach type types [
		either all [
			typeset? type 
			result: find-type type :value
		][return result][if equal? type type? :value [return type]]
	]
	none
]

protect/hide/words exclude words-of self select spec-of self 'exports
