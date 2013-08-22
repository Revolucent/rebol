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
	Title: "Revolucent CSV Library"
	Date: 2013-04-23
	Name: net.revolucent.core
	Version: 0.9.0
	Type: module
	Exports: [^ attempt-to identity none-if-empty symbol transform-unless-empty log]
	Needs: [2.101.0]	
	License: MIT	
]

identity: func [o] [o]

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

attempt-to: closure [
	"Returns a fuction that attempts to convert a value to the given type"
	datatype [datatype!]
][
	func [val] [attempt [to datatype val]]
]

none-if-empty: func [
	"If the series is empty, return NONE, else the series."
	series [series!]
] [
	either empty? series [none] [series]
]

^: func [
	"Lambda, e.g., ^[ x | x + 1 ]"
	lambda [block!]
	/local
		f-spec
		f-body
][
	unless parse lambda [copy f-spec to '| '| copy f-body to end] [
		do make error! rejoin ["Invalid lambda: " mold/flat lambda]
	]
	func f-spec f-body
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

logging: off

log: funct [
	"Alias for PRINT used for logging. Enable with log/set on."
	value
	/set "Use to turn logging on or off"
	/with
		logger "Function to use for logging. Defaults to PRINT."
][
	default logger :print
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

