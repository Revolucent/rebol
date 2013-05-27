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
	Exports: [^ attempt-to identity none-if-empty transform-unless-empty]
	Needs: [2.101.0]	
	License: MIT	
]

identity: func [o] [o]

attempt-to: closure [
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

^: funct [
	spec [block!]
	/local
		f-spec f-body
][
	parse [copy f-spec to '| '| copy f-body to end]
	funct f-spec f-body
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
