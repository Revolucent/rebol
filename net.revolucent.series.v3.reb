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
	Exports: [intersperse]
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
		if (length? result) > 0 [apply :append [series :value false none only]]
		append/only series :elem
	]
	result
]
