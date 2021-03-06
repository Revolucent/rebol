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
	Date: 2013-04-23
	Name: net.revolucent.parse.csv
	Version: 3.0.0
	Type: module
	Exports: [csv csv-object csv-block csv-fields read-csv-file]
	Needs: [
		2.101.0 
		net.revolucent.core.v3
	]	
	License: MIT
	History: [
		2013-04-24 {Renamed parse-csv to READ-CSV in anticipation of WRITE-CSV. Fixed a bug that occurs when the escape-char and quote-char are the same.}
	]
]

csv-fields: closure [
	"Takes a block of transforms and returns a function that applies them to the CSV items in a row."
	transforms [block!]
][
	transforms: reduce transforms
	funct [name item] [
		f: select transforms name
		f item
	]
]

; Here's an example of CSV-OBJECT, which is a higher-order function.
;
; First, let's look at a usage of CSV without using CSV-OBJECT.
;
; csv/by [
; 	probe read-csv {"Music For The Masses",88}
; ] funct [items] [
; 	object [
;		name: items/1
;		year: to integer! items/2
;	]
; ]
;
; For each row of output, the function passed to the /BY refinement
; creates an object. This gets pretty tedious when there are many columns, so:
;
; csv/by [
;	probe read-csv {"Music For The Masses",88}
; ] csv-object [name year]
;
; CSV-OBJECT returns a dynamically created function that maps the first
; CSV field to 'NAME and the second one to 'YEAR. We can use integers
; before the names to specify which columns we want:
;
; csv-object [2 firstname lastname 14 vin date]
;
; In the case above, the second field will be mapped to 'FIRSTNAME, the third to 'LASTNAME,
; the fourteenth to 'VIN and the fifteenth to 'DATE. All other fields will be ignored.
csv-object: closure [
	"Returns a function that creates an object by mapping values to names."
	names [block!] "E.g., [fname lname 7 age]"
	/by
		transform "Transform to be applied to each field" ; It's best to make this function using CSV-FIELDS
	/local
		name-map
		n
		name
		rules
][
	default transform func [name item] [item]
	name-map: copy []
	rules: [
		(n: 1)
		some [
			set n integer!
		|	set name word! (repend name-map [n name] ++ n)
		]
		end
	]
	unless parse names rules [do make error! "Invalid names specifier."]
	funct [
		items [block!]
	][
		o: copy []
		foreach [n name] name-map [
			repend o [to set-word! name transform name items/:n]
		]
		object o
	]
]

csv-block: closure [
	"Returns a function that creates a block by choosing a subset of values from another block."
	indexes [block!] "E.g., [2 4 7]" ; Choose fields 2, 4, and 7 in that order
	/by
		transform "Transform to be applied to each field." ; It's best to make this function using CSV-FIELDS
][
	default transform func [index item] [item]
	funct [
		items [block!]
	][
		b: copy []
		foreach index indexes [
			append b transform index items/:index
		]
		b
	]
]

; CSV creates an environment in which CSV operations can be performed by, for
; instance, setting the quote character or the separator, etc. It then injects
; the parse-csv function into the provided block. parse-csv will parse
; a single row of CSV input according to the given settings. E.g.,
;
; csv/sep [
; 	probe read-csv {a~b}
; ] #"~"
;
; The parse-csv function is valid only within the given block. This will not work:
;
; csv [] probe read-csv {a,b}
;
; (Unless of course someone has defined a different parse-csv outside of this module.)
csv: funct [
	body [block!]
	/separator
		separator-char [char!]
	/quote
		quote-char [char!]
	/escape
		escape-char [char!]
	/by
		transform [any-function!]
	/local
		chunk
		item
][
	default escape-char #"\"
	default quote-char #"^""
	default separator-char #","
	default transform :identity
	
	replace whitespace-chars: copy " ^-" separator-char ""
	replace whitespace-chars quote-char ""
	replace whitespace-chars escape-char ""
	whitespace: charset whitespace-chars
	non-separator-chars: complement charset separator-char
	non-quote-chars: complement charset quote-char
	escaped-quote: rejoin [escape-char quote-char]
	
	items: copy []
	non-quoted-item: [copy item any non-separator-chars (append items item)]
	escaped-quote: rejoin [escape-char quote-char]
	either equal? escape-char quote-char [
		quoted-item: [
			any whitespace
			quote-char 
			any [ 
				escape: thru quote-char any whitespace separator-char :escape break
			|	copy chunk to escaped-quote escaped-quote (repend item [chunk quote-char]) 
			]
			copy chunk to quote-char (append item chunk)
			quote-char
			(append items item)
			any whitespace
		]			
	][ ; quote-char and escape-char are not equal
		quoted-item: [
			any whitespace
			quote-char 
			any [ copy chunk to escaped-quote escaped-quote (repend item [chunk quote-char]) ]
			copy chunk to quote-char (append item chunk)
			quote-char
			(append items item)
			any whitespace
		]						
	]
	
	item-rule: [[(item: copy "") quoted-item | non-quoted-item]] 
	rules: [item-rule any [separator-char item-rule] end]

	do func [read-csv] body func [
		line [string!]
	][
		items: copy [] ; This variable's scope is CSV, not READ-CSV. It is used inside the PARSE rules.
		either parse line rules [transform items] [do make error! rejoin ["Invalid line: " line]]
	]
]

read-csv-file: funct [
	file [file!]
	/headers "First row is header row"
	/sep
		sep-char [char!]
	/quote
		quote-char [char!]
	/escape
		escape-char [char!] "Defaults to none" ; I.e., assume quotes are never escaped	
	/by
		transform [any-function!] "Transform a row into e.g. an object"
][
	default transform :identity
	
	body: copy [
		header: headers
		items: copy []
		foreach line read/lines file [
			unless header [
				append/only items read-csv line
			]
			header: false
		]
		items
	]
	
	apply :csv [body sep sep-char quote quote-char escape escape-char by :transform]
]
