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
	Exports: [csv csv-object csv-block csv-fields parse-csv-file]
	Needs: [
		2.101.0 
		http://rebol.revolucent.net/net.revolucent.core.v3.r
	]	
	License: MIT
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
; 	probe parse-csv {"Music For The Masses",88}
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
;	probe parse-csv {"Music For The Masses",88}
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
; the PARSE-CSV function into the provided block. PARSE-CSV will parse
; a single row of CSV input according to the given settings. E.g.,
;
; csv/with-separator [
; 	probe parse-csv {a~b}
; ] #"~"
;
; The PARSE-CSV function is valid only within the given block. This will not work:
;
; csv [] probe parse-csv {a,b}
;
; (Unless of course someone has defined a different PARSE-CSV outside of this module.)
csv: funct [
	"Provides a context in which the PARSE-CSV function can operate on CSV rows."
	body [block!]
	/with-separator
		sep-char [char!]
	/with-quote
		quote-char [char!]
	/with-escape
		escape-char [char!] "Defaults to none" ; I.e., assume quotes are never escaped	
	/by
		transform [any-function!] "Transform a row into e.g. an object"
	/local
		chunk [string!]
		item [string!]
		items [block!]
][
	default quote-char #"^""
	default sep-char #","
	default transform :identity
	
	whitespace-chars: copy " ^-"
	remove-each char whitespace-chars [any [equal? sep-char char equal? quote-char char]]
	whitespace: charset whitespace-chars
	characters: complement charset rejoin [whitespace-chars sep-char quote-char]
	either escape-char [
		; If we have an escape char our quoted-item becomes much more complex
		escaped-quote: rejoin [escape-char quote-char]
		quoted-item: [
			quote-char 
			any [ copy chunk to escaped-quote escaped-quote (repend item [chunk quote-char]) ]
			copy chunk to quote-char (append item chunk)
			quote-char
		]
	] [
		quoted-item: [quote-char copy item to quote-char quote-char]
	]
	match-item: [
		(item: copy "")
		any whitespace
		opt [quoted-item | copy item some characters]
		any whitespace
	]
	rules: [
		any [
			match-item
			sep-char
			(append items item)
		]
		match-item
		end
		(append items item)
	]	
	
	use [parse-csv] [
		parse-csv: func [
			line [string!]
		][
			items: copy []
			either parse line rules [transform items] [do make error! rejoin ["Invalid line: " line]]
		]
		do bind body 'parse-csv
	]
]

parse-csv-file: funct [
	file [file!]
	/headers "First row is header row"
	/with-separator
		sep-char [char!]
	/with-quote
		quote-char [char!]
	/with-escape
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
				append/only items parse-csv line
			]
			header: false
		]
		items
	]
	
	apply :csv [body with-separator sep-char with-quote quote-char with-escape escape-char by :transform]
]