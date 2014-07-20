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
	Title: "Revolucent IO Library"
	Date: 2013-09-10
	Name: net.revolucent.core
	Version: 3.0.0
	Type: module
	Exports: [for-dir]
	Needs: [2.101.0]	
	License: MIT
]

for-dir: funct [
	"Iterate over directory contents."
	'word [word!] "Word to set each time"
	dir [file!] "Directory to recurse"
	body [block!] "Block to execute each time"
	/recurse "Recurse into subdirectories"
	/only "Do not execute BODY for directories"
	/with
		'options [word! block!] "Additional options"
][
	assert [dir? dir]
	default options copy []
	process: func reduce [word] body
  if ! block? options [options: reduce [options]]
	skip-hidden: find options 'skip-hidden
	iterate: func [
		dir [file!]
	][
		foreach file read dir [
			unless all [skip-hidden equal? #"." first file] [
				file: rejoin [dir file]
				either ! dir? file [
					process file
				][
					if ! only [process file]
					if recurse [iterate file]
				]
			]
		]
	]
	iterate dir
	exit
]

