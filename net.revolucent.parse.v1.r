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
	title: "Revolucent Parse Utilities Module"
	name: net.revolucent.parse
	type: module
	file: %net.revolucent.parse.v1.r
	author: "Gregory Higley"
	date: 2010-08-01
	version: 1.0.1
	needs: [
		2.100.99
		http://r3.revolucent.net/net.revolucent.core.v1.r 1.3.4
	]
	exports: [
		digit
		extract-ips
		ip
	]
	history: [
		1.0.1 {Added a dependency to NET.REVOLUCENT.CORE/PROTECT-MODULE.}
		1.0.0 {Added DIGIT, IP and EXTRACT-IPS}
	]	
	purpose: "A place for parsing :)"
]

digit: [ #"0" | #"1" | #"2" | #"3" | #"4" | #"5" | #"6" | #"7" | #"8" | #"9" ]

ip: object [
	octet: [ 1 3 digit ]
	address: [ octet #"." octet #"." octet #"." octet ]
]

extract-ips: funct [
	{Extracts all of the valid IPv4 addresses from a given string and returns them as a block of tuples.}
	string [any-string!]
][
	address: none
	addresses: copy []
	rules: [
		any [
			to digit
			copy address [ ip/address any [#"." ip/octet] ] (
				attempt [
					address: to tuple! address
					if equal? 4 length? address [
						append addresses address
					]
				]
			)
		|	skip
		]
	]
	either parse string rules [addresses] [none]
]

protect-module self