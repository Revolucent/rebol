REBOL [
	needs: [
		http://r3.revolucent.net/net.revolucent.core.v1.r
	]
]

enable-debug [status output]
; enable-debug [status]

max-iteration-depth: none

push: :append

pop: funct [
	series [series!]
][
	end: last series
	remove back tail series
	end
]

for9: funct [
	'word [word!]
	body [block!]
][
	for n 1 9 1 [
		set word n
		do body
	]
]

solved?: funct [
	puzzle [block!]
][
	foreach row puzzle [
		foreach cell row [
			if block? cell [
				return false
			]
		]
	]
	return true
]

try-solve: funct [
	puzzle [block!]
	/local
		x y
][
	; Reusable blocks
	assign-numbers: [
		cell: puzzle/:y/:x
		if integer? cell [append numbers cell]
	]
	simplify-cell: [
		row: puzzle/:y
		cell: row/:x
		if block? cell [
			cell: exclude cell numbers
			if empty? cell [
				debug-print/for "Detected an unsolvable puzzle." [status]
				debug-probe/for puzzle [output]
				return none
			]
			if equal? 1 length? cell [cell: first cell]
			poke row x cell
		]
	]
	; Solver
	solved: false
	while [! solved] [
		stuck: copy/deep puzzle
		; Each row
		for9 y [
			numbers: copy []
			for9 x assign-numbers
			unless empty? numbers [
				for9 x simplify-cell
			]
		]
		; Each column
		for9 x [
			numbers: copy []
			for9 y assign-numbers
			unless empty? numbers [
				for9 y simplify-cell
			]
		]
		; Each group
		for gx 0 2 1 [
			for gy 0 2 1 [
				numbers: copy []
				for x (gx * 3 + 1) (gx * 3 + 3) 1 [
					for y (gy * 3 + 1) (gy * 3 + 3) 1 bind/copy assign-numbers 'x
				]
				for x (gx * 3 + 1) (gx * 3 + 3) 1 [
					for y (gy * 3 + 1) (gy * 3 + 3) 1 bind/copy simplify-cell 'x
				]				
			]
		]
		; Detect a stuck puzzle
		if all [equal? stuck puzzle] [
			return puzzle
		]
	]	
	; Return puzzle
	puzzle
]

iter: 0
solve: func [
	puzzle [block!]
	/mark
		depth [integer!]
][
	if ! mark [depth: 1]
	if all [max-iteration-depth greater-or-equal? depth max-iteration-depth] [
		do make error! "Maximum iteration depth reached."
	]
	++ iter
	debug-print/for ["Iteration:" iter "Depth:" depth] [status]
	if solved? puzzle [return puzzle]
	use [x y guess row cell temp-guess temp-row solution] [
		guess: copy/deep puzzle
		for9 y [for9 x [
			row: guess/:y
			cell: row/:x
			if block? cell [
				temp-guess: copy/deep guess
				foreach n cell [
					temp-row: temp-guess/:y
					poke temp-row x n
					solution: try-solve temp-guess
					if solution [
						either solved? solution [
							return solution
						][
							solution: solve/mark solution (depth + 1)
							if solution [return solution]
						]
					]
				]
			]
		]]
	]
	none
]

puzzles: copy []
puzzle: none
add-puzzle: [if puzzle [append/only puzzles try-solve puzzle]]
foreach line read/lines %sudoku.txt [
	either equal? "Grid" copy/part line 4 [
		do add-puzzle
		puzzle: copy []
	][
		row: copy []
		foreach n line [
			either equal? #"0" n [
				append/only row copy [1 2 3 4 5 6 7 8 9]
			][
				append/only row to integer! to string! n
			]
		]
		append/only puzzle row
	]
]
do add-puzzle

probe solve [[2 4 7 9 8 1 3 5 6] [9 6 5 2 7 3 1 8 4] [1 3 8 5 6 4 2 7 9] [6 7 3 1 9 5 4 9 8] [5 1 5 8 2 9 [6 7] 3 7] [4 [8 9] 2 7 3 6 5 1 5] [3 [8 9] 1 6 5 7 8 4 2] [7 2 9 3 4 8 5 6 1] [8 [5 8] 4 6 1 2 7 9 3]]