main: main.lisp 
	urn.lua main.lisp -o main

run: main
	love .
