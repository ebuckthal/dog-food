main: main.lisp 
	urn.lua main.lisp -o main

run: main.lua
	love .
