main: main.lisp 
	urn.lua main.lisp -o main

run: main.lua
	love .

watch:
	@echo "watching for changes to main.lisp ..."
	@fswatch -0 main.lisp | xargs -0 -n 1 -I {} make run 
