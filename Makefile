SHELL := /bin/bash
URN := ~/.urn/bin/urn.lua

main.lua: main.lisp 
	$(URN) main.lisp -o main

run: main.lua
	love .

test:
	$(URN) helper-test.lisp --run

clean:
	rm -f main.lua

watch:
	@echo "watching for changes to main.lisp ..."
	@fswatch -0 main.lisp | xargs -0 -n 1 -I {} make run 
