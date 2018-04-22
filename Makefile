SHELL := /bin/bash
URN := ~/.urn/bin/urn.lua

main.lua: main.lisp 
	$(URN) main.lisp -o main

run: main.lua
	love .

clean:
	rm main.lua

watch:
	@echo "watching for changes to main.lisp ..."
	@fswatch -0 main.lisp | xargs -0 -n 1 -I {} make run 
