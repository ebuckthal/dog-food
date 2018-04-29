SHELL := /bin/bash
URN := ~/.urn/bin/urn.lua
FILE_NAME := dog-eat-food-world

main.lua: *.lisp
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

dist/$(FILE_NAME).love: main.lua
	mkdir -p dist
	zip -9 dist/$(FILE_NAME).love *.lua assets/*.png

dist-win: dist/$(FILE_NAME).love
	mkdir -p dist/$(FILE_NAME)-win32
	cat love/win32/love.exe dist/${FILE_NAME}.love > dist/$(FILE_NAME)-win32/$(FILE_NAME).exe
	cp -f love/win32/*.{txt,dll} dist/$(FILE_NAME)-win32
	cd dist; zip -9 -r $(FILE_NAME)-win32.zip $(FILE_NAME)-win32/*

dist-osx: dist/${FILE_NAME}.love
	cp -rf love/osx/Dog\ Eat\ Food\ World.app dist/
	cp dist/${FILE_NAME}.love dist/Dog\ Eat\ Food\ World.app/Contents/Resources
	cd dist; zip -9 -r $(FILE_NAME)-osx.zip Dog\ Eat\ Food\ World.app

clean-dist:
	rm -rf dist/*

dist: clean-dist dist-win dist-osx
