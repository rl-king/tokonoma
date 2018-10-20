default: help

all: haskell elm

clearcache:
	@rm ./cache/*

elm:
	@elm make --debug ./src/elm/Main.elm --output=./main.js

haskell:
	@stack build

help:
	@echo "all -- compile haskell and elm"
	@echo "clearcache -- remove all files in ./cache"
	@echo "elm -- compile elm to ./index.html"
	@echo "haskell -- compile haskell"
	@echo "server -- start haskell server"
	@echo "watch -- watch haskell files and compile on change"

server:
	@stack exec tokonoma

watch:
	@stack build --file-watch
