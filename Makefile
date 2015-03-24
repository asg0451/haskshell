all:
	happy Parser.y -i && cabal build
