all:
	happy Parser.y -i && ghc Parser.hs
