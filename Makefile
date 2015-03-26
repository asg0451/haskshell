all: Parser.y Lexer.x Main.hs
	happy -g Parser.y -i && alex Lexer.x && cabal build
