all: Parser.y Lexer.x Main.hs
	alex Lexer.x && happy -g Parser.y && cabal install
