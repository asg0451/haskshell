all:
	happy Parser.y -i && cabal build
clean:
	rm *.hi *.o
