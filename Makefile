FLAGS=-fno-warn-name-shadowing
EXTRAFLAGS=-XTypeFamilies -XFlexibleInstances -XDeriveGeneric -fno-warn-name-shadowing -XScopedTypeVariables

main: Main.hs Parse.hs Player.hs
	ghc ${FLAGS} ${EXTRAFLAGS} Main.hs -o pitcher-viz

nowarn: Main.hs Parse.hs Player.hs
	ghc ${EXTRAFLAGS} Main.hs -o pitcher-viz

clean:
	rm -f *.hi
	rm -f *.o
	rm -f Main
