.PHONY: all test clean

all:
	cabal build -j1
	mv dist/build/Interpreter-exe/Interpreter-exe .

test:
	cabal test -j1 --log="/dev/stdout"

clean:
	cabal clean