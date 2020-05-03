.PHONY: all test clean

all:
	cabal build -j1
	mv dist/build/Interpreter-exe/Interpreter-exe .

test:
	cabal test -j1 --test-log="/dev/stdout"

clean:
	cabal clean -j1