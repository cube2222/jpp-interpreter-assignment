.PHONY: all test clean

all:
	cabal build
	mv dist-newstyle/build/*/*/Interpreter-0.1.0.0/x/Interpreter-exe/build/Interpreter-exe/Interpreter-exe .

test:
	cabal test --test-log="/dev/stdout"

clean:
	cabal clean