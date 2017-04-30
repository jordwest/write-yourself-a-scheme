.PHONY: clean

build/main: evaluator.hs parser.hs main.hs
	mkdir -p build
	stack ghc -- -o build/main --make main.hs

clean:
	rm -rf build
	rm ./*.o
	rm ./*.hi

run: build/main
	./build/main "(+ 100 18 2)"