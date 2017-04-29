.PHONY: clean

build/main: main.hs
	mkdir -p build
	(cd build/ && ghc -o main --make ../main.hs)

clean:
	rm -rf build
	rm ./*.o
	rm ./*.hi