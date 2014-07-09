OUT=shk3
FLAGS=-W -O2
CFLAGS=

all: build

build:
	ghc $(FLAGS) $(CFLAGS) -isrc src/Main -o $(OUT)

clean:
	rm -fv $(OUT)
	find -name '*.o' -print0 | xargs -0 rm -fv
	find -name '*.hi' -print0 | xargs -0 rm -fv
