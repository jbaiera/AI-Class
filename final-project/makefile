
all : 
	ghc --make main
	ghc --make humanplay
	ghc --make findvalid

clean : 
	find . | grep "\.o$$" | xargs rm -f
	find . | grep "\.hi$$" | xargs rm -f
	rm -f findvalid
	rm -f humanplay
	rm -f main

