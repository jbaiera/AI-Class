
all : 
	ghc --make main
	ghc --make humanplay
	ghc --make findvalid
	ghc --make aiplay

clean : 
	find . | grep "\.o$$" | xargs rm -f
	find . | grep "\.hi$$" | xargs rm -f
	rm -f findvalid
	rm -f humanplay
	rm -f aiplay
	rm -f main

