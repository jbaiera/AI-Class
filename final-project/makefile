
all : 
	ghc --make main
	ghc --make humanplay
	ghc --make findvalid
	ghc --make aiplay
	ghc --make rab4grSims1
	ghc --make rab4grSims2
	ghc --make rab4hmSims1
	ghc --make rab4hmSims2
	ghc --make rab6hmSims1
	ghc --make rab6hmSims2
	ghc --make greedySims1
	ghc --make greedySims2

wc : 
	find . | grep "\.hs$$\|\.sql$$\|\.php$$\|\.css$$" | xargs wc -l
	find . | grep "\.hs$$" | xargs wc -l

clean : 
	find . | grep "\.o$$" | xargs rm -f
	find . | grep "\.hi$$" | xargs rm -f
	rm -f findvalid humanplay aiplay main
	rm -f rab4grSims1 rab4grSims2 rab4hmSims1 rab4hmSims2 rab6hmSims1 rab6hmSims2 greedySims1 greedySims2


