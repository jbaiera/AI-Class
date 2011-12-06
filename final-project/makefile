
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
	ghc --make rab6hmSims3
	ghc --make rab6hmSims4
	ghc --make rab6hmSims5
	ghc --make rab6hmSims6
	ghc --make rab6hmSims7
	ghc --make rab6hmSims8
	ghc --make rab6hmSims9
	ghc --make rab6hmSims10
	ghc --make greedySims1
	ghc --make greedySims2

sims : 
	ghc --make rab4grSims1
	ghc --make rab4grSims2
	ghc --make rab4hmSims1
	ghc --make rab4hmSims2
	ghc --make rab6hmSims1
	ghc --make rab6hmSims2
	ghc --make rab6hmSims3
	ghc --make rab6hmSims4
	ghc --make rab6hmSims5
	ghc --make rab6hmSims6
	ghc --make rab6hmSims7
	ghc --make rab6hmSims8
	ghc --make rab6hmSims9
	ghc --make rab6hmSims10
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
	rm -f rab6hmSims1 rab6hmSims2 rab6hmSims3 rab6hmSims4 rab6hmSims5 rab6hmSims6 rab6hmSims7 rab6hmSims8 rab6hmSims9 rab6hmSims10


