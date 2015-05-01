
all : build

build : site
	./site build

site : site.hs blog.cabal .cabal-sandbox
	cabal install
	touch site
	./site rebuild

.cabal-sandbox :
	cabal sandbox init
	
