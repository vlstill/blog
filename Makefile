
all : build test


build : site
	./site build

site : site.hs blog.cabal .cabal-sandbox
	cabal install
	touch site
	./site rebuild

.cabal-sandbox :
	cabal sandbox init

test : build
	rsync -avc --del --progress _site/ antea:public_html/staging

deploy : build
	rsync -avc --del --progress _site/ antea:public_html --exclude tmp --exclude norway
