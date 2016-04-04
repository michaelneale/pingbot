image:
	docker build -t pingbot .

shell:  image
	docker run -v $(shell pwd):/app -it pingbot

# run this when brand new
cabal-init:
	cabal update
	[ -d .cabal-sandbox ] || cabal sandbox init 
	cabal install --only-dependencies

install: image
	docker run -v $(shell pwd):/app pingbot ./build.sh 
