CABAL?=cabal
DATE := $(shell dpkg-parsechangelog 2>/dev/null | grep Date | cut -d " " -f2-)

build: tags propellor.1 configured
	$(CABAL) build
	@if [ -d dist-newstyle ]; then \
		ln -sf $$(cabal exec -- sh -c 'command -v propellor-config') propellor; \
	else \
		ln -sf dist/build/propellor-config/propellor-config propellor; \
	fi

install:
	install -d $(DESTDIR)/usr/bin $(DESTDIR)/usr/src/propellor
	if [ -d dist-newstyle ]; then \
		install -s $$(cabal exec -- sh -c 'command -v propellor') $(DESTDIR)/usr/bin/propellor; \
	else \
		install -s dist/build/propellor/propellor $(DESTDIR)/usr/bin/propellor; \
	fi
	mkdir -p gittmp
	if [ "$(CABAL)" = ./Setup ]; then \
		./Setup sdist --output-directory=gittmp; \
	else \
		$(CABAL) sdist -o - | (cd gittmp && tar zx --strip-components=1); \
	fi
	# cabal sdist does not preserve symlinks, so copy over file
	cd gittmp && for f in $$(find -type f); do rm -f $$f; cp -a ../$$f $$f; done
	# reset mtime on files in git bundle so bundle is reproducible
	find gittmp -print0 | xargs -0r touch --no-dereference --date="$(DATE)"
	export GIT_AUTHOR_NAME=build \
	&& export GIT_AUTHOR_EMAIL=build@buildhost \
	&& export GIT_AUTHOR_DATE="$(DATE)" \
	&& export GIT_COMMITTER_NAME=build \
	&& export GIT_COMMITTER_EMAIL=build@buildhost \
	&& export GIT_COMMITTER_DATE="$(DATE)" \
	&& cd gittmp && git init \
	&& git add . \
	&& git commit -q -m "distributed version of propellor" \
	&& git bundle create $(DESTDIR)/usr/src/propellor/propellor.git master HEAD \
	&& git show-ref master --hash > $(DESTDIR)/usr/src/propellor/head
	rm -rf gittmp

clean:
	rm -rf dist dist-newstyle configured Setup \
		tags propellor propellor.1 privdata/local
	find . -name \*.o -exec rm {} \;
	find . -name \*.hi -exec rm {} \;

# hothasktags chokes on some template haskell etc, so ignore errors
# duplicate tags with Propellor.Property. removed from the start, as we
# often import qualified by just the module base name.
tags:
	@find . | grep -v /.git/ | grep -v /tmp/ | grep -v dist/ | grep -v /doc/ | egrep '\.hs$$' | xargs hothasktags 2>/dev/null | perl -ne 'print; s/Propellor\.Property\.//; print' | sort > tags || true

configured: propellor.cabal
	@if [ "$(CABAL)" = ./Setup ]; then ghc --make Setup; fi
	@$(CABAL) configure
	touch configured

propellor.1: doc/usage.mdwn doc/mdwn2man
	doc/mdwn2man propellor 1 < doc/usage.mdwn > propellor.1

.PHONY: tags
