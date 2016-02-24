build:
	elm make Main.elm

watch:
	-$(MAKE) build
	fswatch *.elm | \
	while read f; do \
	    echo "======================================================================="; \
	    echo; echo; echo $$f; \
	    $(MAKE) build; \
	done

browse:
	browser-sync start --server --startPath index.html --files index.html

