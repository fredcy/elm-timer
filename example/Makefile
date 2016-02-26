build: main.html sleep.html

main.html: Main.elm
	elm make Main.elm --output $@

sleep.html: Sleep.elm
	elm make Sleep.elm --output $@

watch:
	-$(MAKE) build
	fswatch Main.elm Sleep.elm ../Timer.elm | \
	while read f; do \
	    echo "======================================================================="; \
	    echo; echo; echo $$f; \
	    $(MAKE) build; \
	done

browse:
	browser-sync start --server --startPath main.html --files main.html sleep.html

