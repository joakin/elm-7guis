MINIFY:=0
ELM_MAKE_FLAGS:=

TASKS_HTML:=$(wildcard src/tasks/*/index.html)
TASKS_HTML_COMPILED:=$(patsubst src/%,public/%,$(TASKS_HTML))

TASKS_ELM:=$(wildcard src/tasks/*/Main.elm)
TASKS_JS:=$(patsubst src/%/Main.elm,public/%/elm.js,$(TASKS_ELM))

.PHONY=all
all: $(TASKS_HTML_COMPILED) $(TASKS_JS) public/index.html

.PHONY=clean
clean:
	rm -rf public/*

.PHONY=dist
dist: MINIFY:=1
dist: ELM_MAKE_FLAGS += --optimize
dist: clean all

public/index.html: src/index.html public/index.css
	cp $< $@

public/index.css: src/index.css
	cp $< $@

public/tasks/%/index.html: src/tasks/%/index.html
	mkdir -p $(dir $@)
	cp $< $@

public/tasks/%/elm.js: src/tasks/%/Main.elm src/tasks/%/*.elm
	@echo "Compiling $@ from $<"
	elm make $< --output=$@ $(ELM_MAKE_FLAGS)
	@if [ "$(MINIFY)" = "1" ]; then \
		echo "Minifying..."; \
		node_modules/.bin/elm-minify "$@" --overwrite; \
	fi

.PHONY=watch
watch:
	@find src -name '*.elm' -or -name '*.html' -or -name '*.css' | entr $(MAKE)
