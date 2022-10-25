MINIFY:=0
ELM_MAKE_FLAGS:=

TASKS_HTML:=$(wildcard src/Tasks/*/index.html)
TASKS_HTML_COMPILED:=$(patsubst src/Tasks/%, public/tasks/%, $(TASKS_HTML))

TASKS_ELM:=$(wildcard src/Tasks/*/Main.elm)
TASKS_JS:=$(patsubst src/Tasks/%/Main.elm, public/tasks/%/elm.js, $(TASKS_ELM))

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

public/tasks/%/index.html: src/Tasks/%/index.html
	mkdir -p $(dir $@)
	cp $< $@

public/tasks/%/elm.js: src/Tasks/%/Main.elm src/Tasks/%/*.elm
	@echo "Compiling $@ from $<"
	elm make $< --output=$@ $(ELM_MAKE_FLAGS)
	@if [ "$(MINIFY)" = "1" ]; then \
		echo "Minifying..."; \
		node_modules/.bin/elm-minify "$@" --overwrite; \
	fi

.PHONY=watch
watch:
	@find src -name '*.elm' -or -name '*.html' -or -name '*.css' | entr $(MAKE)
