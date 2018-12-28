MINIFY:=0
ELM_MAKE_FLAGS:=

TASKS_HTML:=$(wildcard src/tasks/*/index.html)
TASKS_HTML_COMPILED:=$(patsubst src/%,public/%,$(TASKS_HTML))

.PHONY=all
all: $(TASKS_HTML_COMPILED) public/index.html

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

public/tasks/%/index.html: src/tasks/%/index.html public/tasks/%/elm.js
	cp $< $@

.PRECIOUS: public/tasks/%/elm.js
public/tasks/%/elm.js: src/tasks/%/Main.elm
	@echo "Compiling $@ from $<"
	elm make $< --output=$@ $(ELM_MAKE_FLAGS)
	@if [ "$(MINIFY)" = "1" ]; then \
		echo "Minifying..."; \
		node_modules/.bin/uglifyjs "$@" --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output="$@"; \
	fi

src/tasks/%/Main.elm: $(wildcard src/tasks/%/*.elm)

.PHONY=watch
watch:
	@find src -name '*.elm' -or -name '*.html' -or -name '*.css' | entr $(MAKE)