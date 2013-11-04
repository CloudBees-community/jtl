rebar = ./rebar

compile: deps
	$(rebar) compile

quick:
	$(rebar) compile skip_deps=true

deps:
	$(rebar) get-deps

refresh-deps:
	$(rebar) delete-deps
	$(rebar) get-deps

tests=""

.PHONY: test
test: compile
ifeq ($(tests), "")
	$(rebar) -j1 eunit
else
	$(rebar) -j1 eunit suite=$(tests)
endif

.PHONY: doc
doc:
	$(rebar) doc

clean:
	$(rebar) clean

opts=
shell: compile
	erl -pa ebin $(wildcard deps/*/ebin) -s jtl_reloader ${opts}
