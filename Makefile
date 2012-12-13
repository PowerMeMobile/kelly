NAME=kelly

all: generate

get-deps:
	@./rebar get-deps

clean:
	@./rebar clean

dialyze:
	@./rebar clean
	@./rebar compile debug_info=1
	@./rebar dialyze skip_deps=true
	@./rebar clean

build-plt:
	@./rebar build-plt skip_deps=true

compile: get-deps
	@./rebar compile

generate: compile
	@rm -rf ./rel/$(NAME)
	@./rebar generate

compile-fast:
	@./rebar compile

generate-fast: compile-fast
	@rm -rf ./rel/$(NAME)
	@./rebar generate

console:
	./rel/$(NAME)/bin/$(NAME) console

develop:
	./rel/$(NAME)/bin/$(NAME) develop

gdb:
	./rel/$(NAME)/bin/$(NAME) gdb

release: generate
	./rel/create-release.sh

update-deps:
	./rebar update-deps