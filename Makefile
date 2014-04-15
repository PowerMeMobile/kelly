NAME=kelly
OTP_PLT=~/.otp.plt
PRJ_PLT=$(NAME).plt

all: generate

generate: compile xref
	@rm -rf ./rel/$(NAME)
	@./rebar generate

compile: get-deps
	@./rebar compile

get-deps:
	@./rebar get-deps

update-deps:
	@./rebar update-deps

xref: compile
	@./rebar xref skip_deps=true

clean:
	@./rebar clean

dialyze: $(OTP_PLT) compile $(PRJ_PLT)
	@dialyzer --plt $(PRJ_PLT) -r ./subapps/*/ebin

$(OTP_PLT):
	@dialyzer --build_plt --output_plt $(OTP_PLT) --apps erts \
		kernel stdlib crypto mnesia sasl common_test eunit ssl \
		asn1 compiler syntax_tools inets

$(PRJ_PLT):
	@dialyzer --add_to_plt --plt $(OTP_PLT) --output_plt $(PRJ_PLT) \
	-r ./deps/*/ebin ./subapps/*/ebin

console:
	@./rel/$(NAME)/bin/$(NAME) console

develop:
	@./rel/$(NAME)/bin/$(NAME) develop

gdb:
	@./rel/$(NAME)/bin/$(NAME) gdb

release: generate
	@./rel/create-release.sh

configure:
	@./rel/files/http_conf.sh

api-test:
	@./rebar skip_deps=true eunit suites=kelly_http_api_addr2cust_test
	@./rebar skip_deps=true eunit suites=kelly_http_api_customers_test
	@./rebar skip_deps=true eunit suites=kelly_http_api_gateways_test
	@./rebar skip_deps=true eunit suites=kelly_http_api_networks_test
	@./rebar skip_deps=true eunit suites=kelly_http_api_network_maps_test
	@./rebar skip_deps=true eunit suites=kelly_http_api_providers_test

simple-test: generate
	@./test/simple_test

tags:
	@find . -name "*.[e,h]rl" -print | etags -
