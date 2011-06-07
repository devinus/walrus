.PHONY: all compile test dialyze clean

all: compile

compile:
	@./rebar compile

test:
	@./rebar eunit

dialyze: compile
	@./rebar dialyze

clean:
	@./rebar clean
