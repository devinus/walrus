GIT=`which git`
ERL=`which erl`
ERLC=`which erlc`
ESCRIPT=`which escript`
REBAR=$(ESCRIPT) rebar

BLD=.build

.PHONY: test doc

all: deps compile

deps: rebar
	@exec $(REBAR) get-deps
	@exec $(REBAR) update-deps

compile:
	@exec $(REBAR) compile

doc: rebar compile
	@-rm -rf doc
	@-exec $(REBAR) doc skip_deps=true

test: compile
	@-mkdir -p .eunit
	@-cp -r priv/fixtures .eunit/
	@-cp -r priv/expects .eunit/
	@$(REBAR) eunit skip_deps=true

clean: rebar
	@exec $(REBAR) clean
	@-rm -rf doc

distclean: clean
	@-exec $(REBAR) delete-deps 
	@-rm -rf deps
	
rebar:
	@-rm -rf $(BLD)
	@mkdir -p $(BLD)
	@cd $(BLD) && $(GIT) clone -q git://github.com/basho/rebar.git 
	@cd $(BLD)/rebar && exec make
	@-mv $(BLD)/rebar/rebar .
	@-rm -rf $(BLD)