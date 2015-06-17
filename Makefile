REBAR = rebar
NAME=jsonx
PLT_NAME=$(NAME).plt

.PHONY: all xref erl test clean doc

all: clean erl xref test

erl:
	$(REBAR) compile

xref:
	$(REBAR) xref

test:
	$(REBAR) eunit

clean:
	@$(REBAR) clean
	@-rm -rvf deps ebin doc .eunit
	@-rm README.html
	@-rm c_src/*.o
	@-rm priv/*.so
	@-rm */*~

doc:
	$(REBAR) doc


$(PLT_NAME): 
	dialyzer --build_plt --apps erts kernel stdlib crypto mnesia sasl common_test eunit --output_plt $(PLT_NAME)

dialyze: erl $(PLT_NAME)
	dialyzer --check_plt --plt $(PLT_NAME) -c .
	dialyzer -c ebin
