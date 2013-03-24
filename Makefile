REBAR = rebar

.PHONY: all xref erl test clean doc

all: erl xref test

erl:
	$(REBAR) compile

xref:
	$(REBAR) xref

test:
	@mkdir -p .eunit
	$(REBAR) eunit

clean:
	@$(REBAR) clean
	@-rm -rvf deps ebin doc .eunit
	@-rm c_src/*.o
	@-rm priv/*.so
	@-rm */*~

doc:
	$(REBAR) doc
