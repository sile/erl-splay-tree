all: compile xref eunit                                                   

compile:
	@rebar compile

xref:
	@rebar xref

clean:
	@rebar clean

eunit:
	@rebar eunit

edoc:
	@rebar doc

dialyzer-init:
	dialyzer --build_plt --apps erts kernel -r ebin

dialyzer:
	dialyzer --src -r src/