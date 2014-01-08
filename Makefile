ERLC_OPTS="[warnings_as_errors, warn_export_all, warn_untyped_record]"

all: compile eunit                                                   

compile:
	@ERL_COMPILER_OPTIONS=$(ERLC_OPTS) ./rebar compile

xref:
	@./rebar xref

clean:
	@./rebar clean

eunit:
	@./rebar eunit

edoc:
	@./rebar doc

.dialyzer.plt:
	touch .dialyzer.plt
	dialyzer --build_plt --plt .dialyzer.plt --apps erts kernel stdlib

dialyze: .dialyzer.plt
	dialyzer --plt .dialyzer.plt -r ebin
