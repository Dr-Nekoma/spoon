run:
	rebar3 escriptize
	./_build/default/bin/spoon

repl:
	rebar3 shell

compile:
	rebar3 compile

docs:
	rebar3 edoc
