all:
	rebar compile

deps:
	rebar get-deps

console: all
	erl -pa ebin -pa deps/*/ebin -sname folsomix -s folsomix_app test_run