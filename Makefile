.PHONY: try
try:
	touch src/test1.erl
	rebar3 compile
