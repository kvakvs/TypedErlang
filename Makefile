.PHONY: try
try:
	rm ebin/*.beam
	rebar compile
