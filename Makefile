.PHONY: try
try:
	rm ebin/*.beam || true
	rebar compile
