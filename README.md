# TypedErlang and C++ code generator

This project is an experiment. It attempts to enforce type specs (TODO) in 
compiled Erlang and then at Core stage produces a C++ source for Erlang module.
It uses helper library (TODO) which provides term handling functions and BIF's.

The problem to dispatch multiple C++ functions running in parallel is solvable
and is left for the future.

The perfect goal would be to get a HelloWorld native executable.
