# TypedErlang and LLVM code generator

This project is an experiment. It adds extra stages to Erlang compiler which 
enforce type specs in modules and attempt to benefit from this information 
while generating LLVM IR output. The perfect goal would be to get a HelloWorld 
native executable.
