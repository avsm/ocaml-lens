build:
	dune build

test:
	dune runtest

doc:
	dune build @doc

clean:
	dune clean

all-supported-ocaml-versions:
	dune build --workspace dune-workspace.dev @install @runtest

.PHONY: build test doc clean all-supported-ocaml-versions
