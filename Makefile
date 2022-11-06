hooks:
	cp .hooks/* .git/hooks

format:
	dune build @fmt --auto-promote
