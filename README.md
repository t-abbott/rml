# RML

An ML-style language with refinement types.

## Development

### Commit hooks

Set up pre-commit hooks with `make hooks`.

The commit hook scripts are stored in the `.hooks/` folder
which git doesn't read from,
so `make hooks` copies the contents of `.hooks/` to `.git/hooks/`.
If you change any files in `.hooks/` you'll need to run `make hooks` again.

## TODO

- only allow annotations on variables
- add `//`-style comments
