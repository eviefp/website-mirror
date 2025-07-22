# CI

Problems:

- `nix develop` takes a long time for people to run
- it's easy to forget to generate the artifacts via `website-engine`
- it's hard to view changes as a reviewer, especially if it's to the html templates/css

## 1: nix cache

We could add a cache/push the nix build artifacts *somewhere* such that it's faster to run `nix develop`.

The main issue with this solution is, it doesn't touch any of the other solutions.

## 2: run some GH actions on the mirror

This could help a little bit, but by the time these actions run, it's already too late and we need to revert commits/push fixes, rather than pre-emptively review/ask for changes.

## 3: run actions on codeberg

Apparently we could self-host [Forgejo](https://forgejo.org/] to have an action runner for our stuff. If we do decide to have a self-hosted server, we could use it to run builds, checks, and even host virtual paths for PRs so we could preview the changes (although this may require more work).

The server could also be used as a nix cache.

This feels like it would solve most if not all the problems, but it's also the most expensive/complicated to get going.

That being said, it shouldn't be too hard to get a cheap NixOS VM running. There's Forgejo modules to easily set it up.
