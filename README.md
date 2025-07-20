# Generating/testing locally

```sh
nix develop # enter the shell with the static generator
website-engine # run the static gen name
# ^ repeat as needed when changing the static files
http-server generated # start a http server to view changes
```

## "Deploying"

You'll need to copy `generated/` to the `pages` branch and push that.

TODO:
- [ ] automate the process
- [ ] figure out how to easily be able correlate `pages` with `main` commits

Is this too annoying?
