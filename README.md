# Generating/testing locally

```sh
nix develop # enter the shell with the static generator
website-engine # run the static gen name
# ^ repeat as needed when changing the static files
http-server docs # start a http server to view changes
```

## Deploying

Pushes to `main` will automatically get mirrored to github to https://github.com/eviefp/website-mirror and deployed via github pages to https://group-meowing.ro

> [!WARNING]
> The generated website HTML is currently stored in the repository under `docs`.
> When submitting your PR, make sure to include the generated HTML along with your other changes.

## Idei de articole:
- tips and tricks pentru feminizare?
