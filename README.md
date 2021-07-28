# Build

## NixOs

The `default.nix` includes justinwoo's `easy-ps.nix`.

* $> nix-shell

Using `spago`

* $> spago build

Or

* $> spago bundle-app

to produce the file `index.js` in the working directory.

* $> mv index.js public/app.js

Open `public/index.html` in browser

# Develop

Invoke editor from inside nix shell, like this

* $> nix-shell
* $> emacs src/Main.purs
