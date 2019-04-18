A Purescript Hello-World program for NixOS using [justinwoo](https://github.com/justinwoo)'s [psc-package2nix](https://github.com/justinwoo/psc-package2nix) and [easy-purescript-nix](https://github.com/justinwoo/easy-purescript-nix).

# Build

## On NixOS

```shell
$ nix-shell
$ns> purp build
$ns> purp bundle -o public/app.js
```

## On other Linux

Requires

* psc-package
* purs

```shell
$ psc-package build
$ purs bundle './output/**/*.js' -m Main --main Main -o public/app.js
```

# Run

```
$ node public/app.js
```

or navigate browser to `public/index.html`.
