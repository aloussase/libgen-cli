libgen-cli
==========

This is a CLI tool for downloading books from libgen.is. It is built with the
UNIX philosophy in mind, which means it works well with other tools such as awk.

Example:

```bash
libgen-cli "some book title" | awk -F'|' '$5 ~ /pdf/ {print "$1 $2"}' | xargs libgen-cli -d
```

Run `libgen-cli --help` to see other options. For example, to run interactively
you can use the _-i_ flag.

![Imgur](https://i.imgur.com/gVYpLo4.gif)

Installation
------------

The easiest way to install is using [ghcup](https://www.haskell.org/ghcup/). Once
you have that set up, use it to install [cabal](https://www.haskell.org/cabal/)
and run the following commands

```
git clone https://github.com/aloussase/libgen-cli
cd libgen-cli
cabal install
```

That's it. Alternatively, grab a binary from the [releases]() page.

Dependencies
------------

These dependencies are for debian-based systems, you'll have to adapt them to your
own:

```
libghc-curl-dev libgmp-dev
```

LICENSE
-------

MIT
