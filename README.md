libgen-cli
==========

This is a CLI tool for downloading books from libgen.is. It is built with the
UNIX philosophy in mind, which means it works well with other tools such as awk.

Example:

```bash
libgen-cli "some book title" | awk -F'|' '$5 ~ /pdf/ {print "$1 $2"}' | xargs libgen-cli -d
```

Run libgen-cli --help to see other options. For example, to run interactively
you can use the -i flag.

Dependencies
------------

These dependencies are for debian-based systems, you'll have to adapt them to your
own:

    libghc-curl-dev
    libgmp-dev

LICENSE
-------

MIT
