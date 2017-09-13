# Copy of the lecture notes

This folder contains a backup copy of
the
[lecture notes available online](http://www.scs.stanford.edu/16wi-cs240h/slides/),
in case they are not available anymore.

To generate the slides in html format make sure to install `pandoc`:

```sh
stack install --flag pandoc:highlighting-kate pandoc
```

And then run:

```sh
pandoc slidy-url=slidy-dir --self-contained -s -t slidy -o outfile infile 
```
