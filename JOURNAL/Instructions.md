
# Journal instructions


## Format for entries


### Filenames should be numerical sorted

- `./YYYY/YYYY-MM-DD_0000.md`
- `./YYYY/YYYY-MM-DD.md`


### File template

```
---
tags: [ key_1 key_2 ]
---

## 2022-02-21 00:00 [USERNAME]

Text for this entry.
Use simple text, it will parsed as
markdown.
Tags and other meta data go to yaml header.

```

## Build a journal document entries files

There is a `Makefile` to automate the process.

- "`make`"      for default
- "`make all`"  the defaults (pdf and html)
- "`make pdf`"  nice!
- "`make html`" why not?
- "`make odt`"  are you kidding?

Document options are defined by:

- `Makefile`
- `.index.yaml` 
- `.pre_process.sh`
- `.columns.lua`

At least you need `pandoc` installed.

