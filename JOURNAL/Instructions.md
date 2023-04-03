
# Journal instructions


## Format for entries


### Filenames should be numerical sorted

- `./YYYY/YYYYMMDD_0000.md`
- `./YYYY/YYYYMMDD.md`


### File template

```
## 2022-02-21 00:00 [USERNAME]

[//]: # (Keywords: #key_1, #key_2)

Text for this entry.
Use simple text, it will parsed as
markdown.

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

