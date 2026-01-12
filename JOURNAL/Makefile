
.PHONY: preprocess
.DEFAULT_GOAL := all
all: pdf html

## list source files
markdowns := $(shell find -name "*.md" | grep "[0-9]\{4\}" | sort -V)


## pre pandoc run
preprocess: Readme.md
Readme.md: $(markdowns)
	./.pre_process.sh $(markdowns)


## create pdf file
TARGET_pdf = Journal.pdf
pdf: $(TARGET_pdf)
$(TARGET_pdf): Makefile Readme.md
	-pandoc                             \
	  --standalone                      \
	  --listings                        \
	  --highlight-style kate            \
	  --metadata-file   .index.yaml     \
	  --from            markdown        \
	  --to              pdf             \
	  --lua-filter      .columns.lua    \
	  --pdf-engine      xelatex         \
	  --pdf-engine-opt="-shell-escape"  \
	  --out             '$(TARGET_pdf)' \
	  Readme.md


## create html file
TARGET_html = Journal.html
html: $(TARGET_html)
$(TARGET_html): Makefile Readme.md
	-pandoc                              \
	  --standalone                       \
	  --metadata-file   .index.yaml      \
	  --mathjax                          \
	  --lua-filter      .columns.lua     \
	  --from            markdown         \
	  --to              html             \
	  --out             '$(TARGET_html)' \
	  Readme.md


