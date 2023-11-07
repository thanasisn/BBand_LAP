
.PHONY := preprocess
.DEFAULT_GOAL := all
all: pdf html

## list source files
markdowns := $(shell find -name "*.md" | grep "[0-9]\{4\}" | sort -V)


## pre pandoc run
preprocess: Readme.md
Readme.md: $(markdowns)
	./.pre_process.sh $(markdowns)


## create pdf file
TARGET = Journal.pdf
pdf: $(TARGET)
$(TARGET): Makefile Readme.md
	-pandoc                           \
	  --standalone                    \
	  --listings                      \
	  --highlight-style kate          \
	  --metadata-file   .index.yaml   \
	  --from            markdown      \
	  --to              pdf           \
	  --lua-filter      .columns.lua  \
	  --pdf-engine      xelatex       \
	  --pdf-engine-opt  -shell-escape \
	  --out             '$@'          \
	  Readme.md

	  #--toc                           \

## create doc file
TARGET = Journal.odt
odt: $(TARGET)
$(TARGET): Makefile Readme.md
	-pandoc                           \
	  --standalone                    \
	  --metadata-file   .index.yaml   \
	  --from            markdown      \
	  --to              odt           \
	  --out             '$@'          \
	  Readme.md

	  #--toc                           \

## create html file
TARGET = Journal.html
html: $(TARGET)
$(TARGET): Makefile Readme.md
	-pandoc                           \
	  --standalone                    \
	  --metadata-file   .index.yaml   \
	  --mathjax                       \
	  --lua-filter      .columns.lua  \
	  --template        .easy-pandoc-templates-master/html/uikit.html \
	  --from            markdown      \
	  --to              html          \
	  --out             '$@'          \
	  Readme.md

	  # --toc                           \

