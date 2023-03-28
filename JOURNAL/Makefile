
.PHONY := preprocess
.DEFAULT_GOAL := all
all: pdf html 

## list source files
markdowns := $(shell find -name "*.md" | sort -V)


## pre pandoc run
preprocess: .markdowncontent
.markdowncontent: $(markdowns)
	./.pre_process.sh $(markdowns)


## create pdf file
TARGET = Journal.pdf
pdf: $(TARGET)
$(TARGET): Makefile .markdowncontent
	-pandoc                           \
	  --standalone                    \
	  --listings                      \
	  --highlight-style kate          \
	  --toc                           \
	  --metadata-file   .index.yaml   \
	  --from            markdown      \
	  --to              pdf           \
	  --lua-filter      .columns.lua  \
	  --pdf-engine      xelatex       \
	  --pdf-engine-opt  -shell-escape \
	  --out             '$@'          \
	  .markdowncontent


## create doc file
TARGET = Journal.odt
odt: $(TARGET)
$(TARGET): Makefile .markdowncontent
	-pandoc                           \
	  --standalone                    \
	  --toc                           \
	  --metadata-file   .index.yaml   \
	  --from            markdown      \
	  --to              odt           \
	  --out             '$@'          \
	  .markdowncontent


## create html file
TARGET = Journal.html
html: $(TARGET)
$(TARGET): Makefile .markdowncontent
	-pandoc                           \
	  --standalone                    \
	  --toc                           \
	  --metadata-file   .index.yaml   \
	  --mathjax                       \
	  --lua-filter      .columns.lua  \
	  --template        .easy-pandoc-templates-master/html/uikit.html \
	  --from            markdown      \
	  --to              html          \
	  --out             '$@'          \
	  .markdowncontent


