#!/bin/bash
## created on 2023-11-11

#### Prepare and update Readme file

OUTPUT="$HOME/BBand_LAP/Readme.md"

(
cat "$HOME/BBand_LAP/.about.md"
cat "$HOME/BBand_LAP/.databasestats.md"
cat "$HOME/BBand_LAP/.details.md"
) > "$OUTPUT"

"$HOME/PROGRAMS/gh-md-toc" --insert --no-backup "$OUTPUT"


exit
