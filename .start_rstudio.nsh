#!/usr/bin/env nix-shell
#### Drop in this project environment
#!nix-shell /home/athan/BBand_LAP/.BBand_LAP_env.nix -i bash

## Start rstudio for this project
export QT_XCB_GL_INTEGRATION=none
export GTK_THEME=Adwaita:dark
setsid env GTK_THEME=Adwaita:dark rstudio &
