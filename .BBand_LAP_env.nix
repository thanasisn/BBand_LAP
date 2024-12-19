let
  ## Nix repo for R 4.3.3
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/019f5c29c5afeb215587e17bf1ec31dc1913595b.tar.gz") {};

  ## System packages
  system_packages = builtins.attrValues {
    inherit (pkgs)
      adwaita-qt
      glibcLocales
      nix
      R;
  };

  ## R packages
  rpkgs = builtins.attrValues {
    inherit (pkgs.rPackages)
      ## for rendering
      formatR
      knitr
      rmarkdown
      ## for data analysis
      RNetCDF
      arrow
      caTools
      data_table
      dbplyr
      digest
      doMC
      dplyr
      foreach
      janitor
      lubridate
      metrica
      reticulate
      zoo
      ## for data display
      ggplot2
      ggpmisc
      ggpp
      htmlwidgets
      pander
      plotly
      scales
      tidyr
      ## general use
      Hmisc
      fs
      gdata
      lintr
      optparse
      renv
      stringr
      ;
  };

  ## R packages from github
  git_archive_pkgs = [

   (pkgs.rPackages.buildRPackage {
      name = "colorout";
      src = pkgs.fetchgit {
        url    = "https://github.com/jalvesaq/colorout/";
        rev    = "2a5f21496162ea30684d2783e3a204f4756db4e8";
        sha256 = "sha256-RglnS3QS85598qC87uciW/d64mgeavUDccdXt+GKwFM=";
      };
      propagatedBuildInputs = builtins.attrValues {
        inherit (pkgs.rPackages) ;
      };
    })

    (pkgs.rPackages.buildRPackage {
      name = "duckdb";
      version = "1.1.3-1";
      src = pkgs.fetchgit {
        url = "https://github.com/duckdb/duckdb-r/";
        rev = "c043baa69d202263750bd19a14eb04f6e735c5b6";
        sha256 = "sha256-MFX2tGp+qz6b0EH9iuU5484e3tUACFxDrJ6IAA2GNrk=";
      };
      propagatedBuildInputs = builtins.attrValues {
        inherit (pkgs.rPackages)
          DBI;
      };
    })

   ];

  ## Feed Rstudio with this environment packages
  wrapped_pkgs = pkgs.rstudioWrapper.override {
    packages = [ git_archive_pkgs rpkgs ];
  };

in

pkgs.mkShell {

  LOCALE_ARCHIVE = if pkgs.system == "x86_64-linux" then "${pkgs.glibcLocales}/lib/locale/locale-archive" else "";
  LANG           = "en_US.UTF-8";
  LC_ALL         = "en_US.UTF-8";
  LC_TIME        = "en_US.UTF-8";
  LC_MONETARY    = "en_US.UTF-8";
  LC_PAPER       = "en_US.UTF-8";
  LC_MEASUREMENT = "en_US.UTF-8";

  buildInputs = [ git_archive_pkgs rpkgs  system_packages  wrapped_pkgs ];

  shellHook = ''
    ## for rstudio GUI
    export QT_XCB_GL_INTEGRATION=none
    alias rstudio='setsid rstudio &'
    ## check for dependencies
    Rscript -e "renv::update(check=TRUE)"
    echo "Show recomendation"
  '';

}
