# NOTE: INSTALL Rtools First: https://cran.r-project.org/bin/windows/Rtools/

installed_packages <- installed.packages()[, "Package"]

cran_packages <-
  c("cobalt", "devtools", "digest", "dplyr", "ggplot2", "ggsci", "here",
    "kableExtra", "labelled", "rpact", "stringr", "table1", "tidyr", "xfun")

github_packages <-
  c("jbetz-jhu/impart")

### Install Packages from CRAN #################################################
packages_to_install <-
  setdiff(
    x = cran_packages,
    y = installed_packages
  )

if(length(packages_to_install)){
  install.packages(packages_to_install)
}

### Install Packages from GitHub ###############################################
github_packages <-
  strsplit(
    x = github_packages,
    split = "/"
  ) |>
  lapply(
    FUN = function(x) data.frame(user = x[1], package = x[2])
  ) |>
  do.call(what = rbind)

github_packages_to_install <-
  setdiff(
    x = github_packages$package,
    y = installed_packages
  )

if(length(github_packages_to_install) > 0){
  gh_to_install <-
    subset(
      github_packages, package %in% github_packages_to_install
    )
  
  for(i in 1:nrow(gh_to_install)){
    devtools::install_github(
      repo = with(gh_to_install[i,], paste0(user, "/", package))
    )
  }
}