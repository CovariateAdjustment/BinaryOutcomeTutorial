# NOTE: INSTALL Rtools First: https://cran.r-project.org/bin/windows/Rtools/

installed_packages <- installed.packages()[, "Package"]

cran_packages <-
  c("cobalt", "config", "devtools", "digest", "dplyr", "ggplot2", "ggsci",
    "here", "kableExtra", "labelled", "pwr", "rpact", "stringr", "table1",
    "tidyr", "xfun")

github_packages <-
  c("jbetz-jhu/impart")

cran_packages_to_install <-
  setdiff(
    x = cran_packages,
    y = installed_packages
  )

if(length(cran_packages_to_install) > 0){
  install.packages(cran_packages_to_install)
}

gh_packages_df <-
  strsplit(
    x = github_packages,
    split = "/"
  ) |>
  lapply(
    FUN = function(x) data.frame(gh_user = x[1], repo = x[2])
  ) |>
  do.call(what = rbind)

github_packages_to_install <-
  setdiff(
    x = gh_packages_df$repo,
    y = installed_packages
  )

if(length(github_packages_to_install) > 0){
  github_packages_to_install_df <-
    subset(
      x = gh_packages_df,
      repo %in% github_packages_to_install
    )
  
  for(i in 1:nrow(github_packages_to_install_df)){
    devtools::install_github(
      repo = 
        with(
          data = github_packages_to_install_df[i,],
          expr = paste0(gh_user, "/", repo)
        ),
      force = TRUE
    )
  }
}