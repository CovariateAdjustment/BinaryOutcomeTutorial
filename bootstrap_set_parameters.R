bootstrap_set_parameters <-
  function(
    bootstrap_n = 10000,
    parallel = "no",
    ncpus = getOption("boot.ncpus", 1L),
    ...
  ){
    return(
      c(
        list(
          R = bootstrap_n,
          parallel = "no",
          ncpus = getOption("boot.ncpus", 1L)
        ),
        list(...)
      )
    )
  }