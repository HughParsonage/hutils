
check_pkg_dependencies <- function(pkg_root = NULL,
                                   on_success = NULL,
                                   on_fail = stop,
                                   reader = readLines) {
  
  if (!exists("startsWith", mode = "function")) {
    stop("`startsWith` not present, possibly due to insufficient R version.\n\t", 
         "Current:\t", getRversion(), "\n\t",
         "Required:\t", "3.3.0", ".")
  }
  if (!requireNamespace("rcheology", quietly = TRUE)) {
    stop("package:rcheology not installed, but required. ",
         "Run `install.packages('rcheology')` and try again.")
  }
  if (is.null(pkg_root)) {
    if (file.exists("DESCRIPTION")) {
      pkg_root <- "."
    } else {
      
    }
  } else {
    if (length(pkg_root) > 1L) {
      return(sapply(pkg_root,
                    check_pkg_dependencies,
                    on_success = on_success, 
                    on_fail = on_fail,
                    reader = reader))
    }
    
    if (!file.exists(file.path(pkg_root, "DESCRIPTION"))) {
      return(on_fail("Unable to find DESCRIPTION"))
    }
  }
  
  
  strSplitter <- function(x) {
    if (!is.character(x)) {
      warning("strSplitter provided with non-character object.")
      return("")
    }
    unlist(strsplit(x, split = "(?<=\\()\\b", perl = TRUE))
  }
  
  rFunsDefined <-
    lapply(dir(file.path(pkg_root, "R"),
               pattern = "\\.R",
               full.names = TRUE),
           reader) %>%
    lapply(trimws) %>%
    lapply(function(x) x[!startsWith(x, "#")]) %>%
    lapply(function(x) {
      sub("^([A-Za-z0-9_\\.]+)\\s*([=]|<-)\\s*function\\(.*$", 
          "\\1",
          grep("^([A-Za-z0-9_\\.]+)\\s*([=]|<-)\\s*function\\(.*$", 
               x,
               perl = FALSE,
               value = TRUE))
    }) %>%
    unlist %>% 
    unique
  
  RfunsUsed <- 
    lapply(dir(file.path(pkg_root, "R"), 
               pattern = "\\.R$",
               full.names = TRUE),
           function(x) {
             setdiff(all.names(parse(x)),
                     all.vars(parse(x)))
           }) %>%
    unlist %>%
    unique

  
  stated_R_dep <- 
    desc::desc_get_deps(file = pkg_root) %>%
    as.data.table %>%
    .[type == "Depends" & package == "R"] %>%
    .[["version"]]
  
  if (length(stated_R_dep)) {
    if (length(stated_R_dep) != 1L) {
      warning("Multiple stated R dependencies, using first...")
      stated_R_dep <- stated_R_dep[1]
    }
  } else {
    stated_R_dep <- ">= 1.0.0"
  }
  
  # ">= 3.9.9  => ">=" "3.9.9"
  stated_R_dep <- strsplit(stated_R_dep, split = " ")[[1L]]
  R_dep_comparator <- stated_R_dep[1]
  stated_R_dep <- stated_R_dep[2]

  
  Rversion <- NULL
  
  base_funs_used_by_version_introduced <- 
    rcheology::rcheology %>%
    as.data.table %>%
    .[package == "base"] %>%
    unique(by = "name") %>%
    .[name %chin% RfunsUsed] %>%
    setkey(Rversion)
  
  
  base_funs_introduced_later_than_stated_dep <-
    switch(R_dep_comparator,
           "<" = base_funs_used_by_version_introduced[Rversion <= stated_R_dep],
           "<=" = base_funs_used_by_version_introduced[Rversion < stated_R_dep],
           ">=" = base_funs_used_by_version_introduced[Rversion > stated_R_dep],
           ">" = base_funs_used_by_version_introduced[Rversion >= stated_R_dep],
           stop("Unexpected R dependency: ", stated_R_dep, "."))
           
  
  if (nrow(base_funs_introduced_later_than_stated_dep)) {
    cat(basename(pkg_root), ":\n")
    print(base_funs_introduced_later_than_stated_dep)
    return(on_fail("Base function not respecting dependency:\n\t",
                   base_funs_introduced_later_than_stated_dep[1][[1L]], "\t", 
                   base_funs_introduced_later_than_stated_dep[1][[2L]]))
  }
  on_success
}

skip_only_on_cran <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true") ||
      identical(Sys.getenv("TRAVIS"), "true") ||
      identical(Sys.getenv("APPVEYOR"), "True") ||
      nzchar(Sys.getenv("CODECOV_TOKEN"))) {
    return(invisible(TRUE))
  }
  testthat::skip("On CRAN")
}

