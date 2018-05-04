if (getRversion() >= "3.6") {
  # dev changes to ALTREP mean release is broken
  if (!requireNamespace("data.table", quietly = TRUE) && 
      package_version("data.table") >= "1.11.0") {
    install.packages("data.table")
  }
  data.table::update.dev.pkg()
}
