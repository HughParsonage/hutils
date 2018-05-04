if (getRversion() >= "3.6") {
  # dev changes to ALTREP mean release is broken
  if (!requireNamespace("data.table", quietly = TRUE)) {
    install.packages("data.table")
  }
  data.table::update.dev.pkg()
]
