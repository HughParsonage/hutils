
# Used to stabilize set.seed because of PR17494
# and incremented r76165
set.seed <- function(seed, kind = NULL, normal.kind = NULL) {
  suppressWarnings(RNGversion("3.5.0"))
  base::set.seed(seed, kind = kind, normal.kind = normal.kind)
}
