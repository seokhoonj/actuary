
# basic functions ---------------------------------------------------------

unilen <- function(x) length(unique(x))
getsiz <- function(x) format(object.size(x), unit = "Mb")
lsjoin <- function(..., by, all = FALSE, all.x = all, all.y = all, sort = TRUE) {
  l <- list(...)
  Reduce(function(...) merge(..., by = by, all = all, all.x = all.x, all.y = all.y, sort = sort), l)
}
strati <- function(data, var, size, method, verbose) {
  var <- vapply(substitute(var), deparse, FUN.VALUE = "character")
  var <- names(data)[match(var, names(data), 0L)]
  if (missing(method)) method <- "srswor"
  if (missing(verbose)) verbose <- TRUE
  grp <- data[, .(n = .N), by = var]
  if (size < 1) {
    grp[, s := ceiling(n*size)]
    grp[, p := s/n]
  } else {
    low <- min(grp$n)
    grp[, s := min(low, size)]
    grp[, p := s/n]
  }
  if (verbose) print(grp)
  ss <- strata(data = data, stratanames = var, size = grp$s, method = method)
  getdata(data, ss)
}
