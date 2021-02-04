
# basic functions ---------------------------------------------------------

unilen <- function(x) length(unique(x))
get_size <- function(x) format(object.size(x), unit = "Mb")
get_mode <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
get_summary <- function(x) {
  col <- names(x)
  N <- nrow(x)
  n <- sapply(x, function(x) sum(!is.na(x)))
  missing <- sapply(x, function(x) sum(is.na(x)))
  distinct <- sapply(x, unilen)
  mode <- sapply(x, get_mode)
  data.table(col, n, missing, distinct, prop = missing / N, mode)
}
rm_unique <- function(x) {
  uniqx <- sapply(x, unilen)
  x[, `:=`(names(uniqx[uniqx==1L]), NULL)]
}
glue_code <- function(x) paste0(x[!is.na(x)], collapse = "|")
rcast <- function(x, id_var, value_var, prefix = "var", glue = TRUE) {
  id_var <- vapply(substitute(id_var), deparse, FUN.VALUE = "character")
  id_var <- names(x)[match(id_var, names(x), 0L)]
  value_var <- deparse(substitute(value_var))
  x[, `:=`(rank, rank(get(x), ties.method = "first")), by = id_var]
  form <- formula(paste(paste(id_var, collapse = " + "), " ~ rank"))
  z <- dcast.data.table(x, formula = form, value.var = value_var)
  dcast_var <- paste0(prefix, str_pad(names(z)[-match(id_var, names(z), 0L)],
                                      width = nchar(length(names(z)) - length(id_var)),
                                      pad = "0"))
  vars <- c(id_var, dcast_var)
  setnames(z, vars)
  if (glue) {
    z <- data.table(z[, ..id_var], var = apply(z[, ..dcast_var], 1, glue_code))
    setnames(z, c(id_var, prefix))
  }
  x[, rank := NULL]; gc()
  return(z)
}
join <- function(..., by, all = FALSE, all.x = all, all.y = all, sort = TRUE) {
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
