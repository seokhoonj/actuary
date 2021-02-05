
# basic functions ---------------------------------------------------------

unilen <- function(x) length(unique(x))
get_size <- function(x, unit = "Mb") format(object.size(x), unit = unit)
get_mode <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
get_summ <- function(x) {
  col <- names(x)
  N <- nrow(x)
  n <- sapply(x, function(x) sum(!is.na(x)))
  missing <- sapply(x, function(x) sum(is.na(x)))
  distinct <- sapply(x, unilen)
  mode <- sapply(x, get_mode)
  data.table(col, n, missing, distinct, prop = missing / N, mode)
}
get_prop <- function(data, id_var, uniq_var, multiple = 1, round = 5) {
  id_var <- vapply(substitute(id_var), deparse, FUN.VALUE = "character")
  id_var <- names(data)[match(id_var, names(data), 0L)]
  uniq_var <- deparse(substitute(uniq_var))
  z <- data[, .(n = .N, uniq_n = uniqueN(get(uniq_var))), by = id_var]
  z[, n_prop := round(n / sum(n) * multiple, round)]
  z[, uniq_n_prop := round(uniq_n / sum(uniq_n) * multiple, round)]
  setorderv(z, id_var)
  print(z)
  invisible(z)
}
rm_ws <- function(x) {
  col <- names(sapply(x, class)[sapply(x, class) == "character"])
  x[, (col) := lapply(.SD, trimws), .SDcols = col]
}
rm_uniq <- function(x) {
  uniqx <- sapply(x, unilen)
  x[, `:=`(names(uniqx[uniqx==1L]), NULL)]
}
glue_code <- function(x) paste0(x[!is.na(x)], collapse = "|")
rcast <- function(data, id_var, value_var, prefix, glue = FALSE) {
  id_var <- vapply(substitute(id_var), deparse, FUN.VALUE = "character")
  id_var <- names(data)[match(id_var, names(data), 0L)]
  value_var <- deparse(substitute(value_var))
  if (missing(prefix)) prefix <- value_var
  data[, `:=`(rank, rank(get(value_var), ties.method = "first")), by = id_var]
  form <- formula(paste(paste(id_var, collapse = " + "), " ~ rank"))
  z <- dcast.data.table(data, formula = form, value.var = value_var)
  dcast_var <- paste0(prefix, str_pad(names(z)[-match(id_var, names(z), 0L)],
                                      width = nchar(length(names(z)) - length(id_var)),
                                      pad = "0"))
  vars <- c(id_var, dcast_var)
  setnames(z, vars)
  if (glue) {
    z <- data.table(z[, ..id_var], var = apply(z[, ..dcast_var], 1, glue_code))
    setnames(z, c(id_var, prefix))
  }
  data[, rank := NULL]; gc()
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
  grp[, g := 1:nrow(grp)]
  if (size < 1) {
    grp[, s := ceiling(n*size)]
    grp[, p := s/n]
  } else {
    low <- min(grp$n)
    grp[, s := min(low, size)]
    grp[, p := s/n]
  }
  if (verbose) {
    cat("Sampling proportion:",
        paste0(round(sum(grp$s) / sum(grp$n) * 100, 3), " %\n"))
    print(grp)
  }
  data[grp, on = var, g := g]
  g <- data[["g"]]
  s <- grp[["s"]]
  v <- unlist(lapply(1:nrow(grp), function(x) sample(which(g == x), s[x])))
  data[, g := NULL]
  data[v]
  # ss <- strata(data = data, stratanames = var, size = grp$s, method = method)
  # getdata(data, ss)
}
