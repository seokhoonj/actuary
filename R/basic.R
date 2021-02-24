
# basic functions ---------------------------------------------------------

unilen <- function(x) length(unique(x))
get_pattern <- function(pattern, x) {
  r <- regexpr(pattern, x, perl = TRUE)
  z <- rep(NA, length(x))
  z[r != -1] <- regmatches(x, r)
  z
}
get_size <- function(x, unit = "Mb") {
  env <- ls(envir = parent.frame())
  var <- vapply(substitute(x), deparse, FUN.VALUE = "character")
  var <- gsub("\"", "", var)
  if (any(var == "ls")) {
    obj <- env
  } else {
    obj <- env[match(var, env, 0L)]
  }
  if (!length(obj)) stop("No object's found.")
  sz <- sapply(obj, function(x) object.size(get(x)))
  szs <- c(sz, sum(sz))
  m <- switch(tolower(unit), kb = 1, mb = 2, gb = 3)
  z <- data.table(obj = c(names(sz), "total"), size = round(szs / 1024^m, 3), unit = unit)
  return(z)
}
get_mode <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
get_info <- function(x) {
  col <- names(x)
  class <- sapply(x, class)
  N <- nrow(x)
  n <- sapply(x, function(x) sum(!is.na(x)))
  missing <- sapply(x, function(x) sum(is.na(x)))
  distinct <- sapply(x, unilen)
  mode <- sapply(x, get_mode)
  data.table(col, class, n, missing, distinct, prop = missing / N, mode)
}
get_prop <- function(data, id_var, uniq_var, sum_var, multiple = 1, round = 5) {
  id_var <- vapply(substitute(id_var), deparse, FUN.VALUE = "character")
  id_var <- names(data)[match(id_var, names(data), 0L)]
  if (!missing(uniq_var)) {
    uniq_var <- deparse(substitute(uniq_var))
    if (!missing(sum_var)) {
      sum_var <- deparse(substitute(sum_var))
      z <- data[, .(n = .N, uniq_n = uniqueN(get(uniq_var)), sum = sum(get(sum_var))), by = id_var]
      z[, n_prop := round(n / sum(n) * multiple, round)]
      z[, uniq_n_prop := round(uniq_n / sum(uniq_n) * multiple, round)]
      z[, sum_prop := round(sum / sum(sum) * multiple, round)]
    } else {
      z <- data[, .(n = .N, uniq_n = uniqueN(get(uniq_var))), by = id_var]
      z[, n_prop := round(n / sum(n) * multiple, round)]
      z[, uniq_n_prop := round(uniq_n / sum(uniq_n) * multiple, round)]
    }
  } else {
    if (!missing(sum_var)) {
      sum_var <- deparse(substitute(sum_var))
      z <- data[, .(n = .N, sum = sum(get(sum_var))), by = id_var]
      z[, n_prop := round(n / sum(n) * multiple, round)]
      z[, sum_prop := round(sum / sum(sum) * multiple, round)]
    } else {
      z <- data[, .(n = .N), by = id_var]
      z[, prop := round(n / sum(n) * multiple, round)]
    }
  }
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
cut_age <- function(data, var, interval, right = FALSE) {
  age <- eval(substitute(var), data)
  min <- floor(min(age) / interval) * interval
  max <- ceiling(max(age) / interval) * interval
  if (max(age) == max) max <- ceiling(max(age) / interval + 1) * interval
  cut(age, breaks = seq(min, max, interval), right = right)
}
cut_threshold <- function(x) {
  cut(abs(x),
      breaks = c(0, .05, .1, .2, .3, .5, 1., max(abs(x))+1e-8),
      labels = c("< 0.05", "< 0.10", "< 0.20", "< 0.30", "< 0.50", "< 1.0", ">= 1.0"),
      right = FALSE)
}
calc_rp <- function(f, r, w, d, qs, qs_r, denom) {
  f * r * (1-w) * (1-d) * qs * qs_r / denom
}
calc_rate <- function(rp, f, w, d, qs, qs_r, denom) {
  rp / (f * (1-w) * (1-d) * qs * qs_r / denom)
}
theme_view <- function(x.angle = 0) {
  theme(
    text = element_text(family = "Malgun Gothic"),
    title = element_text(family = "Malgun Gothic"),
    strip.text.x = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold", angle = x.angle),
    axis.text.y = element_text(face = "bold"),
    legend.position = "bottom"
  )
}
theme_save <- function(x.angle = 0) {
  theme(
    text = element_text(family = "Malgun Gothic"),
    title = element_text(family = "Malgun Gothic"),
    strip.text.x = element_text(size = 17, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold", angle = x.angle),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )
}
to_a1_col <- function(x) {
  tbl <- seq_along(LETTERS)
  names(tbl) <- LETTERS

  if (x <= 26) {
    return(paste0(names(tbl[x]), collapse = ""))
  }

  quo_vec <- vector(mode = "integer")
  rem_vec <- vector(mode = "integer")
  i <- 1L
  while (x > 26) {
    quo_vec[i] <- quo <- x %/% 26
    rem_vec[i] <- rem <- x %% 26
    x <- quo
    i <- i+1L
  }
  quo <- quo_vec[length(quo_vec)]
  z <- c(quo, rev(rem_vec))
  for (i in rev(2:length(z))) {
    if (z[i] == 0) {
      z[i] <- 26L
      z[i-1] <- z[i-1] - 1L
    }
  }
  return(paste0(names(tbl[z]), collapse = ""))
}
to_r1c1_col <- function(x) {
  tbl <- seq_along(LETTERS)
  names(tbl) <- LETTERS

  spl <- unlist(strsplit(x, split = "", perl = TRUE))
  num <- tbl[spl]
  dig <- rev(seq_along(num)-1)
  return(sum(num * 26L^dig))
}
mv_cell <- function(cell, r, c) {
  tbl <- seq_along(LETTERS)
  names(tbl) <- LETTERS

  row <- as.integer(get_pattern("[0-9]+", cell))
  row <- row + r

  col <- get_pattern("[A-Z]+", cell)
  col <- to_r1c1_col(col)
  col <- col + c

  paste0(to_a1_col(col), row)
}
di_cell <- function(x, y) {
  r1 <- as.integer(get_pattern("[0-9]+", x))
  r2 <- as.integer(get_pattern("[0-9]+", y))
  c1 <- to_r1c1_col(get_pattern("[A-Z]+", x))
  c2 <- to_r1c1_col(get_pattern("[A-Z]+", y))
  r_d <- r2 - r1
  c_d <- c2 - c1
  return(c(r_d, c_d))
}
get_rp_table <- function(path, sheet, m_cell_s, m_cell_e, f_cell_s, f_cell_e, cedent) {
  rp_table_m <- read_excel(path = path,
                           sheet = sheet,
                           range = anchored(m_cell_s, dim = di_cell(m_cell_s, m_cell_e)+1L),
                           col_names = TRUE)
  setDT(rp_table_m)
  rp_table_m <- rp_table_m[complete.cases(rp_table_m)]
  rp_pre = data.table(cedent = cedent, age = (1:nrow(rp_table_m))-1L, gender = "M")
  rp_table_m <- cbind(rp_pre, rp_table_m)
  rp_table_m <- melt(data = rp_table_m, id.var = c("cedent", "age", "gender"), variable.name = "risk", value.name = "rate")

  rp_table_f = read_excel(path = path,
                          sheet = sheet,
                          range = anchored(f_cell_s, dim = di_cell(f_cell_s, f_cell_e)+1L),
                          col_names = TRUE)
  setDT(rp_table_f)
  rp_table_f <- rp_table_f[complete.cases(rp_table_f)]
  rp_pre = data.table(cedent = cedent, age = (1:nrow(rp_table_f))-1L, gender = "F")
  rp_table_f <- cbind(rp_pre, rp_table_f)
  rp_table_f <- melt(data = rp_table_f, id.var = c("cedent", "age", "gender"), variable.name = "risk", value.name = "rate")

  rp_table <- rbind(rp_table_m, rp_table_f)
  setorder(rp_table, risk, gender, age)
  return(rp_table)
}
