
.onLoad = function(libname, pkgname) {
  opts = c("actuary.eps" = "1e-8")
  for (i in setdiff(names(opts),names(options()))) {
    eval(parse(text=paste0("options(", i ,"=", opts[i], ")")))
  }
  invisible()
}
