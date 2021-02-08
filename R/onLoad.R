actuary.theme <- function(strip.size, x.size, x.angle) {
  theme(
    text = element_text(family = "Malgun Gothic"),
    title = element_text(family = "Malgun Gothic"),
    strip.text.x = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  )
}

.onLoad = function(libname, pkgname) {
  opts = c("actuary.eps" = "1e-8",
           "actuary.theme" = actuary.theme)
  for (i in setdiff(names(opts),names(options()))) {
    eval(parse(text=paste0("options(", i ,"=", opts[i], ")")))
  }
  invisible()
}
