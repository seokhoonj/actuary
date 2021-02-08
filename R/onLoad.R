.view.theme <- theme(
  text = element_text(family = "Malgun Gothic"),
  title = element_text(family = "Malgun Gothic"),
  strip.text.x = element_text(face = "bold"),
  axis.text.x = element_text(face = "bold", angle = 90),
  axis.text.y = element_text(face = "bold"),
  legend.position = "bottom"
)
.save.theme <- theme(
  text = element_text(family = "Malgun Gothic"),
  title = element_text(family = "Malgun Gothic"),
  strip.text.x = element_text(size = 17, face = "bold"),
  axis.text.x = element_text(size = 12, face = "bold", angle = 90),
  axis.text.y = element_text(size = 12, face = "bold"),
  legend.position = "bottom"
)

.onLoad = function(libname, pkgname) {
  opts = c("actuary.eps" = "1e-8",
           "view.theme" = .view.theme,
           "save.theme" = .save.theme)
  for (i in setdiff(names(opts),names(options()))) {
    eval(parse(text=paste0("options(", i ,"=", opts[i], ")")))
  }
  invisible()
}
