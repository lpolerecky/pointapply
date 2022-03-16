.onUnload <- function (libpath) {
  library.dynam.unload("pointapply", libpath)
}
