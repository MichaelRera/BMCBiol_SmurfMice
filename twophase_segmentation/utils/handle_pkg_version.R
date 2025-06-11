load_segmented <- function(version = c("1.6", "2")) {
  version <- match.arg(version)
  if (version == "1.6") {
    detach_package(segmented)
    require(segmented, lib.loc = "~/work/side/two_phase_segmentation/renv/library/R-4.2/x86_64-pc-linux-gnu/")
  } else if (version == "2") {
    detach_package(segmented)
    require(segmented, lib.loc = "~/work/side/two_phase_segmentation/renv/library2/R-4.2/x86_64-pc-linux-gnu/")
  } else {
    stop("Error: package version not available: must be '1.6' or '2'.")
  }
}
detach_package <- function(pkg, character.only = FALSE) {
  if(!character.only) {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search()) {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}