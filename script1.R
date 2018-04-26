## Bienvenue au monde de la programmation avec Git

summarize.some.cols = function(mat, col.inds) {
  out = tryCatch({
    apply(mat[,col.inds,drop=FALSE], MARGIN=2, FUN=summary)
  }, error = function(e) {
    cat("Encountered the following error:\n")
    cat("\t", e$message, "\n")
    cat("Returning NULL \n")
    return(NULL)
  })
  if (is.null(out)) return(NULL)
  colnames(out) = paste("Col", col.inds)
  return(out)
}


mat = matrix(rnorm(16), 4, 4)

summarize.some.cols(mat, 1:2)

summarize.some.cols(mat, 2:4)

