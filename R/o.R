"o" <-
function (covars) {
  cvtemp <- cropmiss(covars)$covars
  checker <- function(col) any(col[1]!=col)
  
  if (length(cvtemp>0)) thing <- all(apply(cvtemp,2,checker)) else thing <- T
  if (!any(cvtemp[,1]!=1)&!thing) out <- as.matrix(covars) else out <- as.matrix(cbind(rep(1,dim(covars)[1]),covars))
  if (is.null(colnames(out))) colnames(out) <- rep(" ",dim(out)[2])
  colnames(out)[1] <- "const"
  out
}

