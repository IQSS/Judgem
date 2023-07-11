"rowm" <-
function (matrx,index) {
  out <- matrix(matrx[index,],length(index))
  rownames(out) <- index
  out
}

