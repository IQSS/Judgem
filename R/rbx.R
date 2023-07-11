"rbx" <-
function (dr,bits) {
  if (!is.null(bits)) {
    blnk <- array(0,c((dim(dr)[1]+length(bits)),dim(dr)[2],dim(dr)[3]))
    for (i in 1:dim(dr)[3]) blnk[,,i] <- rb (dr[,,i],bits)
    blnk
  } else dr
}

