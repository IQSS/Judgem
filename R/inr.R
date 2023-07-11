"inr" <-
function (r1) {
  d1 <- function (r) if (r == 1) 0 else r
  sapply (r1,d1)
}

