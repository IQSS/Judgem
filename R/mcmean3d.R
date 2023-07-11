"mcmean3d" <-
function (cube,col) apply (cube,3,apply,2,weighted.mean,col)

