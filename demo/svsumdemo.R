#library(Judgem)
data(house6311)

#define uncontested for its purpose as a covariate.
unc <- function(inp) -1*(inp<0.05)+1*(inp>0.95)

#even though it's hard-coded into house6311, here it is again.
elecyears <- as.numeric(names(house6311))

#load the object.
j.ob <- judgem(modelform=VOTE~unc(VOTE)+INC,voteform=TURNOUT~1,
                    data=house6311,
                    use.last.votes=T,subset=DELSOUTH==0)


#get some seats-votes summaries.
j.ob <- judgem(judgem.object=j.ob,routine="svsum",year=which(elecyears==1944))
j.ob

judgem(judgem.object=j.ob,routine="svsum",year=which(elecyears==1990))

judgem(judgem.object=j.ob,routine="svsum",year=which(elecyears==1992))

