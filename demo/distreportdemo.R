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

#what happens if...
plot(judgem(judgem.object=j.ob,routine="distreport",year=which(elecyears==1984)))
judgem(judgem.object=j.ob,routine="winvote",winvote=0.52,year=which(elecyears==1984))

#...everyone quit Congress in 1984? (j/k)
plot(judgem(judgem.object=j.ob,routine="distreport",new.predictors=list("INC",0),year=which(elecyears==1984)))
judgem(judgem.object=j.ob,routine="winvote",winvote=0.52,new.predictors=list("INC",0),year=which(elecyears==1984))


