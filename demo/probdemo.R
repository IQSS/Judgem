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


#run some "prob" routines.
j.ob.1 <- judgem(judgem.object=j.ob,routine="prob",voterange=c(0.5,1),voteshares=45:55/100,year=which(elecyears==1944))
plot(j.ob.1)
#plot(j.ob.1,filename="1944house")
