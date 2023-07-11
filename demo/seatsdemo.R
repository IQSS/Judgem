#library(Judgem)
data(house6311)

#define uncontested for its purpose as a covariate.
unc <- function(inp) -1*(inp<0.05)+1*(inp>0.95)
#zero <- function(a) 0*a

#even though it's hard-coded into house6311, here it is again.
elecyears <- as.numeric(names(house6311))

#load the object.
j.ob <- judgem(modelform=VOTE~unc(VOTE)+INC,voteform=TURNOUT~1,
                    data=house6311,same.districts=(elecyears%%10!=2),
                    use.last.votes=T,subset=DELSOUTH==0,prelim=FALSE)

#run some seats routines.
j.ob.1 <- judgem(judgem.object=j.ob,routine="seats",voterange=c(0.3,0.7),year=which(elecyears==1904))
plot(j.ob.1)
#plot(j.ob.1,filename="1904house")
