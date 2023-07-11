\name{judgem}
\alias{judgem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{judgem}
\description{
Two-party election modelling and simulation for R.
}
\usage{
judgem(modelform = ~1, voteform = NULL, same.districts = NULL, data,
uncons.method = "default", uncons.low = 0.05, uncons.low.new = 0.1,
uncons.high = 0.95, uncons.high.new = 0.9, use.last.votes = T, simnum =
102, simdepth = simnum, prelim = T, weight = "constant",

routine = NULL, year = NULL, judgem.object = NULL, predict = F,
new.predictors = NULL, extras = NULL,

Evar = F, distselect = NULL, plotfile=NULL,
voterange = c(0.45, 0.55),
voteshares = seq(min(voterange), max(voterange), length.out =
round((max(voterange) - min(voterange)) * 100) + 1),

voteorshift = NULL, winvote = 0.5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modelform}{The model formula; left side contains the vote
    outcome as one party's share of the total vote, right side contains predictors.}
  \item{voteform}{The vote distribution formula. Left side contains one
    or two columns with actual and/or eligible turnout, right side
    contains the number of seats per district.}
  \item{same.districts}{A vector noting whether an election had the same
  district map as the previous one.}
  \item{data}{A list containing all the elections to be modelled in
    the system. Each item of the list should be an election variable;
    each election variable should be a data frame containing the
    information for each district.}
  \item{uncons.method}{A choice between "nochange","default","impute"
    and "delete" for the way to deal with uncontested districts.}
  \item{uncons.low}{The value below which a district should be
    considered uncontested by party 1.}
  \item{uncons.low.new}{If method is "default", districts uncontested by
    party 1 will be considered to have this amount of the vote share for the
    purposes of the analysis.}
  \item{uncons.high}{The value above which a district should be
    considered uncontested by party 2.}
  \item{uncons.high.new}{If method is "default", districts uncontested by
    party 2 will be considered to have this amount of the vote share for the
    purposes of the analysis.}
  \item{use.last.votes}{T/F whether a previous election's votes should
    be automatically included as a current election's predictor. This is
  overridden if the district maps are not identical due to
  same.districts or an unequal number of districts between two elections.}
  \item{simnum,simdepth}{The number of elections to be simulated for
    analysis.}
  \item{prelim}{T/F whether a preliminary analysis should be conducted
    at the first stage.}
  \item{weight}{A choice between "constant","elgvotes","actvotes" and
    "seats" for the type of weights used in the linear model of each election.}
  \item{routine}{The routine to run in order to obtain desired
    quantities of interest.}
  \item{year}{The election for which the routine will be run.}
  \item{judgem.object}{A pre-existing object from which judgem() will
    obtain the model.}
  \item{predict}{T/F whether prediction, rather than analysis, should be
  executed.}
  \item{new.predictors}{A list of variable names and their replacement
    values for counterfactual analysis or prediction.}
  \item{extras}{Extra districts to be added to the system after analysis
  but before reporting. Typically uncontested districts whose results
  are unlikely to change.}
  \item{Evar}{T/F whether judgem() should report errors in expected values of
    quantities rather than in the quantities themselves.}
  \item{distselect}{Districts to examine in the analysis. This is
    dependent on the routine.}
  \item{plotfile}{...}
  \item{voterange}{A low and high vote share between which analysis will
  be conducted.}
  \item{voteshares}{A selection of vote shares, or shifts from the
    observed vote, over which analyses will be run.}
  \item{voteorshift}{Specifies whether voteshares refers to vote shares
    or vote shifts.}
  \item{winvote}{The parameter needed for routine winvote.}
  \item{\dots}{Extra commands to be used with model.frame(), a routine
    used in assembling the linear model for each year.}
}
\value{

  judgem() always returns an object of class judgem.

  If no pre-existing object is given (i.e. judgem.object=NULL) judgem()
  will attempt to use the model formula and data provided to assemble
  a judgem object.

  Otherwise, judgem() will perform the routine requested in option
  "routine", and the function will return a judgem object with
  judgem.object\$output containing the out from the chosen routine.

}
\references{See Judgem website for more information.}
\author{Gary King & Andrew C. Thomas}
\examples{

#Demo files are available through the following commands:
demo(seatsdemo)
demo(probdemo)
demo(svsumdemo)
demo(distreportdemo)

data(house6311)
#columns: STATE,DIST,INC,VOTE,TURNOUT,DELSOUTH

#operators:
unc <- function(inp) -1*(inp<0.05)+1*(inp>0.95)

j.ob <- judgem(modelform=VOTE~unc(VOTE)+INC,voteform=TURNOUT~1,
               data=house6311,
               use.last.votes=T,subset=DELSOUTH==0)

summary(j.ob)
#summary(j.ob,which(house6311$years==1942))

years <- seq (1896,1992,by=2)

j.ob <- judgem(routine="distreport",judgem.object=j.ob,year=which(years==1962),new.predictors=list("INC",0),voterange=c(0.1,0.9))
j.ob

#seats-votes curve
j.ob <- judgem(routine="seats",jud=j.ob,year=which(years==1986),voterange=c(0.2,0.8))
plot(j.ob)

}
\keyword{file}
