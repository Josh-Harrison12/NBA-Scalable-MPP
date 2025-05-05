library(hoopR)
library(rio)
library(knitr)
library(dplyr)
library(ggplot2)
library(gclus)
library(GGally)
library(rworldmap)
library(ggmosaic)
library(VIM)
library(gridExtra)
library(REAT)
library(jsonlite)

####Loading Data:
nba_player_box <- load_nba_player_box(2022:2023)
names(nba_player_box)
nba_pbp <- load_nba_pbp(seasons = 2022:2023)
unique(nba_pbp$type_text)

aggr(nba_pbp,prop=FALSE,combined=TRUE,numbers=TRUE,sortVars=TRUE,sortCombs=TRUE)

####Rescaling Coordinates

#Stretching Y Coordinate
ScaleFac <- 19/13.75
test <- data.frame(nba_pbp$type_text,nba_pbp$coordinate_x_raw,nba_pbp$coordinate_x,nba_pbp$coordinate_y_raw,nba_pbp$coordinate_y)

nba_pbp$YCoord <- round(nba_pbp$coordinate_y_raw*ScaleFac,2)
nba_pbp$XCoord <- nba_pbp$coordinate_x_raw

#replacing incorrect coordinates
nba_pbp$XCoord[nba_pbp$XCoord=="-214748340"] <- 25
nba_pbp$YCoord[nba_pbp$YCoord=="-296743195.27"] <- 47


### Zones

nba_pbp$zone <- rep(0,dim(nba_pbp)[1])

Zone3 <- which((((3 <= as.numeric(unlist(nba_pbp[,"XCoord"])))&(as.numeric(unlist(nba_pbp[,"XCoord"]))<=47)) & (-10<=as.numeric(unlist(nba_pbp[,"YCoord"]))& as.numeric(unlist(nba_pbp[,"YCoord"])<=14))|
                  (((14<as.numeric(unlist(nba_pbp[,"YCoord"]))) & (as.numeric(unlist(nba_pbp[,"YCoord"]))<=27.9)) & (((25 - sqrt((23.9^2)-((as.numeric(unlist(nba_pbp[,"YCoord"]))-4)^2)))<= (as.numeric(unlist(nba_pbp[,"XCoord"])))) & ((as.numeric(unlist(nba_pbp[,"XCoord"]))) <= (25 + sqrt((23.9^2)-((as.numeric(unlist(nba_pbp[,"YCoord"]))-4)^2))))))))

Zone1 <- which((0<=as.numeric(unlist(nba_pbp[,"XCoord"])) & as.numeric(unlist(nba_pbp[,"XCoord"]))<=50)&(47<as.numeric(unlist(nba_pbp[,"YCoord"]))& as.numeric(unlist(nba_pbp[,"YCoord"]<=94))))

z2<- c(1:dim(nba_pbp)[1])
z13 <- c(Zone1,Zone3)
Zone2 <- setdiff(z2,z13)

nba_pbp[Zone3,"zone"] <- 3
nba_pbp[Zone1, "zone"] <- 1
nba_pbp[Zone2,"zone"] <- 2

nba_pbp$shot_attempted <- rep(0,dim(nba_pbp)[1])
nba_pbp[which(nba_pbp$zone==3 & nba_pbp$shooting_play==TRUE),"shot_attempted"] <- 2

nba_pbp[which(nba_pbp$zone==2 & nba_pbp$shooting_play==TRUE),"shot_attempted"] <- 3

nba_pbp[which(nba_pbp$zone==1 &nba_pbp$shooting_play==TRUE),"shot_attempted"] <- 3

nba_pbp$shot_attempted

#Checking which shots my coordinate system miscategorized
which(grepl("three point",nba_pbp$text) & nba_pbp$shot_attempted == 2)
which(grepl("two point",nba_pbp$text) & nba_pbp$shot_attempted == 3)

#Creating column containing actual shot attempted
Attempted3 <- which(grepl("three point",nba_pbp$text))
Attempted2 <- which(grepl("two point", nba_pbp$text))
FreeThrows <- which(grepl("free throw",nba_pbp$text))

nba_pbp$actual_attempt <- rep(0,dim(nba_pbp)[1])
nba_pbp[Attempted3,"actual_attempt"] <- 3
nba_pbp[Attempted2,"actual_attempt"] <- 2
nba_pbp[FreeThrows,"actual_attempt"] <- 2

#Doesn't cover all of them as some say things like "10 foot floater" that requires understanding of the coordinate system to categorize.

shots <- c(Attempted3, Attempted2, FreeThrows)
s_remainders <- setdiff(which(nba_pbp$shooting_play==TRUE),shots)
nba_pbp[s_remainders,"actual_attempt"] <- nba_pbp[s_remainders,"shot_attempted"]

nba_pbp[which(nba_pbp$shooting_play==TRUE)& nba_pbp$actual_attempt==0,]

A3 <- which(nba_pbp$score_value==3)
A2 <- which(nba_pbp$score_value==2)
A1 <- which(nba_pbp$score_value==1)
SuccessfulShots <- c(A3,A2,A1)
OtherShots <- setdiff(which(nba_pbp$shooting_play==TRUE), SuccessfulShots)

nba_pbp$Attempt <- rep(0,dim(nba_pbp)[1])
nba_pbp[A3,"Attempt"] <- 3
nba_pbp[A2,"Attempt"] <- 2
nba_pbp[A1,"Attempt"] <- 1
nba_pbp[OtherShots, "Attempt"] <- nba_pbp[OtherShots, "actual_attempt"]

###Creating Copy Of Data Without Unneccessary columns:
nba_pbp <- as_tibble(nba_pbp) 
#format for creating new matrix
pbp <- as_tibble(nba_pbp[,c(1,4,7,6,8,11,12,18,67,70,21,22,13,26,31,65,66,17,10,37,28,23,58)])
#format for renaming columns:
colnames(pbp)[which(colnames(pbp)=="clock_display_value" )]<-"clock" 
colnames(pbp)[which(colnames(pbp)=="period_number" )]<-"period" 
colnames(pbp)[which(colnames(pbp)=="home_score" )]<-"hscore" 
colnames(pbp)[which(colnames(pbp)=="away_score" )]<-"ascore" 
colnames(pbp)[which(colnames(pbp)=="type_text" )]<-"ptype" 
colnames(pbp)[which(colnames(pbp)=="scoring_play" )]<-"scorep" 
colnames(pbp)[which(colnames(pbp)=="score_value" )]<-"scorev" 
colnames(pbp)[which(colnames(pbp)=="shooting_play" )]<-"shootp" 
colnames(pbp)[which(colnames(pbp)=="coordinate_x_raw" )]<-"Xcoord" 
colnames(pbp)[which(colnames(pbp)=="coordinate_y_raw" )]<-"Ycoord" 
colnames(pbp)[which(colnames(pbp)=="season_type" )]<-"seasont" 
colnames(pbp)[which(colnames(pbp)=="away_team_abbrev" )]<-"ateam"
colnames(pbp)[which(colnames(pbp)=="home_team_abbrev" )]<-"hteam" 
colnames(pbp)[which(colnames(pbp)=="team_id" )]<-"teamID" 
pbp <- mutate(pbp,nba_pbp$team_id)
colnames(pbp)[which(colnames(pbp)=="nba_pbp$team_id" )]<-"tOnBall"
colnames(pbp)[which(colnames(pbp)=="Attempt" )]<-"sattempt"

###Investigating 2 Pointers
#Attempting to investigate Shot Type Distributions
Plots <- list()
for(i in unique(pbp$ptype[which(pbp$shootp==TRUE& pbp$sattempt==2)])){
  PlotDF <- as_tibble(pbp[which(pbp$ptype==i & pbp$sattempt==2),c(2,7,8)])
  Ploti <- ggplot(PlotDF,aes(x=scorev)) + geom_bar(fill="purple") + labs(title=i) + theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
  Plots <- list(Plots, Ploti)
}
print(Plots)

###Investigating 3 Pointers
#Attempting to investigate Shot Type Distributions
Plots2 <- list()
for(i in unique(pbp$ptype[which(pbp$shootp==TRUE& pbp$sattempt==3)])){
  PlotDF <- as_tibble(pbp[which(pbp$ptype==i & pbp$sattempt==3),c(2,7,8)])
  Ploti <- ggplot(PlotDF,aes(x=scorev)) + geom_bar(fill="purple") + labs(title=i)
  Plots2 <- list(Plots2, Ploti)
}
print(Plots2)

###Categorizing Shots
#136 different event types to address
#I will attempt to categorize them all suitably here
#Shooting Plays
FreeT <- c( "Free Throw - 1 of 2", "Free Throw - Flagrant 1 of 2","Free Throw - 2 of 2", "Free Throw - Flagrant 2 of 2","Free Throw - Technical", "Free Throw - Flagrant 1 of 1","Free Throw - 1 of 1","Free Throw - Flagrant 1 of 3", "Free Throw - 1 of 3","Free Throw - Flagrant 2 of 3","Free Throw - 2 of 3","Free Throw - Flagrant 3 of 3","Free Throw - 3 of 3", "Free Throw - Clear Path 1 of 2", "Free Throw - Clear Path 2 of 2")

#2 point shots
shots_2s <- c("Step Back Jump Shot", "Floating Jump Shot", "Turnaround Fade Away Jump Shot", "Pullup Jump Shot", "Driving Floating Jump Shot", "Step Back Bank Jump Shot", "Turnaround Jump Shot","Driving Floating Bank Jump Shot", "Jump Shot","Fade Away Jump Shot", "Jump Shot Bank", "Tip Shot", "Running Jump Shot", "Driving Hook Shot", "Pullup Bank Jump Shot", "Turnaround Hook Shot","Hook Turnaround Bank", "Fade Away Bank Jump Shot", "Hook Driving Bank", "Turnaround Fadeaway Bank Jump Shot", "Hook Shot Bank", "Hook Shot", "Running Pullup Jump Shot", "Driving Jump Shot Bank", "Turnaround Bank Jump Shot","No Shot (Default Shot)")

Layups <- c("Running Layup Shot", "Running Finger Roll Layup", "Driving Finger Roll Layup", "Layup Shot Putback", "Cutting Layup Shot", "Layup Running Reverse", "Layup Driving Reverse", "Alley Oop Layup Shot", "Reverse Layup Shot", "Finger Roll Layup", "Running Alley Oop Layup Shot", "Cutting Finger Roll Layup Shot", "Driving Layup Shot", "Layup Shot")

Dunks <- c("Running Dunk Shot", "Cutting Dunk Shot", "Tip Dunk Shot", "Driving Dunk Shot", "Dunk Shot", "Alley Oop Dunk Shot", "Running Alley Oop Dunk Shot", "Putback Dunk Shot", "Reverse Dunk Shot", "Running Reverse Dunk Shot", "Driving Reverse Dunk Shot")

twos <- c(Layups,Dunks,shots_2s)
#3 point shots
Bank3 <- c("Step Back Bank Jump Shot", "Jump Shot Bank", "Pullup Bank Jump Shot", "Driving Jump Shot Bank", "Fade Away Bank Jump Shot")

HighChance3 <- c("Jump Shot", "Pullup Jump Shot", "Step Back Jump Shot", "Running Jump Shot", "Running Pullup Jump Shot", "Turnaround Jump Shot","No Shot (Default Shot)")

LowChance3 <- c("Fade Away Jump Shot", "Driving Floating Jump Shot", "Turnaround Fade Away Jump Shot", "Driving Floating Bank Shot", "Floating Jump Shot", "Turnaround Bank Jump Shot", "Turnaround Fadeaway Bank Jump Shot" )

threes <- c(Bank3, HighChance3, LowChance3)
#Now will use this to rename shot types %in%:

pbp[(pbp$ptype %in% shots_2s) & (pbp$sattempt==2),"ptype"] <- "Standard_2"
pbp[(pbp$ptype %in% Layups) & (pbp$sattempt==2),"ptype"] <- "Layup"
pbp[(pbp$ptype %in% Dunks) & (pbp$sattempt==2),"ptype"] <- "Dunk"

pbp[(pbp$ptype %in% FreeT),"ptype"] <-"FreeT"

pbp[(pbp$ptype %in% Bank3) & (pbp$sattempt==3),"ptype"] <-"Bank_3"
pbp[(pbp$ptype %in% HighChance3) & (pbp$sattempt==3),"ptype"] <-"Standard_3"
pbp[(pbp$ptype %in% LowChance3) & (pbp$sattempt==3),"ptype"] <-"Risky_3"

pbp$ptype[which(pbp$ptype=="Cutting Layup Shot"|pbp$ptype=="Tip Dunk Shot"| pbp$ptype=="Layup Shot"|pbp$ptype=="Driving Floating Bank Jump Shot"| pbp$ptype=="Driving Layup Shot")] <- "Risky_3"

unique(pbp$ptype[pbp$shootp==TRUE])

###Categorizing other events
unique(pbp$ptype[which(pbp$shootp==FALSE)])

Def_Foul <- c("Personal Take Foul", "Double Technical Foul", "Flagrant Foul Type 1", "Delay Technical", "Flagrant Foul Type 2", "Clear Path Foul", "Loose Ball Foul", "Kicked Ball","Lane", "Defensive 3-Seconds Technical", "Transition Take Foul","Shooting Foul", "Personal Foul", "Defensive Goaltending")

Off_Foul <- c("Inbound Turnover","5 Second To Basket Turnover", "Punched Ball Turnover", "Offensive Foul", "Double Dribble Turnover", "Shot Clock Turnover", "Traveling", "Offensive Charge", "Lane Violation Turnover", "Kicked Ball Turnover", "Jumpball Violation Turnover", "Palming Turnover", "8-Second Turnover", "3-Second Turnover", "Basket from Below Turnover", "Offensive Goaltending Turnover", "Disc Dribble Turnover", "Illegal Assist Turnover", "Too Many Players Turnover", "5-Second Turnover", "Excess Timeout Turnover", "5-Second Back to the Basket Turnover" )

Out <- c("Out of Bounds - Step Turnover", "Out of Bounds - Lost Ball Turnover", "Back Court Turnover", "Out of Bounds - Bad Pass Turnover", "Out of Bounds - Step Turnover")

AwayFromBall_Foul <- c("Technical Foul", "Hanging Technical Foul", "Away From Play Foul", "Double Personal Foul", "Double Lane", "Too Many Players Technical","Excess Timeout Technical", "Away from Play Foul")

pbp[(pbp$ptype %in% Def_Foul),"ptype"] <- "Def_Foul"
pbp[(pbp$ptype %in% Off_Foul),"ptype"] <- "Off_Foul"
pbp[(pbp$ptype %in% AwayFromBall_Foul),"ptype"] <- "AFBall_Foul"
pbp[(pbp$ptype %in% Out),"ptype"] <- "Out_of_Bounds"
pbp$ptype[which(pbp$ptype=="Bad Pass\nTurnover")] <- "U_Pass"
pbp$ptype[which(pbp$ptype=="Lost Ball Turnover")] <- "U_Dribble"
pbp$ptype[which(pbp$ptype=="Offensive Rebound")] <- "Off_Rebound"
pbp$ptype[which(pbp$ptype=="Defensive Rebound")] <- "Def_Rebound"

unique(pbp$ptype[which(pbp$shootp==FALSE)])

###Time Intervals:
pbp$RealTime<-format(as.POSIXct(pbp$wallclock,format="%Y-%m-%dT%H:%M:%SZ"), format= "%H:%M:%S")
pbp$InSeconds <- ((as.numeric(format(as.POSIXct(pbp$wallclock,format="%Y-%m-%dT%H:%M:%SZ"), format= "%H"))*60)+as.numeric(format(as.POSIXct(pbp$wallclock,format="%Y-%m-%dT%H:%M:%SZ"), format= "%M")))*60+as.numeric(format(as.POSIXct(pbp$wallclock,format="%Y-%m-%dT%H:%M:%SZ"), format= "%S"))

pbp$IntereventTime <- c(0,diff(pbp$InSeconds))
pbp$IntereventTime[which(pbp$period==1 & pbp$ptype=="Jumpball")] <- "0"

###Some Plots
games <- c("401442535","401442534","401442533","401442532","401442531","401442530","401442529","401442528","401442527","401442526")
First10 <- 	pbp[(pbp$game_id %in% games),]
First10cfd <- ecdf(First10$IntereventTime)
plot(First10cfd)
two_pointers <- subset(pbp, sattempt==2)
plot(XCoord ~ YCoord, data = two_pointers)

###Calculating Interevent Times
pbp_Test <- as.data.frame(pbp)

EventsNotToBeModelled <- c(
  "Ref-Initiated Review (Supported)", "Ref-Initiated Review (Overturned)", "Coach's Challenge (replaycenter)", "Ref-Initiated Review (Stands)", "No Violation", "Substitution", "Defensive Goaltending", "Challenge", "Not Available", "End Period", "Delay of Game", "Full Timeout", "End Game", "Coach's Challenge", "Coach's Challenge (Overturned)", "Coach's Challenge (Stands)", "Coach's Challenge (Supported)","No Turnover"
)


ENIM <- which(pbp$ptype %in% EventsNotToBeModelled)
Updates <- ENIM + 1
Clock_Seconds <- as.numeric(format(as.POSIXct(pbp_Test$clock,format="%M:%S"), format= "%M"))*60 + as.numeric(format(as.POSIXct(pbp_Test$clock,format="%M:%S"), format= "%S"))
pbp_Test$Clock_Seconds <- Clock_Seconds

ClockDiff <- c(0,diff(pbp_Test$Clock_Seconds))
RealTimeDiff <- c(0,diff(pbp_Test$InSeconds))
pbp_Test$RealTimeDiff <- RealTimeDiff
pbp_Test[FreeThrows,"IntereventTime"] <- RealTimeDiff[FreeThrows]
pbp_Test$ClockDiff <- ClockDiff

#First Assigning Times To just be time difference
pbp_Test[,"IntereventTime"]<- abs(ClockDiff)


#Didn't Work For Free Throws, thus implement real time
FreeThrows <- which(pbp_Test$ptype == "FreeT")
pbp_Test$IntereventTime[FreeThrows] <- pbp_Test$RealTimeDiff[FreeThrows]
####pbp_Test <- pbp_Test[-1232878,]

#Removing The Time Delay Events:
pbp_Test <- pbp_Test[-ENIM,]
pbp_Test <- pbp_Test[-which(pbp_Test$ptype=="Offensive Foul Turnover"),]



#We have issue when time is 0
ZeroTime <- which(pbp_Test$ClockDiff==pbp_Test$RealTimeDiff & pbp_Test$ClockDiff==0)
pbp_Test[ZeroTime,"IntereventTime"] <- 0.5
pbp_Test[1,"IntereventTime"] <- 0

#Creating Time of event in each game
#Making start of each game at t=0
pbp_Test$StartOfGame <- rep(0,dim(pbp_Test)[1])
pbp_Test[which(pbp_Test$ptype=="Jumpball" & pbp_Test$period==1),"StartOfGame"] <- "1"


pbp_Test$IntereventTime[which(pbp_Test$StartOfGame==1)] <- "0"

pbp_Test$IntereventTime[which(pbp_Test$IntereventTime==0 & pbp_Test$StartOfGame==0)] <- pbp_Test$RealTimeDiff[which(pbp_Test$IntereventTime==0 & pbp_Test$StartOfGame==0)]

#Fixing Those Smaller Than 0
pbp_Test[which(as.numeric(pbp_Test$IntereventTime) < 0), "IntereventTime"] <- (as.numeric(pbp_Test[which(as.numeric(pbp_Test$IntereventTime) < 0),"InSeconds"]))
which(as.numeric(pbp_Test$IntereventTime) < 0)

#Fixing Zeros
pbp_Test$IntereventTime[which(pbp_Test$IntereventTime==0 & pbp_Test$StartOfGame==0)] <- "0.5"

which(pbp_Test$IntereventTime==0 & pbp_Test$StartOfGame==0)

which(pbp_Test$IntereventTime==0 & pbp_Test$StartOfGame==1)

pbp_Test[which(pbp_Test$ptype=="Jump Ball"),"ptype"] <- "Jumpball"
pbp_Test$Def_Ind <- rep(20,dim(pbp_Test)[1])
pbp_Test$Def_Ind[which(pbp_Test$ptype=="Def_Rebound")] <- 1
pbp_Test[which(pbp_Test$ptype=="Def_Rebound" | pbp_Test$ptype=="Off_Rebound"),"ptype"] <- "Rebound"

unique(pbp_Test$ptype)

###Missing Data
####### TEAM ID #########
#Filling in teamIDs
which(is.na(pbp_Test$teamID))
pbp_Test$teamID[c(48820,94546,102982,141330,406605,521857,572488,683088,683102,683103,683119,682144,683208,770218,776142,777251,794973,825018,835826,847503,988094,1009716,1022204)] <- c(25,28,17,20,22,2,6,24,24,24,24,24,24,15,17,7,20,22,12,2,25,18)

#Removing Duplicate Events
pbp_Test <- pbp_Test[c(-209341,-389078,-453616,-597082,-632342,-683131),]

####### INTEREVENT TIMES ########

#Interevent Times
pbp_Test$IntereventTime[which(is.na(pbp_Test$IntereventTime))] <- (pbp_Test$InSeconds[which(is.na(pbp_Test$IntereventTime))] - pbp_Test$InSeconds[(which(is.na(pbp_Test$IntereventTime))-1)])
unique(pbp_Test$ptype)
which(is.na(pbp_Test$IntereventTime))
#Fixing Outliers
hist(as.numeric(pbp_Test$IntereventTime))
which(as.numeric(pbp_Test$IntereventTime)>50000)
pbp_Test$IntereventTime[which(as.numeric(pbp_Test$IntereventTime)>50000)] <- abs(pbp_Test$ClockDiff[which(as.numeric(pbp_Test$IntereventTime)>50000)])
which(as.numeric(pbp_Test$IntereventTime)>50000)
which(pbp_Test$IntereventTime==0 & pbp_Test$StartOfGame==0)

pbp_Test[which(as.numeric(pbp_Test$IntereventTime)>500),"IntereventTime"] <- abs(pbp_Test$RealTimeDiff[which(as.numeric(pbp_Test$IntereventTime)>500)])
which(as.numeric(pbp_Test$IntereventTime)>500)
#For Some Reason These Events have all entries recorded as their id. Need to be removed. THIS MAY NOT BE AN ISSUE ANYMORE AFTER QUICK FIX
pbp_Test[which(as.numeric(pbp_Test$IntereventTime)>500),"IntereventTime"] <- 10

#I will sample the empirical distributions 
unique(pbp_Test$ptype[which(pbp_Test$IntereventTime==0 & pbp_Test$StartOfGame==0)])
pbp_Test$IntereventTime[which(pbp_Test$IntereventTime==0 & pbp_Test$StartOfGame==0)] <- abs(pbp_Test$RealTimeDiff[which(pbp_Test$IntereventTime==0 & pbp_Test$StartOfGame==0)])
which(pbp_Test$IntereventTime==0 & pbp_Test$StartOfGame==0)

#2 events that have 0 clock diff time but pass over midnight, they take very little time but crossed midnight
#pbp_Test[which(as.numeric(pbp_Test$IntereventTime)>500),"IntereventTime"] <- 0.5

pbp_Test$IntereventTime[which(pbp_Test$IntereventTime<0)] <- abs(as.numeric(pbp_Test$IntereventTime[which(pbp_Test$IntereventTime<0)]))

#Need to deal with missing data still
#All missing data appears to be part of two games
unique(pbp_Test$game_id[which(is.na(pbp_Test$IntereventTime))])
pbp_Test$game_id[which(is.na(pbp_Test$IntereventTime))]
pbp_Test <- pbp_Test[-which(is.na(pbp_Test$IntereventTime)),]
#
pbp_Test[682898,'teamID'] <- 6

###Creating Event Times:
EventTest <- as_tibble(pbp_Test)
EventTest <- EventTest %>% group_by(game_id,period) %>% mutate(EventTimes = cumsum(IntereventTime))
pbp_Test <- EventTest
which(is.na(pbp_Test$EventTimes))
which(pbp_Test$EventTimes=="0" & pbp_Test$StartOfGame =="0")

###Splitting Shots Into Successful And Unsuccessful:
Succ <- which(pbp_Test$scorep==TRUE)
UnSucc <- which(pbp_Test$scorep==FALSE & pbp_Test$shootp==TRUE)

pbp_Test$ptype[Succ] <- paste("S_",pbp_Test$ptype[Succ],sep = "")
pbp_Test$ptype[UnSucc] <- paste("U_",pbp_Test$ptype[UnSucc],sep = "")

###Splitting Into Home and Away
AwayEvents <- which(pbp_Test$away_team_id==pbp_Test$tOnBall)
HomeEvents <- which(pbp_Test$home_team_id==pbp_Test$tOnBall)

pbp_Test$ptype[HomeEvents] <- paste("Home_",pbp_Test$ptype[HomeEvents],sep = "")
pbp_Test$ptype[AwayEvents] <- paste("Away_",pbp_Test$ptype[AwayEvents],sep = "")

###Creating Final Zones
pbp_Test$FinZone <- rep(0,dim(pbp_Test)[1])

TwoPointZone <- which((((3 <= as.numeric(unlist(pbp_Test[,"XCoord"])))&(as.numeric(unlist(pbp_Test[,"XCoord"]))<=47)) & (-10<=as.numeric(unlist(pbp_Test[,"YCoord"]))& as.numeric(unlist(pbp_Test[,"YCoord"])<=14))|
                         (((14<as.numeric(unlist(pbp_Test[,"YCoord"]))) & (as.numeric(unlist(pbp_Test[,"YCoord"]))<=27.9)) & (((25 - sqrt((23.9^2)-((as.numeric(unlist(pbp_Test[,"YCoord"]))-4)^2)))<= (as.numeric(unlist(pbp_Test[,"XCoord"])))) & ((as.numeric(unlist(pbp_Test[,"XCoord"]))) <= (25 + sqrt((23.9^2)-((as.numeric(unlist(pbp_Test[,"YCoord"]))-4)^2))))))))

OutOfTPZ <- setdiff(1:nrow(pbp_Test),TwoPointZone)

z2<- c(1:dim(nba_pbp)[1])
z13 <- c(Zone1,Zone3)
Zone2 <- setdiff(z2,z13)

pbp_Test[TwoPointZone,"FinZone"] <- 2
pbp_Test[OutOfTPZ,"FinZone"] <- 1
pbp_Test$FinZone[which(pbp_Test$shootp==TRUE)] <- pbp_Test$sattempt[which(pbp_Test$shootp==TRUE)]
pbp_Test$FinZone[which(pbp_Test$FinZone==3)] <- 1

which(!(pbp_Test$FinZone %in% c(1,2)))

#We have problem with free throws
pbp_Test$FinZone[which(grepl("FreeT",pbp_Test$ptype))] <- 2
#Great Success
pbp_Test$FinZone[which(pbp_Test$ptype=="Rebound")] <- 2
pbp_Test$FinZone[which(pbp_Test$Def_Ind==1)] <- 1


pbp_Test[which(pbp_Test$ptype=="AFBall_Foul"),"ptype"] <-	"Away_AFBall_Foul"
pbp_Test[which(pbp_Test$ptype=="Away_S_AFBall_Foul"),"ptype"] <- "Away_AFBall_Foul"

pbp_Test[which(pbp_Test$ptype=="Away_AFBall_Foul"|pbp_Test$ptype=="Away_Def_Foul"|pbp_Test$ptype=="Away_Off_Foul"),"ptype"] <- "Away_Foul"
pbp_Test[which(pbp_Test$ptype=="Home_AFBall_Foul"|pbp_Test$ptype=="Home_Def_Foul"|pbp_Test$ptype=="Home_Off_Foul"),"ptype"] <- "Home_Foul"
pbp_Test[406421,"ptype"] <- "Home_Ejection"

###Creating Final Dataset:
mod_pbp <- pbp_Test[,c(1,20,23,13,33,2,17,16,34,11,12,5)]
mod_pbp[c(1:10),]
which(is.na(mod_pbp$teamID))
which(is.na(mod_pbp$EventTimes))
which(is.na(mod_pbp$FinZone))
mod_pbp[682815,'ptype'] <- 'Away_Rebound'
unique(mod_pbp$ptype)
##Can be fixed as we have home and away events

#These Are All-star Games That Will Be Removed:
#Fixing Jumpball Allocations
mod_pbp[c(48820 ,94364,141148 ,521672   ,572303   ,775896  , 777005   ,794727   ,824772 ,835579  ,847256 ,1009469 ,1021957),'ptype'] <- c('Away_Jumpball','Away_Jumpball','Away_Jumpball','Away_Jumpball','Away_Jumpball','Home_Jumpball','Home_Jumpball','Away_Jumpball','Away_Jumpball','Away_Jumpball','Away_Jumpball','Home_Jumpball','Home_Jumpball')

#Fixing False Free Throw Data:
mod_pbp[682857,'ptype'] <- 'Away_S_FreeT'
mod_pbp[682858,'ptype'] <- 'Away_U_FreeT'
#Fixing False Rebound Data:
mod_pbp[c(682843 ,682874, 682898 , 682962),'ptype'] <- c('Away_Rebound','Away_Rebound','Away_Rebound','Away_Rebound') #Used to all be Def_Rebound
#There's a foul that scored points which needs to be altered:
mod_pbp$ptype[which(mod_pbp$ptype=='Away_S_AFBall_Foul')] <- 'Away_Foul'
unique(mod_pbp$ptype)
mod_pbp[682413,'teamID'] <- 24
mark_df <- data.frame(Away = levels(as.factor(mod_pbp$ptype))[1:21], Home = levels(as.factor(mod_pbp$ptype))[22:42])
mod_pbp$ptype <- as.numeric(factor(mod_pbp$ptype))
mod_pbp$ptype <- mod_pbp$ptype-1
mod_pbp$teamID <- mod_pbp$teamID-1
mod_pbp$FinZone <- mod_pbp$FinZone-1
GameFix1 <- unique(mod_pbp$game_id[which(mod_pbp$teamID>32)])[1]
GameFix2 <- unique(mod_pbp$game_id[which(mod_pbp$teamID>32)])[2]
mod_pbp <- mod_pbp[-which(mod_pbp$game_id==GameFix1),]
mod_pbp <- mod_pbp[-which(mod_pbp$game_id==GameFix2),]

###Heatmaps:
y <- seq(14,27.9,length=200)
x1 <- c(3,25-sqrt((23.9^2)-((y-4)^2)))
x2 <- rev(c(47,25+sqrt((23.9^2)-((y-4)^2))))
arc <- data.frame(c(x1,x2),c(14,y,rev(y),14))
colnames(arc)<- c("x","y")

CourtPlot <- function(df,id){
  d <- df[which(df$game_id==id),]
  ggplot(d,aes(x=XCoord,y=YCoord)) + geom_density_2d_filled(aes(fill=..level..),bins=9,alpha=0.8) + scale_fill_brewer(palette="YlOrRd") +  geom_rect(aes(xmin=0,xmax=50,ymin=0,ymax=94), fill=NA , colour="black", size=1) + geom_segment(aes(x=0,xend=50,y=47,yend=47),size=1, colour="black") + geom_segment(aes(x=3,xend=3,y=0,yend=14),size=1, colour="black") + geom_segment(aes(x=47,xend=47,y=0,yend=14),size=1, colour="black") + 
    geom_path(data=arc,aes(x=x,y=y),size=1, colour="black") + theme_minimal() + labs(title=paste(paste(d[1,"hteam"],df[1,"ateam"], sep=" vs "),df$game_date[which(df$game_id==id)][1], sep = " - "))
}

CourtPlotStat <- function(df,id,stat){
  d <- df[which(df$game_id==id & (df$ptype %in% c(paste("Home_S_",stat,sep=""),paste("Home_U_",stat,sep=""),paste("Away_S_",stat,sep=""),paste("Away_U_",stat,sep=""),paste("Away_",stat,sep=""),paste("Home_,stat,sep=")))),]
  ggplot(d,aes(x=XCoord,y=YCoord)) + geom_density_2d_filled(aes(fill=..level..),bins=9,alpha=0.8) + scale_fill_brewer(palette="YlOrRd") +  geom_rect(aes(xmin=0,xmax=50,ymin=0,ymax=94), fill=NA , colour="black", size=1) + geom_segment(aes(x=0,xend=50,y=47,yend=47),size=1, colour="black") + geom_segment(aes(x=3,xend=3,y=0,yend=14),size=1, colour="black") + geom_segment(aes(x=47,xend=47,y=0,yend=14),size=1, colour="black") + 
    geom_path(data=arc,aes(x=x,y=y),size=1, colour="black") + theme_minimal() + labs(title=paste(paste(paste(d[1,"hteam"],d[1,"ateam"], sep=" vs "),stat,sep=" - "),df$game_date[which(df$game_id==id)][1],sep=" - "))
}

CourtPlotStatHoA <- function(df,id,stat,HoA){
  d <- df[which((df$ptype %in% c(paste(paste(HoA,"_S_",sep=""),stat,sep=""),paste(paste(HoA,"_U_",sep=""),stat,sep=""),paste(HoA,"_",sep = ""),stat,sep=""))),]
  d <- d[which(d$game_id==id),]
  ggplot(d,aes(x=XCoord,y=YCoord)) + geom_density_2d_filled(aes(fill=..level..),bins=9,alpha=0.8) + scale_fill_brewer(palette="YlOrRd") +  geom_rect(aes(xmin=0,xmax=50,ymin=0,ymax=94), fill=NA , colour="black", size=1) + geom_segment(aes(x=0,xend=50,y=47,yend=47),size=1, colour="black") + geom_segment(aes(x=3,xend=3,y=0,yend=14),size=1, colour="black") + geom_segment(aes(x=47,xend=47,y=0,yend=14),size=1, colour="black") + 
    geom_path(data=arc,aes(x=x,y=y),size=1, colour="black") + theme_minimal() + labs(title=paste(HoA,paste(stat,df$game_date[which(df$game_id==id)][1],sep=" - "),sep=" - "))
}

CourtPlotStatHoASoU <- function(df,id,stat,HoA,SoU){
  d <- df[which((df$ptype %in% paste(paste(HoA,paste("_",paste(SoU,"_",sep=""),sep=""),sep=""),stat,sep=""))),]
  d <- d[which(d$game_id==id),]
  ggplot(d,aes(x=XCoord,y=YCoord)) + geom_density_2d_filled(aes(fill=..level..),bins=9,alpha=0.8) + scale_fill_brewer(palette="YlOrRd") +  geom_rect(aes(xmin=0,xmax=50,ymin=0,ymax=94), fill=NA , colour="black", size=1) + geom_segment(aes(x=0,xend=50,y=47,yend=47),size=1, colour="black") + geom_segment(aes(x=3,xend=3,y=0,yend=14),size=1, colour="black") + geom_segment(aes(x=47,xend=47,y=0,yend=14),size=1, colour="black") + 
    geom_path(data=arc,aes(x=x,y=y),size=1, colour="black") + theme_minimal() + labs(title=paste(HoA,paste(stat,df$game_date[which(df$game_id==id)][1],sep=" - "),sep=" - "))
}

CP2 <- CourtPlot(pbp_Test,401468017)
CP2
CP3 <- CourtPlot(pbp_Test,401468016)
CP3
CP4 <- CourtPlot(pbp_Test,401468015)
CP4

Risky3Plot <- CourtPlotStat(pbp_Test,401468017,"Risky_3")
Risky3Plot
LayupPlot <- CourtPlotStat(pbp_Test,401468017,"Layup")
LayupPlot
OffRebPlot <- CourtPlotStat(pbp_Test,401468017,"Off_Rebound")
OffRebPlot
UPassPlot <- CourtPlotStat(pbp_Test,401468017,"Pass")
UPassPlot
Standard3Plot <- CourtPlotStat(pbp_Test,401468017,"Standard_3")
Standard3Plot

Standard3PlotHome <- CourtPlotStatHoA(pbp_Test,401468017,"Standard_3", "Home")
Standard3PlotHome
Standard3PlotAway <- CourtPlotStatHoA(pbp_Test,401468017,"Standard_3", "Away")
Standard3PlotAway

CourtPlotStatHoASoU(pbp_Test,"Standard_3","Home","S")
CourtPlotStatHoASoU(pbp_Test,"Standard_3","Home","U")

###Play Plots
#ADD ARRWOS
library(grid)  # for arrow()

EventTimes <- ave(pbp_Test$IntereventTime,cumsum(pbp_Test$IntereventTime==0),FUN=cumsum)

#Need to be able to identify each play
#Identify Terminating Nodes
T_nodes <- c(which(grepl("Out_of_Bounds",pbp_Test$ptype)),which(grepl("Foul",pbp_Test$ptype)), which(grepl("S_",pbp_Test$ptype)))
mod_pbp_Plays <- data.frame(pbp_Test)
mod_pbp_Plays$tNodes <- rep(0, dim(mod_pbp_Plays)[1])
mod_pbp_Plays$tNodes[T_nodes] <- 1 
#Starting Nodes
mod_pbp_Plays$sNodes <- rep(0,dim(mod_pbp_Plays)[1])
mod_pbp_Plays$sNodes[(T_nodes+1)[-(length(T_nodes+1))]] <- 1
mod_pbp_Plays$sNodes[which(mod_pbp_Plays$EventTimes=="0")] <- 2
mod_pbp_Plays$PlayID <- ave(mod_pbp_Plays$sNodes,cumsum(mod_pbp_Plays$sNodes==2),FUN=cumsum)

#Home and Away Colours
Colours <- which(grepl("Away",mod_pbp_Plays$ptype))
mod_pbp_Plays$Colours <- rep("blue",dim(mod_pbp_Plays)[1])
mod_pbp_Plays$Colours[Colours] <- "red"
##Make Rebounds Circles that surround The shot if it's a block
Rebounds <- which(grepl("Rebound",mod_pbp_Plays$ptype))
mod_pbp_Plays$Rebounds <- rep("Events",dim(mod_pbp_Plays)[1])
mod_pbp_Plays$Rebounds[Rebounds] <- "Rebounds"
mod_pbp_Plays$Rebounds <- factor(mod_pbp_Plays$Rebounds)
##Maybe Make Green Circle for succesful shots
S_Shots <- which(grepl("S_",mod_pbp_Plays$ptype))
mod_pbp_Plays$sShots <- rep("0",dim(mod_pbp_Plays)[1])
mod_pbp_Plays$sShots[S_Shots] <- "1" 

#Function For Plot
PlayPlot <- function(df,id,play){
  df <- df[which(df$PlayID==play & df$game_id==id),]
  s<- df[which(df$sShots=="1"),]
  pp_plot <- ggplot(df,aes(x=XCoord,y=YCoord)) +
    geom_rect(aes(xmin=0,xmax=50,ymin=0,ymax=94), fill=NA , colour="black", size=1) + geom_segment(aes(x=0,xend=50,y=47,yend=47),size=1, colour="black") + geom_segment(aes(x=3,xend=3,y=0,yend=14),size=1, colour="black") + geom_segment(aes(x=47,xend=47,y=0,yend=14),size=1, colour="black") + 
    geom_path(data=arc,aes(x=x,y=y),size=1, colour="black") +
    geom_path(arrow = arrow(type = "closed", length = unit(0.1, "inches"))) + geom_point(aes(color=Colours, shape=Rebounds, size=Rebounds)) +  scale_shape_manual(values = c("Events" = 19, "Rebounds" = 21)) +
    scale_size_manual(values = c("Events" = 3,"Rebounds" = 5)) +
    geom_point(data=s,aes(x=XCoord,y=YCoord),color="green", size = 4, stroke = 1, shape = 5, fill = NA) + theme(panel.background = element_rect(fill="navajowhite1")) + labs(title=paste(paste("Game",id,sep=" "),paste("Play", play, sep=""),sep = " - "))
  print(pp_plot)
}

#Plotting Graphs

PlayPlot(mod_pbp_Plays,401468017,36)
PlayPlot(mod_pbp_Plays,401468017,14)
PlayPlot(mod_pbp_Plays,401468017,11)

for(u in 41:60){
  PlayPlot(mod_pbp_Plays,401468017,u)
}

###Interevent Time Dist
#Density
TimeHist <- ggplot(data=pbp_Test[which(as.numeric(pbp_Test$IntereventTime)<150),],aes(x=as.numeric(IntereventTime))) + geom_histogram(binwidth=1,alpha=0.8,fill="purple") + labs(x="Interevent Times", title = "Histogram Of Interevent Times",y="Frequency") +theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
TimeHist

#Cumulative PDF
plot(ecdf(pbp_Test$IntereventTime[which(as.numeric(pbp_Test$IntereventTime)<100)]),main="Empirical CDF Of Interevent Times in the NBA", xlab="Interevent Time", ylab="CDF")

###Diggle's K-Estimate
#Fix Times Before Checking If this works
DiggleKest <- function(df,t_s){
  t_list <- as.numeric(df$EventTimes)
  t <- t_s
  Tnum <- max(t_list)
  n <- length(t_list)
  n_sq <- n^2
  #Matrix of |ti-tj|:
  M1 <- abs(outer(t_list,t_list,"-"))
  #For Ws we need vector of T-ti
  T_diff <- Tnum-t_list
  V_min <- pmin(t_list,T_diff)
  #We need to take M1 and subtract the first entry of this from every row
  W_M <- M1 - V_min
  #Need To Check which are equal to or below 0 and make those entries 1 and others 2
  W_M[which(W_M > 0)] <- 2
  W_M[which(W_M <= 0)] <- 1
  diag(W_M) <- 0
  #Now need to find indicator matrix
  I_list <- lapply(t, function(x){return(M1-x)})
  I_list <- lapply(I_list, function(x){x[which(x > 0)]<- 10
  return(x)})
  I_list <- lapply(I_list, function(x){x[which(x <= 0)]<- 1
  return(x)})
  I_list <- lapply(I_list, function(x){x[which(x == 10)]<- 0
  return(x)})
  I_list <- lapply(I_list, function(x){diag(x)<- 0
  return(x)})
  #Now we have the 2 matrices for each t, need to calculate Diggle's Estimate:
  V_Sums <- as.numeric(as.vector(lapply(I_list,function(x){M <- W_M%*%x
  y <- sum(diag(M))
  return(y)})))
  #Below This Point Works
  constant <- Tnum/n_sq
  V_Kest <- V_Sums*(constant)
  return(V_Kest)
}


#Seems as though the dispersion plot works as desired
UDispersionCheck <- function(df,n){
  t <- seq(0:n)
  Kest <- DiggleKest(df,t)
  UD <- Kest-(2*t)
  UD_data <- data.frame(x =t, estimates = UD)
  UD_plot <- ggplot(data = UD_data, aes(x=x,y=estimates)) + geom_point(col = "blue") + theme_minimal() +
    geom_hline(yintercept=0,linetype = 'dashed', col = 'grey',size=1) + labs(title = "Plot Checking For Under Dispersion Using Diggle's K-Estimate", x = "t", y = "Kest(t)-2t") 
  print(UD_plot)
}



UDispersionCheck(pbp_Test[which(pbp_Test$game_id=="401360189" & pbp_Test$period==1),],100)
UDispersionCheck(pbp_Test[which(pbp_Test$game_id=="401358782"& pbp_Test$period==1),],100)
UDispersionCheck(pbp_Test[which(pbp_Test$game_id=="401360323"& pbp_Test$period==1),],100)
UDispersionCheck(pbp_Test[which(pbp_Test$game_id=="401360457"& pbp_Test$period==1),],100)

UDispersionCheck(pbp_Test[which(pbp_Test$game_id=="401360452"& pbp_Test$period==1),],100)

###Post-mark Distributions:
Create Density Plots Of Times For Different Event Types:
  Hist_List <- list()
for(i in unique(pbp_Test$ptype)){
  x <- ggplot(data=pbp_Test[(which(as.numeric(pbp_Test$IntereventTime)<150 & pbp_Test$ptype==i)+1),],aes(x=as.numeric(IntereventTime))) + geom_histogram(binwidth=1,alpha=0.8,fill="skyblue") + labs(x="Interevent Times", title =paste("Histogram Of Interevent Times",i,sep=" -")) +theme_minimal()+
    scale_x_continuous(limits = c(0, 150))
  Hist_List <- list(Hist_List, x)
}
print(Hist_List)


#Create vector identifying if shot follows Offensive Rebound
Following_OR <- which(pbp_Test$ptype %in% c("Away_Off_Rebound","Home_Off_Rebound"))+1
pbp_Test$F_OR <- rep(0,dim(pbp_Test)[1])
pbp_Test[Following_OR,"F_OR"] <- 1


#Plot Distributions
Dist_1 <- ggplot(data=pbp_Test[which(as.numeric(pbp_Test$IntereventTime)<150 & pbp_Test$shootp==TRUE & pbp_Test$F_OR==0),],aes(x=as.numeric(IntereventTime))) + geom_histogram(binwidth=1,alpha=0.8,fill="skyblue") + labs(x="Interevent Times", title ="Histogram Of Interevent Times") +theme_minimal()
Dist_1

Dist_2 <- ggplot(data=pbp_Test[which(as.numeric(pbp_Test$IntereventTime)<150 & (pbp_Test$shootp==FALSE | pbp_Test$F_OR == 1)),],aes(x=as.numeric(IntereventTime))) + geom_histogram(binwidth=1,alpha=0.8,fill="purple") + labs(x="Interevent Times", title ="Histogram Of Interevent Times") +theme_minimal()
Dist_2

Dist <- ggplot(data=pbp_Test[which(as.numeric(pbp_Test$IntereventTime)<150),],aes(x=as.numeric(IntereventTime))) + geom_histogram(binwidth=1,alpha=0.8,fill="skyblue") + labs(x="Interevent Times", title ="Histogram Of Interevent Times") +theme_minimal()
Dist

#Two Clearly Different Distribution

###SavingData
#Separating Games
games_list <- split(mod_pbp, mod_pbp$game_id)
time_list <- split(mod_pbp$EventTimes, mod_pbp$game_id)
mark_list <- split(mod_pbp$ptype, mod_pbp$game_id)
team_list <- split(mod_pbp$teamID,mod_pbp$game_id)
zone_list <- split(mod_pbp$FinZone,mod_pbp$game_id)

write.csv(mod_pbp, "D:/Uni Work/Year 4/Diss/Diss_R/ProjectDiss/ModelData.csv")
saveRDS(time_list,"D:/Uni Work/Year 4/Diss/Diss_R/ProjectDiss/timeslist.rds")
saveRDS(mark_list,"D:/Uni Work/Year 4/Diss/Diss_R/ProjectDiss/mark_list.rds")
write_json(time_list, "D:/Uni Work/Year 4/Diss/Diss_R/ProjectDiss/timestamps.json")
write_json(mark_list, "D:/Uni Work/Year 4/Diss/Diss_R/ProjectDiss/marklist.json")

#Writing to folder for python script:
write_json(time_list, "C:/Users/joshh/GitFolder/NBAScalableMPP/timestamps.json")
write_json(mark_list, "C:/Users/joshh/GitFolder/NBAScalableMPP/marklist.json")
write_json(team_list, "C:/Users/joshh/GitFolder/NBAScalableMPP/teamlist.json")
write_json(zone_list, "C:/Users/joshh/GitFolder/NBAScalableMPP/zonelist.json")
write.csv(mod_pbp, "C:/Users/joshh/GitFolder/NBAScalableMPP/ModelData.csv")

###For Converting Between Numbers and Names: 
print(ModelData[c(1:5),c(5,6,7,10)])
print(nba_pbp[c(1:5),])
print(levels(as.factor(mod_pbp$ptype)))
print(mark_df)
write.csv(mark_df, "D:/Uni Work/Year 4/Diss/Diss_R/ProjectDiss/mark_df.csv")
mark_vec <- unname(unlist(as.vector(mark_df)))

###Raw Frequencies:
freq_vec22 <- c()
freq_vec23 <- c()
for(u in 0:41){
  indsum22 <- sum(ModelData[which(ModelData$season==2022),"ptype"]==u)
  indsum23 <- sum(ModelData[which(ModelData$season==2023),"ptype"]==u)
  freq_vec22 <- c(freq_vec22,indsum22)
  freq_vec23 <- c(freq_vec23,indsum23)
}
FrequencyTable <- data.frame(Mark = c(1:42), Freq22 = freq_vec22, Freq23 = freq_vec23)
FrequencyTable

