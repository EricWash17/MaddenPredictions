rm(list=ls())
gc()

setwd("C:/Users/Eric/OneDrive - North Carolina State University/Documents/Stats/Sports")
library(data.table)
require(dplyr)
library(plyr)
library(reshape2)
library(boot)
library(zoo)
library(glmnet)
library(rvest)
library(stringr)

## Table of Contents ##
# Line          # Section
# 26 -44        Removing injured players
# 45 - 109      Subsetting by players who actually play
# 110 - 130     Collapsing ratings by position
# 148 - 177     Cumulative Point Differential
# 190 - 220     Merging ratings and team data
# 223 - 239     Difference between home team ratings and away team ratings
# 250 - 264     Logit Model on pre 2017 data
# 265 - 290     Evaluating model on 2017 data
# 300 - 344     Simulations and evaluation of simulation
# 350 - 360     2018 Season
# 360 - 408     Scraping web for injuries then subsetting by injuries and playing time
# 410 - 463     Scraping team schedules 
# 464 - 499     Merging player and team data
# 507 - 560     Model and Simulation on 2018 Data
# 560 +         Tableau: Simulated win distribution by team, postseason odds, rosters




##########################
##                      ##
##    Player Ratings    ##
##                      ##
##########################

## Loading player ratings and injury data
mad <- fread('mad.csv')
#mad[, Position3 := ifelse(Position2 == "DB" | Position2 == "DL" | Position2 == "LB", "Defense", Position2 ) ] # alternative look at Def
inj <- fread('Injuries.csv')

# Merging ratings and injury data
new.mad <- left_join(mad[Year>2013], inj, by = c("Name","Team","Year"))
new.mad[is.na(new.mad)] <- 0 # Giving zeroes to all players who are not injured
new.mad = data.frame(new.mad)

## Removing injured players for each game
# col 7 to 22 show if a player was injured. Col 7 = game 1 and so on.
# Output Rows are player, by game & year. 
new.mad2 = data.frame()
for (i in 7:22){
  mad = new.mad[which(new.mad[,i] < 1), ]
  mad = data.table(mad)
  mad[, Game := i-6 ]
  new.mad2 = rbind(new.mad2, mad, fill = TRUE)
}

# Keeping necessary variables
new.mad = new.mad2[, c("Team", "Name", "OVR", "Position2", "Wins", "Year", "Game"), with=FALSE]

rm(list = c('inj','new.mad2'))

# Making a function to later replicate w/ 2018 data.
# Function selects players who play and then calculates mean rating by position.
PPers = function(x){
  
## Sorting Data by Year, Position and Overall Rating
  # to subset players who actually play.
x <- x[order(Team, Year, Game, Position2, -OVR),]

# Taking 1st QB i.e. QB sorted at the top bc highest rating.  
QB <- x[Position2=='QB', ]
invisible(data.table(QB))
QB1 <- QB[ , .SD[1], by=c('Team', 'Year', 'Game')]
invisible(data.table(QB1))

HB <- x[Position2=='HB', ]
invisible(data.table(HB))
HB1 <- HB[ , .SD[1], by=c('Team', 'Year', 'Game')]
invisible(data.table(HB1))

WR <- x[Position2=='WR', ]
invisible(data.table(WR))
WR1 <- WR[ , .SD[1:4], by=c('Team', 'Year', 'Game')]
invisible(data.table(WR1))

OL <- x[Position2=='OL', ]
invisible(data.table(OL))
OL1 <- OL[ , .SD[1:5], by=c('Team', 'Year', 'Game')]
invisible(data.table(OL1))

DL <- x[Position2=='DL', ]
invisible(data.table(DL))
DL1 <- DL[ , .SD[1:5], by=c('Team', 'Year', 'Game')]
invisible(data.table(DL1))

LB <- x[Position2=='LB', ]
invisible(data.table(LB))
LB1 <- LB[ , .SD[1:4], by=c('Team', 'Year', 'Game')]
invisible(data.table(LB1))

DB <- x[Position2=='DB', ]
invisible(data.table(DB))
DB1 <- DB[ , .SD[1:5], by=c('Team', 'Year', 'Game')]
invisible(data.table(DB1))

start <- rbind(QB1, HB1, WR1, OL1, DL1, LB1, DB1)
data.table(start)

## Combining positional subsets to form new data.table
start <- rbind(QB1, HB1, WR1, OL1, DL1, LB1, DB1)
data.table(start)

## Calculating the mean for each position by team, year, and game
start[, meanovr := mean(OVR), by=c("Position2","Team","Year","Game")]
round(start$meanovr, digits = 2)
start[ , c("Team", "Year", "Game","Position2","Wins","meanovr"), with = FALSE]
}

start = PPers(new.mad)

## Function to reshape from long to wide
teamrate = function(x){
## Mean rating by: Team, Year, Position, & Game
team.data = ddply(x, c("Team", "Position2", "Year","Game"), summarise, mean = mean(meanovr), Wins = mean(Wins))
team.data[, "mean"] = round(team.data$mean, digits = 2)
team.data = data.table(team.data)
team.data = team.data[team.data$mean >0]

## Reshaping data from long to wide
team.data <- dcast(team.data, Team + Game + Year + Wins ~ Position2, value.var = "mean")
team.data = data.table(team.data)
}

team.data = teamrate(start)

# Inputting ratings for 7 missing values, 5 of which are HB.
# Values missing bc team picked up FAs who were not originally on team.
min.hb <- min(na.omit(team.data$HB))
team.data[is.na(team.data)] <- min.hb

## Wins by Team by Year
keycols <- c("Team", "Year")
setkeyv(team.data, keycols)
wins = data.table(team.data[, mean(Wins), by = c('Team', 'Year') ])
colnames(wins)[3] <- "Wins"
wins[, sum(Wins), by = Year] # Making sure 256 for each year

########################################################################################################
########################################################################################################

##########################
##                      ##
##    Game Outcomes     ##
##                      ##
##########################

games <- fread('games.csv')
games = data.table(games)
games.2018 = games[ Year == 2018,]
games = games[ Year < 2018,]
games <- games[order(-Year, Team, Game),]

# Calculating rolling sum of point difference 
diff <- games[, .(y =Diff, roll = cumsum(Diff)), by = c("Team", "Year")]
diff = diff[,4]

# Calculating number of home games 
home <- games[, .(y =Home, roll = cumsum(Home)), by = c("Team", "Year")]
colnames(home)[4] <- "home.roll"
home = home[,4]

games = cbind(games, diff, home)

# Lagging above info. E.g. Week 1 pt difference will be on the the game 2 row <-- need for predicting. 
games[, adj.roll := ifelse(Game == 1, 0,lag(roll))]
games[, adj.home := ifelse(Game == 1, 0,lag(home.roll))]

games[, c('roll', 'home.roll') := NULL]
colnames(games)[9:10] <- c("roll","home.roll")

# Putting a week variables on the ratings data. Necessary for later merge.
wk = games[,c("Team", "Year", "Game","Week")]
team.data = inner_join(team.data,wk, by = c("Team", "Year", "Game"))
team.data = data.table(team.data)
team.data[, Game := NULL]

rm(list = c('wk','home','diff','i','keycols','start'))

######################################################################################
#####################################################################################

##########################
##                      ##
##    Merge Ratings     ##
##     & Outcomes       ##
##                      ##
##########################

big.table = function(t,g){
## Merge Ratings and Outcomes
comp <- inner_join(t, g, by = c("Team", "Year", "Week"))
comp = data.table(comp)

## Calculating Opponents ratings 
opp = t
colnames(opp)[1] <- "Opp"
g[, Opp := NULL]
colnames(g)[1] <- "Opp"

# Merging in Opp team rolling point differentials, etc 
opp <- inner_join(opp, g, by = c("Opp", "Year", "Week"))
opp = data.table(opp)
opp[, c("Diff","Home", "Wins", "Win","Game") := NULL]
colnames(opp)[which(names(opp) == "roll")] = "Opp.Roll"
colnames(opp)[which(names(opp) == "home.roll")] = "Opp.Home"

## Merging Team and Opponent data. In other words, forming match up data.
comp <- inner_join(comp, opp, by = c("Opp", "Year", "Week"))
comp = data.table(comp)
}

comp = big.table(team.data,games)

## Adjusting rolling point differential for number of previous home games. 
## All things being equal, EV[home team] = 3 points. Adjusting for that. 
comp = comp[, adj.roll := roll - 3*home.roll]
comp = comp[, adj.opp.roll := Opp.Roll - 3*Opp.Home]

## Seperating by home and road team. Will run models on home team, which will also have road team data.
home <- comp[Home == 1,]
away <- comp[Home != 1,]

# Calculating the difference between positions for each matchup 
diffs = function(h){
  h[ , DB.Diff := DB.x - DB.y]
  h[ , DL.Diff := DL.x - DL.y]
  h[ , HB.Diff := HB.x - HB.y]
  h[ , LB.Diff := LB.x - LB.y]
  h[ , OL.Diff := OL.x - OL.y]
  h[ , QB.Diff := QB.x - QB.y]
  h[ , WR.Diff := WR.x - WR.y]
#  h[, Cum.Diff := (roll - Opp.Roll) ] # Difference in rolling point differential
  h[, Adj.Cum := (adj.roll - adj.opp.roll) ] # Difference in adjusted point differential
}

home = diffs(home)
######################################################################################
#####################################################################################

##########################
##                      ##
##      Model           ##
##                      ##
##########################
# Training model predictions on pre 2017 data

home.2018 = home[ Year == 2018,] # 2018 data

home.2017 = home[ Year == 2017,] # Test data set
homeT = home[Year < 2017, ] # Training data set

# Logistic regression for each matchup.
master.reg = glm(Win ~  DB.Diff + DL.Diff + HB.Diff + LB.Diff + OL.Diff + QB.Diff + WR.Diff + Adj.Cum,
                 family=binomial(link='logit'), data = homeT)

summary(master.reg)

## Predicting win probability for each home team for each game
home.2017[ , phat := predict(master.reg, home.2017, type='response')]

## Root Mean Square Error Function
RMSE = function(x1, x2) {
  sqrt( mean( (x1-x2)^2 ) )
}

MAE = function(x1, x2) {
  mean( abs(x1-x2) )
}

# Evaluating Game Predictions
RMSE(home.2017$Win, home.2017$phat) #.467
home.2017[order(Week),RMSE(Win,phat), by = Week] # RMSE by week.Improves.

MAE(home.2017$Win, home.2017$phat) #.435

## Confusion  Matrix
DT = home.2017[ , c("Team", "Opp", "Win", "phat","Year", "Week"), with=FALSE]
glm.prob = predict(master.reg, home.2017,type='response')
glm.pred=rep(0, nrow(DT)) 
glm.pred[glm.prob>.5] = 1 
table(glm.pred)
table(glm.pred, DT$Win)
mean(glm.pred==DT$Win) #.617

######################################################################################
#####################################################################################

##########################
##                      ##
##      Simulation      ##
##                      ##
##########################
# Testing simulated predictions

# Simulating probabilities to determine win or loss
obs = nrow(DT)
sim = 1000
mat = data.frame(matrix(runif(obs*sim), obs, sim))

# Win = prob < phat
try = data.table(sapply(mat, function(x) ifelse(x < DT$phat, 1, 0)))
DT = cbind(DT,try) 

# Away Team results = opposite of home team results
DT.away =  1 - DT[, 7:1006]
DT.away = cbind(DT[,c("Opp", "Year")], DT.away)
colnames(DT.away)[1] <- "Team"

#Losing unneeded variables
DT = DT[, -c("Opp", "Week", "phat", "Win")]

# Combining home and away team results
DT = rbind(DT, DT.away)
DT = DT[order(Team,Year)]

# of Season wins by team
exam = DT %>% group_by(Team, Year) %>% summarise_each(funs(sum))
mn = apply(exam[3:1002],1, mean) # Average wins by team

exam = data.table(exam)
mn = data.table(mn)

exam = cbind(exam,mn)
exam = exam [, c("Team", "Year", "mn")]
DF = merge(exam,wins, by = c("Team", "Year"))

# Evaluating Predictions
RMSE(DF$mn,DF$Wins)

DF = DF[ ,diff := Wins - mn]

hist(DF$diff)

rm(list = c('DT', 'DF', 'home.2017','games', 'wins','team.data','mn','try','master.reg','mat','comp' ))

#####################################################
#####                    2018                  ######
#####################################################

## Getting and preparing mid-season 2018 data
ref = fread('abbrev.csv')
ref = ref[, Opp := Team]
week =function(x){ ifelse(x < "2017-09-14", "Week1",ifelse(x < "2017-09-21", "Week2",ifelse(x < "2017-09-28","Week3",ifelse(x < "2017-10-05", "Week4", 
                                                      ifelse(x < "2017-10-05", "Week5",ifelse(x < "2017-10-12", "Week6",ifelse(x < "2017-10-19", "Week7",
                                                      ifelse(x < "2017-10-26", "Week8",ifelse(x < "2017-11-02", "Week9",ifelse(x < "2017-11-09", "Week10",
                                                      ifelse(x < "2017-11-16", "Week11",ifelse(x < "2017-11-23", "Week12",ifelse(x < "2017-11-30", "Week13",
                                                      ifelse(x < "2017-12-07", "Week14",ifelse(x < "2017-12-14", "Week15",ifelse(x < "2017-12-21", "Week16","Week17"))))))))))))))))}


abv = as.list(ref$Ref)

all.tab = data.table()

for (i in abv) {
  injury = paste("https://www.pro-football-reference.com/teams/",i,"/2017_injuries.htm", sep = "")
  population <- injury %>%
    read_html() %>%
    html_nodes(xpath='//*[(@id = "div_team_injuries")]/table[1]') %>%  html_table()
  tab = data.table(population[[1]])
  nm = colnames(tab)
  dates = as.Date(strtrim(nm[2:length(nm)],5), "%m/%d")
  weeks = week(dates)
  colnames(tab) = append(nm[1],weeks)
  tab = tab[, Ref := i]
  all.tab = rbind(all.tab, tab, fill = TRUE)
}

all.tab = inner_join(all.tab,ref[,2:3], by = "Ref")
all.tab = data.table(all.tab)
all.tab[, Ref := NULL]
all.tab[all.tab=='' | all.tab=='Q'] <- 0
all.tab[all.tab=='D' | all.tab=='IR' | all.tab =='O'] <- 1
colnames(all.tab)[1] <- "Name"

# Removing injured players for each game
new.mad = data.frame(new.mad)

new.mad <- left_join(mad[Year==2018], all.tab, by = c("Name","Team"))
new.mad[is.na(new.mad)] <- 0 # Giving zeroes to all players who are not injured

new.mad2 = data.frame()
for (i in 7:ncol(new.mad)){
  mad18 = new.mad[which(new.mad[,i] < 1), ]
  mad18 = data.table(mad18)
  mad18[, Game := i-6 ]
  new.mad2 = rbind(new.mad2, mad18, fill = TRUE)
}

# Subsetting by players who play (function made above)
start = PPers(new.mad2)
#Collapsing by postion on each team
team.data = teamrate(start)
team.data = inner_join(team.data, games.2018[,c('Team','Week','Game')], by = c('Team','Game'))

## Point Differences
all.pts = data.table()
wk = ifelse(Sys.Date() >= "2017-11-16", as.numeric(str_sub(week(Sys.Date()),-2,-1))-1,
            as.numeric(str_sub(week(Sys.Date()),-1,-1))-1)

# Scarpping profootball reference.com's team schedules for each team.
for (i in abv) {
  pts = paste("https://www.pro-football-reference.com/teams/",i,"/2017_games.htm", sep = "")
  population <- pts %>%
    read_html() %>%
    html_nodes(xpath='//*[(@id = "div_games")]/table[1]') %>%  html_table()
  tab = data.table(population[[1]])
  tab =tab[ ,home := ifelse(V9 != "@",1,0) ]
  colnames(tab)[c(1,9:12)] = c("Week","Home","Full","Tm.pts","Opp.pts")
  tab = tab[2:wk, c(1,9:12) ]
  tab =tab[ ,Home := ifelse(Home != "@",1,0) ]
  tab = tab[, Diff := as.numeric(Tm.pts) - as.numeric(Opp.pts)]
  tab = tab[, Ref := i]
  all.pts = rbind(all.pts, tab)
}

# Merging in Opponent
all.pts = inner_join(all.pts, ref[,2:3],by = "Ref")
# Merging in Team name
all.pts = left_join(all.pts,ref[,5:6], by = "Full")
all.pts = data.table(all.pts)
# Getting rid of old team and opponent variables
all.pts = all.pts[, c("Ref", "Full") := NULL]
# Creating Win variable for Team
all.pts = all.pts[, Win := ifelse(Diff > 0, 1, ifelse(Diff <0 ,0,.5))]
all.pts = all.pts[, Week := as.numeric(Week)]
all.pts = na.omit(all.pts)

# Creating cumulative point differential
pt.roll <- all.pts[, .(y =Diff, roll = cumsum(Diff)), by = "Team"]
all.pts = cbind(all.pts, pt.roll)
all.pts = all.pts[, 9:10 := NULL]

# Calculating number of home games 
home.scr <- all.pts[, .(y =Home, roll = cumsum(Home)), by = "Team"]
colnames(home.scr)[3] <- "home.roll"
home.scr = home.scr[,3]
all.pts = cbind(all.pts, home.scr)

rm(list = c('pt.roll', 'home.scr', 'ref','i','population','tab','abv'))

# Merging in Game & Year
all.pts = left_join(games.2018[, c("Win","Diff") := NULL], all.pts[,c('Home','Opp') := NULL], by = c('Team', 'Week'))
all.pts = data.table(all.pts)
all.pts[,c('Tm.pts','Opp.pts') := NULL ]

# Combing Player Data and Team scoring data with Opponent's player and scoring data.
bt = big.table(team.data,all.pts)
colnames(bt)[2] = "Game"
bt = data.table(bt)
bt = bt[, c('Game.y.x', 'Game.x.y', 'Game.y.y') := NULL]

# Setting point differential for remaining games
# Taking the last point differential and merging it for remaining games
max.wk = all.pts[is.na(Diff) == FALSE, max(Week), by = "Team"]
colnames(max.wk)[2] = "Week"
recent = inner_join(max.wk,all.pts[, c("Team","Week","roll","home.roll")], by = c("Team", "Week"))
colnames(recent)[2:4] = c("Max.wk","Rec.Diff", "Max.home")

bt = inner_join(bt,recent, by = "Team")
bt = data.table(bt)
bt[, adj.roll := ifelse(Week == 1 | (Team == 'Buccaneers'& Week ==2) | (Team == 'Dolphins'& Week ==2), 0,lag(roll))]
bt[, adj.home := ifelse(Week == 1 | (Team == 'Buccaneers'& Week ==2) | (Team == 'Dolphins'& Week ==2), 0,lag(home.roll))]
bt[ , c('roll', 'home.roll') := NULL]
bt[, adj.roll := ifelse(as.numeric(Week) > as.numeric(Max.wk),Rec.Diff,adj.roll)]

colnames(recent) = c('Opp','Opp.Max.wk','Opp.Rec.diff','Opp.Max.home')
bt = inner_join(bt,recent, by = "Opp")
bt = data.table(bt)
bt = bt[order(Opp, Week)]

bt[, adj.opp.roll := ifelse(Week == 1 | (Opp == 'Buccaneers'& Week ==2) | (Opp == 'Dolphins'& Week ==2), 0,lag(Opp.Roll))]
bt[, adj.Opp.home := ifelse(Week == 1 | (Opp == 'Buccaneers'& Week ==2) | (Opp == 'Dolphins'& Week ==2), 0,lag(Opp.Home))]
bt[ , c('Opp.Roll', 'Opp.Home') := NULL]
bt[, adj.opp.roll := ifelse(as.numeric(Week) > as.numeric(Max.wk),Opp.Rec.diff,adj.opp.roll)]
bt[, c('Max.wk','Rec.Diff', 'Max.home','Opp.Max.wk','Opp.Rec.diff','Opp.Max.home') := NULL]

## Seperating by home and road team. Will run models on home team, which will also have road team data.
home.2018 <- bt[Home == 1,]
away.2018 <- bt[Home != 1,]

home.2018 = diffs(home.2018)
mean(home.2018$adj.opp.roll)

####################################################################
###################################################################

############################################
#############################################
###########################################
master.reg = glm(Win ~  DB.Diff + LB.Diff + DL.Diff + OL.Diff + WR.Diff + HB.Diff + QB.Diff + Adj.Cum,
                 family=binomial(link='logit'), data = home[Year <2018,])

summary(master.reg)

home.2018 = data.table(home.2018)
# Scoring 2018 data
home.2018[ , phat := ifelse(is.na(Win) == FALSE,Win, predict(master.reg, home.2018, type='response'))]
tab.home = home.2018[,c("Week","Team","Opp","phat","Home", "Win")]
# Home.2018 only has 8 games for each team. Making Away games compatitable to combine
tab.away =home.2018[,c("Week","Team","Opp","phat","Home", "Win")]
# Swapping Team and Opponent, changing home to show road game, and will then rbind
tab.away = tab.away[, Home := 0]
colnames(tab.away)[2:3] <- c("Opp","Team")
# Away team's p(winning) = 1 - home team's p(winning)
tab.away = tab.away[, Win := 1- Win]
tab.away = tab.away[, phat := ifelse(is.na(Win) == FALSE,Win,1- phat)]


tab = rbind(tab.home,tab.away)

tab = tab[order(Team, Week)]

#Writing to CSV to import into Tableau
write.csv(tab,"tab_schedule.csv")

rm(list = c('tab.away', 'tab.home', 'homeT','bt','master.reg','recent','max.wk','tab',
            'all.pts','away','home','all.tab'))

# Predicted value for each game order by home team.
DT.2018 = home.2018[ , c("Team", "Opp", "Win", "phat","Year", "Week"), with=FALSE]

# Building random matrix to "simulate" each game 1000 times.  
obs = nrow(DT.2018)
sim = 1000
mat = data.frame(matrix(runif(obs*sim), obs, sim))

# Win = prob < phat i.e. turning mat into 1's and 0's.
try = data.table(sapply(mat, function(x) ifelse(x < DT.2018$phat, 1, 0)))

# Binding simulated results with game info
DT.2018 = cbind(DT.2018,try)

# Away team's result is opposite of the home team's result.
# Want to make matrix with 16 results for each team.
DT.away =  1 - DT.2018[, 7:1006]
# Only keeping away team
DT.away = cbind(DT.2018[,c("Opp", "Year")], DT.away)
# Losing vars and changing var names to bind 
colnames(DT.away)[1] <- "Team"
DT.2018 = DT.2018[, -c("Opp", "Week", "phat", "Win")]
#Rbind away and home team results to form 512x1002 matirx
DT.2018 = rbind(DT.2018, DT.away)
DT.2018 = DT.2018[order(Team,Year)]

# Summarizing wins by team
exam = DT.2018 %>% group_by(Team, Year) %>% summarise_each(funs(sum))
# Averaging 1000 seasons
mn = apply(exam[3:1002],1, mean)

exam = data.table(exam)
mn = data.table(mn)
exam[,Year := NULL]
# Making Teamname the columns. Matrix is Simulated Seasons by Team
t.exam = t(exam)
t.exam = data.table(t.exam)
colnames(t.exam) <- exam$Team
t.exam = t.exam[-1,]
# Getting rid of the simulations and just keeping average
exam = cbind(exam,mn)
exam = exam [, c("Team", "mn")]

rm(list = c('DT.away','home.2018', 'wk', 'mat','start','try','mn' ))

## Creating a win record data set for Tableau
t.exam = data.table(t.exam)
name = rep("sim", 32)
colnames(t.exam) = name
# Binding first two teams
lt = t.exam[,1]  
df = t.exam[,2]
lt = rbind(lt,df)
#Binding all the teams
for (i in 3:32){
  dtry = t.exam[,i, with = FALSE]
  lt = rbind(lt,dtry)
}

# Each Team listed 1000 times
Team = data.table(rep(exam$Team,1000))
Team = Team[order(V1)]
# Cbinding those teams to create 32,000 simulated recored. Format good for Tableau
lt = cbind(lt, Team)
colnames(lt)<- c("Wins","Team")
# Id for each sumulation. Want to be able to link all records from same simulations
nums = data.table(rep(seq(1,1000,1),32))
colnames(nums) = "Sim"

# Wins, Team, Simulation
lt = cbind(lt, nums)

write.csv(lt, "tab_lt.csv")


#### Calculating playoff, etc. odds

# Defining Each Division
AFC.East = c("Patriots", "Dolphins", "Jets", "Bills")
AFC.North = c("Ravens", "Bengals", "Steelers", "Browns")
AFC.South = c("Texans", "Jaguars", "Colts", "Titans")
AFC.West = c("Chiefs", "Broncos", "Raiders", "Chargers")

NFC.East = c("Giants", "Eagles", "Redskins", "Cowboys")
NFC.North = c("Vikings", "Packers", "Lions", "Bears")
NFC.South = c("Buccaneers", "Saints", "Panthers", "Falcons")
NFC.West = c("49ers", "Seahawks", "Cardinals", "Rams")

lt = data.table(lt)
# Adding new column specifying each team's division in each siumlation
lt = lt[, Division := ifelse(Team %in% AFC.East,"AFC.East",
                               ifelse(Team %in% AFC.North,"AFC.North",
                                      ifelse(Team %in% AFC.South, "AFC.South",
                                             ifelse(Team %in% AFC.West, "AFC.West",
                                                    ifelse(Team %in% NFC.East, "NFC.East",
                                                           ifelse(Team %in% NFC.North, "NFC.North",
                                                                  ifelse(Team %in% NFC.South, "NFC.South","NFC.West")))))))]

# Adding new column that says each team's conference
lt = lt[, Conference := ifelse(Division %in% c("AFC.East","AFC.North","AFC.South", "AFC.West"), "AFC", "NFC")]

keycols <- c("Division", "Sim")
setkeyv(lt, keycols)

# Determing Rank by division based on wins, ! = first place
lt = lt[, Wins := as.numeric(Wins)]
lt = lt[, Div.Rank := frank(-Wins, ties.method = "random"), by = c("Division", "Sim")]
#Teams that did not win division
losers = lt[Div.Rank > 1, ]
losers = data.table(losers)

keycols <- c("Conference", "Sim")
setkeyv(lt, keycols)
# Who makes the playoffs? Got rid of division champs since they auto qualify
# Of the remaining team, the top two from each conference make the playoffs.
losers[, WildCard := frank(-Wins, ties.method = "random"), by = c("Conference", "Sim")]
losers = losers[, c("Team", "Sim", "WildCard")]
#Merging wildcard ranking into data
lt = left_join(lt, losers, by = c("Team", "Sim"))
lt = data.table(lt)
# Binary if team makes playoffs in each respective simulation. 12/32 teams (6 from each conf) make playoffs
lt = lt [ , Playoff := ifelse(Div.Rank == 1, 1, 
                                  ifelse(WildCard < 3,1,0))]
# Binary if team wins division in each respective simulation. 6 Divisions, 3 in each conf.
lt = lt[, Div.Champ := ifelse(Div.Rank == 1,1,0)]
# Team with most wins in each conference gets homefield. 2 conferences.
lt = lt[,HomeField := frank(-Wins, ties.method = "random"), by = c("Conference", "Sim") ]
lt = lt[, HomeField := ifelse(HomeField == 1,1,0)]
# Averaging all the binaries described above for each category by team 
homefield = data.table(lt[, mean(HomeField), by = Team])
colnames(homefield)[2] <- "HomeField"
playoff = data.table(lt[, mean(Playoff), by = Team])
colnames(playoff)[2] <- "Playoffs"
division = data.table(lt[, mean(Div.Champ), by = Team])
colnames(division)[2] <- "Division"

prob = inner_join(playoff, division, by = "Team")
prob = inner_join(prob, homefield, by = "Team")

write.csv(prob, "tab_prob.csv")

## Calculating average rating by position and using to center team position ratings 
team.data = data.table(team.data)
tab.team = team.data[Year == 2018 & Week == 4,-c("Wins", "Year")]
mn =colMeans(tab.team[, 3:9])
mn = t(matrix(mn,7,32))
binds = tab.team[, 3:9] - mn

colSums(binds)
tab.team = cbind(tab.team[,c("Team", "Week")], binds)

write.csv(tab.team,"tab.team.csv")

