rm(list=ls())
gc()

setwd("C:/Users/Eric/OneDrive - North Carolina State University/Documents/Stats/Sports")
library(data.table)

mad <- fread('mad.csv')
summary(mad$Year)


## Sorting Data by Year, Position and Overall Rating
mad <- mad[order(Team, Year, Position2, -OVR),]
data.table(mad)
head(mad)

### Subsetting by position for players who actually play
QB <- mad[Position2=='QB', ]
invisible(data.table(QB))
QB1 <- QB[ , .SD[1], by=c('Team', 'Year')]
invisible(data.table(QB1))

HB <- mad[Position2=='HB', ]
invisible(data.table(HB))
HB1 <- HB[ , .SD[1:2], by=c('Team', 'Year')]
invisible(data.table(HB1))

WR <- mad[Position2=='WR', ]
invisible(data.table(WR))
WR1 <- WR[ , .SD[1:5], by=c('Team', 'Year')]
invisible(data.table(WR1))

OL <- mad[Position2=='OL', ]
invisible(data.table(OL))
OL1 <- OL[ , .SD[1:6], by=c('Team', 'Year')]
invisible(data.table(OL1))

DL <- mad[Position2=='DL', ]
invisible(data.table(DL))
DL1 <- DL[ , .SD[1:5], by=c('Team', 'Year')]
invisible(data.table(DL1))

LB <- mad[Position2=='LB', ]
invisible(data.table(LB))
LB1 <- LB[ , .SD[1:4], by=c('Team', 'Year')]
invisible(data.table(LB1))

DB <- mad[Position2=='DB', ]
invisible(data.table(DB))
DB1 <- DB[ , .SD[1:5], by=c('Team', 'Year')]
invisible(data.table(DB1))

## Combining positional subsets to form new data.table
start <- rbind(QB1, HB1, WR1, OL1, DL1, LB1, DB1)
data.table(start)

## Calculating the mean for each position by team and year
start[, meanovr := mean(OVR), by=c("Position2","Team","Year")]
round(start$meanovr, digits = 2)

rm(list = c('QB1', 'HB1', 'WR1', 'OL1', 'DL1', 'LB1','DB1','QB', 'HB', 'WR', 'OL', 'DL', 'LB','DB' ))


## Collapsing data to only show team level data
library(plyr)

team.data = ddply(start, c("Team", "Position2", "Year"), summarise, mean = mean(meanovr), Wins = mean(Wins))
team.data[, "mean"] = round(team.data$mean, digits = 2)
team.data = data.table(team.data)

## Reshaping data from long to wide
library(reshape2)
team.data <- dcast(team.data, Team + Year + Wins ~ Position2, value.var = "mean")
team.data = data.table(team.data)

## Analysis
library(boot)
master.reg = glm(Wins ~. -Team -Year, family = gaussian, data = team.data[Year<2017,])
summary(master.reg)
1-(master.reg$deviance/master.reg$null.deviance) ## Adj R^2
# Diagnostics
plot(master.reg,caption = list("Residuals vs Fitted", "Normal Q-Q",
                               "Scale-Location", "Cook's distance",
                               "Residuals vs Leverage"))

# Leave one out Cross Vaidation
master.cv = cv.glm(team.data[Year<2017,], master.reg)
sqrt(master.cv$delta)
### 2.72

master.reg2 = glm(Wins ~. -HB -OL -WR -Team -Year, family = gaussian, data = team.data[Year<2017,])
summary(master.reg2)
1-(master.reg2$deviance/master.reg2$null.deviance)
# Diagnostics
plot(master.reg,caption = list("Residuals vs Fitted", "Normal Q-Q",
                               "Scale-Location", "Cook's distance",
                               "Residuals vs Leverage"))

# Leave one out Cross Vaidation
master.cv2 = cv.glm(team.data[Year<2017,], master.reg2)
sqrt(master.cv2$delta)
###2.67

sd(team.data[Year<2017,]$Wins)
###3.069407



## Creating data.table for 2017
team.2017 = team.data[Year == 2017, ]
#team.data.2017 = team.data.2017[,Wins := NULL]

## Creating data.table for 2018
team.2018 = team.data[Year == 2018, ]


# Predicting 2017 Wins for each team
team.2017 = team.2017[, phat := predict.glm(master.reg, team.2017)]
team.2017 = team.2017[, phat2 := predict.glm(master.reg2, team.2017)]


### Normalizing predictions for win total by year
## In other words, making sum(phat, by.(year)) == 256
#team.data = team.data[, phat := predict.glm(master.reg, team.data)]
#team.data = team.data[, phat2 := predict.glm(master.reg2, team.data)]

team.2017[, sums := sum(phat), by=.(Year)]
team.2017[, sums2 := sum(phat2), by=.(Year)]


## Adjusting predictions
team.2017[, phat.x := phat*256/sums]
team.2017[, phat.y := phat2*256/sums]

## deleting the sums columns
team.2017[, sums := NULL]  
team.2017[, sums2 := NULL]

## Making RMSE function
RMSE = function(x1, x2) {
  sqrt( mean( (x1-x2)^2 ) )
}

AMSE = function(x1,x2){
  mean(abs(x1-x2))
}

### RMSE for adjusted predictions
#All positions
RMSE(team.2017$Wins, team.2017$phat.x)
## 2.5
AMSE(team.2017$Wins, team.2017$phat.x)



## Parsimonious model
RMSE(team.2017$Wins, team.2017$phat.y)
##2.52

team.2017 = team.2017[, Error := phat.y - Wins]

write.csv(team.2017, file = "team2.2017.csv")


master.reg = glm(Wins ~ DB + LB + DL + OL + WR + HB + QB, family = gaussian, data = team.data[Year<2018,])
summary(master.reg)


# Predicting 2018 Wins for each team
team.2018 = team.2018[, phat := predict.glm(master.reg, team.2018)]


team.2018[, sums := sum(phat), by=.(Year)]


## Adjusting predictions
team.2018[, phat.x := phat*256/sums]

## deleting the sums columns
team.2018[, sums := NULL]  


library(stargazer)
library(leaps)

stargazer(master.reg, type='latex',out="slide_5_regression.txt", align = TRUE,
          dep.var.labels = "Madden Ratings & Total Wins",
          covariate.labels = c('Secondary','Linebackers','D Line','O Line','Receivers','Running Back','Quarterback', 'Intercept'),
          omit.stat = c('ll','f'))

summary(master.reg)

#### Assigning Conference and Division
AFC.East = c("Patriots", "Dolphins", "Jets", "Bills")
AFC.North = c("Ravens", "Bengals", "Steelers", "Browns")
AFC.South = c("Texans", "Jaguars", "Colts", "Titans")
AFC.West = c("Chiefs", "Broncos", "Raiders", "Chargers")

NFC.East = c("Giants", "Eagles", "Redskins", "Cowboys")
NFC.North = c("Vikings", "Packers", "Lions", "Bears")
NFC.South = c("Buccaneers", "Saints", "Panthers", "Falcons")
NFC.West = c("49ers", "Seahawks", "Cardinals", "Rams")

team.2018 = team.2018[, Division := ifelse(Team %in% AFC.East,"AFC.East",
                             ifelse(Team %in% AFC.North,"AFC.North",
                                    ifelse(Team %in% AFC.South, "AFC.South",
                                           ifelse(Team %in% AFC.West, "AFC.West",
                                                  ifelse(Team %in% NFC.East, "NFC.East",
                                                         ifelse(Team %in% NFC.North, "NFC.North",
                                                                ifelse(Team %in% NFC.South, "NFC.South","NFC.West")))))))]

team.2018 = team.2018[, Conference := ifelse(Division %in% c("AFC.East","AFC.North","AFC.South", "AFC.West"), "AFC", "NFC")]
