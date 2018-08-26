Quaterbacks <- read_csv("~/Downloads/Game_Logs_Quarterback.csv.zip")
Quaterbacks <- Quaterbacks[,-1]
Quaterbacks <- Quaterbacks[,-2]

Quaterbacks_reg <- Quaterbacks[which(Quaterbacks$Season == 'Regular Season'),]
Quaterbacks_reg <- Quaterbacks_reg[which(Quaterbacks_reg$`Games Played` == '1'),]
Quaterbacks_reg_2 <- Quaterbacks_reg[-which(Quaterbacks_reg$`Passes Completed` == '--'),]
Quaterbacks_reg_2 <- Quaterbacks_reg_2[-which(Quaterbacks_reg_2$`Rushing Attempts` == '--'),]
Quaterbacks_reg_2 <- Quaterbacks_reg_2[-which(Quaterbacks_reg_2$`Fumbles` == '--'),]

Quaterbacks_reg_2$Name <- as.factor(Quaterbacks_reg_2$Name)
name_grouped <- group_by(Quaterbacks_reg_2, Name)
name_grouped <- summarise(name_grouped,
                          Fatalities_total=sum(FATALITIES, na.rm = TRUE),
                          Fatalities_mean=mean(FATALITIES, na.rm = TRUE),
                          Injuries_total=sum(INJURIES, na.rm = TRUE),
                          Injuries_mean=mean(INJURIES, na.rm = TRUE))


p <- ggplot(Quaterbacks_reg_2, aes(Name, Rushing Attempts))
p + geom_bar()


#### Data scraping from NFL website - PASSING

library(XML)
# Webscrape the Quater back Passing stats from NFL.com
passing <- "http://www.nfl.com/stats/categorystats?tabSeq=1&statisticPositionCategory=QUARTERBACK&qualified=true&season=2017&seasonType=REG"
quaterbacktable = readHTMLTable(passing, header=F, which=1,stringsAsFactors=F) 

quaterbacktable$standing <- 0

# Name the table columns
colnames(quaterbacktable) <- c("Rk","Player","Team","Pos","Comp","Att","Pct","AttperG","Yds",
"Avg","YdsperG","TD","Int","1st","1st%","Lng","20+","40+","Sck","Rate")

# Remove the commas from Yard Variable
quaterbacktable$Yds<- as.numeric(gsub(",", "", quaterbacktable$Yds))
quaterbacktable$YdsperG <- as.numeric(quaterbacktable$YdsperG)

```
# Order players by no. yards
quaterbacktable$Player <- factor(quaterbacktable$Player, levels = quaterbacktable$Player[order(-quaterbacktable$Yds)])

# Plot the top 20 Quaterbacks by Passing Yards
# Fill bars by yards per game to show consistency.
top20 <- quaterbacktable[1:20,]
p <- ggplot(top20, aes(x=Player, y=Yds, fill=YdsperG))
p + geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
````
```{r}
# Order players by no. yards
quaterbacks$Player <- factor(quaterbacks$Player, levels = quaterbacks$Player[order(-quaterbacks$Yds)])

# Plot the top 20 Quaterbacks by Passing Yards
# Fill bars by yards per game to show consistency.
top15 <- quaterbacks[1:20,]
p <- ggplot(quaterbacks, aes(x=Player, y=Yds, fill=Salary))
p + geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
```
