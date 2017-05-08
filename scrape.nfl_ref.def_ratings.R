
setwd("~/Desktop/R/scraping_NFL")
library(XML)

def_df=as.data.frame(matrix(ncol=14))

names(def_df)=c("Team","W","L","T","W-L%","PF","PA","PD", 
        "MoV","SoS","SRS","OSRS","DSRS", "year")

urlpart1="http://www.pro-football-reference.com/years/"
urlpart3="/"

#starts at about line 129 of html

#this scrapes AFC data
getData= function (urlpart1, urlpart3){
        for (i in 2010:2014){
                URL= paste(urlpart1, i, urlpart3, sep="")
                tablefromURL=readHTMLTable(URL)
                table=tablefromURL[[2]]
                names(table)=c("Team","W","L","T","W-L%","PF","PA","PD", 
                               "MoV","SoS","SRS","OSRS","DSRS")
                table$year=i
                def_df=rbind(table, def_df)
        }
        return (def_df)
}

def_df2= getData(urlpart1, urlpart3)
dim(def_df)#101 rows x 14 cols

save(def_df, file ="nfl_def.dat")

load("nfl_def.dat")
head(def_df2)
tail(def_df)
str(def_df)


#===================
#scraping NFC data
#==================
def_df2=as.data.frame(matrix(ncol=14))

names(def_df2)=c("Team","W","L","T","W-L%","PF","PA","PD", 
                "MoV","SoS","SRS","OSRS","DSRS", "year")

urlpart1="http://www.pro-football-reference.com/years/"
urlpart3="/"
getData= function (urlpart1, urlpart3){
        for (i in 2010:2014){
                URL= paste(urlpart1, i, urlpart3, sep="")
                tablefromURL=readHTMLTable(URL)
                table=tablefromURL[[3]]
                names(table)=c("Team","W","L","T","W-L%","PF","PA","PD", 
                               "MoV","SoS","SRS","OSRS","DSRS")
                table$year=i
                def_df2=rbind(table, def_df2)
        }
        return (def_df2)
}

def_df3= getData(urlpart1, urlpart3)

head(def_df3, n=100)

df_NFC=def_df3[!def_df3$Team %in% c("AFC East", "AFC North", "AFC West", "AFC South", "NFC East", 
                               "NFC North", "NFC West", "NFC South"),]



df_NFC2<-na.omit(df_NFC)
df_NFC2$Team

Dallas Cowboys*      Philadelphia Eagles  New York Giants      Washington Redskins 
Green Bay Packers*   Detroit Lions+       Minnesota Vikings    Chicago Bears       
Carolina Panthers*   New Orleans Saints   Atlanta Falcons      Tampa Bay Buccaneers
Seattle Seahawks*    Arizona Cardinals+   San Francisco 49ers  St. Louis Rams  
#======================
#cleaning data set
#======================

#to delete conference names
df_AFC=def_df[!def_df$Team %in% c("AFC East", "AFC North", "AFC West", "AFC South", "NFC East", 
        "NFC North", "NFC West", "NFC South"),]

df_AFC2<-na.omit(df_AFC)
df_AFC2$Team
New England Patriots Buffalo Bills         Miami Dolphins        New York Jets        
Pittsburgh Steelers  Cincinnati Bengals   Baltimore Ravens+     Cleveland Browns     
Indianapolis Colts   Houston Texans        Jacksonville Jaguars  Tennessee Titans     
Denver Broncos       San Diego Chargers    Kansas City Chiefs    Oakland Raiders 


str(df_AFC)

#=====================
#joining and chnaging names and consolidating
#=====================

df_NFL<- rbind(df_AFC2, df_NFC2)


dim(df_NFL)#160x14 with 1001 obs

df_NFL$Team<-gsub("[\\*+]", "", df_NFL$Team)

df_NFL
"GB", "MIA", "NYJ", "NWE", "SEA", "ARI" , "BUF", "PHI", "WAS", "DAL",
  "ATL", "BLT", "CAR", "CHI", "CIN", "CLE", "Den", "DET", "HOU", "IND",
  "JAX", "KC", "MIN", "NOR", "NYG", "OAK", "PIT", "SD", "SF", "TB",
  "TEN", "STL"



df_NFL$Team<-factor(df_NFL$Team, levels=c("New England Patriots", "Buffalo Bills", "Miami Dolphins","New York Jets",         
        "Pittsburgh Steelers", "Cincinnati Bengals", "Baltimore Ravens", "Cleveland Browns", "Indianapolis Colts",
        "Houston Texans", "Jacksonville Jaguars", "Tennessee Titans", "Denver Broncos", "San Diego Chargers", 
        "Kansas City Chiefs", "Oakland Raiders" , "Dallas Cowboys", "Philadelphia Eagles",  "New York Giants",      
        "Washington Redskins","Green Bay Packers", "Detroit Lions", "Minnesota Vikings", "Chicago Bears",       
        "Carolina Panthers",  "New Orleans Saints",  "Atlanta Falcons", "Tampa Bay Buccaneers", "Seattle Seahawks",
        "Arizona Cardinals", "San Francisco 49ers", "St. Louis Rams" ), 
        labels = c("NWE","BUF","MIA", "NYJ", "PIT","CIN", "BAL", "CLE","IND","HOU", "JAX", "TEN", "Den",  "SD", "KC",
                   "OAK", "DAL", "PHI", "NYG", "WAS",  "GB", "DET",  "MIN", "CHI", "CAR", "NOR",  "ATL","TB",  "SEA", 
                   "ARI" , "SF",  "STL"))

df_NFL
dim(df_NFL)  #160x14
###how to sort by two columns?????????
###attempt 1
#<-c("Team")
#df_NFL[do.call("order", df_NFL[sortnames]),]

###sort_train_df<-train_df[order(train_df$activity),]
#attempt 2
##df_NFL[order(df_NFL[,1],df_NFL[,2]),]

write.csv(df_NFL, file ="df_NFL.csv")
load("def.rank.df.csv")
