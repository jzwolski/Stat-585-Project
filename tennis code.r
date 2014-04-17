###Libraries###
library(XML)
library(plyr)
library(stringr)
library(scrapeR)
library(ggplot2)

###Create table of rankings for the last 20 years###

#Indices:
#11(2013), 57(2012), 104(2011), 150(2010), 202(2009)
#254(2008), 306(2007), 359(2006), 411(2005), 463(2004)
#These will change each week as the rankings are updated weekly.

url <- "http://www.atpworldtour.com/Rankings/Singles.aspx"
doc <- htmlParse(url)
root <- xmlRoot(doc)
years <- getNodeSet(root, "//option[@value]")
result <- ldply(years, function(y) xmlAttrs(y)["value"]) [[1]] [c(13,59,106,152,204,256,
                                                                  308,361,413,465,517,569,
                                                                  621,674,725,777,829,881,
                                                                  934,986)]
result

base <- "http://www.atpworldtour.com/Rankings/Singles.aspx?d="
urls <- paste0(base, rev(result))

rankings <- ldply(as.list(result), function(y) {
  result2 <- as.data.frame(readHTMLTable(paste0(base, y)) [[1]])
  result2$year <- y
  result2
})

names(rankings)[4] <- "Tourn Played"
names(rankings)[3] <- "Week Change"
names(rankings)[2] <- "Points"
names(rankings)[1] <- "Name"

rankings <- subset(rankings, Points != "Points")

rankings$Name <- gsub('[0-9]{1,}\\r\\n\\t\\t\\t\\t\\t\\t\\t\\t\\r\\n\\t\\t\\t\\t\\t\\t\\t\\t\\t', '', rankings$Name)

rankings$LastName <- gsub(',.*', '', rankings$Name)

rankings$FirstName <- gsub('.*,', '', rankings$Name)
rankings$FirstName <- gsub("\\(.*", '', rankings$FirstName)
rankings$FirstName <- substr(rankings$FirstName, 2, nchar(rankings$FirstName)-1)

rankings$Country <- gsub(".*\\(", '', rankings$Name)
rankings$Country <- gsub("\\)", '', rankings$Country)

rankings <- rankings[c(-1)]

rankings$PlayerID <- paste(rankings$FirstName, rankings$LastName, sep=", ")
rankings$PlayerID <- gsub(", ", " ", rankings$PlayerID)

rankings <- rankings[c(8, 6, 5, 7, 4, 1, 2, 3)]

names(rankings)[5] <- "Date"

rankings


###Get links for the players in the rankings table###
getLinks = function() { 
  links = character() 
  list(a = function(node, ...) { 
    links <<- c(links, xmlGetAttr(node, "href"))
    node 
  }, 
  links = function()links)
}

ranklinks <- c("http://www.atpworldtour.com/Rankings/Singles.aspx?d=30.12.2013&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=31.12.2012&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=26.12.2011&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=27.12.2010&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=28.12.2009&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=29.12.2008&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=31.12.2007&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=25.12.2006&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=26.12.2005&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=27.12.2004&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=29.12.2003&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=30.12.2002&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=31.12.2001&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=25.12.2000&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=27.12.1999&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=28.12.1998&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=29.12.1997&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=30.12.1996&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=25.12.1995&r=1&c=#",
               "http://www.atpworldtour.com/Rankings/Singles.aspx?d=26.12.1994&r=1&c=#")

Final = NULL
for (j in 1:20)
{
  #Find all links on the rankings page#
  h1<-getLinks()
  htmlTreeParse(file = ranklinks[j],
                handlers = h1, useInternalNodes = TRUE)
  #h1$links()
  
  #Subset the links to only links for the players#
  files.1<-h1$links()[str_detect(h1$links(), "/Tennis/Players")]
  #head(files.1)
  
  #Add an "a" to remove multiple links for the same player#
  files.2 <- paste(files.1,"a",sep="")
  #head(files.2)
  files.3 <- files.2[str_detect(files.2, ".aspxa")]
  #head(files.3)
  
  files.4<-str_replace_all(files.3, ".aspxa", ".aspx")
  #head(files.4)
  
  #Add the beginning of the url to get the full website address#
  files <- paste("http://www.atpworldtour.com",files.4,sep="")
  #files
  
  #Scrape the website for all players in top 100 for 30.12.2013#
  df = NULL
  for (i in 1:100)
  {
    url <- files[i]
    doc <- scrape(url)[[1]]
    
    #Specify the path to the table to get the labels of the stats
    label.1 <-  getNodeSet(doc, path='//*[@id="playerBioInfoList"]/li/span/text()')
    #label.1
    
    label.2 <- as.character(sapply(label.1,xmlValue))
    #label.2
    
    #Remove the " : "
    stat.name <- gsub(":","",label.2)
    #stat.name
    
    #Specify the path to get the values of the stats
    value.1 <- getNodeSet(doc, path='//*[@id="playerBioInfoList"]/li/text()')
    #value.1
    
    value <- as.character(sapply(value.1,xmlValue))
    #value
    
    #Create a player name column
    player.name.1 <-  getNodeSet(doc, path='//*[@id="playerBioInfoCardHeader"]/h1/text()')
    #player.name.1
    
    player.name <- as.character(sapply(player.name.1,xmlValue))
    #player.name
    
    #Combine the labels and stats for the player
    table <- data.frame(player.name,stat.name, value)
    #table
    table <- table[which(table$stat.name==c("Height","Weight"),]
    table$Height = as.numeric(substring(gsub(".*\\(", '', as.character(table$value)), 1, 3))/2.54
    
    if(i == 1){
      df = table[,-c(2:3)]
    } else {
      df = rbind(df, table[,-c(2:3)])
    }
  }
  if(j == 1){
    Final = df
  } else {
    Final = rbind(Final, df)
  }
}


###Merge the rankings and Final table###

TennisData <- cbind(rankings, Final)

TennisData <- TennisData[c(-9)]

TennisData$Date <- as.factor(TennisData$Date)

TennisData$Date <- factor(TennisData$Date, levels(TennisData$Date)[c(4,1,15,12,10,7,2,18,16,
                                                                     13,8,5,3,19,14,11,9,6,
                                                                     20,17)])

class(TennisData$Date)

mean(TennisData$Height)

Height2 <- tapply(TennisData$Height, TennisData$Date, mean)

Date2 <- unique(TennisData$Date)
Date2 <- factor(Date2, levels(Date2)[c(20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)])
Date2 <- as.factor(seq(1994,2013,by=1))

aggregate(Height~Date, TennisData, mean)
aggregate(Height~Date, TennisData, sd)
aggregate(Height~Date, TennisData, max)
aggregate(Height~Date, TennisData, min)
aggregate(Height~Date, TennisData, median)
qplot(data=NULL, Date2, Height2) + geom_smooth()

#Write to a CSV file
write.csv(TennisData, file="TennisData.csv")