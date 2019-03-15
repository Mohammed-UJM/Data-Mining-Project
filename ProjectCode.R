########################################################################################################
########################################################################################################
####################################### Hello Mr MUHLENBACH  ###########################################

########## before starting I will install and load some packages that I will use in my analysis :

library(gdata)
library(ggplot2)
library(fmsb)
library(gbm)

                                ########## DATA PREPARATION : ###########

########## The first thing I will do is to upload the data in R :

  # votes table :
votes = read.csv("votes.csv")

  # comments table :
comments = read.csv("comments_clean_anonimized.csv")

  # likes on comments table :
likes = read.csv("commentInteractions.csv")

  # churn table :
churn = read.csv("churn.csv")

########## I will store the tables in some variables to see the different after cleaning it :

votes_origin = votes
likes_origin = likes
comments_origin = comments
churn_origin = churn

########## After that I will clean the data and delete what to delete, in this part, I'm going to get inspired by what Jose did because this part is very important for the next steps, but I will add some manipulations that he does not do, like cleaning up NA entries:

  # likes tables :

likes = likes[likes$companyAlias!="58a728a",]
likes = droplevels(likes)
levels(likes$companyAlias) = c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","V","W","X","Y","Z","AA","AB","AC","AD","AE","AF","AG","AH","AI","AJ")
likes$companyAlias = as.factor(likes$companyAlias)
likes$uid = paste(likes$employee,likes$companyAlias)

  # comments table :

comments = unique(comments)
# rename as in prev version
colnames(comments)[7] <- c("date")
colnames(comments)[6] <- c("nolike")
colnames(comments)[5] <- c("like")
colnames(comments)[4] <- c("txt")
colnames(comments)[3] <- c("commentid")
colnames(comments)[2] <- c("coa")
colnames(comments)[1] <- c("id")
comments <- unique(comments)

# remove those companies that has low numbers and no comments
comments <- comments[comments$coa!="5474b9cde4b0bf7614b2c66f",] 
comments <- comments[comments$coa!="58bf03e5cff4fa0004dd44ef",]
comments <- droplevels(comments)
levels(comments$coa) <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","V","W","X","Y","Z","AA","AB","AC","AD","AE","AF","AG","AH","AI","AJ","AK")

# parse dates :
comments$date <- gsub("CEST", "", comments$date)
comments$date <- gsub("CET", "", comments$date)
comments$date <- gsub("  ", " ", comments$date)
comments$date <- as.Date(comments$date, format = "%a %b %d %H:%M:%S %Y")
comments$txt <- as.character(comments$txt)
comments$lcomment <- unlist(lapply(comments$txt,nchar) )

# make unique id for all employees
comments$uid <- paste(comments$id,comments$coa)

  # votes table :

# remove those companies that has low numbers and no comments
votes<-votes[votes$companyAlias!="5474b9cde4b0bf7614b2c66f",]
votes<-votes[votes$companyAlias!="58bf03e5cff4fa0004dd44ef",]
votes<-votes[votes$companyAlias!="573a0671b5ec330003add34a",]
votes <- droplevels(votes)
votes$companyAlias <- factor(votes$companyAlias)
levels(votes$companyAlias) <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","V","W","X","Y","Z","AA","AB","AC","AD","AE","AF","AG","AH","AI","AJ","AK")
votes$uid <- paste(votes$employee,votes$companyAlias)

# parse dates
# votes$dt <- as.POSIXct((votes$voteDate), origin="1970-01-01")
votes$voteDate<- gsub("CEST", "", votes$voteDate)
votes$voteDate<- gsub("CET", "", votes$voteDate)
votes$voteDate<- gsub("  ", " ", votes$voteDate)
votes$voteDate<- as.Date(votes$voteDate, format = "%a %b %d %H:%M:%S %Y")

  # churn table :

churn<-churn[churn$companyAlias!="5474b9cde4b0bf7614b2c66f",] # remove this company that has low numbers and no comments
churn<-churn[churn$companyAlias!="58bf03e5cff4fa0004dd44ef",] # remove this company that has low numbers and no comments
churn<-churn[churn$companyAlias!="573a0671b5ec330003add34a",] # remove this company that has low numbers and no comments
churn <- droplevels(churn)
levels(churn$companyAlias) <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","V","W","X","Y","Z","AA","AB","AC","AD","AE","AF","AG","AH","AJ")

# parse dates
churn$lastParticipationDate<- gsub("CEST", "", churn$lastParticipationDate)
churn$lastParticipationDate<- gsub("CET", "", churn$lastParticipationDate)
churn$lastParticipationDate<- gsub("  ", " ", churn$lastParticipationDate)
churn$lastParticipationDate<- as.Date(churn$lastParticipationDate, format = "%a %b %d %H:%M:%S %Y")
churn[is.na(churn$lastParticipationDate),]
churn<- na.omit(churn)
max(churn$lastParticipationDate)
min(churn$lastParticipationDate)

# parse churn
churn$stillExists<- gsub("true", "1", churn$stillExists)
churn$stillExists<- gsub("false", "0", churn$stillExists)
churn$stillExists<- as.numeric(churn$stillExists)
mean(churn$stillExists)
churn$churn = 1 - churn$stillExists
sum(churn$churn)

churn$uid <- paste(churn$employee, churn$companyAlias)

# remove duplicated and negative id users from churn
churn = churn[!duplicated(churn$uid), ]
churn = churn[!(churn$employee<1), ]

########## After that, I will clean the tables from NA entries :

likes = na.omit(likes)
comments = na.omit(comments)
votes = na.omit(votes)
churn = na.omit(churn)

########## after that I will see to what look like my tables :

votes_origin[1:6,]
votes[1:6,]
likes_origin[1:6,]
likes[1:6,]
comments_origin[1:6,]
comments[1:6,]
churn_origin[1:6,]
churn[1:6,]

                                ########## MODELING AND EVALUATION : ###########

# In this two parts of the analysis, I will do some maipulations on data and each time I will give 
# some explanations to what I obtain (this explanations can be false of course ... some times, but it
# reflect my understanding of the data)

########## The first thing I will do is some descriptive statistics : 

###### How much hapiness there is by company :

happinessMean <- aggregate(vote ~ companyAlias, votes, function(x) mean(x))
colnames(happinessMean)<- c("Company","MeanHappyIndex")
happinessMean

# the same result but this time ordred by decreaseing mean
happinessMean <- happinessMean[with(happinessMean, order(-MeanHappyIndex, Company)), ]
#summary(happinessMean)
happinessMean # the mean notations are from 2.48 to 3.5

# total mean of votes :
mean(votes[,4]) # 2.86 
sd(votes[,4])   # 0.98

# the distribution of notations :
barplot(table(votes[,4])) 
# most employee have 3 as vote rate, the second position is for 4 and after
# that we have 1 and 2

df = data.frame(table(votes[,4]))
df[,2] = df[,2] / sum(df[,2]) * 100
df 

bp<- ggplot(df, aes(x="Votes", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie # almost the half of notations has 3 as value the same result but with a plot (camenbert form)

###### Some histograms that describe the data :

# active users per day : 
ggplot(votes[votes$voteDate>max(votes$voteDate)-380,], aes(voteDate,fill=companyAlias))+   geom_bar(alpha = .95) + labs(title = "Active users per day")
# the number of active users increase through time, for some companies the number of active users was 0
# before Oct2016 like AA to AI

# didtribution of hapiness seen differently by company :
ggplot(votes, aes(vote,fill=companyAlias)) +  geom_bar(alpha = 1) + labs(title = "Distribution of happiness")
# the same as before but this time with companies alias that show witch company is best rated

# distribution of happiness by day of week and by company : 
votes$weekday <- weekdays(votes$voteDate)
ggplot(votes[votes$voteDate>max(votes$voteDate)-380,], aes(weekday,fill=companyAlias))+   geom_bar(alpha = .95)
# we can see here somthing weird, the number of votes in the weekend (Saturday and Sunday) is very 
# low compared to the rest of the week, maybe because the weekend is for rest or when they are at 
# work employees encounter situations that remind them the vote ...

# happiness / weekday distribution ?

hbyw <- aggregate(vote ~ weekday, votes, function(x) mean(x))
colnames(hbyw)<- c("weekday","MeanHappyIndex")
hbyw<- hbyw[with(hbyw, order(-MeanHappyIndex)), ]
qplot(weekday, data=hbyw, geom="bar", weight= MeanHappyIndex, ylab="Happyness")

wee<- rbind(rep(3.0,7) , rep(2.7,7), hbyw$MeanHappyIndex)
colnames(wee)<- hbyw$weekday
radarchart(as.data.frame(wee),axistype=1,cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(2.7,3.0,0.05), cglwd=0.8,pcol=rgb(0.2,0.5,0.5,0.6) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , vlcex=1 ,title="A Tuesday is the least happy day 😱")
# the plot show that tuesday is the least happy day

# influence of weekday on happiness :
hbye <- aggregate(vote ~ uid+weekday, votes, function(x) mean(x))
hbye
colnames(hbye)<- c("uid","weekday","MeanHappyIndex")
ggplot(hbye, aes(MeanHappyIndex,fill=weekday)) +  geom_bar(alpha = 0.5)+ geom_histogram(binwidth = .1) + labs(title = "Influence of weekday on happiness")
# here we can see the last result but with more details on each rate

# average vote by company
votes[1,]
v.co.s <- aggregate(votes$vote, by=list(Category=votes$companyAlias), FUN=sum)
v.co.m <- aggregate(votes$vote, by=list(Category=votes$companyAlias), FUN=mean)
v.co.sd <- aggregate(votes$vote, by=list(Category=votes$companyAlias), FUN=sd)
vote.by.co <- cbind(v.co.s,v.co.m$x,v.co.sd$x)
colnames(vote.by.co) <- c("alias","sum","mean","sd")
vote.by.co # the same thing as before but this time we have the mean and sd for all companies

# the distribution of churns in company through time :
ggplot(churn[churn$stillExists==0,], aes(lastParticipationDate+15,fill=companyAlias)) +
  geom_histogram(alpha = .95,binwidth=7) + xlab("Churn date")
# we can see that churn increase through time between 2015 and 2017 :



################ Finaly to understand the relation between employees satisfaction (happiness) and 
#               churn (turnover) I will plot the mean of votes for each user with respect to churn

satisfaction = aggregate(vote ~ employee, votes, function(x) mean(x))
satisfaction # for each user we have now his vote mean in this list or table

# I will plot this result

plot(satisfaction$employee, satisfaction$vote)

# Now I will compute how many times each user has been churned : 0 : never, 1 : one time, 2 : two times .... 

churned = aggregate(churn ~ employee, churn, function(x) sum(x))
churned # we have now for each user the number of churn in this table

################ It's time now to see with color the distribution of churn (turnover) with respect to
#                                           satisfaction (happiness) :

# I will first combine the to tables :

satisfaction$churn = churned$churn # we have now a third colone in this table that contains the churn information of employees

plot(satisfaction[satisfaction$churn == 0,]$employee, 
     satisfaction[satisfaction$churn == 0,]$vote,
     pch = 20,
     col = "green",main="Average employee vote / the number of times they were churned",
     xlab = "employees",
     ylab = "mean of votes")

abline(h=2.75, col="black")     # we add a line that split the points at the mean vote = 2.5  

# we see clearly that most peaple who have never been fired have an average vote >= 2.75

################# I will add the employees who have a churn value != 0 with different color and I will put a legend for the plot

points(satisfaction[satisfaction$churn == 1,]$employee, 
       satisfaction[satisfaction$churn == 1,]$vote,
       pch = 20,
       col = "red")

legend(x=0,y=1.75,c("0 times", "1 time"), cex=.8,col=c("green", "red"),pch=c(20))

# also for employees hired once, we can see that they have a good average vote that is >= 2.75
# For the rest of the employees who were churned at least 2 times, we can see that in most cases, the average
# is not very important even if it's not perfect because of course we'll have other factors that
# influence the turnover in addition to happiness like money, family ...

# so I can say at this point that it's an openness to my analysis of this dataset, analyze and observe 
# the influence of other factors of employee turnover in companies like money and social status, seems 
# a very important subject to study.

# in the folowing you can see the distribution of other employees who were fired 2 or more times :
points(satisfaction[satisfaction$churn == 2,]$employee, 
       satisfaction[satisfaction$churn == 2,]$vote,
       pch = 20,
       col = "cyan")

points(satisfaction[satisfaction$churn == 3,]$employee, 
       satisfaction[satisfaction$churn == 3,]$vote,
       pch = 20,
       col = "purple")

points(satisfaction[satisfaction$churn == 4,]$employee, 
       satisfaction[satisfaction$churn == 4,]$vote,
       pch = 20,
       col = "blue")

# this two last cases are not very important because we have just one employee per case :

points(satisfaction[satisfaction$churn == 5,]$employee, 
       satisfaction[satisfaction$churn == 5,]$vote,
       pch = 20,
       col = "black")

points(satisfaction[satisfaction$churn == 6,]$employee, 
     satisfaction[satisfaction$churn == 6,]$vote,
     pch = 20,
     col = "black")


########################################### THANK YOU Mr MUHLENBACH #####################################
#########################################################################################################
#########################################################################################################
