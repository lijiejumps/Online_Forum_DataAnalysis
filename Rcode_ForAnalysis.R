#Tidy Data

webforum <- function(){
  original.file = read.csv("D:/2019/FIT 3152- Data Analytics/Assignment 1/webforum.csv", header = TRUE)
  
  #remove rows that have -1 as author ID
  original.file[original.file == -1] = NA
  original.file = na.omit(original.file)
  
  #remove posts with zero WC
  original.file = original.file[which(original.file$WC >= 20),]
  
  #add a column called 'Year'
  original.file$Year = year(original.file$Date)
  
  #swapping 2 columns: PostID and ThreadID
  sorted.webforum = original.file[,c(2,1,3:31)]
  
  #sort according to ascending ThreadID
  sorted.webforum = sorted.webforum[order(sorted.webforum$ThreadID),]
  
  sorted.webforum
}

#R package installations

install.packages("ggplot2")
install.packages("lubridate")
install.packages("plyr")

library(ggplot2)
library(lubridate)
library(plyr)

source('automated-data-processing')
webforum = webforum()

#Figure 1 & 2 - Analysing overall trends 

median_value_of_300_threads = aggregate(webforum[c("WC","Analytic","Clout","Authentic","Tone","affect","posemo","negemo")], by = list(webforum$Year), median)
colnames(median_value_of_300_threads)[1] = 'Year'

reshaped_median = melt(median_value_of_300_threads, id = c("Year"))
colnames(reshaped_median)[2] = "Sentiments"
g.median = ggplot(reshaped_median, aes(x=reshaped_median$Year, y = reshaped_median$value))+ geom_line(aes(color = Sentiments)) + geom_point(aes(color = reshaped_median$Sentiments)) + labs(title = "Change in Sentiments (in median)", x = "Year", y = "Proportion of Sentiments %")
g.median

median_apn = aggregate(webforum[c("affect","posemo","negemo")], by = list(webforum$Year), median)
colnames(median_apn)[1] = 'Year'
median_apn_reshaped = melt(median_apn, id = c('Year'))
colnames(median_apn_reshaped)[2] = 'Sentiments'
g.median.apn = ggplot(median_apn_reshaped, aes(x=median_apn_reshaped$Year, y = median_apn_reshaped$value))+ geom_line(aes(color = Sentiments)) + geom_point(aes(color = median_apn_reshaped$Sentiments)) + labs(title = "Change in Sentiments (in median)", x = "Year", y = "Proportion of Sentiments %")
g.median.apn

#Figure 3 - Correlation matrix (Heat map)

# Getting heat map using correlation values
library(reshape2)
corr = cor(webforum[c(6:30)])
corr_fm = melt(corr)
g_for_all_threads = ggplot(data = corr_fm, aes(x = Var1, y = Var2, fill = value)) + geom_tile(color = "white") + scale_fill_gradient(low = "blue", high = "red")
g_for_all_threads

#Figure 4 - Linear model with 3 predictors

fitted3 = lm(webforum$affect ~ webforum$posemo + webforum$negemo + webforum$anger)
summary(fitted3)

#Figure 5 - Linear model with all predictors except time
fittedAll = lm(affect ~ .-Time, data = webforum)
summary(fittedAll)

#Figure 6 - Boxplot (affects~10 biggest threads)

# find 10 biggest threads

#count no. of posts in each thread
freq_of_posts=as.data.frame(count(webforum,c('ThreadID'))) #frequency of posts in each ThreadID 

#top 10 in descending order
top10_freq_of_posts = freq_of_posts[order(-freq_of_posts$freq),][1:10,]
top10threads = subset(webforum,ThreadID %in% top10_freq_of_posts$ThreadID)


boxplot(top10threads$affect~top10threads$ThreadID[], data = top10threads, xlab = "10 biggest Threads", ylab = "Affect", col=(c("gold","darkgreen")))


#Figure 7 -Boxplot of 10 biggest threads vs remaining threads
remainingThreadIDs = freq_of_posts[order(-freq_of_posts$freq),][11:300,]
remainingThreads = subset(webforum, ThreadID %in% remainingThreadIDs$ThreadID)

webforum_6predictors = webforum[,c(1,9,18,19,20,23,24)]
webforum_6_reshaped = melt(webforum_6predictors, id = 'ThreadID')
colnames(webforum_6_reshaped)[2] = 'Sentiment'

#Boxplot
ggplot(webforum_6_reshaped, aes(ThreadID == '252620' | ThreadID == '127115' | ThreadID == '145223'| ThreadID =='472752'|ThreadID =='283958'|ThreadID =='254138'|ThreadID =='309286'|ThreadID =='191868'|ThreadID =='563904'|ThreadID =='773564', webforum_6_reshaped$value)) +
  geom_boxplot(aes(fill = Sentiment)) + scale_x_discrete(labels =c('Biggest Threads','Remaining Threads')) + xlab("Biggest Threads vs Remaining Threads") + ylab("Proportion of Sentiments(%)") + ggtitle("Top 10 Biggest Threads vs Remaining Threads")

#Figure 8 - Multi-variate graph - posemo analysis

posemoanalysis<-ggplot(top10threads,aes(x=Year,y=posemo, color=factor(ThreadID))) + geom_point() + geom_smooth(method = lm) + stat_smooth(method = "lm", col = "black")

#Hypothesis testing 

#Extracting top 20 threads (Threads with most number of posts)
top20_freq_of_posts = freq_of_posts[order(-freq_of_posts$freq),][1:20,]
top20threads = subset(webforum, ThreadID %in% top20_freq_of_posts$ThreadID)
top20threads$Date = as.Date(top20threads$Date, "%Y-%m-%d")

#Frequency of authors in those top 20 threads
freq_of_authors_in_top20 = as.data.frame(count(top20threads,c('AuthorID')))
top10_authors_freq = freq_of_authors_in_top20[order(-freq_of_authors_in_top20$freq),][1:10,]
top10_authors_thread = subset(webforum, AuthorID %in% top10_authors_freq$AuthorID)

library(reshape2)
corr_10_authors = cor(top10_authors_thread[c(6:31)])
corr_10_authors_reshaped = melt(corr)
g_for_10_authors = ggplot(data = corr_10_authors_reshaped, aes(x = Var1, y = Var2, fill = value)) + geom_tile(color = "white") + scale_fill_gradient(low = "blue", high = "red") + geom_text(aes(label = round(value,2)))
g_for_10_authors

#Figure 9 -Proportion of language change between most active users vs the remaining

groupA_means = aggregate(groupA, list(groupA$Year), mean)
groupB_means = aggregate(groupB, list(groupB$Year), mean)

#Create new column called - "Groups"
groupA_means$Groups = "GroupA"
groupB_means$Groups = "GroupB"

groupA_reshaped = melt(groupA_means[,c('Year','Analytic','Clout','Authentic','Tone','Groups')], id = c('Groups','Year'))
groupB_reshaped = melt(groupB_means[,c('Year','Analytic','Clout','Authentic','Tone','Groups')], id = c('Groups','Year'))


all <- rbind(groupA_reshaped, groupB_reshaped)
analytic <- subset(all, variable=="Analytic")
ggplot(analytic, aes(x=Year, y=value, group=Groups, colour=Groups)) + geom_line() + geom_point()

clout <- subset(all, variable=="Clout")
ggplot(clout, aes(x=Year, y=value, group=Groups, colour=Groups)) + geom_line() + geom_point()

authentic <- subset(all, variable=="Authentic")
ggplot(authentic, aes(x=Year, y=value, group=Groups, colour=Groups)) + geom_line() + geom_point()

tone <- subset(all, variable=="Tone")
ggplot(tone, aes(x=Year, y=value, group=Groups, colour=Groups)) + geom_line() + geom_point()

