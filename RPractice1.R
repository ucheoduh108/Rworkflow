#APTECH MUSIC INDUSTRY

#THE OBJECTIVE OF PROVIDING THIS DATA, lies in building a system that
#recommends new music to the users in this listed community.
#From the available information, it is usually not difficult to 
#determine the support for various individual artists 
#(that is, the frequencies of a specific music genre/artist or song 
#that a user is listening to) as well as the joint support for pairs 
#(or larger groupings) of artists. Here, you need to count the number 
#of incidences across all your network members.

#SOLUTION
library(e1071)
library(caret)
library(mlbench)
library(rpart)
library(mclust)
library(plyr)

lastfm <- read.csv("C:/Users/user/Documents/lastfm.csv")
lastfm[1:19,]
length(lastfm$user)  ##289955 records in the file
summary(lastfm)
str(lastfm)
lastfm$user <- factor(lastfm$user)

str(lastfm$user)
levels(lastfm$user) ## 15000 users
levels(lastfm$artist) ## 1004 artists

library(arules) ## package for association rules
## Computational environment for mining association rules and
## frequent item sets
## we need to manipulate the data a bit for arules

playlist <- split(x = lastfm[,"artist"], f = lastfm$user )
## split into a list of users

playlist <- lapply(playlist,unique) ## remove artist duplicates

playlist[1:2]
## the first two listeners (1 and 3) listen to the following bands

playlist <- as(playlist,'transactions')
playlist
## view this as a list of "transactions"
## transactions is a data class defined in arules

itemFrequency(playlist)
## lists the support of the 1,004 bands
## number of times band is listed to on the shopping trips of 15,000 users
## computes the rel freq each artist mentioned by the 15,000 users

itemFrequencyPlot(playlist, support = 0.08, cex.names = 1.5)
## plots the item frequencies (only bands with > % support)

musicRules <- apriori(playlist, parameter = list(support = 0.01,
                                      confidence = 0.05))
inspect(musicRules)
## Finally, we build the association rules
## only rules with support > 0.01 and confidence > .50
## so it can’t be a super rare band

inspect(subset(musicRules, subset = lift>5))
## filter by lift > 5.
## Among those associations with support > 0.01 and confidence > .50,
## only show those with lift > 5

inspect(sort(subset(musicRules, subset = lift>5), by ='confidence'))
## lastly, order by confidence to make it easier to understand


#INTERPRETATION OF TERMS IN ASSOCIATION- 
#Support is an indication of how frequently a set of items appear in baskets. 
#Confidence is an indication of how often the support-rule has been found to be true. 
#Lift is a measure of association using both support and confidence.


#The lift value of an association rule is the ratio of the confidence of the rule 
#and the expected confidence of the rule. 
#The expected confidence of a rule is defined as the product of the support values
#of the rule body and the rule head divided by the support of the rule body.

