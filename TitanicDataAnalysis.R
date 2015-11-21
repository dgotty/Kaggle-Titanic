# load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# add survived to test data so we can combine data sets
test.Survived <- data.frame(Survived = rep("None", nrow(test)), test[,])
test.Survived <- test.Survived[c(2,1,3,4,5,6,7,8,9,10,11,12)]

# combine data sets
data.combined <- rbind(train, test.Survived)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

str(data.combined)

table(data.combined$Survived)
table(data.combined$Pclass)

library(ggplot2)

train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_histogram(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# examine the first few names in the training data set
head(as.character(train$Name))

# how many unique names are there across both train and test?
length(unique(as.character(data.combined$Name)))

 # get the dupe names and analyze them
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
data.combined[which(data.combined$Name %in% dup.names),]

library(stringr)

misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")), ]
mrs <- data.combined[which(str_detect(data.combined$Name, "Mrs.")), ]
mr <- data.combined[which(str_detect(data.combined$Name, "Mr.")), ]

extractTitle <- function(name) {
  name <- as.character(name)
  
  if(length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if(length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if(length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if(length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}

titles <- NULL

for(i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}

data.combined$Title <- as.factor(titles)

ggplot(data.combined[1:891,], aes(x=Title, fill=Survived)) +
  geom_bar(binwidth = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill="Survived")