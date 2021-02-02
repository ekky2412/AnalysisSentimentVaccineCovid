library(twitteR)
library(RCurl)
library(wordcloud)
library(tm)
library(SnowballC)
library(Rstem)
library(NLP)
library(SentimentAnalysis)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(sentiment)


consumer_key = "KKWw2ERsKER8njU5SiohACat7";
consumer_secret = "GRFzLOd0v0lEB12hQ3UHngxHT11PdIYa0mn8t14PqmhRQpQNtc";
bearer_token = "AAAAAAAAAAAAAAAAAAAAANsjKAEAAAAA11ksFen0WsuiW3orxPBJGd1siVc%3Drr2QbmUjl9ynmxEfTWY5U0UlCOY4cF0mgM28L1U2IRFmLdPzDN";
access_token = "488398717-OeYDNC8ed1ZIwppCdlqllGxOA39RFhNKhOOhEab1";
access_secret = "47nLd2g4MbSy2t5HIG7ZOxTYLhFgdhZBqpk2MAUyuV7KM";
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret);

#mining data twitter
miningtweets <- searchTwitter('Vaccine COVID',lang="en",n=2000, resultType = "mixed");
miningtweets_text <- sapply(miningtweets,function(x) x$getText());
#Tempat penyimpanan file csv
pathOutput = "D:\\KULIAH\\Data Science\\Praktikum\\Project\\"
write.csv(miningtweets_text, paste(pathOutput,'dataTwitter.csv',sep = ''))
str(miningtweets_text);


miningtweets_text = read.csv(paste(pathOutput,'dataTwitter.csv',sep = ''))
miningtweets_text = gsub("https.*"," ", miningtweets_text$x)
miningtweets_text = gsub("&amp", " ", miningtweets_text)
miningtweets_text = gsub("\n", " ", miningtweets_text)
miningtweets_text = sub("^\\s*<U\\+\\w+>\\s*", "", miningtweets_text)
# remove retweet entities
miningtweets_text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", miningtweets_text)
# remove at people
miningtweets_text = gsub("@\\w+", " ", miningtweets_text)
# remove punctuation
miningtweets_text = gsub("[^\x01-\x7F]", " ", miningtweets_text)
miningtweets_text = gsub("[[:punct:]]", " ", miningtweets_text)
# remove numbers
miningtweets_text = gsub("[[:digit:]]", " ", miningtweets_text)
# remove html links
miningtweets_text = gsub("http\\w+", " ", miningtweets_text)
# remove unnecessary spaces
miningtweets_text = gsub("[ \t]{2,}", " ", miningtweets_text)
miningtweets_text = gsub("^\\s+|\\s+$", " ", miningtweets_text)
miningtweets_text = gsub("note", " ", miningtweets_text)




# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

# lower case using try.error with sapply 
miningtweets_text = sapply(miningtweets_text, try.error)
# remove NAs in miningtweets_text
miningtweets_text = miningtweets_text[!is.na(miningtweets_text)]
names(miningtweets_text) = NULL

write.csv(miningtweets_text, paste(pathOutput,'dataCleaned.csv',sep = ''))

miningtweets_text = read.csv(paste(pathOutput,'dataCleaned.csv',sep = ''))
# classify emotion
class_emo = classify_emotion(miningtweets_text, algorithm="bayes", prior=1.0, verbose = TRUE)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"
# classify polarity
class_pol = classify_polarity(miningtweets_text, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]
View(polarity)

# data frame with results
sent_df = data.frame(text=miningtweets_text, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
write.csv(sent_df, paste(pathOutput,'dataSentimen.csv',sep = ''))
View(sent_df)
head(sent_df,20)
table(sent_df$emotion)
# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets") +
  labs(title = "Sentiment Analysis dari Vaksin COVID-19",
       plot.title = element_text(size=12))
plotSentiments1 <- function(sentiment_dataframe, title) 
{
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=emotion)) + 
    geom_bar(aes(y=..count.., fill=emotion)) + 
    scale_fill_brewer(palette="Dark2") + 
    ggtitle(title) + 
    theme(legend.position="right") + 
    ylab("Number of Tweets") + 
    xlab("Emotion Categories")
}

#plotting tweets emotions
plotSentiments1(sent_df, "Sentiment Analysis of Vaccine Covid-19")


# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets") +
  labs(title = "Sentiment Analysis of Vaccine COVID-19",
       plot.title = element_text(size=12))
plotSentiments2 <- function(sent_df, title)
{
  library(ggplot2)
  ggplot(sent_df, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette="RdGy") +
    ggtitle(title) +
    theme(legend.position="right") +
    ylab("Number of Tweets") +
    xlab("Polarity Categories")
}

#plotting tweets polarity
plotSentiments2(sent_df, "Polarity Analysis of Vaccine Covid-19")