# Load libraries
library('tm')
library('ggplot2')
library('plyr')

# Set the global paths
data.path <- file.path("/home/fatu/dev/GitHub/fatu/ML_for_Hackers", "03-Classification", "data")
easyham.path <- file.path(data.path, "easy_ham")

# Simply returns the full text of a given email message
msg.full <- function(path)
{
  con <- file(path, open = "rt", encoding = "latin1")
  msg <- readLines(con)
  close(con)
  return(msg)
}

# email message
get.from <- function(msg.vec)
{
  from <- msg.vec[grepl("From: ", msg.vec)]
  from <- strsplit(from, '[":<> ]')[[1]]
  from <- from[which(from  != "" & from != " ")]
  return(from[grepl("@", from)][1])
}

# Retuns the subject string for a given email message
get.subject <- function(msg.vec)
{
  subj <- msg.vec[grepl("Subject: ", msg.vec)]
  if(length(subj) > 0)
  {
    return(strsplit(subj, "Subject: ")[[1]][2])
  }
  else
  {
    return("")
  }
}

# Similar to the function from Chapter 3, this returns
# only the message body for a given email.
get.msg <- function(msg.vec)
{
  msg <- msg.vec[seq(which(msg.vec == "")[1] + 1, length(msg.vec), 1)]
  return(paste(msg, collapse = "\n"))
}

# Retuns the date a given email message was received
get.date <- function(msg.vec)
{
  date.grep <- grepl("^Date: ", msg.vec)
  date.grep <- which(date.grep == TRUE)
  date <- msg.vec[date.grep[1]]
  date <- strsplit(date, "\\+|\\-|: ")[[1]][2]
  date <- gsub("^\\s+|\\s+$", "", date)
  return(strtrim(date, 25))
}

# This function ties all of the above helper functions together.
# It returns a vector of data containing the feature set
# used to categorize data as priority or normal HAM
parse.email <- function(path)
{
  full.msg <- msg.full(path)
  date <- get.date(full.msg)
  from <- get.from(full.msg)
  subj <- get.subject(full.msg)
  msg <- get.msg(full.msg)
  return(c(date, from, subj, msg, path))
}

# In this case we are not interested in classifiying SPAM or HAM, so we will take
# it as given that is is being performed.  As such, we will use the EASY HAM email
# to train and test our ranker.
easyham.docs <- dir(easyham.path)

easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
easyham.parse <- lapply(easyham.docs,
                        function(p) parse.email(file.path(easyham.path, p)))

# Convert raw data from list to data frame
ehparse.matrix <- do.call(rbind, easyham.parse)
allparse.df <- data.frame(ehparse.matrix, stringsAsFactors = FALSE)
names(allparse.df) <- c("Date", "From.EMail", "Subject", "Message", "Path")

# this by passining them as required partmeters of the function. 
date.converter <- function(dates, pattern1, pattern2)
{
  pattern1.convert <- strptime(dates, pattern1)
  pattern2.convert <- strptime(dates, pattern2)
  pattern1.convert[is.na(pattern1.convert)] <- pattern2.convert[is.na(pattern1.convert)]
  return(pattern1.convert)
}

pattern1 <- "%a, %d %b %Y %H:%M:%S"
pattern2 <- "%d %b %Y %H:%M:%S"

allparse.df$Date <- date.converter(allparse.df$Date, pattern1, pattern2)

# Convert emails and subjects to lower-case
allparse.df$Subject <- tolower(allparse.df$Subject)
allparse.df$From.EMail <- tolower(allparse.df$From.EMail)

# Order the messages chronologically
priority.df <- allparse.df[with(allparse.df, order(Date)), ]

# We will use the first half of the priority.df to train our priority in-box algorithm.
# Later, we will use the second half to test.
priority.train <- priority.df[1:(round(nrow(priority.df) / 2)), ]

priority.train$Date <- as.POSIXct(priority.train$Date)
# The first step is to create rank weightings for all of the features.
# We begin with the simpliest: who the email is from.

# Calculate the frequency of correspondence with all emailers in the training set

from.weight <- ddply(priority.train, .(From.EMail),
                     summarise, Freq = length(Subject))
from.weight <- from.weight[with(from.weight, order(Freq)), ]

# We take a subset of the from.weight data frame to show our most frequent 
# correspondents.
from.ex <- subset(from.weight, Freq > 6)

from.scales <- ggplot(from.ex) +
  geom_rect(aes(xmin = 1:nrow(from.ex) - 0.5,
                xmax = 1:nrow(from.ex) + 0.5,
                ymin = 0,
                ymax = Freq,
                fill = "lightgrey",
                color = "darkblue")) +
  scale_x_continuous(breaks = 1:nrow(from.ex), labels = from.ex$From.EMail) +
  coord_flip() +
  scale_fill_manual(values = c("lightgrey" = "lightgrey"), guide = "none") +
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  ylab("Number of Emails Received (truncated at 6)") +
  xlab("Sender Address") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 5, hjust = 1))
ggsave(plot = from.scales,
       filename = file.path("dev/GitHub/fatu//ML_for_Hackers/04-Ranking/images", "0011_from_scales.pdf"),
       height = 4.8,
       width = 7)
































