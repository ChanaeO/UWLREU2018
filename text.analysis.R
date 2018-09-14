library(dplyr)
library(tidytext)
library(magrittr)
library(sentimentr)
library(qdap)
library(wordcloud2)


#If the user plans on using the same lexicon for multiple data sets, then the 
#user doesn't have to run line 147 "lexicon" again.
create_lexicon <- function(){
  choose_lexicon <- select.list(choices = c("Huliu","Jockers", "Jockers_Rinker", "Loughran_McDonald",  "NRC", "Sentiword", "SlangSD", "SOCAL_Google"), preselect = c("Huliu", "NRC"), 
                                multiple = TRUE, title = "Choose one or more lexicons")
  lc <- 1:length(choose_lexicon)
  
  #Lexicon choice into data frame
  get_lexicon_df <- function(choice, l){
    #Lexicons from 'sentimentr' package
    Huliu <- lexicon::hash_sentiment_huliu
    Jockers <- lexicon::hash_sentiment_jockers
    Jockers_Rinker <- lexicon::hash_sentiment_jockers_rinker
    Loughran_Mcdonald <- lexicon::hash_sentiment_loughran_mcdonald
    NRC <- lexicon::hash_sentiment_nrc
    Sentiword <- lexicon::hash_sentiment_sentiword
    SlangSD<- lexicon::hash_sentiment_slangsd
    SOCAL_Google <- lexicon::hash_sentiment_socal_google
    
    lexicon_choice <- mget(paste(choice[l]))
    lexicon_choice <- list_df2df(lexicon_choice)
    lexicon_choice <- lexicon_choice[,-1]
    return(lexicon_choice)
  }
  joint_lexicons <- get_lexicon_df(choose_lexicon, lc)
  if (length(choose_lexicon)==1) {
    #Create key with sentiment
    is_key(joint_lexicons)
    key <- data.frame(
      words = joint_lexicons$x,
      polarity = joint_lexicons$y,
      stringsAsFactors = FALSE)
    my_key <- as_key(key)
  } 
  else {
    joint_lexicons <-unique(joint_lexicons)
    joint_lexicons <-joint_lexicons[order(joint_lexicons$x),]
    rownames(joint_lexicons) <- 1:nrow(joint_lexicons)
    
    #Find any duplicate words with different values
    d=1:(nrow(joint_lexicons)-1)
    duplicates <- which(joint_lexicons$x[d]==joint_lexicons$x[d+1])
    
    #Duplicate words
    excluded_joint_lexicon <- joint_lexicons[c(duplicates, duplicates + 1),]
    excluded_joint_lexicon <- excluded_joint_lexicon[order(excluded_joint_lexicon$x),]
    rownames(excluded_joint_lexicon) <- 1:nrow(excluded_joint_lexicon)
    
    #Removes all duplicated words
    key_lexicon <-  joint_lexicons[-c(duplicates,duplicates+1),]
    joint_lexicons<-  joint_lexicons[-c(duplicates,duplicates+1),]
    
    #Changing all values to -1 or 1
    positive <-which(key_lexicon$y > 0)
    key_lexicon$y[positive] <- 1
    negative <-which(key_lexicon$y < 0)
    key_lexicon$y[negative] <- -1
    key_lexicon <-unique(key_lexicon)
    
    #Create key with sentiment
    is_key(key_lexicon)
    key <- data.frame(
      words = key_lexicon$x,
      polarity = key_lexicon$y,
      stringsAsFactors = FALSE)
    my_key <- as_key(key)
    
    #Handling excluded Words
    excluded_key_lexicon1 <- c(unique(excluded_joint_lexicon$x))
    sentiment_estimate <- function(word){
      if (is.null(qdap::synonyms(terms = word, return.list = TRUE, multiwords = TRUE, 
                                 report.null = FALSE, synonym.frame = qdapDictionaries::key.syn))==TRUE) {
        sentiment2 = NA
        sentiment3 = NA 
      } else {
        syn <- data_frame("synonym" = qdap::synonyms(terms = word, return.list = TRUE, multiwords = TRUE, report.null = FALSE, synonym.frame = qdapDictionaries::key.syn))
        syn <- data_frame("x" = unlist(syn$synonym[1:nrow(syn)], use.names = TRUE))
        syn <- unique(syn)
        sentiment1 <- syn %>% left_join(my_key, by = "x")
        sentiment1$y[is.na(sentiment1$y)] <- 0 
        sentiment2 <- sum(sentiment1$y)
        sentiment3 <- as.data.frame(table(sentiment1$y))
        if (nrow(sentiment3) < 3){
          if (nrow(sentiment3) == 2){
            if((sentiment3$Var1[1] == -1)==FALSE & (sentiment3$Var1[2] == -1)==FALSE){
              sentiment3 <- weighted.mean(c(0,1), sentiment3$Freq)
            }
            else if((sentiment3$Var1[1] == 0)==FALSE & (sentiment3$Var1[2] == 0)==FALSE){
              sentiment3 <- weighted.mean(c(-1,1), sentiment3$Freq)
            }
            else {
              sentiment3 <- weighted.mean(c(-1,0), sentiment3$Freq)
            }
          } else {
            if((sentiment3$Var1[1] == -1)==TRUE){
              sentiment3 <- weighted.mean(-1, sentiment3$Freq)
            } 
            else if((sentiment3$Var1[1] == 0)==TRUE){
              sentiment3 <- weighted.mean(0, sentiment3$Freq)
            }
            else {
              sentiment3 <- weighted.mean(1, sentiment3$Freq)
            }
          }
        } else {
          sentiment3 <- weighted.mean(c(-1,0,1), sentiment3$Freq)
        }
      }
      sentiment <- data_frame(sentiment2, sentiment3)
      return(sentiment)
    }
    excluded_key_lexicon2<- data.frame(excluded_key_lexicon1, t(matrix(sapply(excluded_key_lexicon1, sentiment_estimate),
                                                                       ncol = length(excluded_key_lexicon1), nrow = 2)), stringsAsFactors = FALSE)
    colnames(excluded_key_lexicon2) <- c("x","sum", "weighted mean")
    ex_na_zero <- which(excluded_key_lexicon2$`weighted mean`== 0 | is.na(excluded_key_lexicon2$`weighted mean`) == TRUE)
    excluded_na_zero <- excluded_key_lexicon2[ex_na_zero, c(1,3)]
    colnames(excluded_na_zero)<- c("x", "y")
    excluded_key_lexicon3 <- excluded_key_lexicon2[-c(ex_na_zero), c(1,3)]
    colnames(excluded_key_lexicon3)<- c("x", "y")
    excluded_key_lexicon3$y <- round(as.numeric(excluded_key_lexicon3$y), digits = 3)
    
    #Add words to lexicon
    df = data.frame(x = "hellspawn", y = -1, stringsAsFactors = FALSE)
    joint_lexicons <- rbind(joint_lexicons, excluded_key_lexicon3, df)
    joint_lexicons <- unique(joint_lexicons)
    joint_lexicons <- joint_lexicons[order(joint_lexicons$x),]
    rownames(joint_lexicons) <- 1:nrow(joint_lexicons)
    
    #Create key with sentiment
    is_key(joint_lexicons)
    key <- data.frame(
      words = joint_lexicons$x,
      polarity = joint_lexicons$y,
      stringsAsFactors = FALSE)
    my_key <- as_key(key)
  }
  return(my_key)
}
lexicon <- create_lexicon()

#Opens a file explorer for the user to choose a file to work with.
output_sentiment_df <- function(my_key){
  file_path <- file.choose()
  get_file_data <- function(file_path){
    pos <- gregexpr("\\\\", file_path)
    pos <- unlist(pos)
    directory <- substr(x= file_path, start = 1, stop = pos[length(pos)]-1)
    directory <- gsub("\\\\", "/", directory)
    setwd(directory)
    pos <- pos[length(pos)] + 1
    load(substr(x= file_path, start = pos, stop = nchar(file_path)))
    file_name <- substr(x= file_path, start = pos, stop = nchar(file_path)-4)
    file_data <- get(paste(file_name))
    return(list(file_name, file_data))
  }
  file <- get_file_data(file_path)
  file_name <- as.character(file[1])
  file_data <- as.data.frame(file[2], stringsAsFactors = FALSE)
  
  #Text into data frame
  text_data <- data_frame("text" = file_data$text)
  p=1:(nrow(text_data))
  text_df <- c(rm_url(text_data$text[p]))
  
  #Advertisement
  ad_indicators<-c("come by", "come in", "come get", "come out", "come play", "join ", 
                   "join us ", "learn about", "learn more", "meet ", "offer", "open house",
                   "send us","showings", "showcase", "step into", "stop by", "stop into", 
                   "ticket", "tomorrow ", "we'll be", "we will be", "we'll see", "we will see", 
                   "Sunday ", "Monday ", "Tuesday ", "Wednesday ", "Thursday ", "Friday ", "Saturday ",
                   "January \\d+", "February \\d+", "March \\d+", "April \\d+", "May \\d+", "June \\d+",
                   "July \\d+", "August \\d+", "September \\d+", "October \\d+", "November \\d+", "December \\d+")
  ad_data <- data.frame(text_df, matrix(sapply(ad_indicators, grepl, text_df, ignore.case = TRUE),
                                        ncol = length(ad_indicators), nrow = length(text_df)))
  colnames(ad_data) <- c("text",ad_indicators)
  ad_data <- transform(ad_data, sum = rowSums(ad_data[-1]))
  
  #Non-advertisement
  na <- which(ad_data$sum ==0)
  non_ad <- data_frame("text" = ad_data$text[na])
  duplicate_text <- which(duplicated(non_ad$text) == FALSE)
  non_ad<- non_ad[duplicate_text,]
  non_ad <- data_frame("text" = gsub("\\.\\.\\.", " ", non_ad$text))
  non_ad <- data_frame("text" = gsub("\\ \\.", " ", non_ad$text))
  non_ad <- data_frame("text" = gsub("\\d+\\.", "", non_ad$text))
  
  sen <- get_sentences(non_ad)
  sentiment_df <- sentiment(sen, polarity_dt = my_key,
                            lexicon::hash_valence_shifters)
  return(list(file_name, file_data, na, non_ad, sentiment_df))
}
sentiment_data <- output_sentiment_df(lexicon)

#Name of file
file_name <- as.character(sentiment_data[1])
#Imported data file
file_data <- as.data.frame(sentiment_data[2], stringsAsFactors = FALSE)
#Index of text that aren't advertisements
non_advertisement_position <- unlist(sentiment_data[3])
#Text data file without advertisements
non_advertisement_data <- as.data.frame(sentiment_data[4], stringsAsFactors = FALSE )
#Text data with polarity values
sentiment_data <- as.data.frame(sentiment_data[5], stringsAsFactors = FALSE)

#Save data
#Files should appear in the directory the data file is in.
save(non_advertisement_data, file = paste(file_name, ".no.ads.rda", sep = ""))
save(sentiment_data, file = paste(file_name, ".sentiment.rda", sep = ""))

#Word cloud of non-advertisement text(optional)
create_wordcloud <- function(my_key, non_ad){
  sentiment_words <- extract_sentiment_terms(text.var = non_ad[1:nrow(non_ad),],
                                             polarity_dt = my_key)
  negative <- list2df(sentiment_words$negative)
  negative <- data_frame(negative[,1])
  negative_count <- as.data.frame(table(negative))
  negative <- data.frame(negative_count,"negative", stringsAsFactors = FALSE)
  colnames(negative) <- c("word", "freq", "sentiment")
  positive <- list2df(sentiment_words$positive)
  positive <- data_frame(positive[,1])
  positive_count <- as.data.frame(table(positive))
  positive <- data.frame(positive_count,"positive", stringsAsFactors = FALSE)
  colnames(positive) <- c("word", "freq", "sentiment")
  
  cloud_df <- rbind(negative, positive)
  n=nrow(cloud_df)
  colors=rep("grey", n)
  colors[cloud_df$sentiment == "negative"] = "Red"
  colors[cloud_df$sentiment == "positive"] = "Blue"
  #Some words might not show in the worcloud if cloud_df has a lot of terms
  #and if the frequency of a word is a big number.
  #To fix this problem, change the size parameter of the function below to a 
  #small number.
  cloud = wordcloud2(data= cloud_df[,-3], size = .5, color = colors)
  return(cloud)
}
wordcloud_data <- create_wordcloud(lexicon, non_advertisement_data)

#Activity based on location(optional)
output_activity_df <- function(file_df, na){
  #Activity
  activity_indicator<-c("water ski", "reuniting", "sail", "cornhole", "visit", "firework", 
                        "walk", "boat", "yoga", "grill out", "draw", "performance",
                        "celebration", "festival", "live music", "fish", "swim", "paddleboard", 
                        "volunteer", "surf", "run", "bike", "biking", "hike", "hiking", 
                        "skate", "longboard", "photo shoot", "camp")
  
  text_data <- data_frame("text" = rm_url(file_df$text), "posted date" = file_df$created_at, "location" = file_df$Name)
  non_ad <- data_frame("text" = text_data$text[na], "posted date" = text_data$`posted date`[na], "location" = text_data$location[na])
  duplicate_text <- which(duplicated(non_ad$text) == FALSE)
  non_ad<- non_ad[duplicate_text,]
  non_ad <- data_frame("text" = gsub("\\.\\.\\.", " ", non_ad$text), "posted date" = non_ad$`posted date`, "location" = non_ad$location)
  non_ad <- data_frame("text" = gsub("\\ \\.", " ", non_ad$text), "posted date" = non_ad$`posted date`, "location" = non_ad$location)
  non_ad <- data_frame("text" = gsub("\\d+\\.", "", non_ad$text), "posted date" = non_ad$`posted date`, "location" = non_ad$location)
  a <- 1:nrow(non_ad)
  act_df <- c(non_ad$text[a])
  act_data <- data.frame(matrix(sapply(activity_indicator, grepl, act_df, ignore.case = TRUE),
                                ncol = length(activity_indicator), nrow = nrow(non_ad)), non_ad$location)
  colnames(act_data) <- c(activity_indicator, "location")
  #
  act_data1 <- by(act_data[,c(1:ncol(act_data)-1)], act_data$location, FUN=colSums)
  act_data1 <- as.data.frame(matrix(unlist(act_data1), nrow=length(unlist(act_data1[1]))))
  location <- unique(data.frame(act_data$location, stringsAsFactors = FALSE))
  location <- data.frame(lapply(location, as.character), stringsAsFactors=FALSE)
  colnames(act_data1) <- (c(location[c(1:nrow(location)), 1]))
  #
  act_data2 <- act_data1
  rownames(act_data2) <- activity_indicator
  act_data2["bike",]<-as.double(act_data2["bike",])+as.double(act_data2["biking",])
  act_data2["hike",]<-as.double(act_data2["hike",])+as.double(act_data2["hiking",])
  act_data2<-act_data2[-c(which(rownames(act_data2)=="biking"| rownames(act_data2)=="hiking")),]
  act_data1 <- cbind("activity"=rownames(act_data2), act_data2)
  rownames(act_data1)<-1:nrow(act_data1)
  return(act_data1)
} 
activity_data <- as.data.frame(output_activity_df(file_data, non_advertisement_position))

#Reminisce based on location(optional)
output_reminisce_df <- function(file_df, na){
  #Throwbacks
  tb_indicator<-c("throwback", "tbt", "take me back","wayback", "wbw", "flashback", "fbf")
  text_data <- data_frame("text" = rm_url(file_df$text), "posted date" = file_df$created_at, "location" = file_df$Name)
  non_ad <- data_frame("text" = text_data$text[na], "posted date" = text_data$`posted date`[na], "location" = text_data$location[na])
  duplicate_text <- which(duplicated(non_ad$text) == FALSE)
  non_ad<- non_ad[duplicate_text,]
  non_ad <- data_frame("text" = gsub("\\.\\.\\.", " ", non_ad$text), "posted date" = non_ad$`posted date`, "location" = non_ad$location)
  non_ad <- data_frame("text" = gsub("\\ \\.", " ", non_ad$text), "posted date" = non_ad$`posted date`, "location" = non_ad$location)
  non_ad <- data_frame("text" = gsub("\\d+\\.", "", non_ad$text), "posted date" = non_ad$`posted date`, "location" = non_ad$location)
  t <- 1:nrow(non_ad)
  tb_df <- c(non_ad$text[t])
  tb_data <- data.frame(matrix(sapply(tb_indicator, grepl, tb_df, ignore.case = TRUE),
                               ncol = length(tb_indicator), nrow = nrow(non_ad)), non_ad$location)
  colnames(tb_data) <- c(tb_indicator, "location")
  tb_data1 <- by(tb_data[,c(1:ncol(tb_data)-1)], tb_data$location, FUN=colSums)
  tb_data1 <- as.data.frame(t(matrix(unlist(tb_data1), nrow=length(unlist(tb_data1[1])))))
  location <- unique(data.frame(act_data$location, stringsAsFactors = FALSE))
  location <- data.frame(lapply(location, as.character), stringsAsFactors=FALSE)
  tb_data1 <- cbind(location, tb_data1)
  tb_data <- transform(tb_data1[,1], sum = rowSums(tb_data1[,-1]))
  colnames(tb_data) <- c("location", "# of tweets")
  return(tb_data)
}
reminisce_data <- as.data.frame(output_reminisce_df(file_data, non_advertisement_position))