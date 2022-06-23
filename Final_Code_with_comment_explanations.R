
#install.packages("biogram")
###### PACKAGES #########
library(pdftools)
library(plyr)
library(dplyr)
library(NLP)
library(tm)  
library(tidytext)
library(stringr)
library(wordcloud)
#library(rcolorbrewer)
#library(wordcloud2)
library(syuzhet)
library(RVerbalExpressions) # https://alexluscombe.ca/blog/rverbalexpressions-a-helpful-tool-for-learning-regex-in-r/ , https://www.r-bloggers.com/2019/09/regex-problem-heres-an-r-package-that-will-write-regex-for-you/
#library(rvest) # https://rvest.tidyverse.org/
library(tidyverse)
library(corpus)
library(vader)
library(tokenizers)    #https://cran.r-project.org/web/packages/tokenizers/vignettes/introduction-to-tokenizers.html
#library(biogram)  #https://www.rdocumentation.org/packages/biogram/versions/1.6.3/topics/count_ngrams

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////




#USER DEFINED!
setwd('Your/Working/Directory/Here') #insert your working directory here (ie where you have all of the files you are interested in preprocessing)
wd <- getwd()


#USER DEFINED!
aud_initials <- c("abbreviated name1","abbreviated name 2", "abbreviated name n") #use this as an override for the regex tokenization in the next few lines


#during tokenization, actual english two letter words have been omitted to prevent undesirable pattern matching
#HOWEVER, if for some reason an auditor name is abbreviated to a two letter word which DOES exist in the english language, set this variable to 
#whatever the abbreviation is (ie Andy Newman might be abbreviated to "an", this will allow for recognition of name)

# !!!!! PLEASE NOTE THAT ABBREVIATIONS WHICH ARE NOT TWO LETTER ENGLISH WORDS ARE MUCH MORE PREFERRABLE AS IT REDUCES THE CHANCES OF ERRONEOUS TOKENIZATION


#USER DEFINED!
auditor_names <- c("name of auditor1","name of auditor 2","name of auditor 3", "name of auditor n") #set to whatever auditors are participating in the audit conversation files being preprocessed

#///////////////////////////////////////////////////////////////////////////

col_aud_names <- paste0(auditor_names, collapse = "|") #collapses the above user defined list of participating auditors into one string object for use in the regex section of this program





files <- list.files(pattern = ".pdf") #checking the current directory for all pdf files and returning a list of pdf file names
#files


folder_name <- str_extract(wd, pattern = '[^\\/]*$')   #this line pulls the last part of the file name for automation of naming other stuff later in this program

only_aud <- data.frame() #setting up a blank dataframe which will receive questions later in this program
full_audit <- data.frame() #creating empty dataframe which all audit conversations will be appended to after preprocessing and tokenization

set.seed(0) #customization/changing not super necessary, however good practice to set seed in case functions  








for (x in 1:length(files)) {
  og_file_name <- basename(files[x])  #grabbing the file name for file creation and naming, includes full directory (ie where file is located) and file type as part of the name (example: "C:/Desktop/folderx/your_file.pdf")
  file_name <- gsub('(\\.[a-z]*?)$', '', og_file_name) #extracting just the file name without the other stuff of the directory (ie file types like ".pdf", and directory information like "user/x/y etc" which are part of the files name)
  
  text <- pdf_text(pdf = og_file_name) %>% tolower() #taking the text and converting to all lowercase (allows for easier pattern matching)
  
  #text <- pdf_text(file.choose()) %>% tolower() #this allows you to pick a file regardless of where it is in your computer 
  #keep this commented unless you are trouble shooting/modifying functionality of this code
  
  raw_text <- "" #creating an empty string which will receive conversation text from all pdf files in directory (however this string is not tokenized as of yet, and must undergo tokenization still)
  for (b in 1:length(text)) {
    raw_text <- paste0(raw_text,text[b])
  }
  
  raw_text <- raw_text %>% tolower()
  
  
  #TOKENIZATION
  #an explanation of what each pattern is matching with can be seen after the set of "token_matchX" variables
  
  token_match1 <- "((\\n\\n)(?=([a-z]{2})(:|;|-)))" 
  token_match2 <- "(\\n(?=(\\{|\\(|\\[)?([a-z]{2,6})(\\)|\\]|\\})? ?(:|;|-| {2,5}|  )))" #new line, possible enclosing char, 2-6 letters possible closer char, optional space colon,semicolon or dash
  token_match3 <- "(\\n(?=[a-z]{2}\\b)(?!ew|aa|ab|ad|ae|ag|ah|ai|al|am|an|ar|as|at|aw|ax|ay|ba|be|bi|bo|by|cv|da|de|do|ed|ef|eh|el|em|en|er|es|et|ex|fa|fe|gi|go|ha|he|hi|hm|ho|id|if|in|is|it|ka|ki|la|li|lo|ma|me|mi|mm|mo|mu|my|na|ne|no|nu|od|oe|of|oh|oi|ok|om|on|op|or|os|ow|ox|oy|pa|pe|pi|po|qi|re|si|so|ta|te|ti|to|uh|um|un|up|us|ut|we|wo|xi|xu|ya|ye|yo|za))"
  token_match4 <- "(\\n(?=((\\(|\\{|\\[)?([a-z]{2,8})(\\)|\\}|\\])?(:|;|-))))"
  token_match5 <- "(\\n(?=\\W{0,2}(audit(or)?\\b)|(investigat[a-z]*?\\b))\\W{0,3})" #tokenzing based on if it is titled auditor or investigator
  token_match6 <- "(\\n\\n(?=\\d))"
  token_match7 <- "(\\n\\n)"
  token_match8 <- "(\\n(?=gretchen |gretchen adams ))" #optionally user defined
  token_match9 <- paste0("(\\n" , "(",paste0(aud_initials, collapse = "|"),")") #this pattern can potentially cause erroneous tokenization but is included in case the user/scribe abbreviates an auditor to an existing two letter english word
  
  #token_match abbreviated to TM in these explanations
  
  #TM1 = new paragraph followed by any two letter string followed by colon, semicolon or dash
  
  #TM2 = new line (\\n) followed by any two letter string optionally enclosed with parentheses, brackets, or curly brackets, followed by colon/semicolon/dash/tab/double space
  
  #TM3 = new line followed by any two letter word PROVIDED SAID WORD IS NOT IN THE LIST (ie does NOT match with ew/aa/ab/.../za)
  
  #TM4 = new line followed by character string which is between 2 and 8 chars long (ie looking for full name here) which is optionally enclosed with parentheses/brackets/curly brackets followed by dash/colon/semicolon
  
  #TM5 = new line followed by the words auditor/audit/investigator followed by 0 to 3 non letter characters
  
  #TM6 = new paragraph provided first character IS a number
  
  #TM7 = just plain old new paragraph
  
  #TM8 =  new line provided first words are "gretchen" OR "gretchen adams" in new line
  
  #TM9 = user defined list of abbreviated names/initials (this is what allows for an override of TM3 should an auditor's name be abbreviated to an existing two letter english word)
    #please note that TM9 has the potential to cause the most errors/erroneous tokenizations and ideally auditor names should be abbreviated to non english words
  
  
  grouped_token_matches <- c(token_match1,token_match2,token_match3,token_match4,token_match5,token_match6,token_match7,token_match8,token_match9) #creating a list so that patterns can be added/removed more easily for tokenization
  token_matche_var <- paste0(grouped_token_matches, collapse = "|")
  
  
  
  token_text <- tokenize_regex(raw_text, pattern = token_matche_var) #this line tokenizes the string of text (variable: raw_text) into chunks by speaker
  
  
  #taking each individual token and appending them to a list (each token/list item represents one or more sentences spoken by an individual)
  listy <- rbind() 
  for (x in 1:length(token_text)) {
    for (y in 1:length(token_text[[x]])) {
      listy <- rbind(listy,token_text[[x]][y])
      
      
      
    }
  }
  
  #Token Cleaning //////////////////////////////////////
                        #1                                  #2                                                    #3                                                      #4
  no_quote <- gsub("\\[.{9,}?\\]", '', listy)  %>% gsub(pattern = "\\(.{9,}?\\)", replacement = '') %>% gsub(pattern = "\\{.{9,}?\\}", replacement = '') %>% gsub(pattern = "\\-{2,}.{9,}?\\-", replacement = '')
  #list of operations being performed on each token in listy to get "no_quote":
    #1) removing all occurrences of "action descrioptions" of audit specifically strings starting with "[" enclosing 9 or more characters and ending with "]"
    #2) same as 1 but removing occurrences of action descriptions enclosed by parentheses
    #3) same as 1 but removing occurrences of action descriptions enclosed by curly brackets
    #4) same as 1 but removing occurrences of action descriptions enclosed by dashes
  
  
  
  cleans <- no_quote 
  
  
  #during cleaning, problematic strings appeared which became their own tokensin the list (including things like date/time stamps)
  blanks <- which(grepl(pattern = '[A-Za-z]+|\\d+',no_quote) == 0) #identification of which list items were these problematic strings
  
  
  if (length(blanks) > 0) {
    cleans <- cleans[-blanks,]
  } else {
    cleans <- cleans
  }
  #this if statement checks for occurrences of the pattern seen in blanks and removes list items if they match
  
  
  
  cleans <- cleans %>% gsub(pattern = "\\n", replacement = ' ') %>% stripWhitespace()
  #taking each token/list item and removing new line characters as they are no longer necessary
  
  
  actual_two_letter_words <- c("ew", "aa", "ab", "ad", "ae", "ag", "ah", "ai", "al", "am", "an", "ar", "as", "at", "aw", "ax", "ay", "ba", "be", "bi", "bo", "by", "da", "de", "do", "ed", "ef", "eh", "el", "em", "en", "er", "es", "et", "ex", "fa", "fe", "gi", "go", "ha", "he", "hi", "hm", "ho", "id", "if", "in", "is", "it", "ka", "ki", "la", "li", "lo", "ma", "me", "mi", "mm", "mo", "mu", "my", "na", "ne", "no", "nu", "od", "oe", "of", "oh", "oi", "ok", "om", "on", "op", "or", "os", "ow", "ox", "oy", "pa", "pe", "pi", "po", "qi", "re", "si", "so", "ta", "te", "ti", "to", "uh", "um", "un", "up", "us", "ut", "we", "wo", "xi", "xu", "ya", "ye", "yo", "za","cv")
  #creating a list of all existing two letter english words
  
  #we are checking to see if 1) there is a deliminator between speaker header and conversation and if so add it to dataframe, additionally if the first two chars 
  receiving_df <- rbind() #construction of new dataframe to pass tokenized/cleaned text to
  
  for (xb in 1:length(cleans)) {
    first_two_letters <- str_extract(cleans[xb], pattern = '^(|\\W*)([a-z]{2})')
    if (grepl(pattern = '^\\W?[a-z]{1,6}\\W{1,3}', cleans[xb]) == 1) {
      receiving_df <- rbind(receiving_df,cleans[xb])
      next
    }
    
    if ((first_two_letters %in% actual_two_letter_words) == 0) {
      receiving_df <- rbind(receiving_df,cleans[xb])
      next
    }
    if ((first_two_letters %in% actual_two_letter_words == 1) & (grepl(cleans[x], pattern = '^(|\\W)([a-z]{2}\\W{1,3})') == 1)) {
      receiving_df <- rbind(receiving_df,cleans[xb])
      next
    } else {
      receiving_df <- receiving_df
    }
  }
  
  
  
  
  
  


  text_df <- as.list(receiving_df) %>% as.data.frame() #conversion of receiving list to dataframe
  
  
  test_list <- data.frame()
  
  
  #as the text is still in the format of "speaker: conversation" this section breaks up the text into two columns, column one being the speaker and column 2 being what they say
  for (y in 1:length(text_df)) {
    if (grepl(text_df[y], pattern = "(:|;| {2,5}|  |-)") == 1 & unlist(gregexpr('(:|;|-| {2,5}|  )', text_df[y]))[1] < 8) {
      c <- unlist(gregexpr('(:|;| {2,5}|  |-)', text_df[y]))[1]
      a <- separate(text_df[y], sep = c, into = c("Speaker","Conversation"), col = 1, extra = "merge")
      test_list <- rbind(test_list,a)
      next
    } else {
      v <- unlist(gregexpr(' ', text_df[y]))[1]
      b <- separate(text_df[y], sep = v, into = c("Speaker","Conversation"), col = 1, extra = "merge")
      test_list <- rbind(test_list,b)
      next
    }
  }
  
  
  #the stings describing actions and containing doc/docs/document/documents often times were tokenized (usually seen as something like "doc123 being shown on screen" in original text)
  #checking for tokens STARTING with doc/document/docs/documents and removing those list items (as they are not conversation)
  has_bad_indices <- grepl(pattern = ' {0,2}doc|docs|document|documents', test_list[,1])
  bad_indices <- which(has_bad_indices == 1)
  
  if (length(bad_indices) > 0) {
    cleaned_list <- test_list[-bad_indices,]
  } else {
    cleaned_list <- test_list
  }
  
  #similarly, timestamps were also assigned their own tokens which need to be removed
  wonky_times <- grepl(test_list[,2], pattern = "-time: \\d{1,2}:\\d{1,2} am|pm -")
  to_be_cut <- which(wonky_times == 1)
  
  if (length(to_be_cut) > 0) {
    cleaned_list <- cleaned_list[-to_be_cut,]
  } else {
    cleaned_list <- cleaned_list
  }
  
  gen_times <- grepl(test_list[,1], pattern = "time|scribe|new scribe|\\-[^a-z]|\\d{1,2}:\\d{1,2}|\\d{1,2}:|\\d{1,2}-|\\d{5,}")
  time_cuts <- which(gen_times == 1)
  
  if (length(time_cuts) > 0) {
    cleaned_list <- cleaned_list[-time_cuts,]
  } else {
    cleaned_list <- cleaned_list
  }
  
  
  
  cleaner_df <- cleaned_list %>% as.data.frame()
  
  
  
  
  delineator <- data.frame("$","$") #when working with multiple audit files representing days/years/etc $ in speaker and conversation columns delineates each time period
  colnames(delineator) <- c("Speaker","Conversation")
  cleaner_tib <- add_row(cleaner_df,delineator) 
  cleanest_tib <- cleaner_tib %>% as_tibble()
  
  full_audit <- rbind(full_audit, cleanest_tib) #full audit with all speakers after cleaning and breaking conversation into tokens
  
#extraction of conversation only spoken by auditor, ie if 
  for (p in 1:nrow(cleanest_tib)) {
    #k <- str_extract(cleanest_tib[p,1], pattern = '.*')
    if (grepl(pattern = paste0('.*', aud_initials, '.*'), cleanest_tib[p,1]) == 1) {
      only_aud <- rbind(only_aud, cleanest_tib[p,])
      next
    }
    else {
      next
    }
  }
  
}
write_me <- only_aud[,2] 


setwd("wherever/you/want/your/file/saved")


write.csv(write_me, file = "your_file_name_here.csv") #exporting the conversation dataframe to csv


#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
######## ANALYSIS ############
#/////////////////////////////



complete_files <- list.files(pattern = ".csv")
complete_files

###### CORP FROM CSV FILE #################
#if you would like to perform analysis on files you create....


setwd("your/directory/here")
#csv_files <- list.files(pattern = ".csv")

file_loc <- "CSV/file/location/on/your/computer"

x <- read.csv(file_loc, header = TRUE)


#https://stackoverflow.com/questions/47406555/error-faced-while-using-tm-packages-vcorpus-in-r
require(tm)

#building corpus (ie list of text) from cleaned conversation csv file
corp1 <- Corpus(DataframeSource(x)) %>% tm_map(stripWhitespace) %>% tm_map(content_transformer(tolower)) %>% tm_map(removeWords, stopwords("english")) %>% tm_map(removePunctuation, ucp = TRUE)
corp.tdm.csv <- TermDocumentMatrix(corp1, control =      #this term document matrix is what I would apply clustering of words, classifications on etc)
                                 list(stopwords = TRUE,
                                      tolower = TRUE,
                                      stemming = FALSE,
                                      removeNumbers = TRUE,
                                      bounds = list(global = c(1, Inf))))




###### CORP FROM TEXT FILE #####
#another method of importing a file (as can be seen from the name this one is for .txt files)
corp2 <- Corpus(file.choose(), readerControl = list(reader = readPlain))


                                #1                              #2                                          #3
clean_corp2 <- corp2 %>% tm_map(stripWhitespace) %>% tm_map(content_transformer(tolower)) %>% tm_map(removeWords, stopwords("english")) #%>% tm_map(removePunctuation, ucp = TRUE)
#actions being performed to get clean_corp2:
  #1) removing excess whitespace
  #2) ensuring that text is lower case
  #3) removing stop words which add noise and get in the way of analysis
  
#removing punctuation
no_punc <- clean_corp %>% tm_map(removePunctuation, ucp = TRUE)
#inspect(clean_corp)

corp.tdm <- TermDocumentMatrix(clean_corp, control =      #this term document matrix is what I would apply clustering of words, classifications on etc)
                                 list(stopwords = TRUE,
                                      tolower = TRUE,
                                      stemming = FALSE,
                                      removeNumbers = TRUE,
                                      bounds = list(global = c(1, Inf))))

tdm.table <- as.table(corp.tdm)
test <- tdm.table[order(tdm.table)]
head(test)

#////////////////////////////////////////////
############ QUESTION EXTRACTION ############
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

df <- data.frame(read.csv(file.choose())) #choose the auditor only conversation file
conversations <- df[,2] #reading what was said
test_list <- list()

for (p in 1:length(conversations)) {
  test_list <- c(test_list,conversations[p])
}

tmatch1 <- "((?<=\\?))"
tmatch2 <- "((?<=\\!))"
tmatch3 <- "((?<=\\.))"
tmatch4 <- "(\\n)"

#breaking text into sentence tokens by matching punctuation


#creating list of patterns to match for easier removal/addition of new patterns
tmatchlist2 <- c(tmatch1,tmatch2,tmatch3,tmatch4) %>% paste0(collapse = "|")


conv_blob <- paste0(test_list, collapse = "\n")
token2 <- tokenize_regex(conv_blob, pattern = tmatchlist2) %>% as.data.frame()


clean_token2 <- data.frame()

#removing unnecessary dashes and new line characters
for (j in 1:nrow(token2)) {
  x <- token2[j,1]
  xy <- gsub(x, pattern = "-|\\n", replacement = "") 
  clean_token2[j,1] <- xy
}

#defining list of english question words
question_words <- paste0(c("Who","what","where","when","why","how","which","whose"), collapse = "|")


clean_token_backup <- clean_token2

#matching sentences which start with above defined list of question words OR phrases ending in question marks
grepl(clean_token_backup, pattern = paste0("(^(who|what|where|when|why|how|which|whose))|(\\?$)"))

#creation of empty dataframe to pass extracted questions
only_questions <- data.frame()

#going item by item of the auditor only conversations and extracting interrogative sentences
for (o in 1:nrow(clean_token_backup)) {
  if (grepl(clean_token_backup[o,1], pattern = "(^(who|what|where|when|why|how|which|whose))|(\\?$)") == 1) {
    only_questions <- rbind(only_questions,clean_token_backup[o,1])
  }
}  

#exporting list of auditor only questions
write.csv(only_questions, file = "Only_Gretchen_Questions.csv")
