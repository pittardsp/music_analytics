# Install Package
library(pdftools)
library(tidytext)

# Get URL of Music Report

url <- "http://www.riaa.com/wp-content/uploads/2018/05/MusicWatch-Consumer-Profile-2017.pdf"
download.file(url,"MusicWatch-Consumer-Profile-2017.pdf")

text <- pdf_text("MusicWatch-Consumer-Profile-2017.pdf") %>% readr::read_lines()

# This is an already aggregated table which is fine but I'll split them up into different
# data frames. Let's process GENDER first

mymusic <- function(text,range=c(4,6)) {
  retlist <- list()
  dflist <- list()
  name.vec <- vector()
  row.titles <- vector()
  hh <- 1
  jj <- 1
  kk <- 1
  
  dfnames <- gsub("\\(|\\)","",text[3])
  dfnames <- gsub("\\b \\b","_",dfnames)
  dfnames <- unlist(strsplit(dfnames,"\\s+"))
  dfnames <- dfnames[dfnames != ""]
  dfnames[6] <- "MUSIC_STREAMERS"
  dfnames[8] <- "FREE_STREAMERS"
  
  for (ii in range[1]:range[2]) {
    tmp <-unlist(strsplit(text[ii],"\\s+"))
    tmp <- tmp[tmp != ""]
  
      if (length(tmp) == 1) {
          name.vec <- c(name.vec,tmp)
      } else {
          tmp <- unlist(strsplit(text[ii],"\\s+")) 
          tmp <- tmp[tmp != ""]
          row.titles[kk] <- tmp[!grepl("%",tmp)]
          kk <- kk + 1
          tmp <- tmp[grep("%",tmp)]
          ntmp <- gsub("%","",tmp)
          ntmp <- as.numeric(ntmp)
          dflist[[jj]] <- ntmp
          jj <- jj + 1
      }
  }
  df <- data.frame(do.call(rbind,dflist))
  names(df) <- dfnames
#  rownames(df) <- row.titles
  df$GENDER <- row.titles
  
  return(df)
}

df <- mymusic(text=text)

dfg <- gather(df,key=Method,value=percent,-GENDER)

myplot <- dfg %>% filter(Method != "POPULATION_13+") %>% 
  ggplot(aes(x=Method,y=percent,fill=GENDER)) + 
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Paired") +
  labs(title="Music Consumer Profile by Gender",
       subtitle="www.musicwatchinc.com") + 
  ylab("Percent") + 
  xlab("How Consumer Obtains Music") + 
  theme_minimal() + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) 
  
ggplotly(myplot)


#

df <- mymusic(text=text,range=c(7,12))
dfg <- gather(df,key=Method,value=percent,-GENDER)
colnames(df)[11] <- "AGE"

dfg <- gather(df,key=Method,value=percent,-AGE)

myplot <- dfg %>% filter(Method != "POPULATION_13+") %>% 
  ggplot(aes(x=Method,y=percent,fill=AGE)) + 
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Paired") +
  labs(title="Music Consumer Profile by Age",
       subtitle="www.musicwatchinc.com") + 
  ylab("Percent") + 
  xlab("How Consumer Obtains Music") + 
  theme_minimal() + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) 

ggplotly(myplot)