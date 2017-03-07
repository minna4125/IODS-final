
#Minna Perälampi
#minna.peralampi@helsinki.fi
#2.3.2017
#Data wrangling for Introduction to Open Data science final assingment
                      
                      
 setwd("C:/Users/Minna/Documents/GitHub/IODS-final")
                      
  #Libraries needed
  library(tidyr) 
  library(dplyr)
                      
  #Reading the anime table
  anime <- read.csv("anime.csv", stringsAsFactors = F,  na.strings=c("0", ""," ", "NA"))
                      
                      
  #Exploring the data set
  summary(anime)
  dim(anime)
  str(anime)
                      
  #Removing rows with NA values from the table
  anime_ <- filter(anime, complete.cases(anime))
                      
  #Making a genre matrix that will replace genre variable
                      
                      
  #Changing genres name to lower case 
   anime_$genre <- tolower(anime_$genre)
                      
                      
  #Creating a new vector with all the genres
  genreslist <- strsplit(anime_$genre, ", ", perl=TRUE)
  genres <- unique(unlist(genreslist))
  genres
                      
                      
  #Making a genre matrix
  genrematrix <- matrix(0, nrow(anime_), length(genres))
  colnames(genrematrix) <- genres
  for (i in 1:nrow(anime_)) {
      genrematrix[i, ] <- genres %in% genreslist[[i]]
  }
                      
  #Making genres groups by clustering
  set.seed(4125)                    
  clusters <- kmeans(genrematrix, centers=7, nstart=30, iter.max=10000)
                      
                      
  #Combinin genre matrix with anime_ table to anime_gen  
  anime_gen <- cbind(anime_, "genre_cluster" = clusters$cluster)
                      
  #Checking
  glimpse(anime_gen)
                      
                      
  #Removing the genre  variable from the anime_gen
  anime_gen <- subset(anime_gen, select = -genre)
                      
  glimpse(anime_gen)
                      
                      
  #Then we make ratings to a factor variable with categories of like, doesn't like and neutral
                      
  anime_gen$rating[anime_gen$rating < 7.5 ] <- "0"
  anime_gen$rating[anime_gen$rating == 7.5 ] <- "0"
  anime_gen$rating[anime_gen$rating > 7.5 ] <- "1"
  anime_gen$rating[anime_gen$rating == 10] <- "1"
                      
  anime_gen$rating <- as.numeric(anime_gen$rating)
                      
  #Changing the type form character to a factor
  anime_gen$type <- as.factor(anime_gen$type)
                      
  #changing episodes from charactor to numeric
  anime_gen$episodes <- as.numeric(anime_gen$episodes)
  #Fixing NA's
  anime_gen <- anime_gen[!is.na(anime_gen$episodes), ]
  
  #Making the anime ids as row names
  rownames(anime_gen) <- anime_gen[anime_gen$anime_id]
                      
  #Removing the anime id variable and the name variable
  anime_gen <- select(anime_gen, -anime_id)
                      
                      
                      
  glimpse(anime_gen)
  write.csv(anime_gen, "anime_gen")
                 