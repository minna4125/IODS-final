#Minna Perälampi
#minna.peralampi@helsinki.fi
#2.3.2017
#Data wrangling for Introduction to Open Data science final assingment


setwd("C:/Users/Minna/Documents/GitHub/IODS-final")

#Libraries needed
library(tidyr); library(dplyr); library(ggplot2); library(corrplot)

#Reading the anime rating table
anime <- read.csv("anime.csv", stringsAsFactors = F )



#Exploring the data set dimensions and variables
summary(anime)
dim(anime)


#Creating new columns for each genre

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

  #Combinin genre matrix with anime_ table to anime_gen  
    anime_gen <- cbind(anime_, genrematrix)
  
  #Checking
    glimpse(anime_gen)

#Removing the genre list variable from the anime_gen
  anime_gen <- subset(anime_gen, select = -genre)

#Checking
  glimpse(anime_gen)

  
######
  g <- anime_gen %>% 
    filter(!is.na(type)) %>%
    ggplot(aes(episodes, rating)) +
    geom_point(aes(color = type)) +
    facet_wrap(~ type, scales = "free_x")

  plot(g)


 
dist_eu <- dist(anime_gen)

plot(dist_eu)








