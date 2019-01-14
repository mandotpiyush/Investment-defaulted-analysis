#	Import the Bollywood data set in Rstudio in a variable named bollywood
  bollywood <- read.csv("bollywood.csv")
  View(bollywood)

#	When you import a data set, R stores character vectors as factors (by default)
# You can check the structure of the data frame by using str()
  str(bollywood)

# You can change the attribute 'Movie' from factor to character type using the given command
  bollywood$Movie <- as.character(bollywood$Movie)
	str(bollywood) 

#Q1.
#	Access the last 10 movies (from the bottom of the Bollywood data frame) using column bollywood$Movie
# Store the names of those movies in last_10 vector (in the same order)
     
	last_10 <- tail(bollywood$Movie, 10)
  last_10

# OR
  n <- nrow(bollywood)
  n
  last_10_movies <- bollywood$Movie[(n-9):n]
  last_10_movies
  
  bollywood[(n-9):n, ]

#Q2.
#	Find out the total number of  missing values (NA) in the bollywood data frame.
# Store the result in na_bollywood vector
     
	na_bollywood <- sum(is.na(bollywood))
  na_bollywood	  
	
#Q3
#	Write the command to find out which movie tops the list in terms of Total Collections
# Store the movie name in variable named top_movie
 
  top_movie <- bollywood$Movie[which.max(bollywood$Tcollection)]
  top_movie

#Q4
#	Write the command to find out which movie comes second on the list in terms of Total Collections
# Store the movie name in variable named top_2_movie

  top_2_collection <- max(bollywood$Tcollection[-which.max(bollywood$Tcollection)])
  top_2_movie <- bollywood$Movie[which(bollywood$Tcollection == top_2_collection)]
  top_2_movie
  
  #OR
  sorted_bollywood <- bollywood[order(bollywood$Ocollection, decreasing = TRUE),]
  top_2nd_movie <- sorted_bollywood$Movie[2]
  top_2nd_movie
    
# Now let's find out the movies shot by Shahrukh, Akshay and Amitabh separately.
# subset() function is used for that. The code has already been written for you. 
	
	shahrukh <- subset(bollywood, Lead == "Shahrukh")
	akshay <- subset(bollywood, Lead == "Akshay")
	amitabh <- subset(bollywood, Lead  == "Amitabh")

	shahrukh[ ,1]
	akshay[ ,1]
	amitabh[ ,1]
	
# You can view what the above data frames look like
  shahrukh
	akshay
	amitabh
	
#Q5
#	What is the total collection of Shahrukh, Akshay and Amitabh movies individually?
# You can use	a column named 'Tcollection' for this 
 
  shahrukh_collection <- sum(shahrukh$Tcollection)
  shahrukh_collection  
  
	akshay_collection <- sum(akshay$Tcollection)
	akshay_collection
    
	amitabh_collection <- sum(amitabh$Tcollection)
  amitabh_collection  
	
#Q6  
# Write command/s to find out how many movies are in Flop, Average, Hit and Superhit categories in the entire Bollywood data set.

  number_of_Flops <- sum(bollywood$Verdict == "Flop")

  number_of_Averages <- sum(bollywood$Verdict == "Average")
  
  number_of_Hits <- sum(bollywood$Verdict == "Hit")
  
  number_of_Superhits <- sum(bollywood$Verdict == "Super Hit")
  
  verdictcounts <- data.frame(number_of_Flops, number_of_Averages, number_of_Hits, number_of_Superhits)
  verdictcounts

  #OR
  summary(bollywood$Verdict)  

#You can use SAPPLY function if you want to apply a function specific columns in a data frame 
#You can write a command to find the maximum value of Ocollection, Wcollection, Fwcollecion and Tcollection using sapply
sapply(bollywood[4:7], mean, na.rm = TRUE)
  
#Q7 
# Write a command to find the names of the movies which have the maximum 
# Ocollection, Wcollection, Fwcollecion & Tcollection
# Store the names of 4 movies in same sequence in movie_result vector

  movie_result <- bollywood$Movie[sapply(bollywood[4:7], which.max)]
  names(movie_result) <- c("Highest Opening Collection", "Highest Weekend collection", "Highest First-week collection", "Highest Total collection")
  movie_result
  