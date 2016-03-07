data(diamonds)
require("ggplot2")

##Prof G: This did not return a dataframe?
#1. extract numeric columns from a data frame
explore1 <- function(df)
{ 
  col <- NULL         #Create a null set to add things to
  for (n in 1:ncol(df)) col <- c(col,df[[n]])  #add the columns of the df to the vector col
  
  return(col)
}

##Prof G: Because explore1 did not return a data frame, it cannot be used to extract the columns
##Prof G: for input into this function. This function expects a df with numeric columns but does
##Prof G: not document that expectation in the header.
#2 Create a function that show pearson correlation coefficients for columns in a df
explore2 <- function(df)
  { name <- NULL
    coeff <- NULL
    coldf <- ncol(df)
    for (n in 1:(coldf-1)) #add the hyphenated pairings to a vector
      for (m in n:coldf) 
        name <- c(name, paste(names(df[n]),names(df[m]),sep="-"))
    
    for (n in 1:(coldf-1))  #create a vector with the pearson coeffs 
      for (m in n:coldf) 
        coeff <- c(coeff, cor(df[[n]],df[[m]],method="pearson"))
    
    
   data <- data.frame(name,coeff) #create a dataframe with the names and coeffs
   print(data)
    
  
    }

#3 Create scatter plots for each pair of numeric variables
##Prof G: This function fails. coldf is not defined.
explore3 <- function(df)
{ for (n in 1:(coldf-1)) #run through each pairing of 
    for (m in n:coldf)
      plot3 <- ggplot(df, aes(x=df[[n]],y=df[[m]]))
      plot3 <- plot3 + labs(title=cor(df[[n]],df[[m]]))  +
      labs(x=names(df[n]), y=names[df[m]]) + 
      geom_point()
      print(plot3)
}




