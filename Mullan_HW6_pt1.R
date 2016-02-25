require("ggplot2")
require("grid")
data(diamonds)

#1 - create a plot to compare price and weight
plot1 <- ggplot(diamonds, aes(x=diamonds[[1]],y=diamonds[[7]]))
plot1 <- plot1 + labs(title="Diamonds - Weight to Price by Color")  +
  labs(x="Weight", y="Price") + 
  geom_point(aes(color = factor(color)))
print(plot1)

#2 same thing but with the natural log of each variable
logweight <- log(diamonds[[1]])
logprice <- log(diamonds[[7]])

loggraph <- ggplot(diamonds, aes(x=logweight,y=logprice))
loggraph <- loggraph + labs(title="Diamonds - Weight to Price by Color")  +
  labs(x="Weight", y="Price") + 
  geom_point(aes(color = factor(color)))
print(loggraph)


#3 create 3 different plots using the residuals of price and carat

linearmodel <- lm(price~carat, diamonds)
priceresid <- residuals(linearmodel)
residgraph <- ggplot(diamonds, aes(x=diamonds[[1]],y=priceresid))
residgraph <- residgraph  + labs(title="Diamonds - Weight to Price by Color") +
  labs(x="Weight", y ="Price Residuals") + geom_point(aes(color=factor(color)))
print(residgraph)


carathist <- diamonds[1]   #df of the first column
bin50 <- (max(diamonds[[1]])-min(diamonds[[1]]))/50  #width of histograms
hist2 <- ggplot(pricehist, aes(x=diamonds[[1]]))
hist2 <- hist2 +
  geom_histogram(colour = 'black',
                 fill = 'gray',binwidth=bin50) +
  labs(x=names(diamonds)[[1]])    #names method will print "carat" on x axis
print(hist2)

pricehist <- data.frame(priceresid)   #df of the residuals of price
bin50 <- (max(diamonds[[7]])-min(diamonds[[7]]))/50  #width of histograms
hist1 <- ggplot(pricehist, aes(x=diamonds[[7]]))
hist1 <- hist1 +
  geom_histogram(colour = 'black',
                 fill = 'gray',binwidth=bin50) +
  labs(x=names(diamonds)[[7]])    #names method will print "price" on x axis
print(hist1)





