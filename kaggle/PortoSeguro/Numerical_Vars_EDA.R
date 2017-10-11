library(data.table)
library(ggplot2)
library(corrplot)
library(grid)



###### Removing tables() ######
rm(list=tables()$NAME)
gc()



### Reading Data ###
eda_train = fread("data/train.csv")
eda_test = fread("data/test.csv")
cols = colnames(fe_train)


### Helper Functions ###
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



### Numeric Attributes ###
numeric_cols = cols[which(sapply(eda_train, class) == "numeric")]
numeric_cols

eda_cols  = c("id", numeric_cols, "target")
eda_train = eda_train[, eda_cols, with=FALSE]
eda_test = eda_test[, c("id", numeric_cols), with=FALSE]


### Summary ###
for(i in 1:length(numeric_cols)){
  print(summary(eda_train[, numeric_cols[i], with=FALSE]))
}

### Correlation Plot
corrplot.mixed(cor(eda_test[, numeric_cols, with=FALSE]), upper="circle", lower="number")



### Density Plot ###
i = 6
col = numeric_cols[i]
col
ggplot(eda_train, aes(get(col), fill = as.factor(target))) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")  

# layout = matrix( c(1,2,3,4), 2, 2, byrow=TRUE)
# multiplot(p1, p2, p3, layout=layout)

