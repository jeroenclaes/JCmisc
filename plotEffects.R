# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
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



plotEffects <-function(predictor, model, x=NULL, y=NULL, title=NULL, ylim=c(-2,2))
{
  require(ggplot2)
  require(grid)
  require(gridExtra)
  require(Hmisc)

  #ES
  effectsEs<-as.data.frame(fixef(model))
  effectsEs$Effects<-row.names(effectsEs)
  
  level<-effectsEs[grep(paste("(?<!(:))(", predictor, "\\[[.a-zA-Z0-9-_+]+\\])(?!(:))", collapse="", sep=""), effectsEs$Effects,perl=T),"Effects"]
 
  
  
  value<-effectsEs[grep(paste("(?<!(:))(", predictor, "\\[[.a-zA-Z0-9-_+]+\\])(?!(:))", collapse="", sep=""), effectsEs$Effects, perl=T),"fixef(model)"]
  value<-as.numeric(value)
  missing.value<-0-sum(value)
  values<-c(value, missing.value)
  level<-gsub(as.character(predictor), "", level) 

  level<-gsub("\\[S.", "", level)
 
level<-gsub("[^[:alnum:] -._:*]", "", level)

missing.level<-levels(model@frame[, predictor])[levels(model@frame[, predictor]) %nin% level]
levels<-c(level, missing.level)
  effectsEs<-data.frame(Levels=factor(levels), Values=as.numeric(as.character(values)))

  
   
  

  #sort by value
  effectsEs$Levels <- factor(effectsEs$Levels, levels =   effectsEs$Levels[order(effectsEs$Values)])
  

       #plot
   
plot1<- ggplot(data=effectsEs, aes(Levels, Values)) + geom_point(size=4, color="#575757") + geom_line(aes(group=1), color="#575757") + theme_minimal(base_size=16, base_family = "Times") + labs(list(title=as.character(title), x=as.character(x), y=as.character(y))) + scale_y_continuous(limits = ylim) 


return(plot1)


}

varimpPlot<-function(varimp)
{
  require(ggplot2)
  require(scales)
  require(plyr)
  data<-as.data.frame(varimp)
  data$labels<-row.names(data)
  
  data$labels<-revalue(data$labels, c("Typical.Action.Chain.Pos"="T.A.C.P"))
  
 data$labels <- factor(data$labels, levels =  data$labels[order(data$varimp)])
 
 plot1<- ggplot(data=data, aes(labels, varimp)) + geom_point(size=2, color="#575757")  + theme_minimal(base_size=14, base_family = "Times") + labs(list(title="", x="", y="")) +theme(plot.margin=unit(c(-1,0,-1,0), "lines"),axis.text.x = element_blank())+ coord_flip()
 return(plot1)
 
}

percentBar<-function(data, dep, ind)
{
  d<-prop.table(table(data[,as.character(ind)], data[, as.character(dep)]), 1)
 
  d2<-as.data.frame(d)
  d2<-subset(d2, d2[, "Var2"]==levels(data[, as.character(dep)])[2])
  d2$Var1 <- factor(d2$Var1, levels =  d2$Var1[order(d2$Freq)])
  ggplot(d2,aes(x = Var1, y = Freq)) +  geom_bar(stat = "identity") +   scale_y_continuous(labels = percent_format())+coord_flip()+theme_minimal(base_size=14, base_family = "Times")+  labs(list(main="", y="", x=""))
}