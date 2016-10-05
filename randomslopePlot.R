#This function will plot by-random-effect coefficients for fixed effects that have a random slope. Useful in e.g., linguistics to study individual variation in the sensitivity to contextual variables. 
randomslopePlot<-function(model,fixed.effect, random.effect, cols=4, ylim=c(-3, 3), fontsize=8, fontfamily="Times New Roman")
{
  
#Grab the coefficients associated with your random effect and store them in a data.frame
f<-as.data.frame(coefficients(model)[random.effect])
#Add a variable for your speakers/random effect
f$random<-row.names(f)


#new coefficient columns with clean labels for each of the levels
n.levels<-length(levels(model@frame[,fixed.effect]))

  stop<-n.levels-1
  for(x in 1:stop)
  {
    column.name<-paste(random.effect, fixed.effect, "S", levels(model@frame[,fixed.effect])[x],"", sep=".", collapse=".")
    f[, levels(model@frame[,fixed.effect])[x]]<-f[, column.name]
    
  }
  for(z in 1:nrow(f))
  {
    

  f[z, levels(model@frame[,fixed.effect])[x+1]]<-0-sum(f[z, c(levels(model@frame[,fixed.effect])[1:stop])])
  }
numPlots<-nrow(f)
layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                 ncol = cols, nrow = ceiling(numPlots/cols))

grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
for (y in 1:numPlots){
  matchidx <- as.data.frame(which(layout == y, arr.ind = TRUE))
  
  plot.data<-data.frame()
  for(a in 1:n.levels)
  {
    row<-data.frame(random=f[y, "random"], fixed=levels(model@frame[,fixed.effect])[a],coef=f[y, levels(model@frame[,fixed.effect])[a]])
    rbind(row, plot.data)-> plot.data
    
  }
  
  plot1<- ggplot(data=plot.data, aes(x=fixed, y=coef)) + geom_point(size=1, color="#575757") + geom_line(aes(group=1), color="#575757") + theme_minimal(base_size=fontsize, base_family = fontfamily)  + theme(plot.title = element_text(size = rel(0.8)))+labs(list(title=as.character(plot.data[1, "random"]), x="", y="")) + scale_y_continuous(limits =ylim) 
  print(plot1, vp = viewport(layout.pos.row = matchidx$row,
                             layout.pos.col = matchidx$col))
  
  
  
  
}
}


