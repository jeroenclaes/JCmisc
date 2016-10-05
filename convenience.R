#Convenience function to load data, set working directory and contrasts

prep <- function()
{
require(lme4)
require(coefplot)
require(Hmisc)
require(party)
require(car)
  options(contrasts = c("contr.Sum", "contr.Poly"))
	setwd("~/Desktop/")	
	
datalh <<- read.csv2("/Users/jeroenclaes/Box Sync/Doctorado/Corpus_LH/LHrecoded.csv", header=TRUE)
datasj<<-read.csv2("/Users/jeroenclaes/Box Sync/Doctorado/Corpus_PR/PRrecoded.csv", header=TRUE)	
datasd<<-read.csv2("/Users/jeroenclaes/Box Sync/Doctorado/Corpus_RD/RDrecoded.csv", header=TRUE)
	
datalh$Tense<<-recode(datalh$Tense, '"All.others"="All.Others"')




}

varimp_plot <-function(varimp, varimp1, varimp2, filename, height=9)
{
require(lattice)
require(latticeExtra)
require(grid)
require(gridExtra)
	

	plot<-dotplot(sort(varimp), stack=T, auto.key = list(space = "top",  columns=2),   main="",   xlab="", ylab="", col=c("#4D4D4D"), family="Times", fontsize=12, cex=0.8)
	 
		plot1<-dotplot(sort(varimp1),stack=T,  auto.key = list(space = "top", columns=2),  main="",   xlab="", ylab="", col=c("#4D4D4D"), family="Times", fontsize=12, cex=0.8)
	 		
		 		plot2<-dotplot(sort(varimp2),stack=T,  auto.key = list(space = "top", columns=2),  main="",    xlab="", ylab="", col=c("#4D4D4D"), family="Times", fontsize=12, cex=0.8)
	 		

			plots<-c(plot, plot1, plot2, layout = c(3, 1), merge.legends=F)
			print_plot(plots, filename, width=15, height=height)
				print(plots)

}

#shorthand function for table sorting

table.sort<-function(t, column)
{

t<- t[order(t[,column]),]
return (t)
}

make_plot<- function(data,data1, data2, x, y=NULL, filename=NULL,  stack=TRUE, height=9)
{
require(lattice)
require(latticeExtra)
require(grid)
require(gridExtra)

	
	if(!is.null(y))
	{
		table<-table(data[,x],data[,y])
		freq<- prop.table(table, 1)
		freq <- freq[order(freq[,1]),]
		print(table)
		print(freq)
		cat("-----------\n")
	table1<-table(data1[,x],data1[,y])
		freq1<- prop.table(table1, 1)
		freq1 <- freq1[order(freq1[,1]),]
		print(table1)
		print(freq1)
		cat("-----------\n")
table2<-table(data2[,x],data2[,y])
		freq2<- prop.table(table2, 1)
		freq2 <- freq2[order(freq2[,1]),]
		print(table2)
		print(freq2)




	
	plot<-dotchart(freq, stack=stack, auto.key = list(space = "top",  columns=2),   main="",   xlab="", ylab="",par.settings=list(superpose.polygon=list(col=c("#4D4D4D","#E6E6E6"), family="Times", fontsize=12, cex=0.8)))
	 
		plot1<-dotchart(freq1,stack=stack,  auto.key = list(space = "top", columns=2),  main="",  xlab="", ylab="", par.settings=list(superpose.polygon=list(col=c("#4D4D4D","#E6E6E6"), family="Times", fontsize=12, cex=0.8)))
		 
		
			plot2<-dotchart(freq2,stack=stack,  auto.key = list(space = "top",  columns=2),  main="", 
 xlab="", ylab="", par.settings=list(superpose.polygon=list(col=c("#4D4D4D","#E6E6E6"), family="Times",  fontsize=12, cex=0.8)))
			
			plots<-c(plot, plot1, plot2, layout = c(3, 1), merge.legends=F)
			print(plots)
			
		}
else
{
	
	table<-table(data[,x])
			table <- sort(table)
		print(table)
		
			table1<-table(data1[,x])
			table1 <- sort(table1)
		print(table)
			table2<-table(data2[,x])
			table2 <- sort(table2)
		print(table)
			print(table1)
				print(table2)

	plot<-dotchart(table, stack=stack,  auto.key = list(space = "top", columns=2
),  main="", 
 xlab="", ylab="", col=c("#E6E6E6", "#4D4D4D"))

	plot1<-dotchart(table1, stack=stack,  auto.key = list(space = "top",  columns=2),  main="", 
 xlab="", ylab="", col=c("#E6E6E6", "#4D4D4D"))

	plot2<-dotchart(table2, stack=stack,  auto.key = list(space = "top",  columns=2),  main="", 
 xlab="", ylab="", col=c("#E6E6E6", "#4D4D4D"))

	plots<-c(plot, plot1, plot2, layout = c(3, 1), merge.legends=F)
	print(plots)

	



		}

	if(!is.null(filename))
{

	print_plot(plots, filename, 16, height)

	dev.off()
}
			
}
#convenience function to print plots with preferred settings
print_plot<-function(plot, filename="plot.png", width=20, height=15)
{
	require(lattice)
require(latticeExtra)
require(grid)
require(gridExtra)
	setwd("~/Desktop")
png(file=filename, width=width, height=height, units="cm",  pointsize=20, bg="transparent",  res=300);trellis.par.set(fontfamily="Times");trellis.par.set(fonsize=12);trellis.par.set(axis.text=list(cex=1.4));trellis.par.set(key.text=list(cex=1.4));print(plot);dev.off()
}

topng<-function(plot, width=200, height=100, pointsize=20)
{
  png(file="plot.png", width=width, height=height, units="mm",  pointsize=pointsize, bg="transparent",  res=300)
  par(mar = c(0, 0,0,0))
  print(plot)
  
  dev.off()
  
}

#Effect plots with all layout settings with just one call

effectPlot<-function(focal.predictor, model, width=40, height=15)
{
  require("effects")
  
  
  plot<-plot(Effect(as.character(focal.predictor),model), main="", xlab="", ylab="")
  print_plot(plot, "plot.png", width, height)
  
  
}




#Convenience function to make simple dataordered frequency PNG plots for 2 datasets without typing all the settings
#Also outputs ordered frequency table
compare<- function(data,data1, x, y=NULL, filename=NULL,  stack=TRUE, height=9)
{
require(lattice)
require(latticeExtra)
require(grid)
require(gridExtra)
	
	if(!is.null(y))
	{
		table<-table(data[,x],data[,y])
		freq<- prop.table(table, 1)
		freq <- freq[order(freq[,1]),]
		print(table)
		print(freq)
		cat("-----------\n")
	table1<-table(data1[,x],data1[,y])
		freq1<- prop.table(table1, 1)
		freq1 <- freq1[order(freq1[,1]),]
		print(table1)
		print(freq1)
		cat("-----------\n")

	
	plot<-dotchart(freq, stack=stack, auto.key = list(space = "top",  columns=2),   main="",   xlab="", ylab="",par.settings=list(superpose.polygon=list(col=c("#4D4D4D","#E6E6E6"), family="Times", fontsize=12, cex=0.8)))
	 
		plot1<-dotchart(freq1,stack=stack,  auto.key = list(space = "top", columns=2),  main="",  xlab="", ylab="", par.settings=list(superpose.polygon=list(col=c("#4D4D4D","#E6E6E6"), family="Times", fontsize=12, cex=0.8)))
		 
		

			
			plots<-c(plot, plot1,layout = c(2, 1), merge.legends=F)
			print(plots)
			
		}
else
{
	
	table<-table(data[,x])
			table <- sort(table)
		print(table)
		
			table1<-table(data1[,x])
			table1 <- sort(table1)
		print(table)


	plot<-dotchart(table, stack=stack,  auto.key = list(space = "top", columns=2
),  main="", 
 xlab="", ylab="", col=c("#E6E6E6", "#4D4D4D"))

	plot1<-dotchart(table1, stack=stack,  auto.key = list(space = "top",  columns=2),  main="", 
 xlab="", ylab="", col=c("#E6E6E6", "#4D4D4D"))



	plots<-c(plot, plot1, layout = c(2, 1), merge.legends=F)
	print(plots)

	



		}

	if(!is.null(filename))
{

	print_plot(plots, filename, width=16, height)
	
}
			
}



overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}


#Convenience function to print model summary for glmer, glm, and ctree 
#Includes C, Somer's Dxy, R2, and %correctly classified

sum_stats<- function(model)
{
if(!require("Hmisc")){
    supressMessages(install.packages("Hmisc"))
        supressMessages(library("Hmisc"))
}
if(!require("car")){
    supressMessages(install.packages("car"))
        supressMessages(library("car"))
}
if(!require("MuMIn")){
    supressMessages(install.packages("MuMIn"))
        supressMessages(library("MuMIn"))
}


if ("glmerMod" %in%  class(model))
{
  
	summary<-summary(model)
c.index<-cindex(model)

try(rsquare<-suppressMessages(r.squaredGLMM(model)))


if(!exists("rsquare")){
  rsquare.full<-"NaN"
  rsquare.fixed<-"NaN"
}
else
{
  try(rsquare.full<-round(rsquare[[2]], 3))
  try(rsquare.fixed<- round(rsquare[[1]], 3))
}

p <- as.numeric(predict(model, type="response")>0.5)
mean(p==data[,dependent])	
table<-table(p,data[,dependent])
correct=(sum(table[1], table[4])/sum(table))*100
fixed.only<-glm(as.formula(nobars(model@call)), data=model@frame, family="binomial")
pF <- as.numeric(predict(fixed.only, type="response")>0.5)
mean(pF==data[,dependent])	
tableF<-table(pF,data[,dependent])
correctF=(sum(tableF[1], tableF[4])/sum(tableF))*100
proportion.correct.full<-round(correct, 3)
proportion.correct.fixed<-round(correctF, 3)
#overdispersion

overdisp<-overdisp_fun(model)


#VIF

vif.fixed.only<-vif(fixed.only)
	
results<-list(summary=summary, c.index=c.index, rsquare.full=rsquare.full, rsquare.fixed=rsquare.fixed, proportion.correct.full=proportion.correct.full, proportion.correct.fixed=proportion.correct.fixed, vif.fixed.only=vif.fixed.only, overdispersion=overdisp)
return(results)
	
	}
	else if("glm" %in% class(model))
{
summary<-summary(model)
	data<-model$data
	dependent<-all.vars(formula(model)[[2]])
	
c.fixed<- round(somers2(fitted(model), as.numeric(data[,dependent])-1)[[1]], 3)
dxy.fixed<-round(somers2(fitted(model), as.numeric(data[,dependent])-1)[[2]], 3)

# R Squared

r.square.fixed<-round(r.squaredGLMM(model)[[1]], 3)

#Proportion correct

p <- as.numeric(predict(model, type="response")>0.5)
mean(p==data[,dependent])	
table<-table(p,data[,dependent])
correct<-(sum(table[1], table[4])/sum(table))*100
proportion.correct.fixed<-round(correct, 3)
#Predictions vs observed table
	
#VIF

vif.fixed.only<-vif(model)	

results<-list(summary=summary, c.fixed=c.fixed, dxy.fixed=dxy.fixed,  rsquare.fixed=rsquare.fixed,  proportion.correct.fixed=proportion.correct.fixed, vif.fixed.only=vif.fixed.only)
return(results)

}
	else if ("BinaryTree" %in% class(model))
	{
		
	dependent<-model@data@get("response")[,1]

#Somers2
your.ctree.pred <- unlist(treeresponse(model))[c(FALSE, TRUE)]
c.full<-somers2(your.ctree.pred, as.numeric(dependent) - 1)[[1]]
dxy.full<-somers2(your.ctree.pred, as.numeric(dependent) - 1)[[2]]


#Predictions correct
table<-table(predict(model), dependent)
correct<-(sum(table[1], table[4])/sum(table))*100
proportion.correct.full<-round(correct, 3)

results<-list( c.full=c.full, dxy.full=dxy.full,  proportion.correct.fixed=proportion.correct.full)
return(results)



}
else
{
	stop("This function requires a model object of the class glm, merMod, or BinaryTree")
}
}


cindex<-function(model)
{
  require("Hmisc")
  if ("glmerMod" %in%  class(model)[1])
  {
    
  data<-model@frame
  dependent<-all.vars(formula(model)[[2]])
  fixed.only<-glm(as.formula(nobars(formula(model@call))), family="binomial", data=data)
  
  c.full<-round(somers2(fitted(model), as.numeric(data[,dependent])-1)[[1]], 3)
  dxy.full<- round(somers2(fitted(model), as.numeric(data[,dependent])-1)[[2]], 3)
  c.fixed<-round(somers2(fitted(fixed.only), as.numeric(data[,dependent])-1)[[1]], 3)
  dxy.fixed<- round(somers2(fitted(fixed.only), as.numeric(data[,dependent])-1)[[2]], 3)
  
  result<-list(c.full=c.full, dxy.full=dxy.full, c.fixed=c.fixed, dxy.fixed=dxy.fixed)

  }
  else if("glm" %in% class(model)[1])
  {	
    data<-model$data
  dependent<-all.vars(formula(model)[[2]])
  
  c.fixed<- round(somers2(fitted(model), as.numeric(data[,dependent])-1)[[1]], 3)
  dxy.fixed<-round(somers2(fitted(model), as.numeric(data[,dependent])-1)[[2]], 3)
  result<-list(c.fixed=c.fixed, dxy.fixed=dxy.fixed)
  }
  else if ("BinaryTree" %in% class(model)[1])
  {
    
    dependent<-model@data@get("response")[,1]
    
    #Somers2
    your.ctree.pred <- unlist(treeresponse(model))[c(FALSE, TRUE)]
    c.fixed<-somers2(your.ctree.pred, as.numeric(dependent) - 1)[[1]]
    dxy.fixed<-somers2(your.ctree.pred, as.numeric(dependent) - 1)[[2]]
    
    result<-list(c.fixed=c.fixed, dxy.fixed=dxy.fixed)
  }
  else if("RandomForest"  %in% class(model)[1])
  {
    predictions<-treeresponse(model)
    your.ctree.pred <- unlist(predictions)[c(FALSE, TRUE)]
    c.full<-somers2(your.ctree.pred, as.numeric(model@data@get("response")[,1]) - 1)[[1]]
    dxy.full<-somers2(your.ctree.pred, as.numeric(model@data@get("response")[,1]) - 1)[[2]]
    
    result<-list(c.fixed=c.fixed, dxy.fixed=dxy.fixed)
    
  }
  else if ("bam" %in% class(model)[1])
  {

    dependent<-model$y
    c.full<- round(somers2(fitted(model), dependent), 3)[[1]]
    dxy.full<-round(somers2(fitted(model), dependent), 3)[[2]]
    result<-list(c.full=c.full, dxy.full=dxy.full)
  }
  else if ("constparty" %in% class(model)[1])
  {
    model<-as.constparty(model)
    dependent<-model[1]$data[, 1]
    c.full<- round(somers2(predict(model, type="prob")[,2], as.numeric(dependent)-1), 3)[[1]]
    dxy.full<-round(somers2(predict(model, type="prob")[, 2], as.numeric(dependent)-1), 3)[[2]]
    result<-list(c.full=c.full, dxy.full=dxy.full)
  }
  
  else
  {
    stop("Argument model must of class: bam(mgcv), glm (base), glmerMod (lme4), BinaryTree (party), RandomForest (party), constparty (partykit),  or gam (mgcv")
  }

  return(result)
  
}


cp<-function(model1, model2)
{
 require("MuMIn")
  if ((c("glmerMod", "bam") %in%  class(model1)) && (c("glmerMod", "bam") %in%  class(model2)))
  {
if (AICc(model1)<AICc(model2))
{
  cat ("Best model:", as.character(as.formula(model1@call)), "\n\n")
cat("AICc model 1: ", AICc(model1), "\n")
cat("AICc model 2: ", AICc(model2), "\n")
dif<-AICc(model2)-AICc(model1)
cat("Difference: ",dif, "\n")
}
  else
  {
    cat ("Best model:", as.character(as.formula(model2@call)), "\n\n") 
    cat("AICc model 1: ", AICc(model1), "\n")
    cat("AICc model 2: ", AICc(model2), "\n")
    dif<-AICc(model1)-AICc(model2)
    cat("Difference: ",dif, "\n")
  }
  }
  else
  {
    stop("This function requires objects of the class 'glmerMod'")
  }
  
}


tabler<-function(var, dep) { 
  require("vcd")
      t<-table(factor(var), factor(dep))
     labels<-row.names(t)
       marg<-prop.table(t, 1)
     results<-data.frame(row.names=F)
      for (y in 1:length(labels))
     {
               f<-data.frame(Levels=labels[y], N=paste(t[y, 2], "/", sum(t[y,]), sep="", collapse=""), Perc=round(marg[y, 2]*100, 2))  
              rbind(f, results)->results
      }
     cat("\n\n-------Table---------\n")
         print(results)
   cat("\n\n-------Fisher Significance---------\n")
         print (fisher.test(t, simulate.p.value = T))
        cat("\n\n-------Effect size---------\n")
         print(assocstats(t))
     
}

summaryRandomForest<-function(model)
{
  require(Hmisc)
  require(party)
  if(c("RandomForest")  %in% class(model))
  {
    data<-model@data@get("input")
    predictions<-treeresponse(model)
    data$predictions<- do.call("rbind", lapply(predictions, as.data.frame))[, 2]
    overall.mean<-mean(data$predictions)
    number.vars<-ncol(data)-1
    
    smmry<-data.frame(Predictors=character(), Levels=character(), Mean.prob=numeric(), Effect=numeric())
    new<-data.frame(Predictors="(intercept)", Levels=" ", Mean.prob=overall.mean, Effect=0)
    rbind(smmry, new)->smmry
    
    for(y in 1:number.vars)
    {
      dlevels<-as.list(levels(factor(data[, y])))
      
      for (i in 1:length(dlevels))
      {
        
        effect<-mean(data[data[, y]==dlevels[[i]],"predictions"])-overall.mean
      
        
        new<-data.frame(Predictors=colnames(data)[y], Levels=dlevels[[i]], Mean.prob=mean(data[data[, y]==dlevels[[i]],"predictions"]), Effect= effect)
        rbind(smmry, new)->smmry
        
      }
      
    }
    
    
    your.ctree.pred <- unlist(predictions)[c(FALSE, TRUE)]
    c.full<-somers2(your.ctree.pred, as.numeric(model@data@get("response")[,1]) - 1)[[1]]
    dxy.full<-somers2(your.ctree.pred, as.numeric(model@data@get("response")[,1]) - 1)[[2]]
    
    
    result<-list(call=model@data, summary=smmry, cIndex=c.full, dxy=dxy.full)
    return(result)
    
  }
  else
  {
    stop("'model' should be a party random forest")
  }
  
}

batchRecode<-function(vector, regular.expression, new.value)
{
  vector.type<-class(vector)
  vector<-as.character(vector)
  vector[grep(regular.expression, vector)]<-new.value
  if(vector.type=="factor")
  {
    vector<-as.factor(vector)
  }
  
  return(vector)
}


