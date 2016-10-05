# This function takes a lme4 model object  and the corresponding dataframe object  as its input. 
#  The function then sets up a cluster to recombine the model terms in all possible subsets  using the pdredge function of the MuMIn package
# use: dredge<-par_dredge (model, data, 4)
# The subsets argument defines subsets that should always/not be included in the model. See ??MuMIn::dredge under "subsetting"
#I strongly recommend that you use detectCores() from the Parallel package to set the number of cores/workers


par_dredge<-function (model, data, cores, subsets=NULL)
{
	
	if(!require("lme4")){
    install.packages("lme4")
    library("lme4")
}

	if(!require("snow")){
    install.packages("snow")
    library("snow")
}
if(!require("MuMIn")){
    install.packages("MuMIn")
    library("MuMIn")
}


options(na.action = "na.fail")
cat(sep="\n", "Setting up cluster")
cat(sep="\n", "\n")
clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", cores), type = clusterType))
clusterEvalQ(clust, library(lme4))
clusterEvalQ(clust, library(stats4))
clusterExport(clust, deparse(substitute(data)), envir=environment())
clusterExport(clust, deparse(substitute(model)), envir=environment())
if(is.null(subsets))
{
  dredged_model<- pdredge(get(deparse(substitute(model)), envir=environment()), cluster = clust)
}
else
{
  dredged_model<- pdredge(get(deparse(substitute(model)), envir=environment()), cluster = clust, subset(subset))
}

return(dredged_model)
stopCluster(clust)
}
