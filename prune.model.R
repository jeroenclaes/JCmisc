#License
#This script is released under the terms of the Creative Commons Attribution-Non Commercial 2.0 Generic (CC BY-NC 2.0) license. Please refer to:https://creativecommons.org/licenses/by-nc/2.0/"

#This function will take a full model, and remove random slopes one by one, checking each time wether taking out the random slope lowers the AICc statistic compared to the original full model or a more plausible, previous model. (=it will perform step-down/backward model selection). Following Burnham & Anderson (2002:70) the script assumes by default that AICc differences > 2 suggest that the model with the higher AICc has considerably less support than the candidate model with a lower AICc. If a model has a AICc that is more than 2 units smaller than the previous candidate model, the script will remove the predictor. Custom thresholds can be set if required, by adding threshold=3, 4, or whatever value to the function call
#It will not remove random intercept terms, so don't forget to check afterwards that these are appropriate.
#Assuming this slimmed down random effects structure, it will do the same for the interactions and later the fixed effects in your model. When the elimination of a fixed effect is evaluated, the script will also eliminate any remaining slopes and interactions associated with this fixed effect to avoid having a slope/interaction in the model for a fixed effect that was eliminated.
#IMPORTANT: As a hack, this script will replace : in the formula with *. If you need a full interaction, code a new factor for this interaction.

#Remember Harrell, F. (2001). Regression Modeling strategies: 56-57, the biggest danger of stepwise variable selection is "f. It allows us to not think about the problem."

#use: pruned.model<-prune.model(model)

prune.model<-function(model, verbose=F, threshold=1.99)
{
  require("MuMIn")
  require("car")
  require("lme4")
  
  #slim down random effects first
  pruned.model<-prune.random(model, verbose=verbose, threshold=threshold)
  #then step down the fixed effects
  pruned.model2<-prune.fixed(pruned.model$model, verbose=verbose,threshold=threshold)
 #merge output of two functions 
  rbind(pruned.model$dropped.terms, pruned.model2$dropped.terms)->dropped.terms
  rbind(pruned.model$akaike.weights, pruned.model2$akaike.weights)->akaike.weights
  akaike.weights$weight<-Weights(akaike.weights$AICc)
  importance<-pruned.model2$importance
  output<-list(model=pruned.model2$model, dropped.terms=dropped.terms[order(dropped.terms$AICcDifference),], akaike.weights=akaike.weights[order(akaike.weights$weight),], call=pruned.model2$call)
 
return(output)
}



#this function will take a full model, remove fixed effects and interactions one by one and compare the AICc values with either the original, full model or a previous better fitting model. It is intended as a **tool** to help you decide which fixed effects (terms and interactions) are useful to model your data
#As a hack, this script will replace : in the formula with *. If you need a full interaction, code a new factor for this interaction.
#it will also calculate AICc and Akaike Weights for all candidate models that are considered along the way.
#By summing together the Aikaike Weights of the models that contain a particular term/interaction, it provides an estimate of the relative importance of the variable (See Burnham & Anderson, 2002). The implementation is rather hacky, so don't build conclusions on it. 

#use: pruned.model<-prune.fixed(model)

#The output is a list of the following objects:
#$dropped.terms: dataframe with names of the variables that were removed by the script, with the AICc values of the last model they appeared in, the first model without them, and the difference between those two
#$akaike.weights:dataframe with the model calls and the AICc values  of all the models that were evaluated. Also includes a transformation of the AICc values, the models' Akaike Weights
#$variable.importance: data frame with variable names and sum of the akaike weigths of the models they appear in
#$model: the "best" candidate model
#$call: the model call of the "best" candidate model


prune.fixed<-function(model, verbose=F, threshold=1.99)
  
{
  require("MuMIn")
  require("car")
  require("lme4")
  
  if(class(model) %in% c("glmerMod"))
  {
    tryCatch({
      
      terms.start<-as.list(attributes(terms(as.formula(nobars(model@call))))$term.labels)
      
      terms<-as.list(attributes(terms(as.formula(nobars(model@call))))$term.labels)
      terms<-gsub("[\\:]+", "\\*", terms,  perl=T) 
      formula<-as.formula(model@call)
      i=1
      modelbase<-model
      dropped.terms<-data.frame(dropped.term=character(), AICcWith=numeric(), AICcWithout=numeric(), AICcDifference=numeric(), stringsAsFactors=FALSE)
      akaike.weights<-data.frame(call=character(), AICc=numeric(), weight=numeric())
      weight<-data.frame(call=paste(unlist(terms), sep=" ", collapse=" "), AICc=AICc(modelbase), weight=0)
      rbind(weight, akaike.weights)->akaike.weights
      

   
      
      
      #First we strip the interaction effects one by one
      if(any(grep("(\\:|\\*)", terms, perl=T)))
      {
        if(isTRUE(verbose))
        {
          cat("Working on the interaction terms \n\n")
        }
        
        interactions<-terms[grep("(\\:|\\*)", terms, perl=T)]

          z=1
          for(z in 1:length(interactions))
          {
            
            if(isTRUE(verbose))
            {
              cat("Working on: ", as.character(trimws(interactions[[z]])), "\n\n")
            }
            
            updated<-update.formula(formula, paste("~.-",as.character(trimws(interactions[[z]])))) 
   
            #search for random slopes attached to these interactions effects and remove these as well
            random.effects<-findbars(updated)
            
            new.random<-gsub("[\\s]+", "",as.character(random.effects), perl=T)
            new.random<-gsub("[\\*]+", ":",as.character(random.effects), perl=T)
          interactions[[z]]<-gsub("[\\*]+", ":",as.character(interactions[[z]]), perl=T)
          interactions[[z]]<-gsub("[\\s]+", "",as.character(interactions[[z]]), perl=T)
            new.random<-gsub(as.character(interactions[[z]]), "", new.random, perl=T)
            
            new.random<-gsub("[\\+]+", " ", new.random, perl=T)
            
            new.random<-gsub("[\\s]+", " ", new.random, perl=T) 
            new.random<-gsub("[\\s]+", "\\+", new.random, perl=T) 
            new.random<-gsub("[\\:]+", "\\*", new.random, perl=T) 
            new.random<-gsub("(\\+\\*\\+)+", "\\*", new.random, perl=T)
            new.random<-gsub("(\\+)+", "\\+", new.random, perl=T)
            new.random<-gsub("([\\+]+[\\|])", " \\|", new.random, perl=T) 
            new.random<-gsub("([\\|][\\+]+)", " \\|", new.random, perl=T)
            
            updated<-nobars(updated)
            
            k=1
            for (k in 1:length(new.random)) {updated<-update.formula(updated, paste("~.+(",new.random[k],")"))}
            updated<-gsub("[\\:]", "\\*", as.character(updated),  perl=T)
            updated<-as.formula(paste(updated[2], updated[1], updated[3], collapse="", sep=""), env=environment(fun=as.formula(model@call)))
            
          
            
            
            #Run the model again to see whether this is an improvement in information-theoretic terms
           
          
             model2<-glmer(updated, data=model@frame, family=model@resp$family$family, control = glmerControl(optimizer = model@optinfo$optimizer,  calc.derivs = FALSE)) 
            
            weight<-data.frame(call=paste(unlist(as.list(attributes(terms(as.formula(nobars(model2@call))))$term.labels)), sep=" ", collapse=" "), AICc=AICc(model2), weight=0)
            rbind(weight, akaike.weights)->akaike.weights
            
           
            
            if(round(AICc(modelbase)-AICc(model2)) > threshold)
            {
              
              drop<-data.frame(dropped.term=interactions[[z]], AICcWith=AICc(modelbase), AICcWithout=AICc(model2), AICcDifference=AICc(modelbase)-AICc(model2))
            rbind(drop, dropped.terms)->dropped.terms
            modelbase<-model2
            formula<-as.formula(modelbase@call)
              if(isTRUE(verbose))
              {
                cat("Dropped interaction: ", as.character(trimws(interactions[[z]])), "\n\n")
              }
              
              
           }
            
            
          }
          
        }
        
        
        
        
  #Now we do the same for the independent fixed effects
      
      fixed.effects<-as.list(attributes(terms(as.formula(nobars(modelbase@call))))$term.labels)
      terms<-as.list(attributes(terms(as.formula(nobars(modelbase@call))))$term.labels) 
      interactions<-terms[grep("(\\:|\\*)", terms, perl=T)]
      
for (l in 1:length(fixed.effects))
{
  
  if(any(grep("(\\:|\\*)", fixed.effects[l], perl=T)))
  {
    fixed.effects[l]<-""
  }
}
      if(isTRUE(verbose))
      {
        cat("Working on the main effects\n\n")
      }
      
      fixed.effects<-fixed.effects[fixed.effects!=""]
      i=1
      for (i in 1:length(fixed.effects))
      {
       
        if(isTRUE(verbose))
        {
          cat("Working on: ", as.character(trimws(fixed.effects[[i]])), "\n\n")
        }
        
        
        #Remove 1 fixed effect
        updated<-update.formula(formula, paste("~.-",as.character(trimws(fixed.effects[[i]])), sep="", collapse=""))

        
        #Search for remaining partial interactions attached to this fixed effect and remove these as well
        
        
        
        if(any(grep(fixed.effects[[i]], interactions)))
        {
          interactions<-interactions[grep(fixed.effects[[i]], interactions)]
          z=1
          for(z in 1:length(interactions))
          {
            
            updated<-update.formula(updated, paste("~.-",as.character(trimws(interactions[[z]])), sep="", collapse="")) 
            
          }

        }
       
        #search for random slopes attached to this fixed effect and remove these as well
        random.effects<-findbars(updated)
        
        new.random<-gsub("[\\s]+", "",as.character(random.effects), perl=T)
      
        new.random<-gsub("\\*", ":",new.random)
        new.random<-gsub(paste("(?<!(:))(", gsub("\\*", ":", fixed.effects[[i]]), ")(?!(:))", collapse="", sep=""), "", new.random, perl=T)
      
        
        new.random<-gsub(":", "\\*", new.random)
        new.random<-gsub("[\\+]+", " ", new.random, perl=T)
        new.random<-gsub("[\\s]+", " ", new.random, perl=T) 
        new.random<-gsub("[\\s]+", "\\+", new.random, perl=T) 
        new.random<-gsub("[\\:]+", "\\*", new.random, perl=T) 
        new.random<-gsub("(\\+\\*\\+)+", "\\*", new.random, perl=T)
        new.random<-gsub("(\\+)+", "\\+", new.random, perl=T)
        new.random<-gsub("([\\+]+[\\|])", " \\|", new.random, perl=T) 
        new.random<-gsub("([\\|][\\+]+)", " \\|", new.random, perl=T)
        
        updated<-nobars(updated)
        
        k=1
        for (k in 1:length(new.random)) {updated<-update.formula(updated, paste("~.+(",new.random[k],")"))}
        updated<-gsub("[\\:]", "\\*", as.character(updated),  perl=T)
        updated<-as.formula(paste(updated[2], updated[1], updated[3], collapse="", sep=""), env=environment(fun=as.formula(model@call)))
      
  
        #Run the model again to see whether this is an improvement
        model2<-glmer(updated, data=model@frame, family=model@resp$family$family, control = glmerControl(optimizer = model@optinfo$optimizer,  calc.derivs = FALSE)) 
        
       
        weight<-data.frame(call=paste(unlist(as.list(attributes(terms(as.formula(nobars(model2@call))))$term.labels)), sep=" ", collapse=" "), AICc=AICc(model2), weight=0)
        rbind(weight, akaike.weights)->akaike.weights
        
        
        
        if(round(AICc(modelbase)-AICc(model2)) > threshold )
        {
          
          drop<-data.frame(dropped.term=fixed.effects[[i]], AICcWith=AICc(modelbase), AICcWithout=AICc(model2), AICcDifference=AICc(modelbase)-AICc(model2))
          rbind(drop, dropped.terms)->dropped.terms
          modelbase<-model2
          formula<-as.formula(modelbase@call)
          if(isTRUE(verbose))
          {
            cat("Dropped fixed effect: ", trimws(gsub(":", "\\*", as.character(fixed.effects[[i]]),  perl=T)),"\n\n")
          }
          
        }
        
      }
      
      akaike.weights$weight<-Weights(akaike.weights$AICc)
      importance<-data.frame(variable=character(), importance=numeric(), n.models=numeric(), stringsAsFactors = F)
      i=0
      for (i in 1:length(terms.start))
      {
        formulas<-gsub("\\*", ":", akaike.weights$call)
        formulas<-sub("[\\s]+", "",formulas, perl=T)
        variable.importance<-sum(akaike.weights[grep(paste("((?<!(:))",gsub("\\*", ":", terms[i]),")(?!(:))", collapse="", sep=""), formulas, perl=T), "weight"])
        n.models<-length(grep(paste("((?<!(:))",gsub("\\*", ":", terms[i]),")(?!(:))", collapse="", sep=""), formulas, perl=T))
        im<- data.frame(variable=as.character(terms.start[i]), importance=variable.importance, n.models=n.models, stringsAsFactors = F)
        rbind(im, importance)->importance
      }
      
      
      
      return(output<-list(variable.importance=importance[order(importance$importance),], dropped.terms=dropped.terms[order(dropped.terms$AICcDifference),], akaike.weights=akaike.weights[order(akaike.weights$weight),], model=modelbase, call=as.formula(modelbase@call)))
    }, warning=function(w)
    {cat ("Warning", as.character(w))

    }, 
    error=function(e)
    {cat ("Error", as.character(e))

    }
    )
    
  }
  else
  {
    stop("This function requires a model of the class glmerMod")
  }
  
  
}



#this function will take a full model, remove random slopes one by one and compare the AICc values with either the original, full model or a previous better fitting model. It is intended as a **tool** to help you decide which random slopes are useful to model your data
#IMPORTANT: As a hack, this script will replace : in the formula with *. If you need a full interaction, code a new factor for this interaction.
#it will also calculate AICc and Akaike Weights for all candidate models that are considered along the way.

#use: pruned.model<-prune.random(model)
#for verbose output, add: verbose=T

#The output is a list of the following objects:
#$dropped.terms: dataframe with names of the slopes that were removed by the script, with the AICc values of the last model they appeared in, the first model without them, and the difference between those two
#$akaike.weights:dataframe with the model calls and the AICc values  of all the models that were evaluated. Also includes a transformation of the AICc values, the models' Akaike Weights
#$model: the "best" candidate model
#$call: the model call of the "best" candidate model

prune.random<-function(model, verbose=F, threshold=1.99)
{
  require("MuMIn")
  require("lme4")
  
  
  if(class(model) %in% c("glmerMod"))
  {
    
    
    dropped.terms<-data.frame(dropped.term=character(), AICcWith=numeric(), AICcWithout=numeric(), AICcDifference=numeric())
    
    akaike.weights<-data.frame(call=character(), AICc=numeric(), weight=numeric())
    weight<-data.frame(call=paste((findbars(as.formula(model@call))), sep="/", collapse=""), AICc=AICc(model), weight=0)
    rbind(weight, akaike.weights)->akaike.weights
    
   
    random<-findbars(as.formula(model@call))
    i=1
    modelbase<-model
    formula.base<-as.formula(modelbase@call)
    
    intercepts<-as.list(names(VarCorr(modelbase)))   
    
    tryCatch({
      i=1
      
      for (i in 1:length(intercepts))
      {
        
        if(ncol(ranef(model)[[i]])> 1)
        {
          
          position<-grep(intercepts[i], findbars(formula.base))
          random.slopes<-gsub(as.character(intercepts[i]),"", as.character(findbars(formula.base)[[position]]), perl=T)
          random.slopes<-gsub("\\|", "", random.slopes)
          random.slopes<-gsub("1 \\+", "", random.slopes)
          random.slopes<-gsub("1[ ]+\\+", "", random.slopes)
          random.slopes <- trimws(as.character(random.slopes[random.slopes !=""])) 
          random.slopes<-strsplit(random.slopes, "\\+")[[1]]
          y=1
          for (y in 1:length(random.slopes))
          {
            random.slopes[y]<-gsub("[\\s+]", "", random.slopes[y], perl=T)
            random.slopes[y]<-gsub("\\*", ":", random.slopes[y])
            new.random<-gsub("[\\s]+", "",as.character(findbars(formula.base)[position]), perl=T)
        new.random<-gsub("\\*", ":", new.random)
            
    
            if(isTRUE(verbose))
            {
              cat("Working on: ",as.character(intercepts[i]),"/", as.character(trimws(random.slopes[y])), "\n\n")
            }
        random.slopes[y]<-gsub("[\\s]+", "", random.slopes[y], perl=T)
            new.random<-gsub(paste("(?<!(:))(",random.slopes[y], ")(?!(:))", collapse="", sep=""), "", new.random, perl=T)
            

            
            new.random<-gsub("[\\+]+", " ", new.random, perl=T)
        
            new.random<-gsub("[\\s]+", " ", new.random, perl=T) 
            new.random<-gsub("[\\s]+", "\\+", new.random, perl=T) 
            new.random<-gsub("[\\:]+", "\\*", new.random, perl=T) 
            new.random<-gsub("(\\+\\*\\+)+", "\\*", new.random, perl=T)
            new.random<-gsub("(\\+)+", "\\+", new.random, perl=T)
            new.random<-gsub("([\\+]+[\\|])", " \\|", new.random, perl=T) 
            new.random<-gsub("([\\|][\\+]+)", " \\|", new.random, perl=T) 
       
            
            new.formula<-update.formula(formula.base, paste("~.-(",findbars(formula.base)[position], ")" ))
            
            
            new.formula<-update.formula(as.formula(new.formula), paste("~.+(", new.random, ")"))
          
            
            new.formula<-gsub("[\\:]", "\\*", as.character(new.formula),  perl=T)
            new.formula<-as.formula(paste(new.formula[2], new.formula[1], new.formula[3], collapse="", sep=""))
            
            model2<-glmer(new.formula, data=model@frame, family=model@resp$family$family, control = glmerControl(optimizer = model@optinfo$optimizer,  calc.derivs = FALSE))   
            weight<-data.frame(call=paste("(", as.character(findbars(as.formula(model2@call))), ")", sep="", collapse="/"), AICc=AICc(model2), weight=0)
            rbind(weight, akaike.weights)->akaike.weights
            
            if(round(AICc(modelbase)-AICc(model2)) > threshold)
           {
              
              if(isTRUE(verbose))
              {
                cat("Dropped: ",as.character(intercepts[i]),"/", as.character(trimws(random.slopes[y])), "\n\n")
              }
              drop<-data.frame(dropped.term=paste (random.slopes[[y]], "/", intercepts[[i]]), AICcWith=AICc(modelbase), AICcWithout=AICc(model2), AICcDifference=AICc(modelbase)-AICc(model2))
              rbind(drop, dropped.terms)->dropped.terms
              
             modelbase<-model2
             formula.base<-as.formula(modelbase@call)
              
            }
            
          }
          
        }
        
      }
      
      akaike.weights$weight<-Weights(akaike.weights$AICc)
      
      return(output<-list(dropped.terms=dropped.terms[order(dropped.terms$AICcDifference),], akaike.weights=akaike.weights[order(akaike.weights$weight),], model=modelbase, call=as.formula(modelbase@call)))
    }, warning=function(w)
    {cat ("Warning:", as.character(w))
   
    }, 
    error=function(e)
    {cat ("Error:", as.character(e))
    
    }
    )
    
    
  }
  else
  {
    stop("This function requires a model of the class glmerMod")
  }
  
}
