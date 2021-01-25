wtp <- function(cost, attr, model) {
  
  wtp_values =data.frame(wtp =numeric(), robse=numeric() , robt= numeric() ) 
  attr <- attr[-which(attr==cost)]
  
  for (a in attr) {
    
    
    deltaMethod_settings=list(operation="ratio", parName1=a, parName2=cost)
    wtp_values[which(attr==a),]<- apollo_deltaMethod(model, deltaMethod_settings)
    
  }
  wtp_values$wtp <- wtp_values$wtp*-1
  wtp_values$robse <- wtp_values$robse*1
  wtp_values$robt <- wtp_values$robt*-1
  wtp_values$pVal <- (1-pnorm((abs(wtp_values$robt))))*2
  
  rownames(wtp_values) <- attr
  return(wtp_values) 
  
}



# function to estmimate simple mnl
mnl <- function(){
  apollo_initialise()
  
  
  
  
  ### Set core controls
  apollo_control <<- list(
    modelName  ="Pilot Data Tofu",
    modelDescr ="Simple MNL model",
    indivID    ="RID"
  )
  
  
  apollo_beta <<-c(bascA = 0,
                   bascB = 0 ,
                   bcost = 0.2,
                   bmanufEU = -0.4,
                   bmanufnonEU = -1,
                   bcultEU = -0.2,
                   bcultnonEU = -0.4,
                   bCONV = -0.4)
  
  
  ### keine Parameter fix halten
  apollo_fixed <<- c()
  
  ### validieren
  #assign("apollo_beta",apollo_beta, envir = .GlobalEnv)
  #assign("apollo_fixed",apollo_fixed, envir = .GlobalEnv)
  #assign("apollo_control",apollo_control, envir = .GlobalEnv)
  apollo_inputs = apollo_validateInputs()
  
  
  apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
    
    ### Function initialisation: do not change the following three commands
    ### Attach inputs and detach after function exit
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))
    
    ### Create list of probabilities P
    P = list()
    
    ### List of utilities (later integrated in mnl_settings below)
    V = list()
    V[['alt1']] = bascA + bcost*a1_x4 + bmanufEU*a1_manuf_EU + bmanufnonEU*a1_manuf_nonEU + bcultEU*a1_cult_EU + bcultnonEU*a1_cult_nonEU + bCONV*a1_x3 
    V[['alt2']] = bascB + bcost*a2_x4 + bmanufEU*a2_manuf_EU + bmanufnonEU*a2_manuf_nonEU + bcultEU*a2_cult_EU + bcultnonEU*a2_cult_nonEU + bCONV*a2_x3    
    V[['alt3']] = 0
    
    
    
    ### Define settings for MNL model component
    mnl_settings = list(
      alternatives  = c(alt1=1, alt2=2, alt3=3) ,
      avail         = 1, # all alternatives are available in every choice
      choiceVar     = pref1,
      V             = V  # tell function to use list vector defined above
      
    )
    
    ### Compute probabilities using MNL model
    P[['model']] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P = apollo_panelProd(P, apollo_inputs, functionality)
    
    ### Average across inter-individual draws - nur bei Mixed Logit!
    ### P = apollo_avgInterDraws(P, apollo_inputs, functionality)
    
    ### Prepare and return outputs of function
    P = apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }
  
  
  model = apollo_estimate(apollo_beta, apollo_fixed,
                          apollo_probabilities, apollo_inputs, 
                          estimate_settings=list(hessianRoutine="maxLik"))
  
  apollo_modelOutput(model, modelOutput_settings)
  
  
  
  return(model)
  
}


# end function mnl

#function to assign labels

ass_labels <- function(path, seelab=TRUE, lower=FALSE) {
  
  labels <- read_excel(path = path, sheet = "dictionary", col_names = TRUE)[-2:-1,2:4]  %>% rename( "Variable"=1 ,   "Labels"=2   ,   "valuelabels"=3)
  
  
  if (lower==TRUE) labels$Variable <- tolower(labels$Variable)
  
  
  covdata <- read_excel(path = path)
  
  
  ## check if variables are all present
  #test 1
  
  cat("Number of variables in dataset: ",length(names(covdata)) ,
      "\n Number of variables in label set:", length(labels$Variable[!is.na(labels$Variable)]), "\n")
  
  if (length(names(covdata)) != length(labels$Variable[!is.na(labels$Variable)]) ){ 
    cat("\n labels do not match, will be ignored, but make sure this is ok. 
           \n Below are the variables that are problematic. \n \n These variables exist in the dataset but not in the label set:", 
        setdiff(names(covdata) , labels$Variable[!is.na(labels$Variable)]) ,
        "\n and these exist in the labels and not in the dataset:" ,
        setdiff(labels$Variable[!is.na(labels$Variable)], names(covdata)))
  }
  
  for (vn in names(covdata)) {
    attr(covdata[[vn]], "label") <- toString(labels[which(labels$Variable==vn),"Labels"])
  }
  
  
  labels$Variable <- na.locf(labels$Variable)
  labels <- labels[!is.na(labels$Labels),]
  
  X <- base::split(labels, labels$Variable)
  
  Y=lapply(X, row_to_names , row_number = 1)
  
  
  for (vn in names(covdata)) {
    
    if (is.null(attr(covdata[[vn]], "class")) == TRUE) {
      attr(covdata[[vn]], "class") <- "haven_labelled"
    }
    
    
    if (exists(vn,Y)==TRUE && nrow(Y[[vn]])>1 ) {
      
      if(seelab==TRUE) cat("\n the variable", vn , " will be labelled \n" )
      
      t <- as.numeric(unlist(Y[[vn]][2]))
      names(t) <- unlist(Y[[vn]][3])
      attr(covdata[[vn]] , "labels") <- t
    }
    else{ if(seelab==TRUE) cat("\n Attention: the values of the variable", vn , " will NOT be labelled \n" )}
    
  }
  
  return(covdata)
  
}  


##for testing
#dce_data <- ass_labels("pilotdata/tofu_main_swe_tofu.xlsx")
