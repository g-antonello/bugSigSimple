#' Author: Giacomo Antonello
#' Date: 2025-03-17
#' 
#' Description:
#' 
#' This function takes a raw bugSigDB input from `bugsigdbr` and generates a 
#' unique idenfier as curatedMetagenomicsData does: full last name, initial(s) of 
#' first name(s) and year of publication. Additionally, it checks if there are 
#' more PMID codes associated with the same ID and adds a .1, .2, for each 
#' duplication
#' 

make_unique_study_ID <- function(bsdb.df){
  # Get first author last name and first name initials 
  bsdb.df$FirstAuthor <- sapply(strsplit(bsdb.df$`Authors list`, split = ", "), "[[", 1)
  
  # Create basic Study code with same structure as other cMD studies
  bsdb.df$`Study code` = paste(gsub(" ", "", bsdb.df$FirstAuthor, fixed = T), bsdb.df$Year, sep = "_")
  
  # fix possible cases where the same first author published more than once in
  # the same year. those need to be separated
  df.split <- split.data.frame(bsdb.df, bsdb.df$`Study code`, drop = FALSE)
  
  tmp <- lapply(df.split, function(x) {
    PMID_ranked <- unique(x$PMID)
    names(PMID_ranked) <- rank(PMID_ranked) - 1
    
    for(i in 1:nrow(x)){
      x[i, "Study code"] <- paste(x[i, "Study code"], names(PMID_ranked)[PMID_ranked == x[i, "PMID"]], sep = ".")
    }
    x$`Study code` <- gsub("\\.0$", "", x$`Study code`)
    return(x)
  })
  
  bsdb_fixed_dups.df <- Reduce(rbind.data.frame, tmp)
  
  #relocate study ID in front
  colIndx <- which(colnames(bsdb_fixed_dups.df) == "Study code")
  bsdb_fixed_dups.df <- cbind.data.frame(bsdb_fixed_dups.df[[colIndx]], bsdb_fixed_dups.df[,-colIndx])
  colnames(bsdb_fixed_dups.df)[1] <- "Study code"
  
  return(bsdb_fixed_dups.df)
}