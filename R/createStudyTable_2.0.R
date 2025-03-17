#' Create Study Table 2.0
#' 
#' A rewritten function that should take better care of edge cases.
#' Furthermore, it was written fully in base R to avoid depending on tidyverse
#'
#' @param bsdb.df \code{data.frame} as generated with `bugsigdbr::importBugSigDB()`
#' @param rows_are \code{character}, either `experiments` or `studies`. See details for more information
#' @param includeAlso \code{character}
#' @details
#' `rows_are` If `experiments`, each row will be an experiment 
#' (could be more per study). If `studies`, each row will be a study. 
#' In that case, Sample sizes cannot be generalized, and only details study 
#' level details will be returned
#' 
#' @returns \code{data.frame}
#' @export
#'
#' @examples
#' library(bugsigdbr)
#' bsdb <- importBugSigDB(cache = FALSE)
#' efo_id <- "MONDO:0005180" 
#' host <- "Homo sapiens"
#' 
#' bsdb_PD_signatures <- bsdb[grepl(efo_id, bsdb$`EFO ID`) & (bsdb$`Host species` == host),]
#' # experiments as rows
#' createStudyTable_2.0(bsdb_PD_signatures, includeAlso = c("Sequencing type", "Sequencing platform"))
#' 
#' # studies as rows
#' createStudyTable_2.0(bsdb_PD_signatures, rows_are = "studies", includeAlso = c("Sequencing type", "Sequencing platform"))


createStudyTable_2.0 <- function(bsdb.df, rows_are = "experiments", includeAlso = NULL){
  
  # input validation
  if(!(rows_are %in% c("experiments", "studies"))){
    stop("`rows_are` parameter not valid, it should be either 'experiments' or 'studies'")
  }
  
  if(!is_null(includeAlso)){
    if(!all(includeAlso %in% colnames(bsdb.df))){
      stop(paste("The following columns are not found in the input data frame:", paste(includeAlso[!(includeAlso %in% colnames(bsdb.df))], collapse = ", ")))
    }
  }
  
  # fix doi codes
  bsdb.df$DOI <- complete_DOI_link(bsdb.df$DOI)
  
  # Create study IDs and put them in front
  # reorder study codes alphabetically
  bsdb.df <- make_unique_study_ID(bsdb.df)
  
  # Make selection vector for the columns generally of interest
  cols_to_keep <- c("Study code", "Condition", "Body site", "Experiment", "Group 1 sample size", "Group 0 sample size", "Study design", includeAlso, "PMID", "DOI")
  names(cols_to_keep) <- c("Study code", "Condition", "Body site", "Experiment", "Cases", "Controls", "Study design", includeAlso, "PMID", "DOI")
    
  bsdb_subsetCols.df <- bsdb.df[, cols_to_keep]
  colnames(bsdb_subsetCols.df) <- names(cols_to_keep)
  
  # remove duplicate rows, that are due to increased/decreased levels in the
  # `Abundance in Group 1` column
  bsdb_subsetCols.df <- bsdb_subsetCols.df[!duplicated(bsdb_subsetCols.df),]
  
  # Create StudyCount column
  
  study_ranks <- tibble("Study code" = unique(bsdb_subsetCols.df$`Study code`), StudyCount = 1:length(unique(bsdb_subsetCols.df$`Study code`)))
  
  tmp <- merge(study_ranks, bsdb_subsetCols.df, by = "Study code", all.y = TRUE)
  table_intermediate.df <- cbind.data.frame(tmp$StudyCount, tmp[, c(1, 3:ncol(tmp))])
  colnames(table_intermediate.df)[1] <- "StudyCount"
  
  if(rows_are == "experiments"){
    table_finished.df <- table_intermediate.df
  }
  
  if(rows_are == "studies"){
    table_finished.df <- table_intermediate.df[, c("StudyCount", "Study code", "Study design", "PMID", "DOI")]
    table_finished.df <- table_finished.df[!duplicated(table_finished.df),]
    }
  
  return(table_finished.df)
}
