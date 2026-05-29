library(scholar)

id <- "IRM3zPcAAAAJ"

get_scholar_pubs <- function(id) {
  scholar_profile <- try(scholar::get_profile(id), silent = TRUE)
  if (inherits(scholar_profile, "try-error") | isTRUE(is.na(scholar_profile))) {
    scholar_profile <- readRDS("scholar_profile.RData")
  } else {
    saveRDS(scholar_profile, "scholar_profile.RData")
  }
  scholar_pubs <- try(scholar::get_publications(id, flush = TRUE, sortby = "year"), silent = TRUE)
  if (inherits(scholar_pubs, "try-error") | isTRUE(is.na(scholar_pubs))) {
    scholar_pubs <- readRDS("scholar_pubs.RData")
  } else {
    saveRDS(scholar_pubs, "scholar_pubs.RData")
  }
  scholar_pubs[sapply(scholar_pubs, is.factor)] <- lapply(scholar_pubs[sapply(scholar_pubs, is.factor)], as.character)
  return(scholar_pubs)
}

clean_pubs_for_dt <- function(pubs, write = FALSE) {
  pubs$author <- sapply(pubs$author, function(x) {
    if (is.na(x)) {
      return(NA)
    } else {
      auths <- strsplit(x, ", ")
      if (length(auths[[1]]) > 3) {
        return(paste(auths[[1]][1], "et al."))
      } else {
        # replace last comma with "and"
        auths[[1]][length(auths[[1]])] <- paste("and", auths[[1]][length(auths[[1]])])
        return(paste(auths[[1]], collapse = ", "))
      }
    }
  })
  pubs <- pubs[, c("author", "year", "title", "journal", "cites")]
  if (write) {
    write.csv(pubs, "pubs.csv", row.names = FALSE, col.names = c("Author(s)", "Year", "Title", "Publication", "No. of citations"))
  }
  return(pubs)
}
