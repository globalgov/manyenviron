#' Generate agreements
#' 
#' @description Generate a list of fictional agreements and memberships.
#' @param n Integer number of fictional agreement names to generate.
#' @param treaty_type What type of treaty would you likebe returned?
#' By default, "any" treaty.
#' Other options include "bilaterals" and "multilaterals" 
#' @return String vector of fictional agreement names and/or memberships.
#' @name generate_

#' @rdname generate_
#' @examples
#' generate_agreements(12)
#' generate_agreements(12, treaty_type = "bilaterals")
#' generate_agreements(12, treaty_type = "multilaterals")
#' @export
generate_agreements <- function(n = 10, treaty_type = "any"){
  partylib <- trimws(paste(rep(c("Between The Parties of "), n),
                           sample(countryregex$Label, n, replace = TRUE),
                           rep(c(" and "), n),
                           sample(countryregex$Label, n, replace = TRUE)))
  typelib <- c("Agreement", "Convention")
  subjlib <- c("On", "For", "Concerning", "Limiting",
               "Regulating", "Regarding", "Relating To",
               "Cooperation In",
               "On Cooperation In The Area Of", "On Cooperation In The Field Of")
  targetlib <- c("Hydroelectric Power Station and Dams", 
                 "The Brine River", "The Talamasi River",
                 "The Protection Of Migratory Birds And Birds In Danger Of Extinction And Their Environment",
                 "An International Energy Programme",
                 "North Eastern Ocean of Peace",
                 "The Development Of Fisheries",
                 "The Protection Of The Great Border Reef",
                 "The Peaceful Uses Of Atomic Energy And Space Research",
                 "Fishing For King And Tanner Crab",
                 "Peaceful Uses Of Nuclear Energy",
                 "The Fishing Of The Pico-Scindian Herring",
                 "Certain Fishery Problems On The High Seas",
                 "Whaling",
                 "Civil Liability In The Field Of Maritime Carriage Of Nuclear Material",
                 "The Establishment Of An International Fund For Compensation For Oil Pollution Damage",
                 "Marine Pollution By Dumping From Ships And Aircraft",
                 "The Protection Of Migratory Birds And Game Mammals",
                 "Navigation And Fisheries Safety",
                 "Fisheries",
                 "Environmental Protection",
                 "The Conduct Of Fishing Operations",
                 "The Development Of Geothermal Energy",
                 "Energy Research And Development",
                 "Shrimp")
  if (treaty_type == "bilaterals") {
    out <- trimws(paste(sample(typelib, n, replace = TRUE),
                        partylib,
                        sample(subjlib, n, replace = TRUE),
                        sample(targetlib, n, replace = TRUE)))
  } else if (treaty_type == "multilaterals") {
    out <- trimws(paste(sample(typelib, n, replace = TRUE),
                        sample(subjlib, n, replace = TRUE),
                        sample(targetlib, n, replace = TRUE)))
  } else {
    bilaterals <- round(sum(grepl("between", agreements$HUGGO$Title,
                                  ignore.case = TRUE)) / length(agreements$HUGGO$Title),
                        digits = 1)
    partylib[sample(n*bilaterals)] <- ""
    out <- trimws(paste(sample(typelib, n, replace = TRUE),
                        partylib,
                        sample(subjlib, n, replace = TRUE),
                        sample(targetlib, n, replace = TRUE)))
  }
  out
}

#' @rdname generate_
#' @param list Would you like members to be listed in one observation?
#' By default, FALSE.
#' @examples
#' generate_memberships(12)
#' generate_memberships(12, treaty_type = "bilaterals")
#' generate_memberships(12, treaty_type = "multilaterals")
#' generate_memberships(12, list = TRUE)
#' @export
generate_memberships <- function(n = 10, treaty_type = "any", list = FALSE) {
  membs <- countryregex$StatID
  g_agreements <- data.frame(title = generate_agreements(n = n, treaty_type =
                                                         treaty_type),
                           nm = sample(100, n), membs = 0)
  coment <- sapply(countryregex[, 3], function(x) grepl(x, g_agreements$title,
                                                        ignore.case = T,
                                                        perl = T) * 1)
  colnames(coment) <- countryregex[, 1]
  rownames(coment) <- g_agreements$title
  out <- unname(apply(coment, 1, function(x) paste(names(x[x == 1]),
                                                   collapse = ", ")))
  out[out == ""] <- NA
  for (k in seq_len(length(g_agreements$nm))) {
    g_agreements$membs[k] <- ifelse(grepl("Between The Parties of",
                                        g_agreements$title[k]),
                                  out[k],
                                  trimws(paste(sample(membs,
                                                      as.numeric(g_agreements$nm[k])),
                                               collapse = ", ")))
  }
  g_agreements <- dplyr::select(g_agreements, -nm) %>%
    dplyr::rename(membership = membs)
  if (list == FALSE) {
    g_agreements <- plyr::ddply(g_agreements, .(title), function(DF) {
      data.frame(membership = trimws(strsplit(DF$membership, ",")[[1]]))
    })
  }
  g_agreements
}
