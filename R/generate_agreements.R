#' Generate agreements
#' 
#' @description Generate a list of fictional agreements and memberships.
#' @param n Integer number of fictional agreement names to generate.
#' @return String vector of fictional agreement names and/or memberships.
#' @name generate_

#' @rdname generate_
#' @examples
#' generate_agreements(12)
#' @export
generate_agreements <- function(n = 10){
  typelib <- c("Agreement", "Convention")
  # partylib <- c("Between The Parties")
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
  trimws(paste(sample(typelib, n, replace = TRUE),
               sample(subjlib, n, replace = TRUE),
               sample(targetlib, n, replace = TRUE)))
}

#' @rdname generate_
#' @examples
#' generate_memberships(12)
#' @export
generate_memberships <- function(n = 10) {
  membs <- countryregex$StatID
  agreements <- data.frame(title = generate_agreements(n = n),
                           nm = sample(100, n), membs = 0)
  for (k in seq_len(length(agreements$nm))) {
    agreements$membs[k] <- trimws(paste(sample(membs, as.numeric(agreements$nm[k])),
                                        collapse = ", "))
  }
  agreements <- plyr::ddply(agreements, .(title), function(DF) {
    data.frame(membership = trimws(strsplit(DF$membs, ",")[[1]]))
    })
  agreements
}
