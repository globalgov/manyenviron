load("data-raw/Envr_Refer/ecorefer.RData")
load("data-raw/Envr_Refer/envrefer.RData")

ref <- as.data.frame(as.matrix(eco_refer)) %>% 
  dplyr::rename(References = V1)


ref$References <-gsub("c|\\(|\\)|\"", "", as.character(ref$References))
ref$References <-gsub("\\,", " ", as.character(ref$References))
