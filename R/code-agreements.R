# Create table of reference for ID

#Load the agreements database with the 3 main datasets (IEADB, ECOLEX, GNEVAR)
load(file= "data/agreements.rda")

# Select the variables needed for the table
IEADB2 <- agreements$IEADB %>% dplyr::rename("IEADB Title" = "Title", 
                                 "IEADB Beg" = "Beg") %>% 
  dplyr::select(`IEADB Title`, `IEADB Beg`, `IEADB_ID`)

GNEVAR2 <- agreements$GNEVAR %>% dplyr::rename("GNEVAR Title" = "Title",
                                   "GNEVAR Beg" = "Beg") %>% 
  dplyr::select(`GNEVAR Title`, `GNEVAR Beg`, `GNEVAR_ID`, `ECOLEX_ID`, `IEADB_ID`)

ECOLEX2 <- agreements$ECOLEX %>% dplyr::rename("ECOLEX Title" = "Title",
                                   "ECOLEX Beg" = "Beg") %>% 
  dplyr::select(`ECOLEX Title`, `ECOLEX Beg`, `ECOLEX_ID`)

# Merge the datasets according to the same source ID
df_NEW <- merge(IEADB2, GNEVAR2, on = "IEADB_ID", all = TRUE )

df_NEW2 <- merge(df_NEW, ECOLEX2, on = "ECOLEX_ID", all = TRUE)

# Create the ID column
df_NEW2$ID <- paste0("ID-",formatC(1:nrow(df_NEW2), width = 5, flag = 0))

# Select the variables needed for the final table
FINAL_TABLE <- df_NEW2 %>% 
  dplyr::select(ID, IEADB_ID, `IEADB Title`, `GNEVAR Title`, `ECOLEX Title`, `IEADB Beg`,`GNEVAR Beg`, `ECOLEX Beg`, GNEVAR_ID, ECOLEX_ID) %>% 
  dplyr::arrange(ID)

FINAL_TABLE <- FINAL_TABLE[!duplicated(FINAL_TABLE$IEADB_ID, incomparables = NA),]

# Apply the ID in the agreement database
IEA <- FINAL_TABLE %>% 
  dplyr::select(ID, IEADB_ID)

GNE <- FINAL_TABLE %>% 
  dplyr::select(ID, GNEVAR_ID)

ECO <- FINAL_TABLE %>% 
  dplyr::select(ID, ECOLEX_ID)

agreements$IEADB <- merge(agreements$IEADB, IEA, all.x = TRUE)
agreements$GNEVAR <- merge(agreements$GNEVAR, GNE, all.x = TRUE)
agreements$ECOLEX <- merge(agreements$ECOLEX, ECO, all.x = TRUE)


