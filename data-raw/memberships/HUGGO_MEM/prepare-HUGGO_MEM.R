# HUGGO_MEM Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many packages universe.

# Stage one: Collecting data
HUGGO_MEM <- readr::read_csv("data-raw/memberships/HUGGO_MEM/gnevar.csv")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'HUGGO_MEM' object until the object created
# below (in stage three) passes all the tests.
HUGGO_MEM <- as_tibble(HUGGO_MEM) %>%
  janitor::remove_empty(which = "cols") %>%
  dplyr::mutate(gengID = GENG,
                ecolexID = ECOLEX,
                ieaID = IEA,
                stateID = Country,
                stateRat = messydates::as_messydate(Approval),
                stateSignature = messydates::as_messydate(Signature),
                stateWithdrawal = messydates::as_messydate(Withdrawal1),
                stateWithdrawal2 = messydates::as_messydate(Withdrawal2),
                Signature = messydates::as_messydate(DocSign),
                Force = messydates::as_messydate(DocForce),
                Term = messydates::as_messydate(DocEnd),
                stateForce = messydates::as_messydate(InForce1),
                stateForce2 = messydates::as_messydate(InForce2),
                stateForce3 = messydates::as_messydate(InForce3),
                verified = case_when(X == "%" ~ "verified",
                                     X == "?" ~ "not verified"),
                Title = manypkgs::standardise_titles(Title),
                ProvisionalApp = messydates::as_messydate(ProvisionalApp),
                Deposit = messydates::as_messydate(Deposit),
                Beg = dplyr::coalesce(Signature, stateRat, stateForce),
                End = dplyr::coalesce(Term, stateWithdrawal)) %>%
  dplyr::select(stateID, Title, Beg, End, Signature, stateSignature,
                stateRat, Force, stateForce, stateForce2, stateForce3,
                Term, stateWithdrawal, stateWithdrawal2,
                gengID, ecolexID, ieaID, Comments, Deposit, obsolete,
                ProvisionalApp, Reservation, verified) %>%
  dplyr::arrange(Beg) %>%
  dplyr::distinct()

# Add treatyID column
HUGGO_MEM$treatyID <- manypkgs::code_agreements(HUGGO_MEM,
                                                HUGGO_MEM$Title,
                                                HUGGO_MEM$Beg)

# Add MEA edges data (MGENG dataset)
# For some more information about the variables and codes,
# please see the documentation in the data-raw folder.
MEA_edges <- readr::read_delim("data-raw/agreements/HUGGO/MEA.Edges v1.0.csv",
                               delim = ";", escape_double = FALSE,
                               trim_ws = TRUE)
# Let's wrangle some variables to keep consistent.
names(MEA_edges) <- gsub("\\.", "", names(MEA_edges))
names(MEA_edges) <- gsub("^Membership", "", names(MEA_edges))
names(MEA_edges) <- gsub("^dateOf", "", names(MEA_edges))
MEA_edges <- as_tibble(MEA_edges) %>%
  dplyr::select(-'1') %>%
  janitor::remove_empty(which = "cols") %>%
  manydata::transmutate(gengID = GENGID,
                        stateID = Country,
                        stateSignature = messydates::as_messydate(MembSignx),
                        stateRat = messydates::as_messydate(MembRatx),
                        stateForce_ecolex = messydates::as_messydate(MembForcex),
                        stateForce_iea = messydates::as_messydate(MembForcey),
                        stateForce2 = messydates::as_messydate(EntryintoForce2),
                        stateWithdrawal = messydates::as_messydate(Withdrawal),
                        ProvisionalApp = messydates::as_messydate(ProvisionalApplication),
                        Succession = messydates::as_messydate(Succession),
                        Deposit = messydates::as_messydate(DepositofInstrument),
                        CooperatingNonparty = messydates::as_messydate(CooperatingNonparty),
                        DefiniteSignature = messydates::as_messydate(DefiniteSignature),
                        Consent = messydates::as_messydate(ConsentToBeBound),
                        Acceptance = messydates::as_messydate(AcceptanceApproval),
                        Accession = messydates::as_messydate(AccessionApprobation),
                        Consolidation = messydates::as_messydate(Consolidation),
                        Acceptance = messydates::as_messydate(AccessionApprobation2)) %>%
  dplyr::distinct()

# Add titles and ID variables from HUGGO agreements data
agreements <- manyenviron::agreements$HUGGO %>%
  dplyr::select(c(gengID, Title, Beg, End, Signature, Force, ecolexID, ieaID))
MEA_edges <- dplyr::inner_join(MEA_edges, agreements, by = "gengID") %>%
  dplyr::distinct()

# Add treatyID column
MEA_edges$treatyID <- manypkgs::code_agreements(MEA_edges,
                                                MEA_edges$Title,
                                                MEA_edges$Beg)

# Join Data
HUGGO_MEM <- dplyr::full_join(HUGGO_MEM, MEA_edges) %>%
  dplyr::distinct()

# Add manyID column
manyID <- manypkgs::condense_agreements(var = HUGGO_MEM$treatyID)
HUGGO_MEM <- dplyr::left_join(HUGGO_MEM, manyID, by = "treatyID")

# Reorder variables
HUGGO_MEM <- dplyr::relocate(HUGGO_MEM, c("manyID", "treatyID", "stateID",
                                          "Title", "Beg", "End", "Signature",
                                          "Force")) %>%
  dplyr::mutate(across(everything(), ~stringr::str_replace_all(., "^NA$",
                                                               NA_character_))) %>%
  dplyr::distinct() %>%
  plyr::ddply("manyID", zoo::na.locf, na.rm = FALSE) %>%
  dplyr::distinct() %>%
  dplyr::group_by(manyID) %>%
  tidyr::fill(.direction = "downup") %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::drop_na(Title) %>%
  tibble::as_tibble() %>%
  arrange(Beg) %>%
  mutate(Signature = messydates::as_messydate(Signature),
         Force = messydates::as_messydate(Force),
         Beg = messydates::as_messydate(Beg),
         End = messydates::as_messydate(End))

# Match titles and dates of verified treaties
# Step one: load data frame of verified treaties
verified <- read.csv("data-raw/agreements/HUGGO/HUGGO_verified.csv")
HUGGO_MEM <- manyenviron::memberships$HUGGO_MEM
# Step two: 
i <- 0
for(i in nrow(verified)){
  manyID <- verified[i, 1]
  treatyID <- verified[i, 13]
  title <- verified[i, 2]
  y <- 0
  y <- which(HUGGO_MEM$manyID == manyID & HUGGO_MEM$treatyID == treatyID &
                         HUGGO_MEM$Title == title)
  # Match dates
  # Beg
  HUGGO_MEM[y, 4] <- messydates::as_messydate(verified[i, 3])
  # End
  HUGGO_MEM[y, 6] <- messydates::as_messydate(verified[i, 4])
  # Signature
  HUGGO_MEM[y, 7] <- messydates::as_messydate(verified[i, 5])
  # Force
  HUGGO_MEM[y, 8] <- messydates::as_messydate(verified[i, 6])
  }


# Confirm dates are not incorrect
# Step one: Determine if stateRat is after stateSignature

ratdates <- HUGGO_MEM %>%
  dplyr::mutate(Correct = dplyr::case_when(stateRat >= stateSignature ~ 1,
                                           stateRat < stateSignature ~ 0)) %>%
  dplyr::filter(Correct == 0)

# Step two: Determine if stateSignature is after general Signature date

signdates <- HUGGO_MEM %>%
  dplyr::mutate(Correct = dplyr::case_when(stateSignature >= Signature ~ 1,
                                           stateSignature < Signature ~ 0)) %>%
  dplyr::filter(Correct == 0)

# Messydates
HUGGO_MEM$stateSignature <- messydates::as_messydate(HUGGO_MEM$stateSignature)
HUGGO_MEM$stateRat <- messydates::as_messydate(HUGGO_MEM$stateRat)
HUGGO_MEM$stateForce <- messydates::as_messydate(HUGGO_MEM$stateForce)
HUGGO_MEM$stateForce2 <- messydates::as_messydate(HUGGO_MEM$stateForce2)
HUGGO_MEM$stateForce3 <- messydates::as_messydate(HUGGO_MEM$stateForce3)
HUGGO_MEM$stateWithdrawal <- messydates::as_messydate(HUGGO_MEM$stateWithdrawal)
HUGGO_MEM$stateWithdrawal2 <- messydates::as_messydate(HUGGO_MEM$stateWithdrawal2)

# CC09DM_1954A
# Correct stateSignature
HUGGO_MEM[which(HUGGO_MEM$manyID == "CC09DM_1954A"), 9] <- messydates::as_messydate("1954-12-04")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CC09DM_1954A" & HUGGO_MEM$stateID == "CHL"), 10] <-
  NA

# CS18PS_1954A 
# Corrrect Beg and Signature (must change dates in HUGGO too)
# HUGGO_MEM[which(HUGGO_MEM$manyID == "CS18PS_1954A"), 4] <- messydates::as_messydate("1954-08-18")
# HUGGO_MEM[which(HUGGO_MEM$manyID == "CS18PS_1954A"), 7] <- messydates::as_messydate("1954-08-18")
# Add stateSignature
# HUGGO_MEM[which(HUGGO_MEM$manyID == "CS18PS_1954A"), 9] <- messydates::as_messydate("1954-08-18")
# Add stateRat: Chile
# HUGGO_MEM[which(HUGGO_MEM$manyID == "CS18PS_1954A" & HUGGO_MEM$stateID == "CHL"), 10] <-
#  messydates::as_messydate("1954-09-23")
# Add stateRat: Ecuador
# HUGGO_MEM[which(HUGGO_MEM$manyID == "CS18PS_1954A" & HUGGO_MEM$stateID == "ECU"), 10] <-
#  messydates::as_messydate("1956-01-24")
# Add stateRat: Peru
# HUGGO_MEM[which(HUGGO_MEM$manyID == "CS18PS_1954A" & HUGGO_MEM$stateID == "PER"), 10] <-
#  messydates::as_messydate("1956-05-10")

# DOM-PAN[CPC]_1989A
# Add stateSignature
HUGGO_MEM[which(HUGGO_MEM$manyID == "DOM-PAN[CPC]_1989A"), 9] <- messydates::as_messydate("1989-10-23")
# Add stateRat: Nicaragua
HUGGO_MEM[which(HUGGO_MEM$manyID == "DOM-PAN[CPC]_1989A" & HUGGO_MEM$stateID == "NIC"), 10] <-
  messydates::as_messydate("1990-12-13")

# INTRTT_1994A
# Beg and Signature dates correspond to the 1994 agreement, but stateSignature,
# stateRat, stateForce, stateWithdrawal and ProvisionalApp correspond to the
# 1983 agreement, which is not in HUGGO nor HUGGO_MEM

# TTICPO_2008A
# Correct Beg and Signature (must change dates in HUGGO too)
# HUGGO_MEM[which(HUGGO_MEM$manyID == "TTICPO_2008A"), 4] <- messydates::as_messydate("2008-06-25")
# HUGGO_MEM[which(HUGGO_MEM$manyID == "TTICPO_2008A"), 7] <- messydates::as_messydate("2008-06-25")

# VB13GV_2008O
# Correct Beg and Signature (must change dates in HUGGO too)
# HUGGO_MEM[which(HUGGO_MEM$manyID == "VB13GV_2008O"), 4] <- messydates::as_messydate("2008-06-25")
# HUGGO_MEM[which(HUGGO_MEM$manyID == "VB13GV_2008O"), 7] <- messydates::as_messydate("2008-06-25")

# NCMFCI_1982A
# Duplication of rows with stateRat date for stateSignature
# Remove duplicated rows
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "NCMFCI_1982A" & HUGGO_MEM$stateSignature != "1982-02-11"), ]

# CAN-USA[LOE]_2005N5
# Duplicated rows and one set with incorrect stateSignature
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "CAN-USA[LOE]_2005N5" & HUGGO_MEM$stateSignature != "2005-12-05"),] 

# AA04CC_2012E3:UNFCCC_1992A
# Remove duplicate rows
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "AA04CC_2012E3:UNFCCC_1992A" & HUGGO_MEM$stateID == "ARE" &
                                is.na(HUGGO_MEM$stateForce_ecolex)),]

# CPRTNF_1962A
# Correct stateSignature dates
HUGGO_MEM[which(HUGGO_MEM$manyID == "CPRTNF_1962A"), 9] <- messydates::as_messydate("1962-07-28")

# SL07GM_1976A
# Correct Beg and Signature (must change HUGGO too)
# HUGGO_MEM[which(HUGGO_MEM$manyID == "SL07GM_1976A"), 4] <- messydates::as_messydate("1976-07-23")
# HUGGO_MEM[which(HUGGO_MEM$manyID == "SL07GM_1976A"), 7] <- messydates::as_messydate("1976-07-23")

# STPFFA_1979A
# Correct stateSignature New Zealand
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID == "NZL"), 9] <- 
  messydates::as_messydate("1979-07-10")

# LRTAP_1979A
# Correct stateSignature USA
HUGGO_MEM[which(HUGGO_MEM$manyID == "LRTAP_1979A" & HUGGO_MEM$stateID == "USA"), 9] <- 
  messydates::as_messydate("1979-11-13")

# FCNEAF_1980A
# Correct stateSignature EUU
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "EUU"), 9] <- 
  messydates::as_messydate("1980-11-18")

# ESPOTF_1983A
# Correct stateSignature Panama
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESPOTF_1983A" & HUGGO_MEM$stateID == "PAN"), 9] <- 
  messydates::as_messydate("1983-03-15")

# GR10BP_1984A
# Correct stateSignature Mozambique, South Africa and Portugal (signed one same day)
HUGGO_MEM[which(HUGGO_MEM$manyID == "GR10BP_1984A"), 9] <- 
  messydates::as_messydate("1984-05-02")

# MNSDOL_1987P
# Correct stateSignature Congo
HUGGO_MEM[which(HUGGO_MEM$manyID == "MNSDOL_1987P" & HUGGO_MEM$stateID == "COG" & HUGGO_MEM$stateSignature == "1987-09-15"), 9] <- 
  messydates::as_messydate("1988-09-15")

# PE09EP_1990P
# Correct dates. Add NAs instead of hashtags
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P" & HUGGO_MEM$stateID == "IRQ"), 9] <- 
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P" & HUGGO_MEM$stateID == "IRQ"), 10] <- 
  NA

## EC09CS_1991A
## Remove duplicate rows with incorrect information
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "EC09CS_1991A" & HUGGO_MEM$stateSignature != "1991-02-01"),]

# CHN-KAZ[HUK]_2013A
# Remove rows including Finland and Norway as parties
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "CHN-KAZ[HUK]_2013A" & is.na(HUGGO_MEM$End)),]

# CMNTBT_1996A
# Correct stateSignature: Djibouti
HUGGO_MEM[which(HUGGO_MEM$manyID == "CMNTBT_1996A"), 9] <- messydates::as_messydate("1996-10-21")

# FC10NP_1993A
# Add stateSignature date: 1993-02-22
HUGGO_MEM[which(HUGGO_MEM$manyID == "FC10NP_1993A"), 9] <- messydates::as_messydate("1993-02-22")


# ACPEEC_1989A
# Correct stateSignature: Cote d'Ivoire
HUGGO_MEM[which(HUGGO_MEM$manyID == "ACPEEC_1989A" & HUGGO_MEM$stateID == "CIV"), 9] <- 
  messydates::as_messydate("1989-12-15")

# INPPSA_1978P1:MARPOL_1973A
# Remove duplicate row with incorrect stateSignature date: Australia
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                                HUGGO_MEM$stateID == "AUS" & HUGGO_MEM$stateSignature != "1978-02-17"),]

# INTCPE_1991P:INTCPE_1990A
# Remove extra rows
# Czechoslovakia
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" & 
                                HUGGO_MEM$stateID == "CSK" & !is.na(HUGGO_MEM$End)),]
# Czechia
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" & 
                                HUGGO_MEM$stateID == "CZE" & HUGGO_MEM$stateSignature != "1993-08-13"),]
# Federal Republic of Germany
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" & 
                                HUGGO_MEM$stateID == "DEU" & !is.na(HUGGO_MEM$End)), ]
# European Union
# According to Treaty of Lisbon, the European Union is to be deemed contracting
# party to all treaties the European Community was part of
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" & 
                                HUGGO_MEM$stateID == "EUE"),]
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                                HUGGO_MEM$stateID == "EUU" & HUGGO_MEM$stateRat == "1993-07-14"), ]
# Slovakia
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                                HUGGO_MEM$stateID == "SVK" & HUGGO_MEM$stateRat != "1993-05-27"),]
# Correct End (Treaty is still in force between DEU and CZE)
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A"), 6] <- NA
# Correct stateSignature
# CZE and SVK stateSignature
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == c("CZE", "SVK")), 9] <- NA
# Correct stateRat
# CZE and SVK stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "CZE"), 10] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "SVK"), 10] <- NA
# Correct stateWithdrawal
# CSK stateWithdrawal: date of dissolution
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == c("CSK")), 15] <- messydates::as_messydate("1992-12-31")
# CZE has not withdrawn
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == c("CZE")), 15] <- NA

# PD04WD_1992A
# Remove duplicate row: New Zealand
duplicate <- which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == "NZL")
HUGGO_MEM <- HUGGO_MEM[-(duplicate[-1]),]

# MNSDOL_1997E
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateSignature < "1997-09-17"),]
# Afghanistan
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateSignature == "2014-07-14"),]
# United Arab Emirates (ARE)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "ARE" &
                                HUGGO_MEM$stateSignature != "2005-02-16"),]
# Argentina (ARG)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "ARG" &
                                HUGGO_MEM$stateRat != "2001-02-15"),]
# Armenia (ARM)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "ARM" &
                                HUGGO_MEM$stateRat != "2008-12-18"),]
# Austria (AUT) 
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "AUT" &
                                HUGGO_MEM$stateRat != "2000-08-07"),]
# Azerbaijan (AZE)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "AZE" &
                                HUGGO_MEM$stateRat != "2000-09-28"),]
# Belgium (BEL)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "BEL" &
                                HUGGO_MEM$stateRat != "2004-08-11"),]
# Benin (BEN) 
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "BEN" &
                                HUGGO_MEM$stateRat != "2007-11-16"),]
# Bulgaria (BGR)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "BGR" &
                                HUGGO_MEM$stateRat != "1999-11-24"),]
# Bosnia and Herzegovina (BIH)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "BIH" &
                                HUGGO_MEM$stateRat != "2003-08-11"),]
# Belize (Belize)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "BLZ" &
                                HUGGO_MEM$stateRat != "2008-01-17"),]
# Brazil (BRA)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "BRA" &
                                HUGGO_MEM$stateRat != "2004-06-30"),]
# Chile (CHL)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "CHL" &
                                HUGGO_MEM$stateRat != "1998-06-17"),]
# China (CHN)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "CHN" &
                                HUGGO_MEM$stateRat != "2010-05-19"),]
# Cote d'Ivoire (CIV) 	
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "CIV" &
                                HUGGO_MEM$stateRat != "2012-06-28"),]
# Cabo Verde (CPV)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "CPV" &
                                HUGGO_MEM$stateRat != "2001-07-31"),]
# Costa Rica (CRI) 
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "CRI" &
                                HUGGO_MEM$stateRat != "2005-12-01"),]
# Cuba (CUB)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "CUB" &
                                HUGGO_MEM$stateRat != "2005-09-12"),]
# Cyprus (CYP) 	
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "CYP" &
                                HUGGO_MEM$stateRat != "2003-06-02"),]
# Germany (DEU)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "DEU" &
                                HUGGO_MEM$stateRat != "1999-01-05"),]
# Djibouti (DJI)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "DJI" &
                                HUGGO_MEM$stateRat != "1999-07-30"),]
# Dominican Republic (DOM)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "DOM" &
                                HUGGO_MEM$stateRat != "2009-10-01"),]
# Algeria (DZA)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "DZA" &
                                HUGGO_MEM$stateRat != "2007-08-06"),]
# Ecuador (ECU)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "ECU" &
                                HUGGO_MEM$stateRat != "2007-02-16"),]
# Egypt (EGY)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "EGY" &
                                HUGGO_MEM$stateRat != "2000-07-20"),]
# Estonia (EST)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "EST" &
                                HUGGO_MEM$stateRat != "2003-04-11"),]
# Fiji (FJI)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "FJI" &
                                HUGGO_MEM$stateRat != "2007-02-19"),]
# Honduras (HND)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "HND" &
                                HUGGO_MEM$stateRat != "2007-09-14"),]
# Croatia (HRV)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "HRV" &
                                HUGGO_MEM$stateRat != "2000-09-08"),]
# Indonesia (IDN)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "IDN" &
                                HUGGO_MEM$stateRat != "2006-01-26"),]
# Iran (IRN)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "IRN" &
                                HUGGO_MEM$stateRat != "2001-10-17"),]
# Iceland (ISL)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "ISL" &
                                HUGGO_MEM$stateRat != "2000-02-08"),]
# Israel (ISR)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "ISR" &
                                HUGGO_MEM$stateRat != "2003-05-28"),]
# Italy (ITA)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "ITA" &
                                HUGGO_MEM$stateRat != "2001-05-01"),]
# Jamaica (JAM)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "JAM" &
                                HUGGO_MEM$stateRat != "2003-09-24"),]
# Jordan (JOR)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "JOR" &
                                HUGGO_MEM$stateRat != "1999-02-03"),]
# Kazakhstan (KAZ)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "KAZ" &
                                HUGGO_MEM$stateRat != "2011-06-28"),]
# Kyrgyzstan (KGZ)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "KGZ" &
                                HUGGO_MEM$stateRat != "2003-05-13"),]
# Saint Kitts and Nevis (KNA)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "KNA" &
                                HUGGO_MEM$stateRat != "1999-02-25"),]
# Saint Lucia (LCA)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "LCA" &
                                HUGGO_MEM$stateRat != "1999-08-24"),]
# Sri Lanka (LKA)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "LKA" &
                                HUGGO_MEM$stateRat != "1999-08-20"),]
# Lithuania (LTU)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "LTU" &
                                HUGGO_MEM$stateRat != "2004-03-17"),]
# Luxembourg (LUX)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "LUX" &
                                HUGGO_MEM$stateRat != "1999-02-08"),]
# Latvia (LVA)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "LVA" &
                                HUGGO_MEM$stateRat != "2002-06-14"),]
# North Macedonia (MKD)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "MKD" &
                                HUGGO_MEM$stateRat != "1999-08-31"),]
# Myanmar (MMR)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "MMR" &
                                HUGGO_MEM$stateRat != "2012-01-30"),]
# Mongolia (MNG) 
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "MNG" &
                                HUGGO_MEM$stateRat != "2002-03-28"),]
# Niger (NER)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "NER" &
                                HUGGO_MEM$stateRat != "1999-10-08"),]
# Nigeria (NGA)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "NER" &
                                HUGGO_MEM$stateRat != "2001-09-27"),]
# Nicaragua (NIC)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "NIC" &
                                HUGGO_MEM$stateRat != "2012-11-07"),]
# Norway (NOR)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "NOR" &
                                HUGGO_MEM$stateRat != "1998-12-30"),]
# New Zealand (NZL)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "NZL" &
                                HUGGO_MEM$stateRat != "1999-06-03"),]
# Oman (OMN)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "OMN" &
                                HUGGO_MEM$stateRat != "2005-01-19"),]
# Panama (PAN)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "PAN" &
                                HUGGO_MEM$stateRat != "1999-03-05"),]
# Peru (PER)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "PER" &
                                HUGGO_MEM$stateRat != "2008-05-20"),]
# Philippines (PHL)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "PHL" &
                                HUGGO_MEM$stateRat != "2006-05-23"),]
# Papua New Guinea (PNG)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "PNG" &
                                HUGGO_MEM$stateRat != "2013-11-12"),]
# Polonia (POL)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "POL" &
                                HUGGO_MEM$stateRat != "1999-12-06"),]
# North Korea (PRK)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "PRK" &
                                HUGGO_MEM$stateRat != "2001-12-13"),]
# Portugal (PRT)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "PRT" &
                                HUGGO_MEM$stateRat != "2003-10-03"),]
# Sudan (SDN)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "SDN" &
                                HUGGO_MEM$stateRat != "2004-05-18"),]
# Senegal (SEN)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "SEN" &
                                HUGGO_MEM$stateRat != "1999-08-12"),]
# Solomon Islands (SLB)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "SLB" &
                                HUGGO_MEM$stateRat != "1999-08-17"),]
# South Sudan (SSD)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "SSD" &
                                HUGGO_MEM$stateRat != "2012-10-16"),]
# Slovakia (SVK)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "SVK" &
                                HUGGO_MEM$stateRat != "1999-11-03"),]
# Slovenia (SVN)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "SVN" &
                                HUGGO_MEM$stateRat != "1999-11-15"),]
# Sweden (SWE)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "SWE" &
                                HUGGO_MEM$stateRat != "1999-07-12"),]
# Syria (SYR)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "SYR" &
                                HUGGO_MEM$stateRat != "1999-11-30"),]
# Chad (TCD)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "TCD" &
                                HUGGO_MEM$stateRat != "2001-05-30"),]
# Togo (TGO)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "TGO" &
                                HUGGO_MEM$stateRat != "2001-11-26"),]
# Thailand (THA)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "THA" &
                                HUGGO_MEM$stateRat != "2003-06-23"),]
# Tajikistan (TJK)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "TJK" &
                                HUGGO_MEM$stateRat != "2009-05-07"),]
# Trinidad and Tobago (TTO)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "TTO" &
                                HUGGO_MEM$stateRat != "1999-06-10"),]
# Uganda (UGA)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "UGA" &
                                HUGGO_MEM$stateRat != "1999-11-23"),]
# Ukraine (UKR)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "UKR" &
                                HUGGO_MEM$stateRat != "2007-05-04"),]
# Uruguay (URY) 
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "URY" &
                                HUGGO_MEM$stateRat != "2000-02-16"),]
# Uzbekistan (UZB)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "UZB" &
                                HUGGO_MEM$stateRat != "2006-10-31"),]
# Venezuela (VEN)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "VEN" &
                                HUGGO_MEM$stateRat != "2002-05-13"),]
# 

# Stage three: Connecting data
# Next run the following line to make HUGGO_MEM available
# within the package.
manypkgs::export_data(HUGGO_MEM, database = "memberships",
                      URL = "Hand-coded by the GGO team")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure
# adherence to certain standards. You can hit Cmd-Shift-T (Mac)
# or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require
# you to return to stage two and further clean, standardise, or wrangle
# your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill
# in as much detail about the variables etc as possible.
