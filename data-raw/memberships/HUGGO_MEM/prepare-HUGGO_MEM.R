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
                Begin = dplyr::coalesce(Signature, stateRat, stateForce),
                End = dplyr::coalesce(Term, stateWithdrawal)) %>%
  dplyr::select(stateID, Title, Begin, End, Signature, stateSignature,
                stateRat, Force, stateForce, stateForce2, stateForce3,
                Term, stateWithdrawal, stateWithdrawal2,
                gengID, ecolexID, ieaID, Comments, Deposit, obsolete,
                ProvisionalApp, Reservation, verified) %>%
  dplyr::arrange(Begin) %>%
  dplyr::distinct()

# Add treatyID column
HUGGO_MEM$treatyID <- manypkgs::code_agreements(HUGGO_MEM,
                                                HUGGO_MEM$Title,
                                                HUGGO_MEM$Begin)

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
  dplyr::select(c(gengID, Title, Begin, End, Signature, Force, ecolexID, ieaID))
MEA_edges <- dplyr::inner_join(MEA_edges, agreements, by = "gengID") %>%
  dplyr::distinct()

# Add treatyID column
MEA_edges$treatyID <- manypkgs::code_agreements(MEA_edges,
                                                MEA_edges$Title,
                                                MEA_edges$Begin)

# Join Data
HUGGO_MEM <- dplyr::full_join(HUGGO_MEM, MEA_edges) %>%
  dplyr::distinct()

# Add manyID column
manyID <- manypkgs::condense_agreements(idvar = HUGGO_MEM$treatyID)
HUGGO_MEM <- dplyr::left_join(HUGGO_MEM, manyID, by = "treatyID")

# Reorder variables
HUGGO_MEM <- dplyr::relocate(HUGGO_MEM, c("manyID", "treatyID", "stateID",
                                          "Title", "Begin", "End", "Signature",
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
  arrange(Begin) %>%
  mutate(Signature = messydates::as_messydate(Signature),
         Force = messydates::as_messydate(Force),
         Begin = messydates::as_messydate(Begin),
         End = messydates::as_messydate(End))

# Match titles and dates of verified treaties from agreements$HUGGO
# Step one: load data frame of verified treaties
verified <- read.csv("data-raw/agreements/HUGGO/HUGGO_verified.csv")
HUGGOback <- HUGGO_MEM
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
  HUGGO_MEM[y, "Begin"] <- messydates::as_messydate(verified[i, "Begin"])
  # End
  HUGGO_MEM[y, "End"] <- messydates::as_messydate(verified[i, "End"])
  # Signature
  HUGGO_MEM[y, "Signature"] <- messydates::as_messydate(verified[i, "Signature"])
  # Force
  HUGGO_MEM[y, "Force"] <- messydates::as_messydate(verified[i, "Force"])
  }

# Confirm dates are not incorrect in rows
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

# Convert to mdate class
HUGGO_MEM$stateSignature <- messydates::as_messydate(HUGGO_MEM$stateSignature)
HUGGO_MEM$stateRat <- messydates::as_messydate(HUGGO_MEM$stateRat)
HUGGO_MEM$stateForce <- messydates::as_messydate(HUGGO_MEM$stateForce)
HUGGO_MEM$stateForce2 <- messydates::as_messydate(HUGGO_MEM$stateForce2)
HUGGO_MEM$stateForce3 <- messydates::as_messydate(HUGGO_MEM$stateForce3)
HUGGO_MEM$stateWithdrawal <- messydates::as_messydate(HUGGO_MEM$stateWithdrawal)
HUGGO_MEM$stateWithdrawal2 <- messydates::as_messydate(HUGGO_MEM$stateWithdrawal2)
HUGGO_MEM$Accession <- messydates::as_messydate(HUGGO_MEM$Accession)
HUGGO_MEM$Acceptance <- messydates::as_messydate(HUGGO_MEM$Acceptance)

# CDSMMZ_1954P20
# Correct stateSignature for all countries
HUGGO_MEM[which(HUGGO_MEM$manyID == "CDSMMZ_1954P20"), "stateSignature"] <-
  messydates::as_messydate("1954-12-04")
# Correct stateRat for Chile
HUGGO_MEM[which(HUGGO_MEM$manyID == "CDSMMZ_1954P20" &
                  HUGGO_MEM$stateID == "CHL"), "stateRat"] <- NA
# Correct stateForce for all countries
HUGGO_MEM[which(HUGGO_MEM$manyID == "CDSMMZ_1954P20"), "stateForce"] <- NA
# Verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "CDSMMZ_1954P20"), "Checked_HUGGO"] <- 1
HUGGO_MEM[which(HUGGO_MEM$manyID == "CDSMMZ_1954P20"), "Verified_HUGGO"] <-
  "stateSignature, stateForce, stateRat, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "CDSMMZ_1954P20"), "Changes_HUGGO"] <-
  "stateSignature, stateForce"
# Chile
HUGGO_MEM[which(HUGGO_MEM$manyID == "CDSMMZ_1954P20" &
                  HUGGO_MEM$stateID == "CHL"), "Changes_HUGGO"] <-
  paste(HUGGO_MEM[which(HUGGO_MEM$manyID == "CDSMMZ_1954P20" &
                          HUGGO_MEM$stateID == "CHL"), "Changes_HUGGO"],
        "stateRat", sep = ", ")

# OP06SP_1952A
# Add stateForce
# All except for Colombia: 1955-05-06
HUGGO_MEM[which(HUGGO_MEM$manyID == "OP06SP_1952A" & HUGGO_MEM$stateID != "COL"), "stateForce"] <- 
  messydates::as_messydate("1955-05-06")
# Colombia
HUGGO_MEM[which(HUGGO_MEM$manyID == "OP06SP_1952A" & HUGGO_MEM$stateID == "COL"), "stateForce"] <- 
  messydates::as_messydate("1980-02-04")
# Verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "OP06SP_1952A"), "Checked_HUGGO"] <- 1
HUGGO_MEM[which(HUGGO_MEM$manyID == "OP06SP_1952A"), "Verified_HUGGO"] <-
  "stateSignature, stateForce, stateRat, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "OP06SP_1952A"), "Changes_HUGGO"] <-
  "stateForce"

# DOM-PAN[CPC]_1989A
# Add stateSignature
HUGGO_MEM[which(HUGGO_MEM$manyID == "DOM-PAN[CPC]_1989A"), "stateSignature"] <-
  messydates::as_messydate("1989-10-23")
# Add NA in stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "DOM-PAN[CPC]_1989A"), "stateRat"] <-
  NA
# Add stateRat: Nicaragua
HUGGO_MEM[which(HUGGO_MEM$manyID == "DOM-PAN[CPC]_1989A" & HUGGO_MEM$stateID == "NIC"),
          "stateRat"] <- messydates::as_messydate("1990-12-13")
# Verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "DOM-PAN[CPC]_1989A"), "Checked_HUGGO"] <- 1
HUGGO_MEM[which(HUGGO_MEM$manyID == "DOM-PAN[CPC]_1989A"), "Verified_HUGGO"] <-
  "stateSignature"
HUGGO_MEM[which(HUGGO_MEM$manyID == "DOM-PAN[CPC]_1989A"), "Changes_HUGGO"] <-
  "stateSignature"
# Nicaragua
HUGGO_MEM[which(HUGGO_MEM$manyID == "DOM-PAN[CPC]_1989A" & HUGGO_MEM$stateID == "NIC"),
          "Changes_HUGGO"] <- "stateSignature, stateRat"

# INTRTT_1994A
# Beg and Signature dates correspond to the 1994 agreement, but stateSignature,
# stateRat, stateForce, stateWithdrawal and ProvisionalApp correspond to the
# 1983 agreement, which is not in HUGGO nor HUGGO_MEM

# TTICPO_2008A
# Correct Beg and Signature (must change dates in HUGGO too)
# HUGGO_MEM[which(HUGGO_MEM$manyID == "TTICPO_2008A"), 4] <- messydates::as_messydate("2008-06-25")
# HUGGO_MEM[which(HUGGO_MEM$manyID == "TTICPO_2008A"), 7] <- messydates::as_messydate("2008-06-25")
# Same as TTICPO_2008A
# VB13GV_2008O
# Correct Beg and Signature (must change dates in HUGGO too)
# HUGGO_MEM[which(HUGGO_MEM$manyID == "VB13GV_2008O"), 4] <- messydates::as_messydate("2008-06-25")
# HUGGO_MEM[which(HUGGO_MEM$manyID == "VB13GV_2008O"), 7] <- messydates::as_messydate("2008-06-25")

# NCMFCI_1982A
# Duplication of rows with stateRat date for stateSignature
# Duplicated rows all have hashtags in stateRat
hashtags <- as.character(HUGGO_MEM[which(HUGGO_MEM$manyID == "NCMFCI_1982A" &
                                           HUGGO_MEM$stateSignature == "1983-07-14"),
                                   "stateRat"])
# Remove duplicated rows
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "NCMFCI_1982A" &
                                HUGGO_MEM$stateRat == hashtags), ]
# Add stateForce
HUGGO_MEM[which(HUGGO_MEM$stateID != "FSM" & HUGGO_MEM$stateID != "TUV" &
                HUGGO_MEM$manyID == "NCMFCI_1982A"), "stateForce"] <-
  messydates::as_messydate("1982-12-02")
# Federal States Micronesia
HUGGO_MEM[which(HUGGO_MEM$stateID == "FSM" & HUGGO_MEM$manyID == "NCMFCI_1982A"),
          "stateForce"] <- messydates::as_messydate("1983-08-14")
# Tuvalu (Accession)
HUGGO_MEM[which(HUGGO_MEM$stateID == "TUV" & HUGGO_MEM$manyID == "NCMFCI_1982A"),
           "stateSignature"] <- NA
HUGGO_MEM[which(HUGGO_MEM$stateID == "TUV" & HUGGO_MEM$manyID == "NCMFCI_1982A"),
          "stateForce"] <- messydates::as_messydate("1991-07-15")
HUGGO_MEM[which(HUGGO_MEM$stateID == "TUV" & HUGGO_MEM$manyID == "NCMFCI_1982A"),
          "Accession"] <- messydates::as_messydate("1991-05-15")
# Add verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "NCMFCI_1982A"), "Checked_HUGGO"] <- 1
HUGGO_MEM[which(HUGGO_MEM$manyID == "NCMFCI_1982A"), "Verified_HUGGO"] <-
  "stateSignature, stateForce, stateRat"
HUGGO_MEM[which(HUGGO_MEM$manyID == "NCMFCI_1982A"), "Changes_HUGGO"] <-
  "stateForce"
# Tuvalu
HUGGO_MEM[which(HUGGO_MEM$stateID == "TUV" & HUGGO_MEM$manyID == "NCMFCI_1982A"),
          "Changes_HUGGO"] <- "stateSignature, stateForce, stateWithdrawal, Accession"
HUGGO_MEM[which(HUGGO_MEM$stateID == "TUV" & HUGGO_MEM$manyID == "NCMFCI_1982A"),
          "Verified_HUGGO"] <- "stateSignature, stateForce, stateRat, stateWithdrawal, Accession"

# CAN-USA[LOE]_2005N5
# Duplicated rows and one set with incorrect stateSignature
# HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "CAN-USA[LOE]_2005N5" & HUGGO_MEM$stateSignature != "2005-12-05"),] 

# AA04CC_2012E3:UNFCCC_1992A
# Remove one fully duplicate row
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "AA04CC_2012E3:UNFCCC_1992A" & HUGGO_MEM$stateID == "ARE" &
                                is.na(HUGGO_MEM$stateForce_ecolex)),]

# STPFFA_1979A
# Not signatories
not_sign <- c("NIU", "MHL", "FSM", "PLW", "PNG", "VUT")
# Correct stateSignature
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID %in% not_sign),
          "stateSignature"] <- NA
# Correct stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & !(HUGGO_MEM$stateID %in% not_sign)),
          "stateRat"] <- NA
# Correct stateSignature New Zealand
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID == "NZL"),
          "stateSignature"] <- messydates::as_messydate("1979-07-10")
# Papua New Guinea Accession
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID == "PNG"),
          "Accession"] <- messydates::as_messydate("1979-09-13")
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID == "PNG"),
          "Changes_HUGGO"] <- "stateSignature, Accession"
# Niue stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID == "NIU"),
          "stateRat"] <- messydates::as_messydate("1979-10-18")
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID == "NIU"),
          "Accession"] <- messydates::as_messydate("1979-10-18")
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID == "NIU"),
          "Changes_HUGGO"] <- "stateSignature, stateRat, Accession"
# Vanuatu stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID == "VUT"),
          "stateRat"] <- messydates::as_messydate("1981-03-09")
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID == "VUT"),
          "Accession"] <- messydates::as_messydate("1981-03-09")
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID == "VUT"),
          "Changes_HUGGO"] <- "stateSignature, stateRat, Accession"
# Palau stateForce
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID == "PLW"),
          "stateForce"] <- messydates::as_messydate("1986-05-14")
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID == "PLW"),
          "Accession"] <- messydates::as_messydate("1986-04-14")
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID == "PLW"),
          "Changes_HUGGO"] <- "stateSignature, stateForce, Accession"
# Marshall Islands
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID == "MHL"),
          "stateForce"] <- messydates::as_messydate("1987-04-26")
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID == "MHL"),
          "Accession"] <- messydates::as_messydate("1986-03-27")
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID == "MHL"),
          "Changes_HUGGO"] <- "stateSignature, stateForce, Accession"
# Add verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A"), "Checked_HUGGO"] <- 1
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A"), "Verified_HUGGO"] <-
  "stateSignature, stateForce, stateRat"
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID %in% not_sign),
          "Verified_HUGGO"] <- "stateSignature, stateForce, stateRat, stateWithdrawal, Accession"
# stateRat for signatories
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & !(HUGGO_MEM$stateID %in% not_sign)),
          "Changes_HUGGO"] <- "stateRat"
# New Zealand
HUGGO_MEM[which(HUGGO_MEM$manyID == "STPFFA_1979A" & HUGGO_MEM$stateID == "NZL"),
          "Changes_HUGGO"] <- "stateSignature, stateRat"

# LRTAP_1979A
# Correct End column (treaty is still in force, although coding has not
# yet changed in HUGGO)
HUGGO_MEM[which(HUGGO_MEM$manyID == "LRTAP_1979A"), "End"] <-
  NA
# Correct stateWithdrawal (only CSK, DEU (GDR), and YUG have withdrawn)
HUGGO_MEM[which(HUGGO_MEM$manyID == "LRTAP_1979A" & HUGGO_MEM$stateID != "CSK" &
                  HUGGO_MEM$stateID != "DEU" & HUGGO_MEM$stateID != "YUG"),
          "stateWithdrawal"] <- NA
# Use function for ecodates

# FCNEAF_1980A
fcn <- HUGGO_MEM %>%
  filter(manyID == "FCNEAF_1980A")
# Remove duplicated rows and correct data
# Bulgaria
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "BGR" &
                  is.na(HUGGO_MEM$stateRat)), ]
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "BGR"),
          "stateForce"] <- messydates::as_messydate("1984-07-24")
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "BGR"),
          "Accession"] <- messydates::as_messydate("1984-07-24")
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "BGR"),
          "Changes_HUGGO"] <- "stateForce, Accession"
# Estonia
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "EST" &
                  HUGGO_MEM$stateRat != "2003-07-07"), ]
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "EST"),
          "stateSignature"] <-NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "EST"),
          "stateForce"] <- messydates::as_messydate("2003-07-07")
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "EST"),
          "Accession"] <- messydates::as_messydate("2003-07-07")
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "EST"),
          "Changes_HUGGO"] <- "stateSignature, stateForce, Accession"
# European Economic Community
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "EUU" &
                  HUGGO_MEM$stateRat != "1981-09-18"), ]
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "EUU"),
          "stateSignature"] <- messydates::as_messydate("1980-11-18")
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "EUU"),
          "stateForce"] <- messydates::as_messydate("1982-03-17")
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "EUU"),
          "Acceptance"] <- messydates::as_messydate("1981-09-18")
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "EUU"),
          "Changes_HUGGO"] <- "stateSignature, stateForce, Acceptance"
# Sweden remove duplicate
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "SWE" &
                  HUGGO_MEM$stateWithdrawal != "1996-07-04"), ]
# Cuba
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "CUB"),
          "stateRat"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "CUB"),
          "Changes_HUGGO"] <- "stateForce"
# Add stateForce for original signatories (not including Spain, Portugal, Poland)
signat <- c("DEU", "DNK", "ISL", "NOR", "RUS", "SWE")
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateRat <=
                  messydates::as_messydate("1982-03-17")), "stateForce"] <-
  messydates::as_messydate("1982-03-17")
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID %in% signat),
          "Changes_HUGGO"] <- "stateForce"
# Spain
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "ESP"),
          "stateForce"] <- messydates::as_messydate("1984-03-09")
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "ESP"),
          "Changes_HUGGO"] <- "stateForce"
# Portugal
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "PRT"),
          "stateForce"] <- messydates::as_messydate("1984-06-29")
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "PRT"),
          "Changes_HUGGO"] <- "stateForce"
# Poland
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "POL"),
          "stateForce"] <- messydates::as_messydate("1984-11-20")
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "POL"),
          "Changes_HUGGO"] <- "stateForce"
# Correct stateWithdrawal
notwithdrawn <- c("BGR", "CUB", "DNK", "DEU", "ISL", "NOR", "PRT", "RUS",
                  "ESP", "EUU")
for (i in 1:length(notwithdrawn)){
  HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == notwithdrawn[i]),
            "stateWithdrawal"] <- NA
  HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID ==
                    notwithdrawn[i]), "Changes_HUGGO"] <-
    paste(HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" &
                            HUGGO_MEM$stateID == notwithdrawn[i]),
                    "Changes_HUGGO"], "stateWithdrawal", sep = ", ")
}
# Correct Accession
notacc <- c("CUB", "DNK", "EUU", "DEU", "ISL", "NOR", "POL", "PRT", "RUS", "ESP",
            "SWE")
for (i in 1:length(notacc)){
  HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == notacc[i]),
            "Accession"] <- NA
  HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID ==
                    notacc[i]), "Changes_HUGGO"] <-
    paste(HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A" &
                            HUGGO_MEM$stateID == notacc[i]),
                    "Changes_HUGGO"], "Accession", sep = ", ")
}
# Add verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A"), "Checked_HUGGO"] <- 1
HUGGO_MEM[which(HUGGO_MEM$manyID == "FCNEAF_1980A"), "Verified_HUGGO"] <-
  "stateSignature, stateForce, stateRat, stateWithdrawal, Accession"


# ESPOTF_1983A
# Remove Panama duplicate row
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "ESPOTF_1983A" & HUGGO_MEM$stateID ==
                                "PAN" & HUGGO_MEM$stateSignature != "1983-03-15"),]
# Correct Panama stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESPOTF_1983A" & HUGGO_MEM$stateID ==
                   "PAN"), "stateRat"] <- NA
# Correct Costa Rica stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESPOTF_1983A" & HUGGO_MEM$stateID ==
                   "CRI"), "stateRat"] <- messydates::as_messydate("1986-08-11")
# Correct Guatemala stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESPOTF_1983A" & HUGGO_MEM$stateID ==
                   "CRI"), "stateRat"] <- messydates::as_messydate("1985-10-09")
# Correct Honduras stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESPOTF_1983A" & HUGGO_MEM$stateID ==
                   "HND"), "stateRat"] <- messydates::as_messydate("1985-06-04")
# Correct United States stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESPOTF_1983A" & HUGGO_MEM$stateID ==
                   "USA"), "stateRat"] <- messydates::as_messydate("1983-07-27")
# Add verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESPOTF_1983A"), "Checked_HUGGO"] <-
  1
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESPOTF_1983A"), "Changes_HUGGO"] <-
  "stateRat"
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESPOTF_1983A"), "Verified_HUGGO"] <-
  "stateSignature, stateRat, stateForce"

# GR10BP_1984A
# Correct stateSignature Mozambique, South Africa and Portugal
HUGGO_MEM[which(HUGGO_MEM$manyID == "GR10BP_1984A"), "stateSignature"] <- 
  messydates::as_messydate("1984-05-02")
# Correct stateRat: NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "GR10BP_1984A"), "stateRat"] <- NA
# Correct stateForce: entry into force same day as signature
HUGGO_MEM[which(HUGGO_MEM$manyID == "GR10BP_1984A"), "stateForce"] <-
  messydates::as_messydate("1984-05-02")
## Add verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESPOTF_1983A"), "Checked_HUGGO"] <-
  1
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESPOTF_1983A"), "Changes_HUGGO"] <-
  "stateSignature, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESPOTF_1983A"), "Verified_HUGGO"] <-
  "stateSignature, stateForce"

# PE09EP_1990P
# Iraq: correct dates. Add NAs instead of hashtags
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P" & HUGGO_MEM$stateID == "IRQ"),
          "stateSignature"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P" & HUGGO_MEM$stateID == "IRQ"),
          "stateRat"] <-NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P" & HUGGO_MEM$stateID == "IRQ"),
          "Changes_HUGGO"] <- "stateSignature, stateRat" 
# Oman
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P" & HUGGO_MEM$stateID == "OMN"),
          "stateSignature"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P" & HUGGO_MEM$stateID == "OMN"),
          "Changes_HUGGO"] <- "stateSignature"
# Add stateForce (except Iran and states that did not ratify)
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P" & !(HUGGO_MEM$stateID %in% c("IRQ", "IRN", "ARE"))),
          "stateForce"] <- messydates::as_messydate("1993-02-01")
# Add stateForce Iran
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P" & HUGGO_MEM$stateID == "IRN"),
          "stateForce"] <- messydates::as_messydate("1993-09-12")
# Add verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P"), "Checked_HUGGO"] <-
  1
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P"), "Verified_HUGGO"] <-
  "stateSignature, stateRat, stateForce"
for (i in 1:nrow(HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P" &
                                 HUGGO_MEM$stateID != c("IRQ", "ARE")), "stateID"])){
  a <- HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P" &
                         HUGGO_MEM$stateID != c("IRQ", "ARE")), "stateID"]
  if (is.na(HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P" &
                            HUGGO_MEM$stateID == as.character(a[i, ])), "Changes_HUGGO"])){
    HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P" &
                      HUGGO_MEM$stateID == as.character(a[i, ])), "Changes_HUGGO"] <-
      "stateForce"
  } else {
    HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P" &
                      HUGGO_MEM$stateID == as.character(a[i, ])), "Changes_HUGGO"] <-
      paste(HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P" &
                              HUGGO_MEM$stateID == as.character(a[i, ])), "Changes_HUGGO"],
            "stateForce", sep = ", ")
  }
}

## EC09CS_1991A
## Remove duplicate rows with hashtags in state Signature
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "EC09CS_1991A" &
                                HUGGO_MEM$stateSignature != "1991-02-01"),]
# Add state Force (March 1 1991 for all parties)
HUGGO_MEM[which(HUGGO_MEM$manyID == "EC09CS_1991A"), "stateForce"] <-
  messydates::as_messydate("1991-03-01")
# Uknown stateRat
# Add state Force (March 1 1991 for all parties)
HUGGO_MEM[which(HUGGO_MEM$manyID == "EC09CS_1991A"), "stateRat"] <- NA
# Add verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "EC09CS_1991A"), "Checked_HUGGO"] <-
  1
HUGGO_MEM[which(HUGGO_MEM$manyID == "EC09CS_1991A"), "Changes_HUGGO"] <-
  "stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "EC09CS_1991A"), "Verified_HUGGO"] <-
  "stateSignature, stateForce"

# CHN-KAZ[HUK]_2013A
# Remove rows including Finland and Norway as parties
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "CHN-KAZ[HUK]_2013A" &
                                HUGGO_MEM$stateID %in% c("NOR", "FIN")),]

# CMNTBT_1996A
# Add Cook Islands
HUGGO_MEM[which(HUGGO_MEM$manyID == "CMNTBT_1996A" & HUGGO_MEM$stateID == "NZL" &
                  HUGGO_MEM$stateSignature == "1997-12-05"), "stateID"] <- "COK"
# Correct stateForce: treaty has not entered into force
HUGGO_MEM[which(HUGGO_MEM$manyID == "CMNTBT_1996A"), "stateForce"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "CMNTBT_1996A"), "Changes_HUGGO"] <- "stateForce"
# Correct stateSignature and stateRat: scrape UN website
content <- read_html("https://treaties.un.org/Pages/ViewDetails.aspx?src=IND&mtdsg_no=XXVI-4&chapter=26&clang=_en")
table <- content %>%
  html_table(fill = TRUE)
table <- table[[1]]
table <- table[19:205,1:3]
# Clean table
table$X1 <- gsub("\\d", "", table$X1)
table$X2 <- gsub("(\\b\\d{4})\\s.", "\\1", table$X2)
table$X3 <- gsub("(\\b\\d{4})\\s.", "\\1", table$X3)

# Add table with iso code
un_iso <- read.csv("data-raw/memberships/HUGGO_MEM/un_iso.csv")
cmntbt <- HUGGO_MEM %>%
  filter(manyID == "CMNTBT_1996A")
for (i in 1:nrow(cmntbt)){
 iso <- as.character(cmntbt[i, "stateID"])
 name <- un_iso[which(un_iso$Alpha.3.code.5. == iso), "Country.name"]
 print(name)
 # Assign dates
 # stateSignature
 if(as.character(table[which(table$X1 == name), "X2"] == "")){
   sign <- 0
 }
 else {
   sign <- as.character(table[which(table$X1 == name), "X2"])
   sign <- messydates::as_messydate(sign)
 }
 # stateRat
 if(as.character(table[which(table$X1 == name), "X3"] == "")){
   rat <-  0
 }
 else {
   rat <- as.character(table[which(table$X1 == name), "X3"])
   rat <- messydates::as_messydate(rat)
 }
 # Compare dates, correct if necessary, and add verification
 # variables for changes
 orig_sign <- HUGGO_MEM[which(HUGGO_MEM$stateID == iso & HUGGO_MEM$manyID ==
                                "CMNTBT_1996A"), "stateSignature"]
 if (is.na(orig_sign)){
   orig_sign <- 0
 }
 orig_rat <- HUGGO_MEM[which(HUGGO_MEM$stateID == iso & HUGGO_MEM$manyID ==
                               "CMNTBT_1996A"), "stateRat"]
 if (is.na(orig_rat)){
   orig_rat <- 0
 }
 # stateRat
 if (rat != orig_rat){
   if (rat == 0){
     HUGGO_MEM[which(HUGGO_MEM$stateID == iso & HUGGO_MEM$manyID ==
                       "CMNTBT_1996A"), "stateRat"] <- NA
     HUGGO_MEM[which(HUGGO_MEM$stateID == iso & HUGGO_MEM$manyID ==
                       "CMNTBT_1996A"), "Changes_HUGGO"] <-
       paste("stateRat, ", HUGGO_MEM[which(HUGGO_MEM$stateID == iso &
                                             HUGGO_MEM$manyID == "CMNTBT_1996A"),
                                     "Changes_HUGGO"], sep = "")
   } else {
   HUGGO_MEM[which(HUGGO_MEM$stateID == iso & HUGGO_MEM$manyID ==
                     "CMNTBT_1996A"), "stateRat"] <- rat
   HUGGO_MEM[which(HUGGO_MEM$stateID == iso & HUGGO_MEM$manyID ==
                     "CMNTBT_1996A"), "Changes_HUGGO"] <-
     paste("stateRat, ", HUGGO_MEM[which(HUGGO_MEM$stateID == iso &
                                           HUGGO_MEM$manyID == "CMNTBT_1996A"),
                                   "Changes_HUGGO"], sep = "")
   }
 }
 # stateSignature
 if (sign != orig_sign){
   if (sign == 0){
     HUGGO_MEM[which(HUGGO_MEM$stateID == iso & HUGGO_MEM$manyID ==
                       "CMNTBT_1996A"), "stateSignature"] <- sign
     HUGGO_MEM[which(HUGGO_MEM$stateID == iso & HUGGO_MEM$manyID ==
                       "CMNTBT_1996A"), "Changes_HUGGO"] <-
       paste("stateSignature, ", HUGGO_MEM[which(HUGGO_MEM$stateID == iso &
                                                   HUGGO_MEM$manyID == "CMNTBT_1996A"),
                                           "Changes_HUGGO"], sep = "")
   } else {
   HUGGO_MEM[which(HUGGO_MEM$stateID == iso & HUGGO_MEM$manyID ==
                     "CMNTBT_1996A"), "stateSignature"] <- sign
   HUGGO_MEM[which(HUGGO_MEM$stateID == iso & HUGGO_MEM$manyID ==
                     "CMNTBT_1996A"), "Changes_HUGGO"] <-
     paste("stateSignature, ", HUGGO_MEM[which(HUGGO_MEM$stateID == iso &
                                                 HUGGO_MEM$manyID == "CMNTBT_1996A"),
                                         "Changes_HUGGO"], sep = "")
   }
 }
 # Verification variables
 HUGGO_MEM[which(HUGGO_MEM$manyID == "CMNTBT_1996A" & HUGGO_MEM$stateID == iso),
           "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce, stateWithdrawal"
 HUGGO_MEM[which(HUGGO_MEM$manyID == "CMNTBT_1996A" & HUGGO_MEM$stateID == iso),
           "Checked_HUGGO"] <- 1
}


# FC10NP_1993A
# Add stateSignature date: 1993-02-22
HUGGO_MEM[which(HUGGO_MEM$manyID == "FC10NP_1993A"), "stateSignature"] <-
  messydates::as_messydate("1993-02-22")
# Correct stateRat: NA instead of hashtags
HUGGO_MEM[which(HUGGO_MEM$manyID == "FC10NP_1993A" & HUGGO_MEM$stateID != "NIC"),
          "stateRat"] <- NA
# Add Nicaragua stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "FC10NP_1993A" & HUGGO_MEM$stateID == "NIC"),
          "stateRat"] <- messydates::as_messydate("1993-12-21")
# Add stateForce for all
HUGGO_MEM[which(HUGGO_MEM$manyID == "FC10NP_1993A"), "stateForce"] <-
  messydates::as_messydate("1999-03-01")
# Add verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "FC10NP_1993A"), "Checked_HUGGO"] <-
  1
HUGGO_MEM[which(HUGGO_MEM$manyID == "FC10NP_1993A"), "Changes_HUGGO"] <-
  "stateSignature, stateRat, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "FC10NP_1993A"), "Verified_HUGGO"] <-
  "stateSignature, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "FC10NP_1993A" & HUGGO_MEM$stateID == "NIC"),
          "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce"
  
# ACPEEC_1989A
# Check with scraped data from UNTS website
content <- read_html("https://treaties.un.org/Pages/showDetails.aspx?objid=08000002800b1e2b")
table <- content %>%
  html_table(fill = TRUE)
table <- table[[6]]
# Change some country names
table <- table %>%
  mutate(Participant = case_when(Participant == "Swaziland" ~ "Eswatini",
                                  Participant == "Cape Verde" ~ "Cabo Verde",
                                 Participant == "Netherlands" ~ "Netherlands (Kingdom of the) ", 
                                 .default = Participant))
# Add stateForce
# Add table with iso code
un_iso <- read.csv("data-raw/memberships/HUGGO_MEM/un_iso.csv")
acpeec <- HUGGO_MEM %>%
  filter(manyID == "ACPEEC_1989A")
for (i in 1:nrow(acpeec)){
  iso <- as.character(acpeec[i, "stateID"])
  name <- un_iso[which(un_iso$Alpha.3.code.5. == iso), "Country.name"]
  print(i)
  print(name)
  # Assign dates
  # stateForce
  if(name %in% table$Participant){
    force<- as.character(table[which(table$Participant == name), "Date of Effect"])
    force <- messydates::as_messydate(force)
  # Compare dates, correct if necessary, and add verification
  # variables for changes
  orig_force <- HUGGO_MEM[which(HUGGO_MEM$stateID == iso & HUGGO_MEM$manyID ==
                                 "ACPEEC_1989A"), "stateForce"]
  if (is.na(orig_force)){
    orig_force <- 0
  }
  # stateSignature
  if (force != orig_force){
    if (force == 0){
      HUGGO_MEM[which(HUGGO_MEM$stateID == iso & HUGGO_MEM$manyID ==
                        "ACPEEC_1989A"), "stateForce"] <- force()
      HUGGO_MEM[which(HUGGO_MEM$stateID == iso & HUGGO_MEM$manyID ==
                        "ACPEEC_1989A"), "Changes_HUGGO"] <-
        paste("stateForce, ", HUGGO_MEM[which(HUGGO_MEM$stateID == iso &
                                                    HUGGO_MEM$manyID == "ACPEEC_1989A"),
                                            "Changes_HUGGO"], sep = "")
    } else {
      HUGGO_MEM[which(HUGGO_MEM$stateID == iso & HUGGO_MEM$manyID ==
                        "ACPEEC_1989A"), "stateForce"] <- force
      HUGGO_MEM[which(HUGGO_MEM$stateID == iso & HUGGO_MEM$manyID ==
                        "ACPEEC_1989A"), "Changes_HUGGO"] <- "stateForce"
    }
  }
  # Verification variables
  HUGGO_MEM[which(HUGGO_MEM$manyID == "ACPEEC_1989A" & HUGGO_MEM$stateID == iso),
            "Verified_HUGGO"] <- "stateSignature, stateForce, stateWithdrawal"
  HUGGO_MEM[which(HUGGO_MEM$manyID == "ACPEEC_1989A" & HUGGO_MEM$stateID == iso),
            "Checked_HUGGO"] <- 1
  }
}
# Add stateForce DMA and COG
HUGGO_MEM[which(HUGGO_MEM$manyID == "ACPEEC_1989A" & HUGGO_MEM$stateID == "COD"),
          "stateForce"] <- messydates::as_messydate("1991-09-01")
HUGGO_MEM[which(HUGGO_MEM$manyID == "ACPEEC_1989A" & HUGGO_MEM$stateID == "COD"),
          "Checked_HUGGO"] <- 1
HUGGO_MEM[which(HUGGO_MEM$manyID == "ACPEEC_1989A" & HUGGO_MEM$stateID == "COD"),
          "Changes_HUGGO"] <- "stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "ACPEEC_1989A" & HUGGO_MEM$stateID == "COD"),
          "Verified_HUGGO"] <- "stateSignature, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "ACPEEC_1989A" & HUGGO_MEM$stateID == "DMA"),
          "stateForce"] <- messydates::as_messydate("1991-09-01")
HUGGO_MEM[which(HUGGO_MEM$manyID == "ACPEEC_1989A" & HUGGO_MEM$stateID == "DMA"),
          "Checked_HUGGO"] <- 1
HUGGO_MEM[which(HUGGO_MEM$manyID == "ACPEEC_1989A" & HUGGO_MEM$stateID == "DMA"),
          "Changes_HUGGO"] <- "stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "ACPEEC_1989A" & HUGGO_MEM$stateID == "DMA"),
          "Verified_HUGGO"] <- "stateSignature, stateForce, stateWithdrawal"
# Correct stateSignature: Côte d'Ivoire
HUGGO_MEM[which(HUGGO_MEM$manyID == "ACPEEC_1989A" & HUGGO_MEM$stateID == "CIV"),
          "stateSignature"] <-messydates::as_messydate("1989-12-15")
HUGGO_MEM[which(HUGGO_MEM$manyID == "ACPEEC_1989A" & HUGGO_MEM$stateID == "CIV"),
          "Changes_HUGGO"] <-paste("stateSignature, ", HUGGO_MEM[which(HUGGO_MEM$manyID == "ACPEEC_1989A" &
                                                                         HUGGO_MEM$stateID == "CIV"), 
                                                                 "Changes_HUGGO"], sep = "")


# INPPSA_1978P1:MARPOL_1973A
# Load dataset with status list (checar fechas bien)
marpol <- read.csv("data-raw/memberships/HUGGO_MEM/marpol_prot1978.csv")
# Add NA values
marpol <- marpol %>%
  mutate(Ratification = case_when(Ratification == "" ~ NA,
                                  .default = Ratification))
# Load table
treaty <- HUGGO_MEM %>%
  filter(manyID == "INPPSA_1978P1:MARPOL_1973A")
ids <- unique(treaty$stateID)
ids <- ids[-which(ids == "DEU")]
# Remove duplicate rows
for (i in 1:length(ids)){
  id <- ids[i]
  rows <- which(HUGGO_MEM$stateID == id & HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A")
  if (length(rows) > 1){
    HUGGO_MEM <- HUGGO_MEM[-(rows[-1]), ]
  }
}
# Verify dates
# Make vector of original signatories (except for Germany)
signat <- c("AUS", "FRA", "LBR", "MEX", "NLD", "POL",
            "ESP", "SWE", "GBR", "USA", "URY")
un_iso <- read.csv("data-raw/memberships/HUGGO_MEM/un_iso.csv")
for (i in 1:length(ids)){
  id <- ids[i]
  name <- un_iso[which(un_iso$Alpha.3.code.5. == id), "Country.name"]
  name2 <- marpol[which(marpol$Country == name), 1]
  # Assign dates
  orig_rat <- HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                                HUGGO_MEM$stateID == id), "stateRat"]
  rat <- messydates::as_messydate(marpol[which(marpol$Country == name), "Ratification"])
  orig_force <- HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                                  HUGGO_MEM$stateID == id), "stateForce"]
  force <- messydates::as_messydate(marpol[which(marpol$Country == name), "Force"])
  # Determine if NAs (only ratification column has NAs)
  if (is.na(orig_rat)){
    orig_rat <- messydates::as_messydate("0001-01-01")
  }
  if (length(rat) == 0 | is.na(rat)){
    rat <- messydates::as_messydate("0001-01-01")
  }
  # Compare, assign and verify
  if (id %in% signat){
    # stateSignature
    orig_sign <- HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                                   HUGGO_MEM$stateID == id), "stateSignature"]
    sign <- messydates::as_messydate("1978-02-17")
    if (sign != orig_sign){
      HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                        HUGGO_MEM$stateID == id), "stateSignature"] <- sign
      HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                        HUGGO_MEM$stateID == id), "Changes_HUGGO"] <- "stateSignature"
    } 
    } else {
      HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                        HUGGO_MEM$stateID == id), "stateSignature"] <- NA
      HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                        HUGGO_MEM$stateID == id), "Changes_HUGGO"] <- "stateSignature"
    }
    # stateRat
    if (rat != orig_rat){
        HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                          HUGGO_MEM$stateID == id), "stateRat"] <- messydates::as_messydate(rat)
        HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                          HUGGO_MEM$stateID == id), "Changes_HUGGO"] <-
          paste(HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                                  HUGGO_MEM$stateID == id), "Changes_HUGGO"],
                "stateRat", sep = ", ")
    }
    #stateForce
    if (force != orig_force){
      HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                        HUGGO_MEM$stateID == id), "stateForce"] <- messydates::as_messydate(force)
      HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                        HUGGO_MEM$stateID == id), "Changes_HUGGO"] <-
        paste(HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                                HUGGO_MEM$stateID == id), "Changes_HUGGO"],
              "stateForce", sep = ", ")
    }
  # Accession
  if (is.na(HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                      HUGGO_MEM$stateID == id), "stateSignature"])){
    HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                      HUGGO_MEM$stateID == id), "Accession"] <- messydates::as_messydate(rat)
    HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                      HUGGO_MEM$stateID == id), "Changes_HUGGO"] <-
      paste(HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" &
                              HUGGO_MEM$stateID == id), "Changes_HUGGO"],
            "Accession", sep = ", ")
  }
}
# Germanies
# West Germany add stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" & HUGGO_MEM$stateID == "DEU" &
                  HUGGO_MEM$stateSignature == "1978-02-17"), "stateRat"] <- messydates::as_messydate("1982-01-21")
HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" & HUGGO_MEM$stateID == "DEU" &
                  HUGGO_MEM$stateSignature == "1978-02-17"), "Changes_HUGGO"] <- "stateRat"
# East Germany
HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" & HUGGO_MEM$stateID == "DEU" &
                  HUGGO_MEM$stateSignature != "1978-02-17"), "stateSignature"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" & HUGGO_MEM$stateID == "DEU" &
                  is.na(HUGGO_MEM$stateSignature)), "stateRat"] <- messydates::as_messydate("1984-04-25")
HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" & HUGGO_MEM$stateID == "DEU" &
                  is.na(HUGGO_MEM$stateSignature)), "stateForce"] <- messydates::as_messydate("1984-07-25")
HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" & HUGGO_MEM$stateID == "DEU" &
                  is.na(HUGGO_MEM$stateSignature)), "stateWithdrawal"] <- messydates::as_messydate("1990-10-03")
HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" & HUGGO_MEM$stateID == "DEU" &
                  is.na(HUGGO_MEM$stateSignature)), "Accession"] <- messydates::as_messydate("1984-04-25")
HUGGO_MEM[which(HUGGO_MEM$manyID == "INPPSA_1978P1:MARPOL_1973A" & HUGGO_MEM$stateID == "DEU" &
                  is.na(HUGGO_MEM$stateSignature)), "Changes_HUGGO"] <-
  "stateSignature, stateRat, stateForce, stateWithdrawal, Accession"
# Successor states
HUGGO_MEM <- HUGGO_MEM %>%
  mutate(stateRat = case_when(manyID == "INPPSA_1978P1:MARPOL_1973A" & stateRat == "0001-01-01"
                              ~ NA,
                              .default = stateRat)) %>%
  mutate(Changes_HUGGO = case_when(manyID == "INPPSA_1978P1:MARPOL_1973A" & is.na(stateRat)
                              ~ gsub(",?\\s*Accession\\b", "", Changes_HUGGO),
                              .default = Changes_HUGGO)) %>%
  mutate(Accession = case_when(manyID == "INPPSA_1978P1:MARPOL_1973A" & Accession == "0001-01-01"
                              ~ NA,
                              .default = Accession))
# Add verification variables
HUGGO_MEM <- HUGGO_MEM %>%
  mutate(Checked_HUGGO = case_when(manyID == "INPPSA_1978P1:MARPOL_1973A" ~  1,
                                   .default = Checked_HUGGO ))%>%
  mutate(Verified_HUGGO = case_when(manyID == "INPPSA_1978P1:MARPOL_1973A" ~ "stateSignature, stateForce, stateRat, stateWithdrawal, Accession",
                                    .default = Verified_HUGGO))

  
# INTCPE_1991P:INTCPE_1990A
# Remove extra rows
# Czechoslovakia
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$treatyID == "INTCPE_1991P" &
                                HUGGO_MEM$stateID == "CSK"),]
# Czechia
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$treatyID == "INTCPE_1991P" &
                                HUGGO_MEM$stateID == "CZE"),]
# Germany
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$treatyID == "INTCPE_1991P" &
                         HUGGO_MEM$stateID == "DEU"),]
deu <- which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" & 
               HUGGO_MEM$stateID == "DEU")
HUGGO_MEM <- HUGGO_MEM[-deu[-1],]
# European Union
# According to Treaty of Lisbon, the European Union is to be deemed contracting
# party to all treaties the European Community was part of
eue <- which(HUGGO_MEM$treatyID == "INTCPE_1991P" & 
               HUGGO_MEM$stateID == "EUE")
HUGGO_MEM <- HUGGO_MEM[-eue[-1],]
euu <- which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" & 
                                      HUGGO_MEM$stateID == "EUU")
HUGGO_MEM <- HUGGO_MEM[-euu[-1],]
# Slovakia
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$treatyID == "INTCPE_1991P" &
                                HUGGO_MEM$stateID == "SVK"),]
# Correct End (Treaty is still in force between DEU and CZE)
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A"), "End"] <- NA
# Correct dates
# CSK: Czechoslovakia
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "CSK"), "stateForce"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "CSK"), "stateWithdrawal"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "CSK"), "Changes_HUGGO"] <-
  "stateForce, stateWithdrawal"
# CZE: Czechia
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                 HUGGO_MEM$stateID == "CZE"), "stateSignature"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "CZE"), "stateRat"] <-
  messydates::as_messydate("1993-01-01")
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "CZE"), "stateWithdrawal"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "CZE"), "Changes_HUGGO"] <-
  "stateSignature, stateRat, stateWithdrawal"
# DEU: Germany
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "DEU"), "stateSignature"] <-
  messydates::as_messydate("1991-12-09")
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "DEU"), "stateRat"] <-
  messydates::as_messydate("1993-07-14")
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "DEU"), "stateWithdrawal"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "DEU"), "Changes_HUGGO"] <-
  "stateSignature, stateRat, stateWithdrawal"
# EUU and EUE: European Union
# EUE correct
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "EUU"), "stateRat"] <-
  messydates::as_messydate("1993-05-27")
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "EUU"), "Changes_HUGGO"] <-
  "stateRat"
# SVK: Slovakia
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "SVK"), "stateSignature"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "SVK"), "stateRat"] <-
  messydates::as_messydate("1993-01-01")
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "SVK"), "Changes_HUGGO"] <-
  "stateSignature, stateRat"
# Add verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A"), "Verified_HUGGO"] <-
  "stateSignature, stateRat, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A"), "Checked_HUGGO"] <- 1

# PD04WD_1992A
# Scrape UNTC website
pdo4wd <- read.csv("data-raw/memberships/HUGGO_MEM/PD04WD_1992A.csv")
colnames(pdo4wd) <- c("Participant", "Signature", "Ratification")
# Clean table
pdo4wd <- pdo4wd %>%
  mutate(Signature = gsub("\\.", " ", Signature)) %>%
  mutate(Signature = sub("\\b(9\\d)\\b", "19\\1", Signature)) %>%
  mutate(Signature = messydates::as_messydate(Signature)) %>%
  mutate(Type  = case_when(
      grepl("\\d+\\sa", Ratification) ~ "Acc",  # Matches digits followed by 'a'
      grepl("\\d+\\sd", Ratification) ~ "Succ", # Matches digits followed by 'd'
      .default =  "Rat"                          # For other cases
    )) %>%
  mutate(Ratification = gsub("\\.", " ", Ratification)) %>%
  mutate(Ratification = gsub("\\sa", "", Ratification)) %>%
  mutate(Ratification = gsub("\\sd", "", Ratification)) %>%
  mutate(Ratification = sub("\\b(9\\d)\\b", "19\\1", Ratification)) %>%
  mutate(Ratification = sub("\\b(0\\d)\\b", "20\\1", Ratification)) %>%
  mutate(Ratification = messydates::as_messydate(Ratification)) %>%
  mutate(Signature = case_when(Signature == "" ~ NA,
                               .default = Signature))
# Clean Sao Tome and Principe
pdo4wd[which(pdo4wd$Participant == "Sao Tome and Principe"), "Ratification"] <-
  messydates::as_messydate("2003-09-09")
pdo4wd[which(pdo4wd$Participant == "Sao Tome and Principe"), "Type"] <-
  "Acceptance"
# Correct duplicate New Zealand entry, corresponds to Cook Islands
HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == "NZL" &
            HUGGO_MEM$stateRat == "1994-07-15"), "stateID"] <- "COK"
# Prepare verification
treaty <- HUGGO_MEM %>%
  filter(manyID == "PD04WD_1992A")
ids <- unique(treaty$stateID)
# Verify stateSignature
for (i in 1:length(ids)){
  id <- ids[i]
  name <- un_iso[which(un_iso$Alpha.3.code.5. == id), "Country.name"]
  name2 <- pdo4wd[which(pdo4wd$Participant == name), "Participant"]
  # Compare stateSignature dates
  orig_sign <- HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                         "stateSignature"]
  if (is.na(orig_sign)){
    orig_sign <- messydates::as_messydate("0001-01-01")
  }
  sign <- pdo4wd[which(pdo4wd$Participant == name), "Signature"]
  if (is.na(sign) | length(as.character(sign)) < 1 | is.null(sign)){
    sign <- messydates::as_messydate("0001-01-01")
  }
  if (sign != orig_sign){
    if (type == "Acc"){
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "stateSignature"] <- NA
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "Changes_HUGGO"] <- "stateSignature"
    } else if (type == "Succ"){
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "stateSignature"] <- NA
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "Changes_HUGGO"] <- "stateSignature"
    } else if (type == "Acceptance"){
    HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
              "stateSignature"] <- NA
  HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
            "Changes_HUGGO"] <- "stateSignature"
    } else if (type == "Rat") {
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "stateSignature"] <- messydates::as_messydate(sign)
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "Changes_HUGGO"] <- "stateSignature"
    }
  }
  if (HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "stateSignature"] == "0001-01-01"){
    HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
              "stateSignature"] <- NA
  }
}
# Verify stateRat
for (i in 1:length(ids)){
  id <- ids[i]
  name <- un_iso[which(un_iso$Alpha.3.code.5. == id), "Country.name"]
  name2 <- pdo4wd[which(pdo4wd$Participant == name), "Participant"]
  # Compare stateRat dates
  orig_rat <- HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                         "stateRat"]
  if (is.na(orig_rat)){
    orig_rat <- messydates::as_messydate("0001-01-01")
  }
  rat <- pdo4wd[which(pdo4wd$Participant == name), "Ratification"]
  type <- pdo4wd[which(pdo4wd$Participant == name), "Type"]
  if (is.na(rat) | length(as.character(rat)) < 1 | is.null(rat)){
    rat <- messydates::as_messydate("0001-01-01")
  }
  if (rat != orig_rat){
    if (type == "Acc"){
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "stateRat"] <- messydates::as_messydate(rat)
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "Accession"] <- messydates::as_messydate(rat)
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "Changes_HUGGO"] <- paste(HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                                                    "Changes_HUGGO"], ", stateRat, Accession", sep = "")
    } else if (type == "Succ"){
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "stateRat"] <- NA
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "Changes_HUGGO"] <- paste(HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                                                    "Changes_HUGGO"], ", stateRat", sep = "")
    } else if (type == "Acceptance"){
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "stateRat"] <- messydates::as_messydate(rat)
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "Acceptance"] <- messydates::as_messydate(rat)
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "Accession"] <- NA
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "Changes_HUGGO"] <- paste(HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                                                    "Changes_HUGGO"], ", stateRat, Accession, Approval", sep = "")
    } else if (type == "Rat") {
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "stateRat"] <- messydates::as_messydate(rat)
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "Accession"] <- NA
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "Changes_HUGGO"] <- paste(HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                                                    "Changes_HUGGO"], ", stateRat, Accession", sep = "")
    }
  }
  if (type == "Rat" & !is.na(HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                                       "Accession"])){
    HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
              "Accession"] <- NA
  }
}
# Add stateForce
for (i in 1:length(ids)){
  id <- ids[i]
  name <- un_iso[which(un_iso$Alpha.3.code.5. == id), "Country.name"]
  staterat <- HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                        "stateRat"]
  type <- pdo4wd[which(pdo4wd$Participant == name), "Type"]
  if (!is.na(staterat) & type != "Succ"){
    if (staterat < messydates::as_messydate("1997-04-29")){
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "stateForce"] <- messydates::as_messydate("1997-04-29")
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "Changes_HUGGO"] <- paste(HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                                                    "Changes_HUGGO"], ", stateForce", sep = "")
    } else {
      stateforce <- staterat + 30
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "stateForce"] <- staterat + 30
      HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                "Changes_HUGGO"] <- paste(HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                                                    "Changes_HUGGO"], ", stateForce", sep = "")
    }
  } else if (is.na(staterat) & type == "Succ"){
    HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
              "stateForce"] <- messydates::as_messydate(pdo4wd[which(pdo4wd$Participant == name), "Ratification"])
    HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
              "Accession"] <- NA
    HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
              "Changes_HUGGO"] <- paste(HUGGO_MEM[which(HUGGO_MEM$manyID == "PD04WD_1992A" & HUGGO_MEM$stateID == id),
                                                  "Changes_HUGGO"], ", stateForce, Accession", sep = "")
  }
}
# stateRat Israel
HUGGO_MEM <- HUGGO_MEM %>%
  mutate(stateRat = case_when(stateRat == "" & manyID == "PD04WD_1992A" ~ NA,
                              .default = stateRat)) %>%
  mutate(stateForce = case_when(stateRat == "" & manyID == "PD04WD_1992A" & stateID == "ISR" ~ NA,
                                .default = stateForce))
# Add Checked status
HUGGO_MEM <- HUGGO_MEM %>%
  mutate(Checked_HUGGO = case_when(manyID == "PD04WD_1992A" ~ 1,
                                   .default = Checked_HUGGO))

# MNSDOL_1997E
# Remove duplicates with incorrect information
# Scrape UN website
content <- read_html("https://treaties.un.org/pages/ViewDetails.aspx?src=TREATY&mtdsg_no=XXVII-2-d&chapter=27&clang=_en")
table <- content %>%
  html_table(fill = TRUE)
table <- table[[1]]
table <- table[21:217,1:2]
colnames(table) <- c("Country", "Ratification")
# Clean table
table <- table %>%
  mutate(Country = gsub("\\s\\d+", "", Country)) %>%
  mutate(Type = sub("\\d{4}\\s(.*)", "\\1", Ratification)) %>%
mutate(Type  = case_when(
  grepl("\\d+\\sa", Ratification) ~ "Acc",  # Matches digits followed by 'a'
  grepl("\\d+\\sd", Ratification) ~ "Succ", # Matches digits followed by 'd'
  grepl("\\d+\\sAA", Ratification) ~ "Approval",
  grepl("\\d+\\sA", Ratification) ~ "Acceptance",
  .default =  "Rat"                          # For other cases
)) %>%
  mutate(Ratification = gsub("\\sa", "", Ratification)) %>%
  mutate(Ratification = gsub("\\sd", "", Ratification)) %>%
  mutate(Ratification = gsub("\\sAA", "", Ratification)) %>%
  mutate(Ratification = gsub("\\bA\\b(?![A-Za-z])", "", Ratification, perl = TRUE))
# Modify names to facilitate process
table <- table %>%
  mutate(Country = case_when(Country == "Netherlands (Kingdom of the)" ~ "Netherlands (Kingdom of the) ",
                             Country == "United Kingdom of Great Britain and Northern Ireland,,," ~
                               "United Kingdom of Great Britain and Northern Ireland",
                             Country == "Democratic People's Republic of Korea" ~
                               "Korea, Dem. People's Rep.",
                             .default = Country))
# Remove duplicates
treaty <- HUGGO_MEM %>%
  filter(manyID == "MNSDOL_1997E")
ids <- unique(treaty$stateID)
# Remove Guyana (fix manually later)
for (i in 1:length(ids)){
  id <- ids[i]
  name <- un_iso[which(un_iso$Alpha.3.code.5. == id), "Country.name"]
  staterat <- as.character(table[which(table$Country == name), "Ratification"])
  staterat <- messydates::as_messydate(staterat)
  HUGGO_MEM[HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == id &
              HUGGO_MEM$stateRat != staterat, "Dup"] <- 1
}
HUGGO_MEM <- HUGGO_MEM %>%
  filter(is.na(Dup))
HUGGO_MEM$id <- NA
HUGGO_MEM$id[HUGGO_MEM$manyID == "MNSDOL_1997E"] <-
  1:length(HUGGO_MEM$manyID[HUGGO_MEM$manyID == "MNSDOL_1997E"])
treaty <- HUGGO_MEM %>%
  filter(manyID == "MNSDOL_1997E")
ids <- unique(treaty$stateID)
for (i in 1:nrow(treaty)){
  stateforce <- as.character(HUGGO_MEM[which(HUGGO_MEM$manyID == "MNSDOL_1997E" &
                                               HUGGO_MEM$id == i), "stateForce"])
  staterat <- as.character(HUGGO_MEM[which(HUGGO_MEM$manyID == "MNSDOL_1997E" &
                                             HUGGO_MEM$id == i), "stateRat"])
  if (stateforce > "1999-11-10"){
    difference <- as.numeric(difftime(as.Date(stateforce), as.Date(staterat), units = "days") / 365.25)
    # Check if date2 is greater than two years from date1
    if (difference > 2) {
      HUGGO_MEM[HUGGO_MEM$manyID == "MNSDOL_1997E" &
                  HUGGO_MEM$id == i, "Dup"] <- 1
    } else {
      HUGGO_MEM[HUGGO_MEM$manyID == "MNSDOL_1997E" &
                  HUGGO_MEM$id == i, "Dup"] <- 0
    }
  } else {
    HUGGO_MEM[HUGGO_MEM$manyID == "MNSDOL_1997E" &
                HUGGO_MEM$id == i, "Dup"] <- 3
  }
}
HUGGO_MEM[which(HUGGO_MEM$manyID != "MNSDOL_1997E"), "Dup"] <- 4
HUGGO_MEM <- HUGGO_MEM %>%
  filter(Dup != 1)

# Manual cleaning
treaty <- HUGGO_MEM %>%
  filter(manyID == "MNSDOL_1997E")
ids <- unique(treaty$stateID)
# United Arab Emirates (ARE)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "ARE" &
                                HUGGO_MEM$stateForce != "2005-05-17"),]
# Belarus (BLR)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "BLR" &
                                HUGGO_MEM$stateForce != "2007-06-11"),]
# Bhutan (BTN)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "BTN" &
                                HUGGO_MEM$stateForce != "2004-11-21"),]
# Botswana (BWA)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "BWA" &
                                HUGGO_MEM$stateForce != "2013-05-22"),]
# Cameroon (CMR)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "CMR" &
                                HUGGO_MEM$stateForce != "2009-11-19"),]
# Democratic Republic of Congo (COD)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "COD" &
                                HUGGO_MEM$stateForce != "2005-06-21"),]
# Djibouti (DJI)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "DJI" &
                                HUGGO_MEM$stateForce != "1999-11-10"),]
# Dominica (DMA)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "DMA" &
                                HUGGO_MEM$stateForce != "2006-06-05"),]
# Dominican Republic (DOM)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "DOM" &
                                HUGGO_MEM$stateForce != "2009-12-30"),]
# Eritrea (ERI)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "ERI" &
                                HUGGO_MEM$stateForce != "2005-10-03"),]
# Ethiopia (ETH)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "ETH" &
                                HUGGO_MEM$stateForce != "2010-02-23"),]
# France (FRA)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "FRA" &
                                HUGGO_MEM$stateForce != "2003-10-23"),]
# United Kingdom (GBR)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "GBR" &
                                HUGGO_MEM$stateForce != "2002-01-10"),]
# Ghana (GHA)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "GHA" &
                                HUGGO_MEM$stateForce != "2005-11-06"),]
# Greece (GRC)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "GRC" &
                                HUGGO_MEM$stateForce != "2006-04-27"),]
# Grenada (GRD)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "GRD" &
                                HUGGO_MEM$stateForce != "1999-11-10"),]
# Guyana (GUY)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "GUY" &
                                HUGGO_MEM$stateForce != "1999-11-10"),]
# Indonesia (IDN)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "IDN" &
                                HUGGO_MEM$stateForce != "2006-04-26"),]
# Liechtenstein (LIE)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "LIE" &
                                HUGGO_MEM$stateForce != "2004-03-22"),]
# Lesotho (LSO)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "LSO" &
                                HUGGO_MEM$stateForce != "2010-07-14"),]
# Moldova (MDA)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "MDA" &
                                HUGGO_MEM$stateForce != "2005-08-22"),]
# Malaysia (MYS)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "MYS" &
                                HUGGO_MEM$stateForce != "2002-01-24"),]
# Namibia (NAM)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "NAM" &
                                HUGGO_MEM$stateForce != "2007-12-30"),]
# Niger (NER)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "NER" &
                                HUGGO_MEM$stateForce != "1999-11-10"),]
# Niue (NIU)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "NIU" &
                                HUGGO_MEM$stateForce != "2004-03-21"),]
# Nauru (NRU)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "NRU" &
                                HUGGO_MEM$stateForce != "2004-12-09"),]
# Pakistan (PAK)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "PAK" &
                                HUGGO_MEM$stateForce != "2005-12-01"),]
# Russia (RUS)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "RUS" &
                                HUGGO_MEM$stateForce != "2006-03-14"),]
# San Marino (SMR)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "SMR" &
                                HUGGO_MEM$stateForce != "2009-07-22"),]
# Serbia (SBR)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "SRB" &
                                HUGGO_MEM$stateForce != "2005-06-20"),]
# Suriname (SUR)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "SUR" &
                                HUGGO_MEM$stateForce != "2006-06-27"),]
# Eswatini (SWZ)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "SWZ" &
                                HUGGO_MEM$stateForce != "2006-03-16"),]
# Tajikistan (TJK)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "TJK" &
                                HUGGO_MEM$stateForce != "2009-08-05"),]
# Timor-Leste (TLS)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "TLS" &
                                HUGGO_MEM$stateForce != "2009-12-15"),]
# Trinidad and Tobago (TTO)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "TTO" &
                                HUGGO_MEM$stateForce != "1999-11-10"),]
# Viet Nam (VNM)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "VNM" &
                                HUGGO_MEM$stateForce != "2005-03-03"),]
# South Africa (ZAF)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "ZAF" &
                                HUGGO_MEM$stateForce != "2005-02-09"),]
# Zambia (ZMB)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MNSDOL_1997E" & HUGGO_MEM$stateID == "ZMB" &
                                HUGGO_MEM$stateForce != "2008-01-09"),]
# Change stateSignature (amendment not signed)
HUGGO_MEM[which(HUGGO_MEM$manyID == "MNSDOL_1997E"), "stateSignature"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "MNSDOL_1997E"), "Changes_HUGGO"] <- "stateSignature"
HUGGO_MEM[which(HUGGO_MEM$manyID == "MNSDOL_1997E"), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "MNSDOL_1997E"), "Checked_HUGGO"] <- 1
# Drop extra columns
HUGGO_MEM <- HUGGO_MEM %>%
  select(-"Dup") %>%
  select(-"id")

# SIASFO_1999A
# Remove duplicate rows with no information for stateSignature
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "SIASFO_1999A" &
                                HUGGO_MEM$stateSignature != "1999-04-09"),]
# Add NAs to stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "SIASFO_1999A"), 10] <- NA


# AI08ZP_1989A
# It appears no such arrangement was signed in 1989.
# If its nonexistence is verified, should be removed from HUGGO too.

# PE05SA_1976P (check for SUCCESSION)
# Albania: Remove duplicate row with no accession date
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "ALB" &
                                is.na(HUGGO_MEM$Accession)),]
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "ALB"), "stateForce"] <-
  messydates::as_messydate("1990-06-29")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "ALB"), "Changes_HUGGO"] <-
  "stateForce"
# Bosnia and Herzegovina: Remove duplicate row with incorrect stateForce_iea
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "BIH" &
                                HUGGO_MEM$stateForce_ecolex != "1994-11-21"),]
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "BIH"), "stateRat"] <-
  messydates::as_messydate("1994-10-22")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "BIH"), "Accession"] <-
  messydates::as_messydate("1994-10-22")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "BIH"), "stateForce"] <-
  messydates::as_messydate("1994-11-21")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "BIH"), "Changes_HUGGO"] <-
 "stateRat, stateForce, Accession"
# Cyprus
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "CYP"), "stateForce"] <-
  messydates::as_messydate("1979-12-19")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "CYP"), "Accession"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "CYP"), "Changes_HUGGO"] <-
  "stateForce, Accession"
# Algeria: Remove duplicate row with incorrect accession date
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "DZA" &
                                HUGGO_MEM$Accession != "1981-03-16"),]
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "DZA"), "stateSignature"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "DZA"), "stateForce"] <-
  messydates::as_messydate("1981-04-15")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "DZA"), "Changes_HUGGO"] <-
  "stateSignature, stateForce"
# Egypt
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "EGY"), "stateForce"] <-
  messydates::as_messydate("1978-09-23")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "EGY"), "Accession"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "EGY"), "Changes_HUGGO"] <-
  "stateForce, Accession"
# Spain
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "ESP"), "stateForce"] <-
  messydates::as_messydate("1978-02-12")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "ESP"), "Accession"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "ESP"), "Changes_HUGGO"] <-
  "stateForce, Accession"
# European Union
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "EUU"), "stateForce"] <-
  messydates::as_messydate("1978-04-15")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "EUU"), "Accession"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "EUU"), "Changes_HUGGO"] <-
  "stateForce, Accession"
# France
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "FRA"), "stateForce"] <-
  messydates::as_messydate("1978-4-10")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "FRA"), "Accession"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "FRA"), "Changes_HUGGO"] <-
  "stateForce, Accession"
# Greece
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "GRC"), "stateForce"] <-
  messydates::as_messydate("1979-02-02")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "GRC"), "Accession"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "GRC"), "Changes_HUGGO"] <-
  "stateForce, Accession"
# Croatia: remove duplicate row with incorrect stateForce_ecolex (succession)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "HRV" &
                   HUGGO_MEM$stateForce_ecolex != "1992-07-12"), ]
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "HRV"), "stateRat"] <-
  messydates::as_messydate("1992-06-12")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "HRV"), "stateForce"] <-
  messydates::as_messydate("1992-07-12")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "HRV"), "stateSignature"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "HRV"), "Changes_HUGGO"] <-
  "stateSignature, stateRat, stateForce"
# Israel
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "ISR"), "stateForce"] <-
  messydates::as_messydate("1984-03-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "ISR"), "Accession"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "ISR"), "Changes_HUGGO"] <-
  "stateForce, Accession"
# Italy
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "ITA"), "stateForce"] <-
  messydates::as_messydate("1979-03-05")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "ITA"), "Accession"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "ITA"), "Changes_HUGGO"] <-
  "stateForce, Accession"
# Lebanon: remove row with incorrect Accession date
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "LBN" &
                   HUGGO_MEM$Accession!= "1977-11-08"), ]
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "LBN"), "stateForce"] <-
  messydates::as_messydate("1978-02-12")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "LBN"), "stateSignature"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "LBN"), "Changes_HUGGO"] <-
  "stateSignature, stateForce"
# Libya
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "LBY"), "stateForce"] <-
  messydates::as_messydate("1979-03-02")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "LBY"), "Accession"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "LBY"), "Changes_HUGGO"] <-
  "stateForce, Accession"
# Morocco
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "MAR"), "stateForce"] <-
  messydates::as_messydate("1980-02-15")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "MAR"), "Accession"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "MAR"), "Changes_HUGGO"] <-
  "stateForce, Accession"
# Monaco
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "MCO"), "stateForce"] <-
  messydates::as_messydate("1978-02-12")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "MCO"), "Accession"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "MCO"), "Changes_HUGGO"] <-
  "stateForce, Accession"
# Slovenia: remove duplicate row with incorrect stateForce_ecolex date
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "SVN" &
                   HUGGO_MEM$stateForce_ecolex != "1994-03-15"), ]
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "SVN"), "stateForce"] <-
  messydates::as_messydate("1994-03-15")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "SVN"), "stateSignature"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "SVN"), "Accession"] <-
  messydates::as_messydate("1993-09-16")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "SVN"), "Changes_HUGGO"] <-
  "stateSignature, stateForce, Accession"
# Syria: remove row with incorrect Accession date
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "SYR" &
                                HUGGO_MEM$Accession != "1978-12-26"), ]
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "SYR"), "stateForce"] <-
  messydates::as_messydate("1979-01-25")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "SYR"), "stateSignature"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "SYR"), "Changes_HUGGO"] <-
  "stateSignature, stateForce"
# Tunisia
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "TUN"), "stateForce"] <-
  messydates::as_messydate("1978-02-12")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "TUN"), "Accession"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "TUN"), "Changes_HUGGO"] <-
  "stateForce, Accession"
# Turkey
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "TUR"), "stateForce"] <-
  messydates::as_messydate("1981-05-06")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "TUR"), "Accession"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "TUR"), "Changes_HUGGO"] <-
  "stateForce, Accession"
# Yugoslavia
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "YUG"), "stateForce"] <-
  messydates::as_messydate("1978-02-12")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "YUG"), "Accession"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "YUG"), "stateWithdrawal"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "YUG"), "Changes_HUGGO"] <-
  "stateForce, Accession"
# Malta
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "MLT"), "stateForce"] <-
  messydates::as_messydate("1978-02-12")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "MLT"), "Accession"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "MLT"), "Changes_HUGGO"] <-
  "stateForce, Accession"
# Serbia and Montenegro
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "SCG"), "stateForce"] <-
  messydates::as_messydate("1992-04-27")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "SCG"), "stateSignature"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "SCG"), "stateWithdrawal"] <-
  messydates::as_messydate("2006-06-03")
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P" & HUGGO_MEM$stateID == "SCG"), "Changes_HUGGO"] <-
  "stateForce, Accession"
# Add verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P"), "Verified_HUGGO"] <-
  "stateSignature, stateRat, stateForce, Accession"
HUGGO_MEM[which(HUGGO_MEM$manyID == "PE05SA_1976P"), "Checked_HUGGO"] <-
  1

# MRT-SEN[SCW]_1978A
# Remove duplicate rows with no stateSignature
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MRT-SEN[SCW]_1978A" &
                                HUGGO_MEM$stateSignature != "1978-12-21"),]
# Add stateRat Mali (https://sgg-mali.ml/JO/1979/mali-jo-1979-573.pdf)
HUGGO_MEM[which(HUGGO_MEM$manyID == "MRT-SEN[SCW]_1978A" & HUGGO_MEM$stateID == "MLI"),
          "stateRat"] <- messydates::as_messydate("1979-05-17")
# Add stateRat Mauritania http://anac.mr/ANAC/JOf/1979/500-501%20fr%20sc.pdf
HUGGO_MEM[which(HUGGO_MEM$manyID == "MRT-SEN[SCW]_1978A" & HUGGO_MEM$stateID == "MRT"),
          "stateRat"] <- messydates::as_messydate("1979-07-20")
# Add NA stateRat Senegal
HUGGO_MEM[which(HUGGO_MEM$manyID == "MRT-SEN[SCW]_1978A" & HUGGO_MEM$stateID == "SEN"),
          "stateRat"] <- NA
# Add stateForce
HUGGO_MEM[which(HUGGO_MEM$manyID == "MRT-SEN[SCW]_1978A"),
          "stateForce"] <- messydates::as_messydate("1981-01-01")
# Add verification
HUGGO_MEM[which(HUGGO_MEM$manyID == "MRT-SEN[SCW]_1978A"),
          "Changes_HUGGO"] <- "stateRat, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "MRT-SEN[SCW]_1978A"),
          "Changes_HUGGO"] <- "stateRat, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "MRT-SEN[SCW]_1978A" & HUGGO_MEM$stateID != "SEN"),
          "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "MRT-SEN[SCW]_1978A" & HUGGO_MEM$stateID == "SEN"),
          "Verified_HUGGO"] <- "stateRat, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "MRT-SEN[SCW]_1978A"),
          "Checked_HUGGO"] <- 1

# CP06AR_1981A
# Cameroon: remove stateSignature (Accession)
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "CMR"), "stateSignature"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "CMR"), "stateForce"] <-
  messydates::as_messydate("1984-08-05")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "CMR"), "Changes_HUGGO"] <-
  "stateSignature, stateForce"
# Benin: remove duplicate row
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "BEN" &
                                HUGGO_MEM$stateForce_ecolex != "1997-12-16"),]
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "BEN"), "stateForce"] <-
  messydates::as_messydate("1997-12-16")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "BEN"), "Changes_HUGGO"] <-
  "stateForce"
# Côte d'Ivoire
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "CIV"), "stateForce"] <-
  messydates::as_messydate("1984-08-05")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "CIV"), "Changes_HUGGO"] <-
  "stateForce"
# Dem. Republic of Congo
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "COD"), "stateSignature"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "COD"), "stateForce"] <-
  messydates::as_messydate("2014-08-17")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "COD"), "Changes_HUGGO"] <-
  "stateSignature, stateForce"
# Congo
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "COG"), "stateForce"] <-
  messydates::as_messydate("1988-02-19")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "COG"), "Changes_HUGGO"] <-
  "stateForce"
# Gabon
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "GAB"), "stateForce"] <-
  messydates::as_messydate("1989-02-11")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "GAB"), "Changes_HUGGO"] <-
  "stateForce"
# Gambia: remove row with incorrect signatureState
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "GMB" &
                                HUGGO_MEM$stateSignature !="1981-06-13"),]
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "GMB"), "stateForce"] <-
  messydates::as_messydate("1984-08-05")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "GMB"), "Changes_HUGGO"] <-
  "stateForce"
# Ghana
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "GHA"), "stateForce"] <-
  messydates::as_messydate("1989-09-18")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "GHA"), "Changes_HUGGO"] <-
  "stateForce"
# Guinea
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "GIN"), "stateForce"] <-
  messydates::as_messydate("1984-08-05")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "GIN"), "Changes_HUGGO"] <-
  "stateForce"
# Guinea Bissau
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "GNB"), "stateForce"] <-
  messydates::as_messydate("2012-04-12")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "GNB"), "Changes_HUGGO"] <-
  "stateForce"
# Liberia: remove duplicate row
duplicate <- which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "LBR")
HUGGO_MEM <- HUGGO_MEM[-duplicate[-1],]
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "LBR"), "stateSignature"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "LBR"), "stateForce"] <-
  messydates::as_messydate("2005-05-21")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "LBR"), "Changes_HUGGO"] <-
  "stateSignature, stateForce"
# Mauritania
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "MRT"), "stateForce"] <-
  messydates::as_messydate("2012-06-17")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "MRT"), "Changes_HUGGO"] <-
  "stateForce"
# Nigeria: remove row with incorrect signatureState 
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "NGA" &
                                HUGGO_MEM$stateSignature !="1981-03-23"),]
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "NGA"), "stateForce"] <-
  messydates::as_messydate("1984-08-05")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "NGA"), "Changes_HUGGO"] <-
  "stateForce"
# Senegal
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "SEN"), "stateForce"] <-
  messydates::as_messydate("1984-08-05")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "SEN"), "Changes_HUGGO"] <-
  "stateForce"
# Sierra Leone: remove row with incorrect stateRat
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "SLE" &
                                HUGGO_MEM$stateRat !="2005-06-07"),]
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "SLE"), "stateSignature"] <-
  NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "SLE"), "stateForce"] <-
  messydates::as_messydate("2005-08-06")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "SLE"), "Accession"] <-
  messydates::as_messydate("2005-08-06")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "SLE"), "Changes_HUGGO"] <-
  "stateSignature, stateForce, Accession"
# Togo
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "TGO"), "stateForce"] <-
  messydates::as_messydate("1984-08-05")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "TGO"), "Changes_HUGGO"] <-
  "stateForce"
# South Africa
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "ZAF"), "stateForce"] <-
  messydates::as_messydate("2002-07-15")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A" & HUGGO_MEM$stateID == "ZAF"), "Changes_HUGGO"] <-
  "stateForce"
# Add verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A"), "Checked_HUGGO"] <-
  1
HUGGO_MEM[which(HUGGO_MEM$manyID == "CP06AR_1981A"), "Verified_HUGGO"] <-
  "stateSignature, stateRat, stateForce, Accession"

# SSTDLV_1985P
# Remove duplicate rows (rows with hashtags instead of NAs)
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "SSTDLV_1985P" & !is.na(HUGGO_MEM$stateSignature)),]
## Add NAs to stateSignature and stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "SSTDLV_1985P"), "stateSignature"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "SSTDLV_1985P"), "stateRat"] <- NA

# FNNRNA_1987R
# Remove duplicate rows with no stateSignature
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "FNNRNA_1987R" &
                                HUGGO_MEM$stateSignature != "1987-10-27"),]
# Add NAs to stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "FNNRNA_1987R"), 10] <- NA

# CU07PP_1991A
# Remove duplicate rows with incorrect stateSignature
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "CU07PP_1991A" &
                                HUGGO_MEM$stateSignature != "1991-10-20"),]
# Add verification rows
HUGGO_MEM[which(HUGGO_MEM$manyID == "CU07PP_1991A"), "Checked_HUGGO"] <-
  1
HUGGO_MEM[which(HUGGO_MEM$manyID == "CU07PP_1991A"), "Verified_HUGGO"] <-
  "stateSignature, stateRat, stateForce"

# ICIPPS_2007E:MARPOL_1973A
# Remove rows in which stateSignature is before Signature.
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "ICIPPS_2007E:MARPOL_1973A" &
                                HUGGO_MEM$stateSignature <= HUGGO_MEM$Signature),]

# DC08MS_1993A
# Remove duplicate rows with no stateSignature
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "DC08MS_1993A" &
                                HUGGO_MEM$stateSignature !="1993-07-14"),]
# Cape Verde: add stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "DC08MS_1993A" & HUGGO_MEM$stateID == "CPV"), "stateRat"] <-
  messydates::as_messydate("1995-12-12")
HUGGO_MEM[which(HUGGO_MEM$manyID == "DC08MS_1993A" & HUGGO_MEM$stateID == "CPV"), "Changes_HUGGO"] <-
  "stateRat"
# Replace hashtags with NAs
HUGGO_MEM[which(HUGGO_MEM$manyID == "DC08MS_1993A" & !(HUGGO_MEM$stateID %in% c("CPV", "MRT", "SEN"))), "stateRat"] <-
  NA
# Add verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "DC08MS_1993A" & HUGGO_MEM$stateID != "CPV"), "Verified_HUGGO"] <-
  "stateSignature"
HUGGO_MEM[which(HUGGO_MEM$manyID == "DC08MS_1993A" & HUGGO_MEM$stateID == "CPV"), "Verified_HUGGO"] <-
  "stateSignature, stateRat"
HUGGO_MEM[which(HUGGO_MEM$manyID == "DC08MS_1993A"), "Checked_HUGGO"] <-
  1

# ESTRCF_1993E:ESTRCF_1985A
# Duplicate rows
# Remove different treatyID
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$treatyID== "ESTRCF_1993E"),]
# Remove rows with no stateSignature
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$treatyID== "ESTRCF_1993E:ESTRCF_1985A" &
                                HUGGO_MEM$stateSignature != "1993-07-14"),]
# Add Cape Verde stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESTRCF_1993E:ESTRCF_1985A" & HUGGO_MEM$stateID == "CPV"), "stateRat"] <-
  messydates::as_messydate("1996-12-12")
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESTRCF_1993E:ESTRCF_1985A" & HUGGO_MEM$stateID == "CPV"), "Changes_HUGGO"] <-
  "stateRat"
# Replace hashtags with NAs
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESTRCF_1993E:ESTRCF_1985A" & !(HUGGO_MEM$stateID %in% c("CPV", "MRT", "SEN"))), "stateRat"] <-
  NA
# Add verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESTRCF_1993E:ESTRCF_1985A" & HUGGO_MEM$stateID != "CPV"), "Verified_HUGGO"] <-
  "stateSignature"
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESTRCF_1993E:ESTRCF_1985A" & HUGGO_MEM$stateID == "CPV"), "Verified_HUGGO"] <-
  "stateSignature, stateRat"
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESTRCF_1993E:ESTRCF_1985A"), "Checked_HUGGO"] <-
  1

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
