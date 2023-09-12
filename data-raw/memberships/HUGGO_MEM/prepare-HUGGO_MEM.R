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
HUGGO_MEM$ProvisionalApp <- messydates::as_messydate(HUGGO_MEM$ProvisionalApp)
HUGGO_MEM$Consent <- messydates::as_messydate(HUGGO_MEM$Consent)
HUGGO_MEM$Changes_HUGGO <- NA
HUGGO_MEM$Verified_HUGGO <- NA
HUGGO_MEM$Checked_HUGGO <- NA
HUGGO_MEM$Succession <- NA

# Successor states
yugos <- c("Serbia", "Bosnia and Herzegovina", "North Macedonia", "Slovenia",
           "Montenegro", "Croatia")
czechos <- c("Czech Republic", "Slovakia")


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
  "stateSignature, stateRat, stateForce, stateWithdrawal, Accession, Acceptance, Succession"

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
HUGGO_MEM[which(HUGGO_MEM$manyID == "GR10BP_1984A"), "Checked_HUGGO"] <-
  1
HUGGO_MEM[which(HUGGO_MEM$manyID == "GR10BP_1984A"), "Changes_HUGGO"] <-
  "stateSignature, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "GR10BP_1984A"), "Verified_HUGGO"] <-
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
# Add stateForce (except States that have not ratified)
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
                                 !(HUGGO_MEM$stateID %in% c("IRQ", "ARE"))), "stateID"])){
  a <- HUGGO_MEM[which(HUGGO_MEM$manyID == "PE09EP_1990P" &
                         !(HUGGO_MEM$stateID %in% c("IRQ", "ARE"))), "stateID"]
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
  
# INTCPE_1991P:INTCPE_1990A
# Remove extra rows
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                                HUGGO_MEM$treatyID == "INTCPE_1991P"),]
# European Union
# According to Treaty of Lisbon, the European Union is to be deemed contracting
# party to all treaties the European Community was part of
euu <- which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" & 
               HUGGO_MEM$stateID == "EUU")
HUGGO_MEM <- HUGGO_MEM[-euu[-1],]
# Remove Czechoslovakia
csk <- which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" &
               HUGGO_MEM$stateID == "CSK")
HUGGO_MEM <- HUGGO_MEM[-(csk),]
# Slovakia
HUGGO_MEM[which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" &
                                HUGGO_MEM$stateID == "SVK"), "stateSignature"] <- messydates::as_messydate("1991-12-09")
HUGGO_MEM[which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "SVK"), "stateRat"] <- messydates::as_messydate("1992-10-08")
HUGGO_MEM[which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "SVK"), "Changes_HUGGO"] <-
  "stateSignature, stateRat, Succession"
HUGGO_MEM[which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "SVK"), "Succession"] <-
  messydates::as_messydate("1993-01-01")
HUGGO_MEM[which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "SVK"), "Verified_HUGGO"] <-
  "stateSignature, stateRat, stateForce, stateWithdrawal, Accession, Succession"
# Czech Republic
HUGGO_MEM[which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "CZE"), "stateSignature"] <- messydates::as_messydate("1991-12-09")
HUGGO_MEM[which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "CZE"), "stateRat"] <- messydates::as_messydate("1992-10-08")
HUGGO_MEM[which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "CZE"), "stateWithdrawal"] <- NA 
HUGGO_MEM[which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "CZE"), "Changes_HUGGO"] <-
  "stateSignature, stateRat, Succession"
HUGGO_MEM[which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "CZE"), "Succession"] <-
  messydates::as_messydate("1993-01-01")
HUGGO_MEM[which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "CZE"), "Verified_HUGGO"] <-
  "stateSignature, stateRat, stateForce, stateWithdrawal, Accession, Succession"
# Germany
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" &
                               HUGGO_MEM$stateID == "DEU" & HUGGO_MEM$stateSignature != "1991-12-09"),]
HUGGO_MEM[which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" &
                   HUGGO_MEM$stateID == "DEU"), "stateWithdrawal"] <- NA
HUGGO_MEM[which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "DEU"), "Verified_HUGGO"] <-
  "stateSignature, stateRat, stateForce, stateWithdrawal, Accession, Succession"
HUGGO_MEM[which(HUGGO_MEM$treatyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "DEU"), "Changes_HUGGO"] <-
  "stateWithdrawal"
# European Union
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A" &
                  HUGGO_MEM$stateID == "EUU"), "Verified_HUGGO"] <-
  "stateSignature, stateRat, stateForce, stateWithdrawal, Accession, Succession"
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A"), "Checked_HUGGO"] <- 1
HUGGO_MEM[which(HUGGO_MEM$manyID == "INTCPE_1991P:INTCPE_1990A"), "End"] <- NA

# SIASFO_1999A
# Remove duplicate rows with no information for stateSignature
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "SIASFO_1999A" &
                                HUGGO_MEM$stateSignature != "1999-04-09"),]
# Add NAs to stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "SIASFO_1999A"), 10] <- NA

# MRT-SEN[SCW]_1978A
# Remove duplicate rows with no stateSignature
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "MRT-SEN[SCW]_1978A" &
                                HUGGO_MEM$stateSignature != "1978-12-21"),]
# Add stateRat Mali (https://sgg-mali.ml/JO/1979/mali-jo-1979-573.pdf)
HUGGO_MEM[which(HUGGO_MEM$manyID == "MRT-SEN[SCW]_1978A" & HUGGO_MEM$stateID == "MLI"),
          "stateRat"] <- messydates::as_messydate("1979-05-17")
# Add stateRat Mauritania (http://anac.mr/ANAC/JOf/1979/500-501%20fr%20sc.pdf)
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

## Agreement Concerning Cooperation In Taking Measures
## CTMPSO_1971A
## Remove duplicate rows
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "CTMPSO_1971A" & HUGGO_MEM$stateSignature !=
                                messydates::as_messydate("1971-09-16")),]
## Add stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "CTMPSO_1971A"), "stateRat"] <- 
  messydates::as_messydate("1971-09-16")
## Add verification
HUGGO_MEM[which(HUGGO_MEM$manyID == "CTMPSO_1971A"), "Changes_HUGGO"] <- "stateRat"
HUGGO_MEM[which(HUGGO_MEM$manyID == "CTMPSO_1971A"), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "CTMPSO_1971A"), "Checked_HUGGO"] <- 1

# Convention on Nature Protection and Wild Life Preservation in the Western Hemisphere
# NPWPWH_1940A
# Correct
HUGGO_MEM[which(HUGGO_MEM$manyID == "NPWPWH_1940A"), "Checked_HUGGO"] <- 1
HUGGO_MEM[which(HUGGO_MEM$manyID == "NPWPWH_1940A"), "stateWithdrawal"] <-
  messydates::as_messydate("9999-12-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "NPWPWH_1940A"), "Changes_HUGGO"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "NPWPWH_1940A"), "Verified_HUGGO"] <-
  "stateSignature, stateRat, stateForce, stateWithdrawal, Accession, Acceptance, Succession"

# Utilization of waters of the Colorado and Tijuana Rivers and of the
# Rio Grande
# MEX-USA[RRG]_1944A
# Mexico ratification
HUGGO_MEM[which(HUGGO_MEM$manyID == "MEX-USA[RRG]_1944A" & HUGGO_MEM$stateID == "MEX"), "stateRat"] <-
  messydates::as_messydate("1945-09-29")
# USA ratification
HUGGO_MEM[which(HUGGO_MEM$manyID == "MEX-USA[RRG]_1944A" & HUGGO_MEM$stateID == "USA"), "stateRat"] <-
  messydates::as_messydate("1945-04-18")
# Correct stateWithdrawal
# USA
HUGGO_MEM[which(HUGGO_MEM$manyID == "MEX-USA[RRG]_1944A" & HUGGO_MEM$stateID == "USA"), "stateWithdrawal"] <-
  messydates::as_messydate("9999-12-31")
# MEX
HUGGO_MEM[which(HUGGO_MEM$manyID == "MEX-USA[RRG]_1944A" & HUGGO_MEM$stateID == "MEX"), "stateWithdrawal"] <-
  messydates::as_messydate("9999-12-31")
# Add verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "MEX-USA[RRG]_1944A"), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "MEX-USA[RRG]_1944A"), 'Checked_HUGGO'] <- 1
HUGGO_MEM[which(HUGGO_MEM$manyID == "MEX-USA[RRG]_1944A"), "Changes_HUGGO"] <- "stateRat, stateWithdrawal"

# Convention Concerning The Regime Of Navigation On The Danube
# RGMNVD_1948A
HUGGO_MEM[which(HUGGO_MEM$manyID == "RGMNVD_1948A"), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "RGMNVD_1948A"), 'Checked_HUGGO'] <- 1
HUGGO_MEM[which(HUGGO_MEM$manyID == "RGMNVD_1948A"), "Changes_HUGGO"] <- NA

# Agreement On Reciprocal Access To Fishing In The Skagerrak And The Kattegat
# RCAFSK_1966A
# Correct stateSignature: all three 19 december 1966
# stateSignature
HUGGO_MEM[which(HUGGO_MEM$manyID == "RCAFSK_1966A"), "stateSignature"] <-
  messydates::as_messydate("1966-12-19")
# Change End
HUGGO_MEM[which(HUGGO_MEM$manyID == "RCAFSK_1966A"), "End"] <- messydates::as_messydate("2012-08-07")
# Verification 
HUGGO_MEM[which(HUGGO_MEM$manyID == "RCAFSK_1966A" & HUGGO_MEM$stateID != "NOR"), "Changes_HUGGO"] <-
  "End, stateSignature"
HUGGO_MEM[which(HUGGO_MEM$manyID == "RCAFSK_1966A"), "Verified_HUGGO"] <-
  "stateSignature, stateRat, stateForce, stateWithdrawal, Accession, Acceptance, Succession"
HUGGO_MEM[which(HUGGO_MEM$manyID == "RCAFSK_1966A" & HUGGO_MEM$stateID == "NOR"), "Changes_HUGGO"] <-
  "End, stateSignature"
HUGGO_MEM[which(HUGGO_MEM$manyID == "RCAFSK_1966A"), "Checked_HUGGO"] <- 1

# PRTLCP_1960A
# Remove duplicates
id <- "PRTLCP_1960A"
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == id & HUGGO_MEM$stateID == "AUT" & HUGGO_MEM$stateSignature != "1960-10-27"),]
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == id & HUGGO_MEM$stateID == "CHE" & HUGGO_MEM$stateSignature != "1960-10-27"),]
# stateSignature
HUGGO_MEM[which(HUGGO_MEM$manyID == id), "stateSignature"] <- messydates::as_messydate("1960-10-27")
# Add changes
HUGGO_MEM[which(HUGGO_MEM$manyID == id & HUGGO_MEM$stateID == "Baden-Württemberg"), "Changes_HUGGO"] <- "stateSignature"
HUGGO_MEM[which(HUGGO_MEM$manyID == id & HUGGO_MEM$stateID == "Bayern"), "Changes_HUGGO"] <- "stateSignature"
# Add Checked
HUGGO_MEM[which(HUGGO_MEM$manyID == id & HUGGO_MEM$stateID != "DEU"), "Checked_HUGGO"] <- 1
# Add Verified
HUGGO_MEM[which(HUGGO_MEM$manyID == id & HUGGO_MEM$stateID == "AUT"), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == id & HUGGO_MEM$stateID == "CHE"), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == id & HUGGO_MEM$stateID == "Baden-Württemberg"), "Verified_HUGGO"] <- "stateSignature, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == id & HUGGO_MEM$stateID == "Bayern"), "Verified_HUGGO"] <- "stateSignature, stateForce"

# NM04RA_1963A
id <- "NM04RA_1963A"
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == id & HUGGO_MEM$stateID == "DNK" & is.na(HUGGO_MEM$stateRat)), ]
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == id & HUGGO_MEM$stateID == "FIN" & HUGGO_MEM$stateRat != "1965-06-16"), ]
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == id & HUGGO_MEM$stateID == "SWE" & HUGGO_MEM$stateRat != "1963-10-17"), ]
HUGGO_MEM[which(HUGGO_MEM$manyID == id & HUGGO_MEM$stateID == "IO-IAEA" & HUGGO_MEM$stateRat != "1963-10-17"), "stateRat"] <- messydates::as_messydate("1963-10-17")
HUGGO_MEM[which(HUGGO_MEM$manyID == id & HUGGO_MEM$stateID == "IO-IAEA" & HUGGO_MEM$stateRat != "1963-10-17"), "stateSignature"] <- messydates::as_messydate("1963-10-17")
HUGGO_MEM[which(HUGGO_MEM$manyID == id & HUGGO_MEM$stateID == "IO-IAEA" & HUGGO_MEM$stateRat != "1963-10-17"), "Changes_HUGGO"] <- "stateSignature, stateRat"  
HUGGO_MEM[which(HUGGO_MEM$manyID == id & HUGGO_MEM$stateID == "SWE" & HUGGO_MEM$stateRat != "1963-10-17"), "stateSignature"] <- messydates::as_messydate("1963-10-17")
HUGGO_MEM[which(HUGGO_MEM$manyID == id & HUGGO_MEM$stateID == "IO-IAEA" & HUGGO_MEM$stateRat != "1963-10-17"), "Changes_HUGGO"] <- "stateSignature"
HUGGO_MEM[which(HUGGO_MEM$manyID == id), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == id), "Checked_HUGGO"] <- 1
HUGGO_MEM[which(HUGGO_MEM$manyID == id), "stateWithdrawal"] <- messydates::as_messydate("9999-12-31")

# BNLHPB_1977P:BNLHPB_1970A
HUGGO_MEM[which(HUGGO_MEM$manyID == "BNLHPB_1977P:BNLHPB_1970A"), "Checked_HUGGO"] <- 1
HUGGO_MEM[which(HUGGO_MEM$manyID == "BNLHPB_1977P:BNLHPB_1970A"), "stateWithdrawal"] <- messydates::as_messydate("9999-12-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "BNLHPB_1977P:BNLHPB_1970A"), "Changes_HUGGO"] <- "stateWithdrawal" 
HUGGO_MEM[which(HUGGO_MEM$manyID == "BNLHPB_1977P:BNLHPB_1970A"), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce, stateWithdrawal, Accession, Acceptance, Succession"

# AI08ZP_1990A
# Remove duplicates
treaty <- HUGGO_MEM %>%
  dplyr::filter(manyID == "AI08ZP_1990A")
ids <- unique(treaty$stateID)
## Remove duplicates
for (i in 1:length(ids)){
  id <- ids[i]
  rows <- which(HUGGO_MEM$stateID == id & HUGGO_MEM$manyID == "AI08ZP_1990A")
  if (length(rows) > 1){
    HUGGO_MEM <- HUGGO_MEM[-(rows[-1]), ]
  }
}
# stateRat (unknown)
HUGGO_MEM[which(HUGGO_MEM$manyID == "AI08ZP_1990A"), "stateRat"] <- NA
# stateSignature
HUGGO_MEM[which(HUGGO_MEM$manyID == "AI08ZP_1990A"), "stateSignature"] <- messydates::as_messydate("1990-09-19")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AI08ZP_1990A" & HUGGO_MEM$stateID == "TUV"), "stateSignature"] <- messydates::as_messydate("1991-05-15")
# stateWithdrawal
HUGGO_MEM[which(HUGGO_MEM$manyID == "AI08ZP_1990A"), "stateWithdrawal"] <- messydates::as_messydate("9999-12-31")
# Changes HUGGO
HUGGO_MEM[which(HUGGO_MEM$manyID == "AI08ZP_1990A"), "Changes_HUGGO"] <- "stateSignature, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AI08ZP_1990A"), "Verified_HUGGO"] <- "stateSignature, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AI08ZP_1990A"), "Checked_HUGGO"] <- 1

# GU13US_1975A (30 05 1975)
HUGGO_MEM[which(HUGGO_MEM$manyID == "GU13US_1975A" & HUGGO_MEM$stateSignature == "1975-05-30"), "stateRat"] <- messydates::as_messydate("1975-05-30")
HUGGO_MEM[which(HUGGO_MEM$manyID == "GU13US_1975A" & HUGGO_MEM$stateSignature == "1975-05-30"), "stateForce2"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "GU13US_1975A" & HUGGO_MEM$stateSignature == "1975-05-30"), "Changes_HUGGO"] <- "stateRat, stateForce2"
HUGGO_MEM[which(HUGGO_MEM$manyID == "GU13US_1975A" & HUGGO_MEM$stateSignature == "1975-05-30"), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "GU13US_1975A" & HUGGO_MEM$stateSignature == "1975-05-30"), "Checked_HUGGO"] <- 1
## GU13US_1975A 1975-12-16
HUGGO_MEM[which(HUGGO_MEM$manyID == "GU13US_1975A" & HUGGO_MEM$stateSignature == "1975-12-16"), "stateRat"] <- messydates::as_messydate("1975-12-16")
HUGGO_MEM[which(HUGGO_MEM$manyID == "GU13US_1975A" & HUGGO_MEM$stateSignature == "1975-12-16"), "stateForce2"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "GU13US_1975A" & HUGGO_MEM$stateSignature == "1975-12-16"), "Changes_HUGGO"] <- "stateRat, stateForce2"
HUGGO_MEM[which(HUGGO_MEM$manyID == "GU13US_1975A" & HUGGO_MEM$stateSignature == "1975-12-16"), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "GU13US_1975A" & HUGGO_MEM$stateSignature == "1975-12-16"), "Checked_HUGGO"] <- 1

# CPRTNF_1976A
# Remove duplicate rows with incorrect dates
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "CPRTNF_1976A" & HUGGO_MEM$stateSignature == "1981-12-07"),]
# Only stateSignature is verified
HUGGO_MEM[which(HUGGO_MEM$manyID == "CPRTNF_1976A"), "Verified_HUGGO"] <- "stateSignature"
HUGGO_MEM[which(HUGGO_MEM$manyID == "CPRTNF_1976A"), "Checked_HUGGO"] <- 1

# CHE-DEU[RDR]_1978A
# Change stateSignature
HUGGO_MEM[which(HUGGO_MEM$manyID == "CHE-DEU[RDR]_1978A"), "stateSignature"] <- messydates::as_messydate("1978-05-31")
# Could not find source for stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "CHE-DEU[RDR]_1978A"), "Changes_HUGGO"] <- "stateSignature"
HUGGO_MEM[which(HUGGO_MEM$manyID == "CHE-DEU[RDR]_1978A"), "Verified_HUGGO"] <- "stateSignature, stateForce, stateWithdrawal, Accession, Acceptance, Succession"
HUGGO_MEM[which(HUGGO_MEM$manyID =="CHE-DEU[RDR]_1978A"), "Checked_HUGGO"] <- 1

# DEU-MEX[FPT]_1974A
# In https://treaties.un.org/doc/Publication/UNTS/Volume%201360/volume-1360-I-22991-Other.pdf
# In https://sre.gob.mx/images/stories/docnormateca/manadmin/2012/8modge.pdf
# Change stateSignature (Agreement by exchange of notes)
HUGGO_MEM[which(HUGGO_MEM$manyID == "DEU-MEX[FPT]_1974A"), "stateSignature"] <- messydates::as_messydate("1974-01-16")
# Change stateForce
HUGGO_MEM[which(HUGGO_MEM$manyID == "DEU-MEX[FPT]_1974A"), "stateForce"] <- messydates::as_messydate("1974-01-16")
# Verification variables
HUGGO_MEM[which(HUGGO_MEM$manyID == "DEU-MEX[FPT]_1974A"), "Changes_HUGGO"] <- "stateSignature, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "DEU-MEX[FPT]_1974A"), "Verified_HUGGO"] <- "stateSignature, stateForce, Accession, Succession"
HUGGO_MEM[which(HUGGO_MEM$manyID == "DEU-MEX[FPT]_1974A"), "Checked_HUGGO"] <- 1
# Could not determine if it is still in force

# AUS-JPN[LLF]_1979P
# Correct manyID and treatyID
# AUS-JPN[LLF]_1979P
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1979P" & HUGGO_MEM$stateSignature == "1980-10-30"), "manyID"] <- "AUS-JPN[LLF]_1980P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1980P" & HUGGO_MEM$stateSignature == "1980-10-30"), "treatyID"] <- "AUS-JPN[LLF]_1980P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1980P"), "Begin"] <- messydates::as_messydate("1980-10-30")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1980P"), "Signature"] <- messydates::as_messydate("1980-10-30")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1980P"), "End"] <- messydates::as_messydate("1981-10-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1980P"), "Force"] <- messydates::as_messydate("1980-11-01")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1980P"), "ieaID"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1980P"), "Changes_HUGGO"] <- "manyID, treatyID, Begin, Signature, End, Force, ieaID"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1980P"), "Verified_HUGGO"] <- "Begin, Signature, End, Force, stateSignature, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1980P"), "Checked_HUGGO"] <- 1
# AUS-JPN[LLF]_1981P
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1979P" & HUGGO_MEM$stateSignature == "1981-10-29"), "manyID"] <- "AUS-JPN[LLF]_1981P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1981P" & HUGGO_MEM$stateSignature == "1981-10-29"), "treatyID"] <- "AUS-JPN[LLF]_1981P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1981P"), "Begin"] <- messydates::as_messydate("1981-10-29")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1981P"), "Signature"] <- messydates::as_messydate("1981-10-29")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1981P"), "End"] <- messydates::as_messydate("1982-10-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1981P"), "Force"] <- messydates::as_messydate("1981-11-01")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1981P"), "ieaID"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1981P"), "Changes_HUGGO"] <- "manyID, treatyID, Begin, Signature, End, Force, ieaID"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1981P"), "Verified_HUGGO"] <- "Begin, Signature, End, Force, stateSignature, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1981P"), "Checked_HUGGO"]<- 1
# AUS-JPN[LLF]_1982P
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1979P" & HUGGO_MEM$stateSignature == "1982-10-28"), "manyID"] <- "AUS-JPN[LLF]_1982P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1982P" & HUGGO_MEM$stateSignature == "1982-10-28"), "treatyID"] <- "AUS-JPN[LLF]_1982P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1982P"), "Begin"] <- messydates::as_messydate("1982-10-28")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1982P"), "Signature"] <- messydates::as_messydate("1982-10-28")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1982P"), "End"] <- messydates::as_messydate("1983-10-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1982P"), "Force"] <- messydates::as_messydate("1982-11-01")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1982P"), "ieaID"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1982P"), "Changes_HUGGO"] <- "manyID, treatyID, Begin, Signature, End, Force, ieaID"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1982P"), "Verified_HUGGO"] <- "Begin, Signature, End, Force, stateSignature, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1982P"), "Checked_HUGGO"]<- 1
# AUS-JPN[LLF]_1983P
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1979P" & HUGGO_MEM$stateSignature == "1983-10-31"), "manyID"] <- "AUS-JPN[LLF]_1983P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1983P" & HUGGO_MEM$stateSignature == "1983-10-31"), "treatyID"] <- "AUS-JPN[LLF]_1983P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1983P"), "Begin"] <- messydates::as_messydate("1983-10-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1983P"), "Signature"] <- messydates::as_messydate("1983-10-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1983P"), "End"] <- messydates::as_messydate("1984-10-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1983P"), "Force"] <- messydates::as_messydate("1983-11-01")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1983P"), "ieaID"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1983P"), "Changes_HUGGO"] <- "manyID, treatyID, Begin, Signature, End, Force, ieaID"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1983P"), "Verified_HUGGO"] <- "Begin, Signature, End, Force, stateSignature, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1983P"), "Checked_HUGGO"]<- 1
# AUS-JPN[LLF]_1984P
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1979P" & HUGGO_MEM$stateSignature == "1984-10-30"), "manyID"] <- "AUS-JPN[LLF]_1984P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1984P" & HUGGO_MEM$stateSignature == "1984-10-30"), "treatyID"] <- "AUS-JPN[LLF]_1984P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1984P"), "Begin"] <- messydates::as_messydate("1984-10-30")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1984P"), "Signature"] <- messydates::as_messydate("1984-10-30")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1984P"), "End"] <- messydates::as_messydate("1985-10-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1984P"), "Force"] <- messydates::as_messydate("1984-11-01")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1984P"), "ieaID"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1984P"), "Changes_HUGGO"] <- "manyID, treatyID, Begin, Signature, End, Force, ieaID"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1984P"), "Verified_HUGGO"] <- "Begin, Signature, End, Force, stateSignature, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1984P"), "Checked_HUGGO"]<- 1
# AUS-JPN[LLF]_1985P
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1979P" & HUGGO_MEM$stateSignature == "1985-10-30"), "manyID"] <- "AUS-JPN[LLF]_1985P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1985P" & HUGGO_MEM$stateSignature == "1985-10-30"), "treatyID"] <- "AUS-JPN[LLF]_1985P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1985P"), "Begin"] <- messydates::as_messydate("1985-10-30")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1985P"), "Signature"] <- messydates::as_messydate("1985-10-30")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1985P"), "End"] <- messydates::as_messydate("1986-10-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1985P"), "Force"] <- messydates::as_messydate("1985-11-01")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1985P"), "ieaID"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1985P"), "Changes_HUGGO"] <- "manyID, treatyID, Begin, Signature, End, Force, ieaID"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1985P"), "Verified_HUGGO"] <- "Begin, Signature, End, Force, stateSignature, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1985P"), "Checked_HUGGO"]<- 1
# AUS-JPN[LLF]_1986P
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1979P" & HUGGO_MEM$stateSignature == "1986-10-30"), "manyID"] <- "AUS-JPN[LLF]_1986P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1986P" & HUGGO_MEM$stateSignature == "1986-10-30"), "treatyID"] <- "AUS-JPN[LLF]_1986P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1986P"), "Begin"] <- messydates::as_messydate("1986-10-30")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1986P"), "Signature"] <- messydates::as_messydate("1986-10-30")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1986P"), "End"] <- messydates::as_messydate("1987-10-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1986P"), "Force"] <- messydates::as_messydate("1986-11-01")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1986P"), "ieaID"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1986P"), "Changes_HUGGO"] <- "manyID, treatyID, Begin, Signature, End, Force, ieaID"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1986P"), "Verified_HUGGO"] <- "Begin, Signature, End, Force, stateSignature, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1986P"), "Checked_HUGGO"]<- 1
# AUS-JPN[LLF]_1987P
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1979P" & HUGGO_MEM$stateSignature == "1987-10-29"), "manyID"] <- "AUS-JPN[LLF]_1987P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1987P" & HUGGO_MEM$stateSignature == "1987-10-29"), "treatyID"] <- "AUS-JPN[LLF]_1987P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1987P"), "Begin"] <- messydates::as_messydate("1987-10-29")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1987P"), "Signature"] <- messydates::as_messydate("1987-10-29")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1987P"), "End"] <- messydates::as_messydate("1988-10-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1987P"), "Force"] <- messydates::as_messydate("1987-11-01")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1987P"), "ieaID"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1987P"), "Changes_HUGGO"] <- "manyID, treatyID, Begin, Signature, End, Force, ieaID"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1987P"), "Verified_HUGGO"] <- "Begin, Signature, End, Force, stateSignature, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1987P"), "Checked_HUGGO"]<- 1
# AUS-JPN[LLF]_1988P
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1979P" & HUGGO_MEM$stateSignature == "1988-10-27"), "manyID"] <- "AUS-JPN[LLF]_1988P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1988P" & HUGGO_MEM$stateSignature == "1988-10-27"), "treatyID"] <- "AUS-JPN[LLF]_1988P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1988P"), "Begin"] <- messydates::as_messydate("1988-10-27")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1988P"), "Signature"] <- messydates::as_messydate("1988-10-27")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1988P"), "End"] <- messydates::as_messydate("1989-10-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1988P"), "Force"] <- messydates::as_messydate("1988-11-01")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1988P"), "ieaID"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1988P"), "Changes_HUGGO"] <- "manyID, treatyID, Begin, Signature, End, Force, ieaID"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1988P"), "Verified_HUGGO"] <- "Begin, Signature, End, Force, stateSignature, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1988P"), "Checked_HUGGO"]<- 1
# AUS-JPN[LLF]_1989P
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1979P" & HUGGO_MEM$stateSignature == "1989-12-15"), "manyID"] <- "AUS-JPN[LLF]_1989P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1989P" & HUGGO_MEM$stateSignature == "1989-12-15"), "treatyID"] <- "AUS-JPN[LLF]_1989P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1989P"), "Begin"] <- messydates::as_messydate("1989-12-15")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1989P"), "Signature"] <- messydates::as_messydate("1989-12-15")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1989P"), "End"] <- messydates::as_messydate("1990-10-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1989P"), "Force"] <- messydates::as_messydate("1989-11-01")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1989P"), "ieaID"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1989P"), "Changes_HUGGO"] <- "manyID, treatyID, Begin, Signature, End, Force, ieaID"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1989P"), "Verified_HUGGO"] <- "Begin, Signature, End, Force, stateSignature, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1989P"), "Checked_HUGGO"]<- 1
# AUS-JPN[LLF]_1990P
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1979P" & HUGGO_MEM$stateSignature == "1990-11-30"), "manyID"] <- "AUS-JPN[LLF]_1990P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1990P" & HUGGO_MEM$stateSignature == "1990-11-30"), "treatyID"] <- "AUS-JPN[LLF]_1990P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1990P"), "Begin"] <- messydates::as_messydate("1990-11-30")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1990P"), "Signature"] <- messydates::as_messydate("1990-11-30")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1990P"), "End"] <- messydates::as_messydate("1991-10-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1990P"), "Force"] <- messydates::as_messydate("1990-11-30")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1990P"), "ieaID"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1990P"), "Changes_HUGGO"] <- "manyID, treatyID, Begin, Signature, End, Force, ieaID"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1990P"), "Verified_HUGGO"] <- "Begin, Signature, End, Force, stateSignature, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1990P"), "Checked_HUGGO"]<- 1
# Remove repeated (with different stateSignature) 1990 agreement
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1979P" & HUGGO_MEM$stateSignature == "1990-12-12"),]
# AUS-JPN[LLF]_1991P
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1979P" & HUGGO_MEM$stateSignature == "1991-12-10"), "manyID"] <- "AUS-JPN[LLF]_1991P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1991P" & HUGGO_MEM$stateSignature == "1991-12-10"), "treatyID"] <- "AUS-JPN[LLF]_1991P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1991P"), "Begin"] <- messydates::as_messydate("1991-12-10")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1991P"), "Signature"] <- messydates::as_messydate("1991-12-10")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1991P"), "End"] <- messydates::as_messydate("1992-10-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1991P"), "Force"] <- messydates::as_messydate("1991-12-10")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1991P"), "ieaID"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1991P"), "Changes_HUGGO"] <- "manyID, treatyID, Begin, Signature, End, Force, ieaID"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1991P"), "Verified_HUGGO"] <- "Begin, Signature, End, Force, stateSignature, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1991P"), "Checked_HUGGO"]<- 1
# AUS-JPN[LLF]_1992P
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1979P" & HUGGO_MEM$stateSignature == "1992-12-21"), "manyID"] <- "AUS-JPN[LLF]_1992P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1992P" & HUGGO_MEM$stateSignature == "1992-12-21"), "treatyID"] <- "AUS-JPN[LLF]_1992P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1992P"), "Begin"] <- messydates::as_messydate("1992-12-21")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1992P"), "Signature"] <- messydates::as_messydate("1992-12-21")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1992P"), "End"] <- messydates::as_messydate("1992-10-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1992P"), "Force"] <- messydates::as_messydate("1992-12-21")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1992P"), "ieaID"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1992P"), "Changes_HUGGO"] <- "manyID, treatyID, Begin, Signature, End, Force, ieaID"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1992P"), "Verified_HUGGO"] <- "Begin, Signature, End, Force, stateSignature, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1992P"), "Checked_HUGGO"]<- 1
# AUS-JPN[LLF]_1994P
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1991P" & HUGGO_MEM$stateSignature == "1994-12-21"), "manyID"] <- "AUS-JPN[LLF]_1994P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1994P" & HUGGO_MEM$stateSignature == "1994-12-21"), "treatyID"] <- "AUS-JPN[LLF]_1994P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1994P"), "Begin"] <- messydates::as_messydate("1994-12-21")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1994P"), "Signature"] <- messydates::as_messydate("1994-12-21")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1994P"), "End"] <- messydates::as_messydate("1995-10-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1994P"), "Force"] <- messydates::as_messydate("1994-12-21")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1994P"), "ieaID"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1994P"), "Changes_HUGGO"] <- "manyID, treatyID, Begin, Signature, End, Force, ieaID"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1994P"), "Verified_HUGGO"] <- "Begin, Signature, End, Force, stateSignature, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1994P"), "Checked_HUGGO"]<- 1
# AUS-JPN[LLF]_1996P
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1979P" & HUGGO_MEM$stateSignature == "1996-06-04"), "manyID"] <- "AUS-JPN[LLF]_1996P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1996P" & HUGGO_MEM$stateSignature == "1996-06-04"), "treatyID"] <- "AUS-JPN[LLF]_1996P"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1996P"), "Begin"] <- messydates::as_messydate("1996-06-04")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1996P"), "Signature"] <- messydates::as_messydate("1996-06-04")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1996P"), "End"] <- messydates::as_messydate("1996-10-31")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1996P"), "Force"] <- messydates::as_messydate("1996-06-04")
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1996P"), "ieaID"] <- NA
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1996P"), "Changes_HUGGO"] <- "manyID, treatyID, Begin, Signature, End, Force, ieaID"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1996P"), "Verified_HUGGO"] <- "Begin, Signature, End, Force, stateSignature, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "AUS-JPN[LLF]_1996P"), "Checked_HUGGO"]<- 1

# CAN-USA[NTS]_2002N4
# Dates correct
# Verify
HUGGO_MEM[which(HUGGO_MEM$manyID == "CAN-USA[NTS]_2002N4"), "Checked_HUGGO"] <- 1
HUGGO_MEM[which(HUGGO_MEM$manyID == "CAN-USA[NTS]_2002N4"), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce, stateWithdrawal, Accession, Succession"

# CAN-USA[NTS]_2005N4
# Change signature dates (also in HUGGO)
HUGGO_MEM[which(HUGGO_MEM$manyID == "CAN-USA[NTS]_2005N4" & HUGGO_MEM$stateID == "USA"), "stateSignature"] <- messydates::as_messydate("2005-04-26")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CAN-USA[NTS]_2005N4" & HUGGO_MEM$stateID == "CAN"), "stateSignature"] <- messydates::as_messydate("2005-06-17")
# Changes
HUGGO_MEM[which(HUGGO_MEM$manyID == "CAN-USA[NTS]_2002N4"), "Changes_HUGGO"] <- "stateSignature"
# Verify
HUGGO_MEM[which(HUGGO_MEM$manyID == "CAN-USA[NTS]_2002N4"), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce, stateWithdrawal, Accession, Succession"
HUGGO_MEM[which(HUGGO_MEM$manyID == "CAN-USA[NTS]_2002N4"), "Checked_HUGGO"] <- 1

# SEN-EC[CFP]_2002A
# Correct
HUGGO_MEM[which(HUGGO_MEM$manyID == "SEN-EC[CFP]_2002A"), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce, stateWithdrawal, Accession, Acceptance, Succession"

# DOM-USA[CFT]_2005A
# Dates correct, no source for stateRat
# Unclear if still in force
HUGGO_MEM[which(HUGGO_MEM$manyID == "DOM-USA[CFT]_2005A"), "Verified_HUGGO"] <- "stateSignature, stateForce, Accession, Succession"
HUGGO_MEM[which(HUGGO_MEM$manyID == "DOM-USA[CFT]_2005A"), "Checked_HUGGO"] <- 1

# CAN-USA[PAC]_1973A
# Change stateSignature
HUGGO_MEM[which(HUGGO_MEM$manyID == "CAN-USA[PAC]_1973A"), "stateSignature"] <- messydates::as_messydate("1973-06-15")
# Verification
HUGGO_MEM[which(HUGGO_MEM$manyID == "CAN-USA[PAC]_1973A"), "Changes_HUGGO"] <- "stateSignature"
HUGGO_MEM[which(HUGGO_MEM$manyID == "CAN-USA[PAC]_1973A"), "Verified_HUGGO"] <- "Begin, Signature, stateSignature, stateRat, stateForce, stateWithdrawal, Accession, Succession"
HUGGO_MEM[which(HUGGO_MEM$manyID == "CAN-USA[PAC]_1973A"), "Checked_HUGGO"] <- 1

# RUS-USA[HSA]_1975A
# Main agreement
# Change End
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[HSA]_1975A" & HUGGO_MEM$Begin == "1975-02-26"), "End"] <- messydates::as_messydate("1976-02-29")
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[HSA]_1975A" & HUGGO_MEM$Begin == "1975-02-26"), "stateWithdrawal"] <- messydates::as_messydate("1976-02-29")
# Add stateForce
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[HSA]_1975A" & HUGGO_MEM$Begin == "1975-02-26"), "stateForce"] <- messydates::as_messydate("1975-02-26")
# No source for stateRat
# Verification
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[HSA]_1975A" & HUGGO_MEM$Begin == "1975-02-26"), "Changes_HUGGO"] <- "End, stateWithdrawal, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[HSA]_1975A" & HUGGO_MEM$Begin == "1975-02-26"), "Verified_HUGGO"] <- "stateSignature, stateForce, stateWithdrawal, Accession, Succession"
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[HSA]_1975A" & HUGGO_MEM$Begin == "1975-02-26"), "Checked_HUGGO"] <- 1
# Extension
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[HSA]_1975A" & HUGGO_MEM$gengID == "GENG-1248" &
                  HUGGO_MEM$stateID == "USA"), "stateSignature"] <- messydates::as_messydate("1975-12-18")
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[HSA]_1975A" & HUGGO_MEM$gengID == "GENG-1248" &
                  HUGGO_MEM$stateID == "RUS"), "stateSignature"] <- messydates::as_messydate("1975-12-30")
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[HSA]_1975A" & HUGGO_MEM$gengID == "GENG-1248"), "End"] <- messydates::as_messydate("1976-02-29")
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[HSA]_1975A" & HUGGO_MEM$gengID == "GENG-1248"), "stateWithdrawal"] <- messydates::as_messydate("1976-02-29")
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[HSA]_1975A" & HUGGO_MEM$gengID == "GENG-1248"), "stateForce"] <- messydates::as_messydate("1975-12-30")
# Verification
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[HSA]_1975A" & HUGGO_MEM$gengID == "GENG-1248"), "Changes_HUGGO"] <- "End, stateSignature, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[HSA]_1975A" & HUGGO_MEM$gengID == "GENG-1248"), "Verified_HUGGO"] <- "stateSignature, stateForce, stateWithdrawal, Accession, Succession"
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[HSA]_1975A" & HUGGO_MEM$gengID == "GENG-1248"), "Checked_HUGGO"] <- 1

# RUS-USA[KTC]_1975N:RUS-USA[KTC]_1971A
# June 1975 extension
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[KTC]_1975N:RUS-USA[KTC]_1971A" & HUGGO_MEM$Begin == "1975-06-30"), "stateForce"] <- messydates::as_messydate("1975-06-30")
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[KTC]_1975N:RUS-USA[KTC]_1971A" & HUGGO_MEM$Begin == "1975-06-30"), "Changes_HUGGO"] <- "stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[KTC]_1975N:RUS-USA[KTC]_1971A" & HUGGO_MEM$Begin == "1975-06-30"), "Verified_HUGGO"] <- "stateSignature, stateForce, stateWithdrawal, Accession, Succession"
# February 1975 extension
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[KTC]_1975N:RUS-USA[KTC]_1971A" & HUGGO_MEM$Begin == "1975-02-26"), "stateForce"] <- messydates::as_messydate("1975-02-26")
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[KTC]_1975N:RUS-USA[KTC]_1971A" & HUGGO_MEM$Begin == "1975-02-26"), "stateWithdrawal"] <- messydates::as_messydate("1975-06-30")
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[KTC]_1975N:RUS-USA[KTC]_1971A" & HUGGO_MEM$Begin == "1975-02-26"), "Changes_HUGGO"] <- "stateWithdrawal, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "RUS-USA[KTC]_1975N:RUS-USA[KTC]_1971A" & HUGGO_MEM$Begin == "1975-02-26"), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce, stateWithdrawal, Accession, Succession"

# ESP-PRT[FSF]_1969A
# Remove entry with incorrect dates
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "ESP-PRT[FSF]_1969A" & HUGGO_MEM$stateSignature != "1969-12-09"),]
# Add Portugal stateRat (https://gddc.ministeriopublico.pt/sites/default/files/documentos/instrumentos/dl197-1970.pdf) (no source for Spanish)
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESP-PRT[FSF]_1969A" & HUGGO_MEM$stateID == "PRT"), "stateRat"] <- messydates::as_messydate("1970-04-17")
# End and stateWithdrawal unclear. Original 20 year term but subject to renewal.
# Verification
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESP-PRT[FSF]_1969A" & HUGGO_MEM$stateID == "PRT"), "Changes_HUGGO"] <- "stateRat"
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESP-PRT[FSF]_1969A" & HUGGO_MEM$stateID == "PRT"), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce, Accession, Succession"
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESP-PRT[FSF]_1969A" & HUGGO_MEM$stateID == "ESP"), "Verified_HUGGO"] <- "stateSignature, stateForce, Accession, Succession"
HUGGO_MEM[which(HUGGO_MEM$manyID == "ESP-PRT[FSF]_1969A"), "Checked_HUGGO"] <- 1

# FRA-GBR[AEM]_1951A
# Change Begin, Signature, stateSignature
HUGGO_MEM[which(HUGGO_MEM$manyID == "FRA-GBR[AEM]_1951A"), "stateSignature"] <- messydates::as_messydate("1951-01-30")
# No source for stateRat
# Verification
HUGGO_MEM[which(HUGGO_MEM$manyID == "FRA-GBR[AEM]_1951A"), "Changes_HUGGO"] <- "stateSignature"
HUGGO_MEM[which(HUGGO_MEM$manyID == "FRA-GBR[AEM]_1951A"), "Verified_HUGGO"] <- "stateSignature, stateForce, stateWithdrawal, Accession, Succession"
HUGGO_MEM[which(HUGGO_MEM$manyID == "FRA-GBR[AEM]_1951A"), "Checked_HUGGO"] <- 1

# ISL-NOR[FCS]_1980A
# Change Begin, Signature, stateSignature
HUGGO_MEM[which(HUGGO_MEM$manyID == "ISL-NOR[FCS]_1980A"), "stateSignature"] <- messydates::as_messydate("1980-05-28")
# No source for rest of dates
HUGGO_MEM[which(HUGGO_MEM$manyID == "ISL-NOR[FCS]_1980A"), "Changes_HUGGO"] <- "stateSignature"
HUGGO_MEM[which(HUGGO_MEM$manyID == "ISL-NOR[FCS]_1980A"), "Verified_HUGGO"] <- "stateSignature"
HUGGO_MEM[which(HUGGO_MEM$manyID == "ISL-NOR[FCS]_1980A"), "Checked_HUGGO"] <- 1

# GUY-RUS[UNF]_1977A
# Add Force
HUGGO_MEM[which(HUGGO_MEM$manyID == "GUY-RUS[UNF]_1977A" & HUGGO_MEM$stateSignature == "1977-05-17"), "stateForce"] <- messydates::as_messydate("1977-05-17")
# Verification
HUGGO_MEM[which(HUGGO_MEM$manyID == "GUY-RUS[UNF]_1977A" & HUGGO_MEM$stateSignature == "1977-05-17"), "Changes_HUGGO"] <- "stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "GUY-RUS[UNF]_1977A" & HUGGO_MEM$stateSignature == "1977-05-17"), "Verified_HUGGO"] <- "stateSignature, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "GUY-RUS[UNF]_1977A" & HUGGO_MEM$stateSignature == "1977-05-17"), "Checked_HUGGO"] <- 1

# BLR-UKR[ERE]_2001A
# Correct dates (no source for other dates)
HUGGO_MEM[which(HUGGO_MEM$manyID == "BLR-UKR[ERE]_2001A"), "Verified_HUGGO"] <- "stateSignature, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "BLR-UKR[ERE]_2001A"), "Checked_HUGGO"] <- 1

# KIR-EC[FFZ]_2003A
# Add stateRat EUE
HUGGO_MEM[which(HUGGO_MEM$manyID == "KIR-EC[FFZ]_2003A" & HUGGO_MEM$stateID == "EUE"), "stateRat"] <- messydates::as_messydate("2003-05-29")
HUGGO_MEM[which(HUGGO_MEM$manyID == "KIR-EC[FFZ]_2003A" & HUGGO_MEM$stateID == "EUE"), "Changes_HUGGO"] <- "stateRat"
HUGGO_MEM[which(HUGGO_MEM$manyID == "KIR-EC[FFZ]_2003A" & HUGGO_MEM$stateID == "KIR"), "Verified_HUGGO"] <- "stateSignature, stateForce, stateWithdrawal, Accession, Succession"
HUGGO_MEM[which(HUGGO_MEM$manyID == "KIR-EC[FFZ]_2003A" & HUGGO_MEM$stateID == "EUE"), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce, stateWithdrawal, Accession, Succession"
HUGGO_MEM[which(HUGGO_MEM$manyID == "KIR-EC[FFZ]_2003A"), "Checked_HUGGO"] <- 1

#  RG04SP_1955R
# Correct Peru stateRat
HUGGO_MEM[which(HUGGO_MEM$manyID == "RG04SP_1955R" & HUGGO_MEM$stateID == "PER"), "stateRat"] <- messydates::as_messydate("1955-09-16")
# Verification
HUGGO_MEM[which(HUGGO_MEM$manyID == "RG04SP_1955R" & HUGGO_MEM$stateID == "PER"), "Changes_HUGGO"] <- "stateRat"
HUGGO_MEM[which(HUGGO_MEM$manyID == "RG04SP_1955R"), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce"
HUGGO_MEM[which(HUGGO_MEM$manyID == "RG04SP_1955R"), "Checked_HUGGO"] <- 1 

# ENFFEC_1923A
HUGGO_MEM[which(HUGGO_MEM$manyID == "ENFFEC_1923A"), "stateSignature"] <- messydates::as_messydate("1923-09-29")
HUGGO_MEM[which(HUGGO_MEM$manyID == "ENFFEC_1923A"), "Changes_HUGGO"] <- "stateSignature"
HUGGO_MEM[which(HUGGO_MEM$manyID == "ENFFEC_1923A"), "Verified_HUGGO"] <- "stateSignature, stateRat, stateForce, stateWithdrawal"
HUGGO_MEM[which(HUGGO_MEM$manyID == "ENFFEC_1923A"), "Checked_HUGGO"] <- 1

# After running scripts to verify dates from files, web, and ecolex
HUGGO_MEM_verified <- dplyr::filter(HUGGO_MEM, Checked_HUGGO == 1)

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
