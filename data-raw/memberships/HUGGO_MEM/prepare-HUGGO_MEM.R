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

# Remove duplicates
# Step one: keep one row for each combination of manyID, treatyID and Title
members <- manyenviron::memberships$HUGGO_MEM %>%
  distinct(manyID, treatyID, Title, Beg, Signature, Force, .keep_all = TRUE)

# Step two: determine which rows have a duplicated manyID
duplicate_manyID <- data.frame(table(members$manyID))
duplicate_manyID <- duplicate_manyID[duplicate_manyID$Freq > 1,]
duplicate_manyID <- members[members$manyID %in% duplicate_manyID$Var1[duplicate_manyID$Freq > 1],]

# Step three: remove fully duplicate rows
HUGGO_MEM <- manyenviron::memberships$HUGGO_MEM

HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "BI07WA_1991A" &
                                HUGGO_MEM$Title == "Bamako Convention On The Ban Of The Import Into Africa And The Control Of Transboundary Movement And Management Of Hazardous Wastes Within Africa"),]
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "ERDWCP_1968A" &
                                HUGGO_MEM$Title == "European Agreement On The Restriction Of The Use Of Certain Detergents In Washing And Cleaning Products (CETS No 064)"),]
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "ESAPFC_1948A" &
                                HUGGO_MEM$Title == "Agreement For The Establishment Of The Asia Pacific Fisheries Council"),]
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "EST-RUS[RFF]_1994A" &
                                HUGGO_MEM$Title == "International Agreement Between The Government Of The Russian Federation And The Government Of The Republic Of Estonia Regarding Cooperation In The Sphere Of Fisheries (1994)"),]
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "IR04SS_1979P:IR04SS_1957A" &
                                HUGGO_MEM$Title == "Protocol Amending The International Convention Relating To The Limitation Of The Liability Of Owners Of Sea-going Ships"),]
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PRTSBS_1972P:PRTSBS_1962A" &
                                HUGGO_MEM$Title == "Protocol Amending The Agreement Concerning Protection Of The Salmon Stock In The Baltic Sea"),]
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "UNCLOS_1982A" &
                                HUGGO_MEM$Title == "Agreement Relating To The Implementation Of Part 11 Of The United Nations Convention On The Law Of The Sea"),]

# Match titles and dates of verified treaties
# Step one: load data frame of verified treaties
verified <- read.csv("data-raw/agreements/HUGGO/HUGGO_verified.csv")
# Step two: 
df <- data.frame()
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
HUGGO_MEM[which(HUGGO_MEM$manyID == "CS18PS_1954A"), 4] <- messydates::as_messydate("1954-08-18")
HUGGO_MEM[which(HUGGO_MEM$manyID == "CS18PS_1954A"), 7] <- messydates::as_messydate("1954-08-18")
# Add stateSignature
HUGGO_MEM[which(HUGGO_MEM$manyID == "CS18PS_1954A"), 9] <- messydates::as_messydate("1954-08-18")
# Add stateRat: Chile
HUGGO_MEM[which(HUGGO_MEM$manyID == "CS18PS_1954A" & HUGGO_MEM$stateID == "CHL"), 10] <-
  messydates::as_messydate("1954-09-23")
# Add stateRat: Ecuador
HUGGO_MEM[which(HUGGO_MEM$manyID == "CS18PS_1954A" & HUGGO_MEM$stateID == "ECU"), 10] <-
  messydates::as_messydate("1956-01-24")
# Add stateRat: Peru
HUGGO_MEM[which(HUGGO_MEM$manyID == "CS18PS_1954A" & HUGGO_MEM$stateID == "PER"), 10] <-
  messydates::as_messydate("1956-05-10")

# DOM-PAN[CPC]_1989A
# Add stateSignature
HUGGO_MEM[which(HUGGO_MEM$manyID == "CC09DM_1954A"), 9] <- messydates::as_messydate("1989-10-23")
# Add stateRat: Nicaragua
HUGGO_MEM[which(HUGGO_MEM$manyID == "CS18PS_1954A" & HUGGO_MEM$stateID == "NIC"), 10] <-
  messydates::as_messydate("1990-12-13")

# INTRTT_1994A
# Beg and Signature dates correspond to the 1994 agreement, but stateSignature,
# stateRat, stateForce, stateWithdrawal and ProvisionalApp correspond to the
# 1983 agreement, which is not in HUGGO nor HUGGO_MEM

# TTICPO_2008A
# Correct Beg and Signature (must change dates in HUGGO too)
HUGGO_MEM[which(HUGGO_MEM$manyID == "TTICPO_2008A"), 4] <- messydates::as_messydate("2008-06-25")
HUGGO_MEM[which(HUGGO_MEM$manyID == "TTICPO_2008A"), 7] <- messydates::as_messydate("2008-06-25")

# VB13GV_2008O
# Correct Beg and Signature (must change dates in HUGGO too)
HUGGO_MEM[which(HUGGO_MEM$manyID == "VB13GV_2008O"), 4] <- messydates::as_messydate("2008-06-25")
HUGGO_MEM[which(HUGGO_MEM$manyID == "VB13GV_2008O"), 7] <- messydates::as_messydate("2008-06-25")

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
