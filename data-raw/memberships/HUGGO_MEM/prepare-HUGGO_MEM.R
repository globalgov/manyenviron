# HUGGO_MEM Preparation Script

# This is a template for importing, cleaning, and exporting data
# for the 'many' packages.

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

# Stage three: reconcile conflicts in data
# Step one: Match titles and dates of verified treaties from agreements$HUGGO
verified <- read.csv("data-raw/agreements/HUGGO/HUGGO_reconciled.csv")
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
  # Begin
  HUGGO_MEM[y, "Begin"] <- messydates::as_messydate(verified[i, "Begin"])
  # End
  HUGGO_MEM[y, "End"] <- messydates::as_messydate(verified[i, "End"])
  # Signature
  HUGGO_MEM[y, "Signature"] <- messydates::as_messydate(verified[i, "Signature"])
  # Force
  HUGGO_MEM[y, "Force"] <- messydates::as_messydate(verified[i, "Force"])
  }

# Step two: Convert dates to mdate class
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
HUGGO_MEM <- dplyr::distinct(HUGGO_MEM)

# Stage four: Merging verified data (HUGGO_MEM_reconciled.csv and HUGGO_MEM_additional.csv) with original data (HUGGO_MEM_original.csv)
HUGGO_MEM_reconciled <- readr::read_csv("data-raw/memberships/HUGGO_MEM/HUGGO_MEM_reconciled.csv") %>%
  dplyr::mutate(Begin = messydates::as_messydate(Begin),
                End = messydates::as_messydate(End),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                stateSignature = messydates::as_messydate(stateSignature),
                stateRat = messydates::as_messydate(stateRat),
                stateForce = messydates::as_messydate(stateForce),
                stateForce2 = messydates::as_messydate(stateForce2),
                stateForce3 = messydates::as_messydate(as.character(stateForce3)),
                Term = messydates::as_messydate(Term),
                stateWithdrawal = messydates::as_messydate(stateWithdrawal),
                stateWithdrawal2 = messydates::as_messydate(as.character(stateWithdrawal2)),
                Deposit = messydates::as_messydate(Deposit),
                ProvisionalApp = messydates::as_messydate(ProvisionalApp),
                Reservation = messydates::as_messydate(Reservation),
                Consent = messydates::as_messydate(Consent),
                Acceptance = messydates::as_messydate(Acceptance),
                Accession = messydates::as_messydate(Accession),
                Succession = ifelse(!is.na(Succession), 1, 0))
HUGGO_MEM_additional <- readr::read_csv("data-raw/memberships/HUGGO_MEM/HUGGO_MEM_additional.csv") %>%
  dplyr::mutate(Begin = messydates::as_messydate(Begin),
                End = messydates::as_messydate(End),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                stateSignature = messydates::as_messydate(stateSignature),
                stateRat = messydates::as_messydate(stateRat),
                stateForce = messydates::as_messydate(stateForce),
                stateForce2 = messydates::as_messydate(as.character(stateForce2)),
                stateForce3 = messydates::as_messydate(as.character(stateForce3)),
                Term = messydates::as_messydate(as.character(Term)),
                stateWithdrawal = messydates::as_messydate(stateWithdrawal),
                stateWithdrawal2 = messydates::as_messydate(as.character(stateWithdrawal2)),
                Deposit = messydates::as_messydate(as.character(Deposit)),
                ProvisionalApp = messydates::as_messydate(as.character(ProvisionalApp)),
                Reservation = messydates::as_messydate(as.character(Reservation)),
                Consent = messydates::as_messydate(as.character(Consent)),
                Acceptance = messydates::as_messydate(Acceptance),
                Accession = messydates::as_messydate(Accession),
                Succession = ifelse(!is.na(Succession), 1, 0))
HUGGO_MEM_new <- dplyr::bind_rows(HUGGO_MEM_reconciled, HUGGO_MEM_additional) %>%
  dplyr::arrange(Begin, manyID)

HUGGO_MEM <- dplyr::full_join(HUGGO_MEM, HUGGO_MEM_new,
                              by = c("manyID", "treatyID", "Title", "stateID"))
HUGGO_MEM <- HUGGO_MEM %>%
  manydata::transmutate(Begin = ifelse(!is.na(Begin.y), Begin.y, Begin.x),
                        End = ifelse(!is.na(End.y), End.y, End.x),
                        Signature = ifelse(!is.na(Signature.y), Signature.y,
                                           Signature.x),
                        Force = ifelse(!is.na(Force.y), Force.y, Force.x),
                        StateSignature = ifelse(!is.na(stateSignature.y),
                                                stateSignature.y,
                                                ifelse(stringr::str_detect(Verified_HUGGO, "stateSignature"),
                                                       stateSignature.y,
                                                       stateSignature.x)),
                        StateRatification = ifelse(!is.na(stateRat.y),
                                                   stateRat.y,
                                                   ifelse(stringr::str_detect(Verified_HUGGO, "stateRat"),
                                                          stateRat.y,
                                                          stateRat.x)),
                        StateForce = ifelse(!is.na(stateForce.y),
                                            stateForce.y,
                                            ifelse(stringr::str_detect(Verified_HUGGO, "stateForce"),
                                                   stateForce.y,
                                                   stateForce.x)),
                        StateForce2 = ifelse(!is.na(stateForce2.y),
                                             stateForce2.y, stateForce2.x),
                        StateForce3 = ifelse(!is.na(stateForce3.y),
                                             stateForce3.y, stateForce3.x),
                        StateEnd = ifelse(!is.na(stateWithdrawal.y),
                                          stateWithdrawal.y,
                                          ifelse(stringr::str_detect(Verified_HUGGO, "stateWithdrawal"),
                                                 stateWithdrawal.y,
                                                 stateWithdrawal.x)),
                        StateEnd2 = ifelse(!is.na(stateWithdrawal2.y),
                                          stateWithdrawal2.y, stateWithdrawal2.x),
                        Term = ifelse(!is.na(Term.y), Term.y, Term.x),
                        gengID = ifelse(!is.na(gengID.y), gengID.y, gengID.x),
                        ieaID = ifelse(!is.na(ieaID.y), ieaID.y, ieaID.x),
                        ecolexID = ifelse(!is.na(ecolexID.y), ecolexID.y, ecolexID.x),
                        Comments = ifelse(!is.na(Comments.y), Comments.y, Comments.x),
                        Deposit = ifelse(!is.na(Deposit.y), Deposit.y, Deposit.x),
                        obsolete = ifelse(!is.na(obsolete.y), obsolete.y, obsolete.x),
                        ProvisionalApp = ifelse(!is.na(ProvisionalApp.y),
                                                ProvisionalApp.y, ProvisionalApp.x),
                        Reservation = ifelse(!is.na(Reservation.y),
                                             Reservation.y, Reservation.x),
                        verified = ifelse(!is.na(verified.y), verified.y,
                                          verified.x),
                        Notes = ifelse(!is.na(Notes.y), Notes.y, Notes.x),
                        stateForce_ecolex = ifelse(!is.na(stateForce_ecolex.y),
                                                   stateForce_ecolex.y,
                                                   stateForce_ecolex.x),
                        stateForce_iea = ifelse(!is.na(stateForce_iea.y),
                                                stateForce_iea.y,
                                                stateForce_iea.x),
                        Consent = ifelse(!is.na(Consent.y),
                                         Consent.y,
                                         Consent.x),
                        Acceptance = ifelse(!is.na(Acceptance.y),
                                            Acceptance.y,
                                            Acceptance.x),
                        Accession = ifelse(!is.na(Accession.y),
                                           Accession.y,
                                           ifelse(stringr::str_detect(Verified_HUGGO, "Accession"),
                                                  Accession.y,
                                                  Accession.x))) %>%
  dplyr::relocate(manyID, treatyID, Title, Begin, stateID, Signature, Force, End,
                  StateSignature, StateRatification, StateForce, StateForce2,
                  StateForce3, StateEnd, StateEnd2,
                  gengID, ieaID, ecolexID, Accession, Succession) %>%
  dplyr::mutate(Begin = messydates::as_messydate(Begin),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                End = messydates::as_messydate(End),
                StateRatification = messydates::as_messydate(StateRatification),
                StateSignature = messydates::as_messydate(StateSignature),
                StateForce = messydates::as_messydate(StateForce),
                StateForce2 = messydates::as_messydate(StateForce2),
                StateForce3 = messydates::as_messydate(StateForce3),
                StateEnd = messydates::as_messydate(StateEnd),
                StateEnd2 = messydates::as_messydate(StateEnd2),
                stateForce_ecolex = messydates::as_messydate(stateForce_ecolex),
                stateForce_iea = messydates::as_messydate(stateForce_iea),
                Term = messydates::as_messydate(Term),
                Accession = messydates::as_messydate(Accession)) %>%
  dplyr::distinct() %>%
  # Remove variables used to track progress in coding
  dplyr::select(-c(Changes_HUGGO, Verified_HUGGO, Checked_HUGGO, verified))

# Mark successor states from Soviet Union, Yugoslavia, and Czechoslovakia in Succession variable
repstateIDs <- manypkgs::code_states(c("Armenia", "Azerbaijan", "Belarus",
                                       "Estonia", "Georgia", "Kazakhstan",
                                       "Kyrgyzstan", "Latvia", "Lithuania",
                                       "Moldova", "Russia", "Tajikistan",
                                       "Turkmenistan", "Ukraine", "Uzbekistan",
                                       "Serbia", "Bosnia and Herzegovina",
                                       "North Macedonia", "Slovenia",
                                       "Montenegro", "Croatia",
                                       "Czech Republic", "Slovakia"),
                                     activity = FALSE,
                                     replace = "ID")

HUGGO_MEM <- HUGGO_MEM %>%
  dplyr::mutate(Succession = ifelse(stateID %in% repstateIDs, 1, Succession))

# Match non-English treaty titles to English treaty titles
noneng <- HUGGO_MEM %>%
  dplyr::filter(manyID == "UB08IB_1893A")
noneng <- noneng %>%
  dplyr::rename(match = manyID, Orig_noneng_title = Title) %>%
  # Non-English titles of the same agreement with an English title added in variable 'Orig_noneng_title',
  # manyID of the non-English agreement added in variable 'match'
  dplyr::arrange(match) %>%
  dplyr::mutate(manyID = "AH12IF_1893A")
noneng <- noneng %>%
  dplyr::select(manyID, Begin, Orig_noneng_title, match)
HUGGO_MEM <- dplyr::left_join(HUGGO_MEM, noneng, by = c("manyID", "Begin"))
# remove rows with non-English titles identified above
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "UB08IB_1893A"),]

# Remove duplicated entries
HUGGO_MEM <- dplyr::distinct(HUGGO_MEM)

# PW05MW_1925P
# remove extra row for AUT
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PW05MW_1925P" & HUGGO_MEM$stateID == "AUT" & is.na(HUGGO_MEM$obsolete)),]

# INTRRW_1946A
# remove extra row for ISL
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRRW_1946A" & HUGGO_MEM$stateID == "ISL" & HUGGO_MEM$StateForce == HUGGO_MEM$StateForce2),]
# remove extra row for PAN
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRRW_1946A" & HUGGO_MEM$stateID == "PAN" & HUGGO_MEM$StateForce == HUGGO_MEM$StateForce2),]
# remove extra row for BRA
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRRW_1946A" & HUGGO_MEM$stateID == "BRA" & is.na(HUGGO_MEM$StateForce3)),]
# remove row with non-NA StateSignature date for SWE that has been verified
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRRW_1946A" & HUGGO_MEM$stateID == "SWE" & !is.na(HUGGO_MEM$StateSignature)),]
# remove extra row for DMA
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRRW_1946A" & HUGGO_MEM$stateID == "DMA" & HUGGO_MEM$StateForce == HUGGO_MEM$StateForce2),]
# remove extra row for URY
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRRW_1946A" & HUGGO_MEM$stateID == "URY" & HUGGO_MEM$StateForce == HUGGO_MEM$StateForce2),]
# remove extra row for BLZ
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRRW_1946A" & HUGGO_MEM$stateID == "BLZ" & HUGGO_MEM$StateForce == HUGGO_MEM$StateForce2),]
# remove extra row for ECU
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRRW_1946A" & HUGGO_MEM$stateID == "ECU" & HUGGO_MEM$StateForce == HUGGO_MEM$StateForce2),]
# remove extra row for ITA
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRRW_1946A" & HUGGO_MEM$stateID == "ITA" & HUGGO_MEM$StateSignature == HUGGO_MEM$StateRatification), ]
# remove extra row for HUN that has been verified
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRRW_1946A" & HUGGO_MEM$stateID == "HUN" & HUGGO_MEM$StateSignature == HUGGO_MEM$StateRatification), ]
# remove row with StateSignature date later than StateRatification date for BEL
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRRW_1946A" & HUGGO_MEM$stateID == "BEL" & HUGGO_MEM$StateSignature > HUGGO_MEM$StateRatification), ]
# remove row with StateSignature date later than StateRatification date for SUR
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRRW_1946A" & HUGGO_MEM$stateID == "SUR" & HUGGO_MEM$StateSignature > HUGGO_MEM$StateRatification), ]
# remove row with StateSignature date later than StateRatification date for CZE
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRRW_1946A" & HUGGO_MEM$stateID == "CZE" & HUGGO_MEM$StateSignature > HUGGO_MEM$StateRatification), ]
# remove extra row for ERI
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRRW_1946A" & HUGGO_MEM$stateID == "ERI" & HUGGO_MEM$StateSignature == HUGGO_MEM$StateRatification), ]

# INTRPB_1950A
# remove extra row for MCO
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRPB_1950A" & HUGGO_MEM$stateID == "MCO" & is.na(HUGGO_MEM$obsolete)),]
# remove extra row for FRA
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRPB_1950A" & HUGGO_MEM$stateID == "FRA" & is.na(HUGGO_MEM$obsolete)),]
# remove extra row for GRC
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRPB_1950A" & HUGGO_MEM$stateID == "GRC" & is.na(HUGGO_MEM$obsolete)),]
# remove extra row for AUT
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRPB_1950A" & HUGGO_MEM$stateID == "AUT" & is.na(HUGGO_MEM$obsolete)),]
# remove extra row for PRT
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRPB_1950A" & HUGGO_MEM$stateID == "PRT" & is.na(HUGGO_MEM$obsolete)),]
# remove extra row for BGR
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "INTRPB_1950A" & HUGGO_MEM$stateID == "BGR" & is.na(HUGGO_MEM$obsolete)),]

# TPNWLA_1967A
# remove extra row for BOL
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "TPNWLA_1967A" & HUGGO_MEM$stateID == "BOL" & is.na(HUGGO_MEM$obsolete)),]

# IR06PC_1969A
# remove extra row for BEL
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "IR06PC_1969A" & HUGGO_MEM$stateID == "BEL" & is.na(HUGGO_MEM$obsolete)),]

# PD05WD_1972A
# removed row without StateForce2 for LBR
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PD05WD_1972A" & HUGGO_MEM$stateID == "LBR" & is.na(HUGGO_MEM$StateForce2)),]
# removed row without StateForce2 for JOR
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PD05WD_1972A" & HUGGO_MEM$stateID == "JOR" & is.na(HUGGO_MEM$StateForce2)),]
# removed row without StateForce2 for IRN
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PD05WD_1972A" & HUGGO_MEM$stateID == "IRN" & is.na(HUGGO_MEM$StateForce2)),]
# removed row without StateForce2 for MUS
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PD05WD_1972A" & HUGGO_MEM$stateID == "MUS" & is.na(HUGGO_MEM$StateForce2)),]
# removed row without StateForce2 for RWA
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PD05WD_1972A" & HUGGO_MEM$stateID == "RWA" & is.na(HUGGO_MEM$StateForce2)),]
# removed row without StateForce2 for GHA
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PD05WD_1972A" & HUGGO_MEM$stateID == "GHA" & is.na(HUGGO_MEM$StateForce2)),]
# removed row without StateForce2 for COD
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PD05WD_1972A" & HUGGO_MEM$stateID == "COD" & is.na(HUGGO_MEM$StateForce2)),]
# removed row without StateForce2 for LSO
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PD05WD_1972A" & HUGGO_MEM$stateID == "LSO" & is.na(HUGGO_MEM$StateForce2)),]
# removed row without StateForce2 for VEN
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PD05WD_1972A" & HUGGO_MEM$stateID == "VEN" & is.na(HUGGO_MEM$StateForce2)),]
# removed row without StateForce2 for NIC
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PD05WD_1972A" & HUGGO_MEM$stateID == "NIC" & is.na(HUGGO_MEM$StateForce2)),]
# removed row without StateForce2 for NZL
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PD05WD_1972A" & HUGGO_MEM$stateID == "NZL" & is.na(HUGGO_MEM$StateForce2)),]
# removed row without StateForce2 for BDI
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PD05WD_1972A" & HUGGO_MEM$stateID == "BDI" & is.na(HUGGO_MEM$StateForce2)),]
# removed row without StateForce2 for LKA
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "PD05WD_1972A" & HUGGO_MEM$stateID == "LKA" & is.na(HUGGO_MEM$StateForce2)),]

# LRTAP_1979A
# Correct stateID for DDR entry
HUGGO_MEM[which(HUGGO_MEM$manyID == "LRTAP_1979A" & HUGGO_MEM$stateID == "DEU" & HUGGO_MEM$StateEnd == "1990-10-03"), 5] <- "DDR"
# removed row without StateForce2 for USA
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "LRTAP_1979A" & HUGGO_MEM$stateID == "USA" & is.na(HUGGO_MEM$StateForce2)),]

# FCNEAF_1980A
# Remove row with non-NA StateEnd for BGR
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "FCNEAF_1980A" & HUGGO_MEM$stateID == "BGR" & !is.na(HUGGO_MEM$StateEnd)),]

# MNSDOL_1987P
# Correct Accession date for NZL
HUGGO_MEM[which(HUGGO_MEM$manyID == "MNSDOL_1987P" & HUGGO_MEM$stateID == "NZL" & !is.na(HUGGO_MEM$Accession)), 19] <- NA
HUGGO_MEM <- dplyr::distinct(HUGGO_MEM)

# CTMHWD_1989A
# Correct Accession date for NZL
HUGGO_MEM[which(HUGGO_MEM$manyID == "CTMHWD_1989A" & HUGGO_MEM$stateID == "NZL" & !is.na(HUGGO_MEM$Accession)), 19] <- NA
HUGGO_MEM <- dplyr::distinct(HUGGO_MEM)

# UNCLOS_1982A
# remove row without stateForce_ecolex and stateForce_iea for AFG
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "UNCLOS_1982A" & HUGGO_MEM$stateID == "AFG" & is.na(HUGGO_MEM$stateForce_ecolex)),]
# Correct Accession date for NZL
HUGGO_MEM[which(HUGGO_MEM$manyID == "UNCLOS_1982A" & HUGGO_MEM$stateID == "NZL" & !is.na(HUGGO_MEM$Accession)), 19] <- NA
HUGGO_MEM <- dplyr::distinct(HUGGO_MEM)

#	NCLRSF_1994A
# remove duplicate entry with wrong End date for AUT
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "NCLRSF_1994A" & HUGGO_MEM$stateID == "AUT" & is.na(HUGGO_MEM$End)),]
# add missing End dates
HUGGO_MEM[which(HUGGO_MEM$manyID == "NCLRSF_1994A" & is.na(HUGGO_MEM$End)), 8] <- messydates::as_messydate("1999-04-09")

# IL07SS_1996A
# remove extra row for GBR
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "IL07SS_1996A" & HUGGO_MEM$stateID == "GBR" & is.na(HUGGO_MEM$obsolete)),]
# remove extra row for DEU
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "IL07SS_1996A" & HUGGO_MEM$stateID == "DEU" & is.na(HUGGO_MEM$obsolete)),]
# remove extra row for CAN
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "IL07SS_1996A" & HUGGO_MEM$stateID == "CAN" & is.na(HUGGO_MEM$obsolete)),]
# remove extra row for SWE
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "IL07SS_1996A" & HUGGO_MEM$stateID == "SWE" & is.na(HUGGO_MEM$obsolete)),]
# remove extra row for FIN
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "IL07SS_1996A" & HUGGO_MEM$stateID == "FIN" & is.na(HUGGO_MEM$obsolete)),]
# remove extra row for DNK
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "IL07SS_1996A" & HUGGO_MEM$stateID == "DNK" & is.na(HUGGO_MEM$obsolete)),]
# remove extra row for NOR
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "IL07SS_1996A" & HUGGO_MEM$stateID == "NOR" & is.na(HUGGO_MEM$obsolete)),]
# remove extra row for NLD
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "IL07SS_1996A" & HUGGO_MEM$stateID == "NLD" & is.na(HUGGO_MEM$obsolete)),]

# IE05PD_2003P:IE05PD_1971A
# remove extra row for ESP
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "IE05PD_2003P:IE05PD_1971A" & HUGGO_MEM$stateID == "ESP" & is.na(HUGGO_MEM$obsolete)),]
# remove extra row for DNK
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "IE05PD_2003P:IE05PD_1971A" & HUGGO_MEM$stateID == "DNK" & is.na(HUGGO_MEM$obsolete)),]
# remove extra row for FRA
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "IE05PD_2003P:IE05PD_1971A" & HUGGO_MEM$stateID == "FRA" & is.na(HUGGO_MEM$obsolete)),]
# remove extra row for IRL
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "IE05PD_2003P:IE05PD_1971A" & HUGGO_MEM$stateID == "IRL" & is.na(HUGGO_MEM$obsolete)),]
# remove extra row for SVN
HUGGO_MEM <- HUGGO_MEM[-which(HUGGO_MEM$manyID == "IE05PD_2003P:IE05PD_1971A" & HUGGO_MEM$stateID == "SVN" & is.na(HUGGO_MEM$obsolete)),]

# Stage five: Format data correctly for export

# Correct StateSignature dates coded #### as missing
HUGGO_MEM <- HUGGO_MEM %>%
  dplyr::mutate(StateSignature = ifelse(grepl("######", StateSignature),
                                        NA, StateSignature),
                StateRatification = ifelse(grepl("######", StateRatification),
                                        NA, StateRatification))

# Make sure all data are in correct class
HUGGO_MEM <- HUGGO_MEM %>%
  dplyr::mutate(across(everything(),
                      ~stringr::str_replace_all(., "^NA$", NA_character_))) %>%
  dplyr::distinct() %>%
  dplyr::mutate(Begin = messydates::as_messydate(Begin),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                End = messydates::as_messydate(End),
                StateRatification = messydates::as_messydate(StateRatification),
                StateSignature = messydates::as_messydate(StateSignature),
                StateForce = messydates::as_messydate(StateForce),
                StateForce2 = messydates::as_messydate(StateForce2),
                StateForce3 = messydates::as_messydate(StateForce3),
                StateEnd = messydates::as_messydate(StateEnd),
                StateEnd2 = messydates::as_messydate(StateEnd2),
                Term = messydates::as_messydate(Term),
                Accession = messydates::as_messydate(Accession)) %>%
  dplyr::arrange(Begin)

# Stage three: Connecting data
# Next run the following line to make HUGGO_MEM available
# within the package.
manypkgs::export_data(HUGGO_MEM, datacube = "memberships",
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
