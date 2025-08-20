# This code is to combine outputs of PAMGUARD/Rocca for different dolphin recordings.
# The output is a dataset to be used in further classificator development.
# The code is adapted from its earlier version in "banter_0.R"

rm(list = ls())


# Packages ----

library(dplyr)
library(readr)
library(readxl)

source("code/misc.R")


# Labeled data ----

## Bottlenose dolphins ----

### Maryland T1C site ----

T1C_2016MarApr_wsl <- read_csv("dataraw/bottle_MarylandT1C/Rocca Whistle Stats_MarApr2016_NoDup.csv",
                               col_select = -1) %>%
    mutate(event.id = paste0("T1C_2016MarApr_", sightingNum))

T1C_2016MayJun_wsl <- read_csv("dataraw/bottle_MarylandT1C/Rocca Whistle Stats_MayJun2016_NoDup.csv",
                               col_select = -1) %>%
    mutate(event.id = paste0("T1C_2016MayJun_", sightingNum))

T1C_2017AprSep_wsl <- read_csv("dataraw/bottle_MarylandT1C/Rocca_Whistle_Stats_T1CAprSep2017.csv") %>%
    mutate(event.id = paste0("T1C_2017AprSep_", sightingNum))

T1C_2017OctDec_wsl <- read_csv("dataraw/bottle_MarylandT1C/Rocca Whistle Stats_OctDec2017_NoDup.csv",
                               col_select = -1) %>%
    mutate(event.id = paste0("T1C_2017OctDec_", sightingNum))

T1C_2018Jul_wsl <- read_csv("dataraw/bottle_MarylandT1C/Rocca_Whistle_Stats_T1C2018.csv") %>%
    mutate(event.id = paste0("T1C_2018Jul_", sightingNum))

T1C_2018JulAug_wsl <- read_csv("dataraw/bottle_MarylandT1C/Rocaa Whistle Stats_NoDup_18T1C.csv",
                               col_select = -1, skip = 1) %>%
    mutate(event.id = paste0("T1C_2018JulAug_", sightingNum))

# Combine all T1C records
T1C_wsl <- bind_rows(T1C_2016MarApr_wsl,
                     T1C_2016MayJun_wsl,
                     T1C_2017AprSep_wsl,
                     T1C_2017OctDec_wsl,
                     T1C_2018Jul_wsl,
                     T1C_2018JulAug_wsl) %>%
    arrange(UTC) %>%
    mutate(species = "Bottlenose",
           Source = "T1C",
           UTCround = as.integer(UTC),
           UTCMilliseconds = as.integer(UTCMilliseconds)) %>%
    distinct(UTCround, UTCMilliseconds, .keep_all = TRUE) %>%
    select(-UTCround)


#### Update event IDs ----

# The event lengths (measured in minutes) are too long
event_len_mins <- tapply(T1C_wsl$UTC, T1C_wsl$event.id, function(x)
    difftime(max(x), min(x), units = 'mins'))
hist(event_len_mins,
     br = 30,
     main = paste("Lengths of", length(unique(T1C_wsl$event.id)), "events"),
     las = 1)

# T1C_wsl has the data sorted chronologically, so
# make sure the event IDs are not interrupted/repeated at a different time
tmp <- rle(T1C_wsl$event.id)

# Repeat a unique number to paste with the current event.ID
NumUn <- rep(1:length(tmp$lengths), times = tmp$lengths)
NumUn <- stringr::str_pad(NumUn, floor(log10(max(NumUn))) + 1, pad = "0")

# Update the event IDs
T1C_wsl$event.id <- paste0(T1C_wsl$event.id, "_", NumUn)

# Update event lengths and replot
event_len_mins <- tapply(T1C_wsl$UTC, T1C_wsl$event.id, function(x)
    difftime(max(x), min(x), units = 'mins'))
hist(event_len_mins,
     br = 30,
     main = paste("Lengths of", length(unique(T1C_wsl$event.id)), "events"),
     las = 1)


# Split events if time gap > TT minutes
# Time threshold, minutes
TT <- 5

# Select events longer than TT, to check
# (the gap of TT minutes cannot occur in shorter events)
longerTT <- names(event_len_mins[event_len_mins > TT])

for (ev in longerTT) {
    # ev = "T1C_2016MarApr_AutoEvent10_0008" # no break
    # ev = "T1C_2016MayJun_AutoEvent7_0345" # with a break
    utc <- T1C_wsl %>%
        filter(event.id == ev) %>%
        pull(UTC)

    # Time differences, minutes
    td <- diff(unclass(utc)) / 60

    if (any(td > TT)) {
        NumUn2 <- rep(NA, length(utc))
        NumUn2[1] <- 0L
        NumUn2[which(td > TT)] <- 1:sum(td > TT)
        NumUn2 <- copyforward(NumUn2)

        # Update the main dataset with new IDs
        T1C_wsl$event.id[T1C_wsl$event.id == ev] <- paste0(T1C_wsl$event.id[T1C_wsl$event.id == ev], "_", NumUn2)
    }
}

# Update event lengths and replot
event_len_mins <- tapply(T1C_wsl$UTC, T1C_wsl$event.id, function(x)
    difftime(max(x), min(x), units = 'mins'))
hist(event_len_mins,
     br = 30,
     main = paste("Lengths of", length(unique(T1C_wsl$event.id)), "events"),
     las = 1)


# Combine short events < TT minutes
# Calculate time gaps between events,
# merge if the gap <= TT and one of the events <= TT minutes long.
event_startend <- tapply(T1C_wsl$UTC, T1C_wsl$event.id, function(x)
    c(Start = min(x), End = max(x)))
tmp <- names(event_startend)
n <- length(event_startend)
event_startend <- event_startend %>%
    bind_rows() %>%
    mutate(event.id = tmp) %>%
    arrange(Start) %>%
    mutate(Gap = c(NA, difftime(Start[-1], End[-n], units = 'mins')),
           Len = difftime(End, Start, units = 'mins')) %>%
    mutate(Merge2previous = (Gap <= TT) & (Len <= TT | c(NA, Len[-n]) <= TT))
mean(event_startend$Merge2previous, na.rm = TRUE)
# 0.08039068
summary(event_startend)

# Check if there are > 1 consecutive events to be merged
RLE <- rle(event_startend$Merge2previous)
RLE <- tibble(lengths = RLE$lengths, values = RLE$values)
for (i in 2:nrow(RLE)) { # i = 3
    if (RLE$values[i]) {
        istart <- sum(RLE$lengths[1:(i - 1)])
        iend <- sum(RLE$lengths[1:i])
        newname <- paste0(event_startend$event.id[istart],
                          "_mergedwith_",
                          RLE$lengths[i])
        events2merge <- event_startend$event.id[istart:iend]
        T1C_wsl$event.id[T1C_wsl$event.id %in% events2merge] <- newname
    }
}

# Update event lengths and replot
event_len_mins <- tapply(T1C_wsl$UTC, T1C_wsl$event.id, function(x)
    difftime(max(x), min(x), units = 'mins'))
hist(event_len_mins,
     br = 30,
     main = paste("Lengths of", length(unique(T1C_wsl$event.id)), "events"),
     las = 1)



## Common dolphins ----

### NOAA ----

NOAA_wsl <- read_csv("dataraw/common_NOAA/Rocaa Whistle Stats_NOAA_NoDup.csv",
                     col_select = -1) %>%
    mutate(species = "Common",
           event.id = paste0("NOAA_", sightingNum))

# Correct the dates, using the original column Source
tmp <- strsplit(NOAA_wsl$Source, "_|-")
tmp <- sapply(tmp, function(x) {
    if (length(x) == 11) {
        x[[6]]
    } else if (length(x) == 7) {
        x[[6]]
    } else if (length(x) == 10) {
        x[[6]]
    } else {
        NA
    }
})
tmp <- as.POSIXct(tmp, format = "%Y%m%d", tz = "GMT")
NOAA_wsl$UTC[!is.na(tmp)] <- na.omit(tmp)

# Update Source to be just "NOAA", not the full file path
NOAA_wsl <- NOAA_wsl %>%
    mutate(Source = "NOAA")


### Watkins ----

Wat_meta <- read_xlsx("dataraw/common_Watkins/Watkins Sound File Metadata.xlsx") %>%
    rename(FileName = `File Name (RN)`,
           UTCtrue = `Date Recorded (OD)`)

Wat_wsl <- read_csv("dataraw/common_watkins/Rocca_Whistle_Stats_WatkinsCD_NoDup.csv",
                    col_select = -1) %>%
    mutate(species = "Common",
           event.id = paste0("Wat_", sightingNum))

# Correct the dates, using the original column Source and metadata
tmp <- strsplit(Wat_wsl$Source, "-")
tmp <- sapply(tmp, function(x) x[[2]])
Wat_wsl <- Wat_wsl %>%
    mutate(Source = "Watkins",
           FileName = tmp) %>%
    left_join(Wat_meta %>% dplyr::select(FileName, UTCtrue),
              by = join_by(FileName)) %>%
    mutate(UTC = UTCtrue) %>%
    select(-UTCtrue)


### Brazil ----

Bra_wsl <- read_csv("dataraw/common_Brazil/Rocca_Whistle_Stats_CD_Bruna_081823.csv") %>%
    mutate(species = "Common",
           event.id = paste0("Bra_", sightingNum)) %>%
    mutate(UTC = as.POSIXct(UTC, format = "%m/%d/%Y %H:%M", tz = "GMT") + UTCMilliseconds/1000*60) %>%
    arrange(UTC)

# Define events as h-minute events within each Source
H = c(1, 2, 3, 5, 10)
tmp <- base::strsplit(Bra_wsl$Source, "-")
Bra_wsl$Source <- sapply(tmp, function(x) x[2])
Sources <- unique(Bra_wsl$Source)

# Add separate columns for event IDs to be used later
sightingNum_H <- lapply(H, function(h) paste0("Bra_", Bra_wsl$Source, "_"))
sightingNum_H <- do.call(cbind, sightingNum_H)
colnames(sightingNum_H) <- paste0("sightingNum_", H)
Bra_wsl <- bind_cols(Bra_wsl, sightingNum_H)

# Within each Source, split records in h-minute events
for (s in Sources) { # s = "B13h55m12s08may2014"; h = 1
    # Time range of the recordings in the file
    r <- Bra_wsl %>%
        filter(Source == s) %>%
        pull(UTC) %>%
        range()

    for (h in H) {
        # Breakpoints to separate the range into h-minute intervals
        br <- seq(from = as.numeric(r[1]) - 1,
                  to = as.numeric(r[2]) + h*60,
                  by = h*60)

        # Name of the separate column
        tmp <- paste0("sightingNum_", h)
        tmp <- which(names(Bra_wsl) == tmp)

        # The separate column
        ntemp <- Bra_wsl[Bra_wsl$Source == s, tmp] %>%
            unlist()
        itemp <- cut(as.numeric(Bra_wsl$UTC[Bra_wsl$Source == s]),
                     labels = 1:(length(br) - 1),
                     breaks = br) %>%
            as.integer()
        Bra_wsl[Bra_wsl$Source == s, tmp] <- paste0(ntemp, itemp)
    }
}

# Choose which separation h is used by setting the corresponding column as event.id
Bra_wsl <- Bra_wsl %>%
    mutate(event.id = sightingNum_5) %>%
    mutate(Source = "Brazil")
length(table(Bra_wsl$event.id))
# 29

# Save Brazil data separately
saveRDS(Bra_wsl, file = "dataderived/dataprocess_Bra_wsl.rds")
readr::write_csv(Bra_wsl, file = "dataderived/dataprocess_Bra_wsl.csv")


## Combine labeled data ----

# Response variable
RESPONSE <- "species"

# Columns with ID information that should not be used for training models
idcols_wsl <- c("Source", "event.id", "call.id", "Year", "Month", "UTC")

# Columns that can be used for training models, to predict the RESPONSE
predictors_wsl <- names(T1C_wsl) %>%
    setdiff(c("Id", "UID", "UpdateOf", "classifiedAs")) %>%
    setdiff(c("encounterCount", "sightingNum", "classifierUsed",
              "classifier2Used", "voteList", "spList")) %>%
    setdiff(c("latitude", "longitude", "offlineEventID")) %>%
    setdiff(c("UTCMilliseconds", "PCLocalTime", "PCTime",
              "ChannelBitmap", "SequenceBitmap", "channelMap",
              "startSample", "startSeconds",
              "detectionType", "bearingAmbiguity", "bearing0",
              "ClickNumber", "SpeciesCode")) %>%
    setdiff(idcols_wsl) %>%
    setdiff(RESPONSE) %>%
    sort()

# Variables that will be selected across files
matched_wsl <- c(RESPONSE, idcols_wsl, predictors_wsl, "UID")

# Combine data and create more variables
D_wsl <- dplyr::bind_rows(T1C_wsl %>% select(any_of(matched_wsl))
                          ,NOAA_wsl %>% select(any_of(matched_wsl))
                          ,Wat_wsl %>% select(any_of(matched_wsl))
                          ,Bra_wsl %>% select(any_of(matched_wsl))
) %>%
    rename(call.id = UID) %>%
    mutate(species = as.factor(species),
           call.id = paste(event.id, call.id, sep = "-")) %>%
    mutate(Year = as.numeric(format(UTC, "%Y")),
           Month = as.numeric(format(UTC, "%m")))

# Remove columns with non-unique values (all values are the same)
inu <- apply(D_wsl, 2, function(x) length(unique(x)) == 1)
names(D_wsl)[inu]
# [1] "bw10db"         "bw10dbHigh"     "bw10dbLow"      "bw3db"
# [5] "bw3dbHigh"      "bw3dbLow"       "clickType"      "dcMean"
# [9] "dcQuarter1Mean" "dcQuarter2Mean" "dcQuarter3Mean" "dcQuarter4Mean"
# [13] "dcStdDev"       "freqPeak"       "ici"            "meanTimeZC"
# [17] "medianTimeZC"   "nCrossings"     "rmsNoise"       "rmsSignal"
# [21] "snr"            "sweepRate"      "varianceTimeZC" "whaleTrain"
predictors_wsl <- setdiff(predictors_wsl, names(D_wsl)[inu])
D_wsl <- D_wsl[, !inu]

# Check counts
with(D_wsl,
     table(Source, Year)
)
#           Year
# Source  1958 1975 1987 2014 2016 2017 2018
# Brazil     0    0    0 3580    0    0    0
# NOAA       0    0    0    0 2637    0    0
# T1C        0    0    0    0 5251 2049 1075
# Watkins  131   97    2    0    0    0    0

dim(D_wsl)
# [1] 14822    57

dim(D_wsl)[1] == length(unique(D_wsl$call.id))

# Save data
saveRDS(predictors_wsl, file = "dataderived/dataprocess_predictors_wsl.rds")
readr::write_csv(predictors_wsl %>% tibble(),
                 col_names = FALSE,
                 file = "dataderived/dataprocess_predictors_wsl.csv")
saveRDS(D_wsl, file = "dataderived/dataprocess_D_wsl.rds")
readr::write_csv(D_wsl, file = "dataderived/dataprocess_D_wsl.csv")


# Unlabeled data ----

## Maryland A5C and metocean buoy site ----
rm(list = ls())

library(dplyr)
library(readr)
library(readxl)

source("code/misc.R")

A5C_2016Summer_wsl <- read_csv("dataraw/A5C/Rocca_Whistle_Stats_A5C_Summer2016_NoDup.csv") %>%
    mutate(event.id = paste0("A5C_2016Summer_", sightingNum))

A5C_2017Nov_wsl <- read_csv("dataraw/A5C/Rocaa Whistle Stats_NoDup_A5CNov2017.csv",
                            col_select = -1) %>%
    mutate(event.id = paste0("A5C_2017Nov_", sightingNum))

A5C_2018SWStudy_wsl <- read_csv("dataraw/A5C/Rocaa Whistle Stats_NoDup_A5C2018_SWStudy.csv",
                                col_select = -1) %>%
    mutate(event.id = paste0("A5C_2018SWStudy_", sightingNum))

MBuoy_2021Winter_wsl <- read_csv("dataraw/A5C/Rocca_Whistle_Stats_MBuoyLS1X_Winter2021_NoDup.csv") %>%
    mutate(event.id = paste0("MBuoy_2021Winter_", sightingNum)) %>%
    arrange(UTC) %>%
    mutate(Source = "MBuoy")

# Combine the records
A5C_wsl <- bind_rows(A5C_2016Summer_wsl,
                     A5C_2017Nov_wsl,
                     A5C_2018SWStudy_wsl) %>%
    arrange(UTC) %>%
    mutate(Source = "A5C") %>%
    bind_rows(MBuoy_2021Winter_wsl) %>%
    mutate(UTCround = as.integer(UTC),
           UTCMilliseconds = as.integer(UTCMilliseconds)) %>%
    distinct(UTCround, UTCMilliseconds, .keep_all = TRUE) %>%
    dplyr::select(-UTCround)

#### Update event IDs ----

# The event lengths (measured in minutes) are too long
event_len_mins <- tapply(A5C_wsl$UTC, A5C_wsl$event.id, function(x)
    difftime(max(x), min(x), units = 'mins'))
hist(event_len_mins,
     br = 30,
     main = paste("Lengths of", length(unique(A5C_wsl$event.id)), "events"),
     las = 1)

# A5C_wsl has the data sorted chronologically, so
# make sure the event IDs are not interrupted/repeated at a different time
tmp <- rle(A5C_wsl$event.id)

# Repeat a unique number to paste with the current event.ID
NumUn <- rep(1:length(tmp$lengths), times = tmp$lengths)
NumUn <- stringr::str_pad(NumUn, floor(log10(max(NumUn))) + 1, pad = "0")

# Update the event IDs
A5C_wsl$event.id <- paste0(A5C_wsl$event.id, "_", NumUn)

# Update event lengths and replot
event_len_mins <- tapply(A5C_wsl$UTC, A5C_wsl$event.id, function(x)
    difftime(max(x), min(x), units = 'mins'))
hist(event_len_mins,
     br = 30,
     main = paste("Lengths of", length(unique(A5C_wsl$event.id)), "events"),
     las = 1)


# Split events if time gap > TT minutes
# Time threshold, minutes
TT <- 5

# Select events longer than TT, to check
# (the gap of TT minutes cannot occur in shorter events)
longerTT <- names(event_len_mins[event_len_mins > TT])

for (ev in longerTT) {
    # ev = "A5C_2016MarApr_AutoEvent10_0008" # no break
    # ev = "A5C_2016MayJun_AutoEvent7_0345" # with a break
    utc <- A5C_wsl %>%
        filter(event.id == ev) %>%
        pull(UTC)

    # Time differences, minutes
    td <- diff(unclass(utc)) / 60

    if (any(td > TT)) {
        NumUn2 <- rep(NA, length(utc))
        NumUn2[1] <- 0L
        NumUn2[which(td > TT)] <- 1:sum(td > TT)
        NumUn2 <- copyforward(NumUn2)

        # Update the main dataset with new IDs
        A5C_wsl$event.id[A5C_wsl$event.id == ev] <- paste0(A5C_wsl$event.id[A5C_wsl$event.id == ev], "_", NumUn2)
    }
}

# Update event lengths and replot
event_len_mins <- tapply(A5C_wsl$UTC, A5C_wsl$event.id, function(x)
    difftime(max(x), min(x), units = 'mins'))
hist(event_len_mins,
     br = 30,
     main = paste("Lengths of", length(unique(A5C_wsl$event.id)), "events"),
     las = 1)


# Combine short events < TT minutes
# Calculate time gaps between events,
# merge if the gap <= TT and one of the events <= TT minutes long.
event_startend <- tapply(A5C_wsl$UTC, A5C_wsl$event.id, function(x)
    c(Start = min(x), End = max(x)))
tmp <- names(event_startend)
n <- length(event_startend)
event_startend <- event_startend %>%
    bind_rows() %>%
    mutate(event.id = tmp) %>%
    arrange(Start) %>%
    mutate(Gap = c(NA, difftime(Start[-1], End[-n], units = 'mins')),
           Len = difftime(End, Start, units = 'mins')) %>%
    mutate(Merge2previous = (Gap <= TT) & (Len <= TT | c(NA, Len[-n]) <= TT))
mean(event_startend$Merge2previous, na.rm = TRUE)
# 0.08115183
summary(event_startend)

# Check if there are > 1 consecutive events to be merged
RLE <- rle(event_startend$Merge2previous)
RLE <- tibble(lengths = RLE$lengths, values = RLE$values)
for (i in 2:nrow(RLE)) { # i = 3
    if (RLE$values[i]) {
        istart <- sum(RLE$lengths[1:(i - 1)])
        iend <- sum(RLE$lengths[1:i])
        newname <- paste0(event_startend$event.id[istart],
                          "_mergedwith_",
                          RLE$lengths[i])
        events2merge <- event_startend$event.id[istart:iend]
        A5C_wsl$event.id[A5C_wsl$event.id %in% events2merge] <- newname
    }
}

# Update event lengths and replot
event_len_mins <- tapply(A5C_wsl$UTC, A5C_wsl$event.id, function(x)
    difftime(max(x), min(x), units = 'mins'))
hist(event_len_mins,
     br = 30,
     main = paste("Lengths of", length(unique(A5C_wsl$event.id)), "events"),
     las = 1)

### Add more variables and save ----

# Combine data and create more variables
A5C_wsl <- A5C_wsl %>%
    rename(call.id = UID) %>%
    mutate(call.id = paste(event.id, call.id, sep = "-")) %>%
    mutate(Year = as.numeric(format(UTC, "%Y")),
           Month = as.numeric(format(UTC, "%m")))

# Remove columns with non-unique values (all values are the same)
inu <- apply(A5C_wsl, 2, function(x) length(unique(x)) == 1)
names(A5C_wsl)[inu]
# [1] "ChannelBitmap"   "SequenceBitmap"  "UpdateOf"
# [4] "classifierUsed"  "classifier2Used" "spList"
# [7] "dcMean"          "dcStdDev"        "dcQuarter1Mean"
# [10] "dcQuarter2Mean"  "dcQuarter3Mean"  "dcQuarter4Mean"
# [13] "freqPeak"        "bw3db"           "bw3dbLow"
# [16] "bw3dbHigh"       "bw10db"          "bw10dbLow"
# [19] "bw10dbHigh"      "rmsSignal"       "rmsNoise"
# [22] "snr"             "nCrossings"      "sweepRate"
# [25] "meanTimeZC"      "medianTimeZC"    "varianceTimeZC"
# [28] "whaleTrain"      "clickType"       "ici"
# [31] "offlineEventID"  "latitude"        "longitude"
A5C_wsl <- A5C_wsl[, !inu]

# Check counts
with(A5C_wsl,
     table(Source, Year)
)
#         Year
# Source  2016 2017 2018 2021 2022
# A5C     7568 3320  569    0    0
# MBuoy      0    0    0 1186  193

dim(A5C_wsl)
# [1] 12836    65

dim(A5C_wsl)[1] == length(unique(A5C_wsl$call.id))

# Save data
saveRDS(A5C_wsl, file = "dataderived/dataprocess_A5C_wsl.rds")
readr::write_csv(A5C_wsl, file = "dataderived/dataprocess_A5C_wsl.csv")


## Maryland CBay BD ----
rm(list = ls())

library(dplyr)
library(readr)
library(readxl)

source("code/misc.R")

CB_2019Summer_wsl <- read_csv("dataraw/bottle_ChesBay/Rocca_Whistle_Stats_ChesBay_Summer2019_trueBD_10db.csv") %>%
    mutate(event.id = paste0("CB_2019Summer_", sightingNum)) %>%
    arrange(UTC) %>%
    mutate(Source = "CB") %>%
    mutate(UTCround = as.integer(UTC),
           UTCMilliseconds = as.integer(UTCMilliseconds)) %>%
    distinct(UTCround, UTCMilliseconds, .keep_all = TRUE) %>%
    dplyr::select(-UTCround)

#### Update event IDs ----

# The event lengths (measured in minutes) are too long
event_len_mins <- tapply(CB_2019Summer_wsl$UTC, CB_2019Summer_wsl$event.id, function(x)
    difftime(max(x), min(x), units = 'mins'))
hist(event_len_mins,
     br = 30,
     main = paste("Lengths of", length(unique(CB_2019Summer_wsl$event.id)), "events"),
     las = 1)

# CB_2019Summer_wsl has the data sorted chronologically, so
# make sure the event IDs are not interrupted/repeated at a different time
tmp <- rle(CB_2019Summer_wsl$event.id)

# Repeat a unique number to paste with the current event.ID
NumUn <- rep(1:length(tmp$lengths), times = tmp$lengths)
NumUn <- stringr::str_pad(NumUn, floor(log10(max(NumUn))) + 1, pad = "0")

# Update the event IDs
CB_2019Summer_wsl$event.id <- paste0(CB_2019Summer_wsl$event.id, "_", NumUn)

# Update event lengths and replot
event_len_mins <- tapply(CB_2019Summer_wsl$UTC, CB_2019Summer_wsl$event.id, function(x)
    difftime(max(x), min(x), units = 'mins'))
hist(event_len_mins,
     br = 30,
     main = paste("Lengths of", length(unique(CB_2019Summer_wsl$event.id)), "events"),
     las = 1)


# Split events if time gap > TT minutes
# Time threshold, minutes
TT <- 5

# Select events longer than TT, to check
# (the gap of TT minutes cannot occur in shorter events)
longerTT <- names(event_len_mins[event_len_mins > TT])

for (ev in longerTT) {
    # ev = "A5C_2016MarApr_AutoEvent10_0008" # no break
    # ev = "A5C_2016MayJun_AutoEvent7_0345" # with a break
    utc <- CB_2019Summer_wsl %>%
        filter(event.id == ev) %>%
        pull(UTC)

    # Time differences, minutes
    td <- diff(unclass(utc)) / 60

    if (any(td > TT)) {
        NumUn2 <- rep(NA, length(utc))
        NumUn2[1] <- 0L
        NumUn2[which(td > TT)] <- 1:sum(td > TT)
        NumUn2 <- copyforward(NumUn2)

        # Update the main dataset with new IDs
        CB_2019Summer_wsl$event.id[CB_2019Summer_wsl$event.id == ev] <- paste0(CB_2019Summer_wsl$event.id[CB_2019Summer_wsl$event.id == ev], "_", NumUn2)
    }
}

# Update event lengths and replot
event_len_mins <- tapply(CB_2019Summer_wsl$UTC, CB_2019Summer_wsl$event.id, function(x)
    difftime(max(x), min(x), units = 'mins'))
hist(event_len_mins,
     br = 30,
     main = paste("Lengths of", length(unique(CB_2019Summer_wsl$event.id)), "events"),
     las = 1)


# Combine short events < TT minutes
# Calculate time gaps between events,
# merge if the gap <= TT and one of the events <= TT minutes long.
event_startend <- tapply(CB_2019Summer_wsl$UTC, CB_2019Summer_wsl$event.id, function(x)
    c(Start = min(x), End = max(x)))
tmp <- names(event_startend)
n <- length(event_startend)
event_startend <- event_startend %>%
    bind_rows() %>%
    mutate(event.id = tmp) %>%
    arrange(Start) %>%
    mutate(Gap = c(NA, difftime(Start[-1], End[-n], units = 'mins')),
           Len = difftime(End, Start, units = 'mins')) %>%
    mutate(Merge2previous = (Gap <= TT) & (Len <= TT | c(NA, Len[-n]) <= TT))
mean(event_startend$Merge2previous, na.rm = TRUE)
# 0
summary(event_startend)

# Check if there are > 1 consecutive events to be merged
RLE <- rle(event_startend$Merge2previous)
RLE <- tibble(lengths = RLE$lengths, values = RLE$values)
for (i in 2:nrow(RLE)) { # i = 3
    if (RLE$values[i]) {
        istart <- sum(RLE$lengths[1:(i - 1)])
        iend <- sum(RLE$lengths[1:i])
        newname <- paste0(event_startend$event.id[istart],
                          "_mergedwith_",
                          RLE$lengths[i])
        events2merge <- event_startend$event.id[istart:iend]
        CB_2019Summer_wsl$event.id[CB_2019Summer_wsl$event.id %in% events2merge] <- newname
    }
}

# Update event lengths and replot
event_len_mins <- tapply(CB_2019Summer_wsl$UTC, CB_2019Summer_wsl$event.id, function(x)
    difftime(max(x), min(x), units = 'mins'))
hist(event_len_mins,
     br = 30,
     main = paste("Lengths of", length(unique(CB_2019Summer_wsl$event.id)), "events"),
     las = 1)

### Add more variables and save ----

# Combine data and create more variables
CB_2019Summer_wsl <- CB_2019Summer_wsl %>%
    rename(call.id = UID) %>%
    mutate(call.id = paste(event.id, call.id, sep = "-")) %>%
    mutate(Year = as.numeric(format(UTC, "%Y")),
           Month = as.numeric(format(UTC, "%m")))

# Remove columns with non-unique values (all values are the same)
inu <- apply(CB_2019Summer_wsl, 2, function(x) length(unique(x)) == 1)
inu["Source"] <- inu["Year"] <- FALSE # make sure these variables stay in the dataset
inu["freqStepUp"] <- FALSE
names(CB_2019Summer_wsl)[inu]
CB_2019Summer_wsl <- CB_2019Summer_wsl[, !inu]

# Check counts
with(CB_2019Summer_wsl,
     table(Source, Year)
)
#        Year
# Source 2019
# CB      102

dim(CB_2019Summer_wsl)
# [1] 102  64

dim(CB_2019Summer_wsl)[1] == length(unique(CB_2019Summer_wsl$call.id))

# Save data
saveRDS(CB_2019Summer_wsl, file = "dataderived/dataprocess_CB_2019Summer_wsl.rds")
readr::write_csv(CB_2019Summer_wsl, file = "dataderived/dataprocess_CB_2019Summer_wsl.csv")

