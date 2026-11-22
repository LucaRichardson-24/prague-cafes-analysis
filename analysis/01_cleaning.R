# 01_cleaning.R
# raw_data.csv -> dataset.csv

raw <- read.csv("analysis/raw_data.csv", stringsAsFactors = FALSE,
                check.names = FALSE, encoding = "UTF-8")


# ---- 1. Inspect ----

dim(raw)
str(raw)
sapply(raw, class)
colSums(is.na(raw) | raw == "")


# ---- 2. Rename columns ----

stopifnot(ncol(raw) == 26)

names(raw) <- c("collector", "visit_time", "district_raw", "area",
                "cafe", "address", "coords_raw", "gmaps_link",
                "espresso_raw", "cappuccino_raw", "flat_white_raw",
                "filter_raw", "payment_raw", "seats_in_raw",
                "seats_out_raw", "rating_raw", "n_ratings_raw",
                "type_raw", "roastery_raw", "photo", "note",
                "auto_marker", "distance_raw", "lat_sheet",
                "lon_sheet", "adj_rating_sheet")

raw$row_id <- seq_len(nrow(raw))

# trim spaces in all text cells
for (col in names(raw)) {
  if (is.character(raw[[col]])) raw[[col]] <- trimws(raw[[col]])
}


# ---- 3. Empty and duplicated rows ----

empty <- raw$cafe == "" & raw$collector == ""
sum(empty)
raw[empty, c("cafe", "payment_raw", "distance_raw")][1:3, ]

d <- raw[!empty, ]
nrow(d)

sum(duplicated(d[, names(d) != "row_id"]))


# ---- 4. Categories ----

table(d$district_raw, useNA = "ifany")
table(d$type_raw, useNA = "ifany")
table(d$payment_raw, useNA = "ifany")

# district: keep the number, rebuild the label (fixes "prauge 1" etc.)
district_num <- as.integer(gsub("[^0-9]", "", d$district_raw))
d$district <- paste("Prague", district_num)
table(d$district_raw, d$district)

# blank cafe type -> NA
d$cafe_type <- d$type_raw
d$cafe_type[d$cafe_type == ""] <- NA
d$cafe_type <- factor(d$cafe_type,
                      levels = c("Independent", "Local Chain",
                                 "International Chain"))
table(d$type_raw, d$cafe_type, useNA = "ifany")

d$payment <- factor(d$payment_raw)

table(d$roastery_raw, useNA = "ifany")
d$is_roastery <- as.integer(d$roastery_raw)


# ---- 5. Prices ----
# Several prices in one cell: keep the first one.
# "NA", "na" and blank all mean the drink is not served -> NA.

price_cols <- c("espresso_raw", "cappuccino_raw",
                "flat_white_raw", "filter_raw")

for (col in price_cols) {
  x <- d[[col]]
  odd <- !is.na(x) & grepl("[^0-9.]", x)
  cat("\n", col, ":", sum(odd), "cells that are not a plain number\n")
  print(d[odd, c("row_id", "cafe", col)])
}

first_price <- function(x) {
  x <- as.character(x)
  x[x %in% c("", "NA", "na")] <- NA
  first <- sub("[/,].*$", "", x)
  as.numeric(trimws(first))
}

d$espresso   <- first_price(d$espresso_raw)
d$cappuccino <- first_price(d$cappuccino_raw)
d$flat_white <- first_price(d$flat_white_raw)
d$filter     <- first_price(d$filter_raw)

check <- grepl("[/,]", d$espresso_raw) | grepl("[/,]", d$cappuccino_raw) |
         grepl("[/,]", d$flat_white_raw) | grepl("[/,]", d$filter_raw)
d[check, c("cafe", "espresso_raw", "espresso", "cappuccino_raw",
           "cappuccino", "flat_white_raw", "flat_white",
           "filter_raw", "filter")]


# ---- 6. Other numbers ----

# thousands separator in number of ratings ("1,020")
odd <- d$n_ratings_raw != "" & grepl("[^0-9]", d$n_ratings_raw)
d[odd, c("cafe", "n_ratings_raw")]

d$n_ratings <- as.numeric(gsub(",", "", d$n_ratings_raw))
d[odd, c("cafe", "n_ratings_raw", "n_ratings")]

d$rating      <- as.numeric(d$rating_raw)
d$seats_in    <- as.numeric(d$seats_in_raw)
d$seats_out   <- as.numeric(d$seats_out_raw)
d$distance_km <- as.numeric(d$distance_raw)


# ---- 7. Coordinates ----

parts <- strsplit(d$coords_raw, ",")
d$lat <- as.numeric(trimws(sapply(parts, `[`, 1)))
d$lon <- as.numeric(trimws(sapply(parts, `[`, 2)))
sum(is.na(d$lat) | is.na(d$lon))
head(d[, c("coords_raw", "lat", "lon")])


# ---- 8. Same cafe entered twice ----
# Pairs under 50 m apart from different students, checked by hand.

near_pairs <- data.frame()
for (i in 1:(nrow(d) - 1)) {
  for (j in (i + 1):nrow(d)) {
    metres <- sqrt(((d$lat[i] - d$lat[j]) * 111000)^2 +
                   ((d$lon[i] - d$lon[j]) * 71000)^2)
    if (metres < 50 && d$collector[i] != d$collector[j]) {
      near_pairs <- rbind(near_pairs, data.frame(
        row_a = d$row_id[i], cafe_a = d$cafe[i],
        row_b = d$row_id[j], cafe_b = d$cafe[j],
        metres = round(metres)))
    }
  }
}
near_pairs

# same name and address = same cafe (row_id numbers)
dup_pairs <- rbind(
  c(2, 144),    # Starbucks Reserve
  c(9, 103),    # Cafe Louvre
  c(44, 97),    # Cafe Vescovi
  c(47, 158),   # Cafe Savoy
  c(49, 159),   # Kampa Cafe
  c(57, 196),   # Caffe Milani
  c(94, 142),   # Duo cafe & bistro
  c(102, 198),  # Cafe-Cafe
  c(106, 200),  # Republica Coffee
  c(110, 197),  # Mondieu
  c(111, 131)   # No26 Espresso
)

# keep the more complete row; if equal, keep the one entered first
used <- c("espresso", "cappuccino", "flat_white", "filter",
          "seats_in", "seats_out", "rating", "n_ratings", "cafe_type")
n_missing <- rowSums(is.na(d[, used]))
names(n_missing) <- d$row_id

drop_ids <- c()
for (k in 1:nrow(dup_pairs)) {
  first  <- dup_pairs[k, 1]
  second <- dup_pairs[k, 2]
  if (n_missing[as.character(second)] < n_missing[as.character(first)]) {
    drop_ids <- c(drop_ids, first)
  } else {
    drop_ids <- c(drop_ids, second)
  }
}

pair_rows <- d[d$row_id %in% dup_pairs, c("row_id", "cafe", "collector",
               "espresso", "filter", "cafe_type", "is_roastery")]
pair_rows$dropped <- pair_rows$row_id %in% drop_ids
pair_rows[order(pair_rows$cafe), ]

d <- d[!(d$row_id %in% drop_ids), ]
nrow(d)


# ---- 9. New variables ----

d$offers_filter <- as.integer(!is.na(d$filter))
d$n_features    <- d$is_roastery + d$offers_filter
d$chain         <- ifelse(d$cafe_type == "Independent", 0, 1)

# km from the Orloj (50.0871 N, 14.4207 E); positive = east / north
d$east_km  <- (d$lon - 14.4207) * 71
d$north_km <- (d$lat - 50.0871) * 111

table(d$offers_filter, is.na(d$filter_raw) | d$filter_raw %in% c("", "na"))
table(d$n_features, d$is_roastery)
table(d$cafe_type, d$chain, useNA = "ifany")


# ---- 10. Check and save ----

clean <- d[, c("row_id", "collector", "cafe", "district", "area",
               "cafe_type", "chain", "payment", "is_roastery",
               "espresso", "cappuccino", "flat_white", "filter",
               "offers_filter", "n_features", "seats_in", "seats_out",
               "rating", "n_ratings", "distance_km", "lat", "lon",
               "east_km", "north_km")]

cat("Rows in raw file:", nrow(raw), " Rows in clean data:", nrow(clean), "\n")
colSums(is.na(clean))
summary(clean[, c("espresso", "cappuccino", "flat_white", "filter",
                  "rating", "n_ratings", "seats_in", "seats_out",
                  "distance_km", "east_km", "north_km")])

stopifnot(all(clean$rating >= 1 & clean$rating <= 5, na.rm = TRUE))
stopifnot(all(clean[, c("espresso", "cappuccino", "flat_white", "filter")] > 0,
              na.rm = TRUE))

write.csv(clean, "analysis/dataset.csv", row.names = FALSE)
