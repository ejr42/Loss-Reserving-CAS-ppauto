# Chain-Ladder Loss Reserving Model
# ---------------------------------------------------------------------------
# Data: CAS Loss Reserving Database, Private Passenger Auto
#       (raw company-level Schedule P extract, 1998-2007 accident years)
# Method: Volume-weighted chain-ladder.
#
# What Chain-Ladder does:
# Claims take several years to fully pay out. At any point, an insurer knows 
# what it has paid on a group of claims (grouped by accident year) but not what
# it will eventually pay. Chain-ladder assumes older, more developed accident
# years show a stable growth pattern, and applies that same pattern to
# younger years to estimate their eventual ultimate cost. The gap between
# projected ultimate and what's been paid so far is the IBNR reserve (Incurred 
# But Not Reported).

raw_file <- "Data/ppauto_pos98-07 (1).csv"

# ---------------------------------------------------------------------------
# STEP 1: Load the raw file and aggregate across all companies.
# The raw CSV is one row per (company, accident year, development lag)
# we sum CumPaidLoss across every company (GRCODE) to get an industry level
# aggregate triangle.
# ---------------------------------------------------------------------------

raw <- read.csv(raw_file)

agg <- aggregate(CumPaidLoss ~ AccidentYear + DevelopmentLag + DevelopmentYear,
                 data = raw, FUN = sum)

# ---------------------------------------------------------------------------
# STEP 2: Cut off at the evaluation date.
# Keep only cells where DevelopmentYear <= the latest AccidentYear in the data
# (2007 in this case) i.e. only what would have actually been observed by then.
# ---------------------------------------------------------------------------

eval_year <- max(agg$AccidentYear)
agg <- agg[agg$DevelopmentYear <= eval_year,]

cat(sprintf("Evaluation year (cutoff): %d\n\n", eval_year))

# ---------------------------------------------------------------------------
# STEP 3: Reshape from long format to a wide triangle.
# Rows = accident year, columns = development lag (1 = first 12 months,
# 2 = next 12 months, etc.). Cells beyond the evaluation cutoff are
# automatically NA, since we already removed those rows.
# ---------------------------------------------------------------------------

triangle <- reshape(agg[, c("AccidentYear", "DevelopmentLag", "CumPaidLoss")],
                    idvar = "AccidentYear", timevar = "DevelopmentLag",
                    direction = "wide")

rownames(triangle) <- triangle$AccidentYear
triangle$AccidentYear <- NULL
colnames(triangle) <- sub("CumPaidLoss\\.", "", colnames(triangle))
triangle <- triangle[, order(as.numeric(colnames(triangle)))]

devs <- as.numeric(colnames(triangle))
n <- length(devs)

cat("Loss triangle (cumulative paid losses, $):\n")
print(round(triangle, 0))
cat("\n")

# ---------------------------------------------------------------------------
# STEP 4: Calculating age-to-age factors.
# For each pair of consecutive lags, look at every accident year where both
# values are known, and take the ratio of total losses at the later lag to
# total losses at the earlier lag (a volume-weighted average).
# ---------------------------------------------------------------------------

ldfs <- numeric(n - 1)
names(ldfs) <- paste0(devs[1:(n - 1)], "-", devs[2:n])

for (i in 1:(n - 1))
  {
  col_a <- triangle[[i]]
  col_b <- triangle[[i + 1]]
  both_known <- !is.na(col_a) & !is.na(col_b)
  ldfs[i] <- sum(col_b[both_known]) / sum(col_a[both_known])
  }

cat("Age-to-age development factors (LDFs):\n")
for (i in seq_along(ldfs))
  {
  cat(sprintf("  Lag %s: %.4f\n", names(ldfs)[i], ldfs[i]))
  }
cat("\n")

# ---------------------------------------------------------------------------
# STEP 5: Chain the factors together into cumulative development factors
# (CDFs), "if I'm sitting at lag X, what do I multiply by to reach
# ultimate?", by multiplying the remaining factors together.
# ---------------------------------------------------------------------------

cdfs <- numeric(n)
for (i in 1:n)
  {
  cdfs[i] <- if (i < n) prod(ldfs[i:(n - 1)]) else 1.0
  }
names(cdfs) <- devs

cat("Cumulative development factors (lag-to-ultimate):\n")
for (i in seq_along(cdfs))
  {
  cat(sprintf("  Lag %s -> Ultimate: %.4f\n", names(cdfs)[i], cdfs[i]))
  }
cat("\n")

# ---------------------------------------------------------------------------
# STEP 6: Project ultimate losses and IBNR for each accident year.
# ---------------------------------------------------------------------------

accident_years <- rownames(triangle)
results <- data.frame(
  AccidentYear = accident_years,
  LatestLag = NA_real_,
  LatestPaid = NA_real_,
  Ultimate = NA_real_,
  IBNR = NA_real_
)

for (i in 1:nrow(triangle))
  {
  row_vals <- as.numeric(triangle[i, ])
  known_idx <- which(!is.na(row_vals))
  last_idx <- max(known_idx)
  
  latest_lag <- devs[last_idx]
  latest_paid <- row_vals[last_idx]
  cdf <- cdfs[last_idx]
  ultimate <- latest_paid * cdf
  
  results$LatestLag[i] <- latest_lag
  results$LatestPaid[i] <- latest_paid
  results$Ultimate[i] <- ultimate
  results$IBNR[i] <- ultimate - latest_paid
  }

cat("Reserve summary by accident year:\n")
print(results)
cat("\n")
cat(sprintf("TOTAL IBNR RESERVE: $%s\n",
            format(round(sum(results$IBNR)),
                   big.mark = ",")))

# ---------------------------------------------------------------------------
# STEP 7: Visualizing the development pattern.
# ---------------------------------------------------------------------------

plot(NULL, xlim = range(devs), ylim = c(0, max(triangle, na.rm = TRUE)),
     xlab = "Development Lag (years)", ylab = "Cumulative Paid Losses ($)",
     main = "Private Passenger Auto Loss Development by Accident Year")

colors <- rainbow(nrow(triangle))
for (i in 1:nrow(triangle))
  {
  row_vals <- as.numeric(triangle[i, ])
  known_idx <- which(!is.na(row_vals))
  lines(devs[known_idx], row_vals[known_idx], type = "o", col = colors[i],
        pch = 16)
  }

legend("bottomright", legend = accident_years, col = colors, lty = 1, pch = 16,
       cex = 0.7, ncol = 2, title = "Accident Year")
