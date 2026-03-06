## --- Brey (1990) production calculation for stacked macrofauna data ---

# Set working directory
setwd("C:/Users/dc07/OneDrive - CEFAS/Desktop/Secondary production data for Clem")

infile  <- "FUTEVID.macro.RData"
outfile <- "FUTEVID.macro.Brey1990Production.RData"

# 1) Load the RData into a temporary environment
e <- new.env()
load(infile, envir = e)

# 2) Identify the data.frame to use (force FUTEVID.macro)
df_names <- ls(e)

if (!"FUTEVID.macro" %in% df_names) {
  stop(
    "Object 'FUTEVID.macro' not found in the RData file. Objects present: ",
    paste(df_names, collapse = ", ")
  )
}

if (!is.data.frame(e[["FUTEVID.macro"]])) {
  stop("Object 'FUTEVID.macro' exists but is not a data.frame (it is: ", class(e[["FUTEVID.macro"]])[1], ").")
}

dat <- e[["FUTEVID.macro"]]

# 3) Check required columns
required <- c("abn", "bio_AFDW", "ScientificName_accepted")
missing <- setdiff(required, names(dat))
if (length(missing) > 0) {
  stop("Missing required column(s): ", paste(missing, collapse = ", "))
}

# 4) Compute Wmean
dat$Wmean <- with(dat,
                  ifelse(!is.na(abn) & !is.na(bio_AFDW) & abn > 0 & bio_AFDW > 0,
                         bio_AFDW / abn,
                         NA_real_))

# 5) Compute log10(P) only where valid
log10P <- with(dat,
               ifelse(!is.na(Wmean) & Wmean > 0 & !is.na(bio_AFDW) & bio_AFDW > 0,
                      -0.473 + 1.007 * log10(bio_AFDW) - 0.274 * log10(Wmean),
                      NA_real_))

dat$prod_brey1990 <- ifelse(!is.na(log10P), 10^log10P, NA_real_)

# 6) Force production to 0 where abundance OR AFDW biomass equals 0
dat$prod_brey1990[!is.na(dat$abn) & !is.na(dat$bio_AFDW) &
                    (dat$abn == 0 | dat$bio_AFDW == 0)] <- 0

# 7) Put modified data frame back into environment
e[[df_name]] <- dat

# 8) Save new RData file
save(list = ls(e), envir = e, file = outfile)

message("Done. Added columns: Wmean, prod_brey1990")
message("Saved to: ", outfile)