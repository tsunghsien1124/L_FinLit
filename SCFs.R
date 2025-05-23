# housekeeping -----------------------------------------------------------------
rm(list = ls())
username <- "User"
# username <- "Tsung-Hsien Li"
setwd(paste0("C:/Users/", username, "/Desktop/Chapter 1"))

# library ----------------------------------------------------------------------
library(dplyr)

# load SCF data sets -----------------------------------------------------------
years.to.load <- 2016
load(paste0("raw data/scf", years.to.load, ".rda"))
df.imp <- as.data.frame(rbind(imp1, imp2, imp3, imp4, imp5))
scf.data.2016 <- df.imp
rm(imp1, imp2, imp3, imp4, imp5, rw, df.imp)
gc()

years.to.load <- 2019
load(paste0("raw data/scf", years.to.load, ".rda"))
df.imp <- as.data.frame(rbind(imp1, imp2, imp3, imp4, imp5))
scf.data.2019 <- df.imp
rm(imp1, imp2, imp3, imp4, imp5, rw, df.imp)
gc()

# years.to.load <- 2022
# load(paste0("raw data/scf", years.to.load, ".rda"))
# df.imp <- as.data.frame(rbind(imp1, imp2, imp3, imp4, imp5))
# scf.data.2022 <- df.imp
# rm(imp1, imp2, imp3, imp4, imp5, rw, df.imp)
# gc()

scf.data.2016 <- scf.data.2016[(20 <= scf.data.2016$age) &
                                 (scf.data.2016$age <= 65), ]
scf.data.2019 <- scf.data.2019[(20 <= scf.data.2019$age) &
                                 (scf.data.2019$age <= 65), ]
# scf.data.2022 <- scf.data.2022[(20 <= scf.data.2022$age) &
#                                  (scf.data.2022$age <= 65), ]

scf.data.2016.wgt = sum(scf.data.2016$wgt)
scf.data.2019.wgt = sum(scf.data.2019$wgt)
# scf.data.2022.wgt = sum(scf.data.2022$wgt)
# scf.data.wgt = scf.data.2016.wgt + scf.data.2019.wgt + scf.data.2022.wgt
scf.data.wgt = scf.data.2016.wgt + scf.data.2019.wgt
scf.data.2016.wgt_share = scf.data.2016.wgt / scf.data.wgt
scf.data.2019.wgt_share = scf.data.2019.wgt / scf.data.wgt
# scf.data.2022.wgt_share = scf.data.2022.wgt / scf.data.wgt

# financial literacy -----------------------------------------------------------
scf.data.2016$FinLit_int <- as.integer(scf.data.2016$x7559 == 1)
scf.data.2016$FinLit_inf <- as.integer(scf.data.2016$x7560 == 5)
scf.data.2016$FinLit_sto <- as.integer(scf.data.2016$x7558 == 5)
scf.data.2016$FinLit <- scf.data.2016$FinLit_sto + scf.data.2016$FinLit_int + scf.data.2016$FinLit_inf
FinLit_int.share.2016 <- sum(scf.data.2016$FinLit_int * scf.data.2016$wgt) / sum(scf.data.2016$wgt) * 100
FinLit_inf.share.2016 <- sum(scf.data.2016$FinLit_inf * scf.data.2016$wgt) / sum(scf.data.2016$wgt) * 100
FinLit_sto.share.2016 <- sum(scf.data.2016$FinLit_sto * scf.data.2016$wgt) / sum(scf.data.2016$wgt) * 100
FinLit.share.2016 <- sum((scf.data.2016$FinLit == 3) * scf.data.2016$wgt) / sum(scf.data.2016$wgt) * 100
scf.data.2016.wgt.FinLit_int_1 <- sum(scf.data.2016$FinLit_int * scf.data.2016$wgt)
scf.data.2016.wgt.FinLit_int_0 <- scf.data.2016.wgt - scf.data.2016.wgt.FinLit_int_1

scf.data.2019$FinLit_int <- as.integer(scf.data.2019$x7559 == 1)
scf.data.2019$FinLit_inf <- as.integer(scf.data.2019$x7560 == 5)
scf.data.2019$FinLit_sto <- as.integer(scf.data.2019$x7558 == 5)
scf.data.2019$FinLit <- scf.data.2019$FinLit_sto + scf.data.2019$FinLit_int + scf.data.2019$FinLit_inf
FinLit_int.share.2019 <- sum(scf.data.2019$FinLit_int * scf.data.2019$wgt) / sum(scf.data.2019$wgt) * 100
FinLit_inf.share.2019 <- sum(scf.data.2019$FinLit_inf * scf.data.2019$wgt) / sum(scf.data.2019$wgt) * 100
FinLit_sto.share.2019 <- sum(scf.data.2019$FinLit_sto * scf.data.2019$wgt) / sum(scf.data.2019$wgt) * 100
FinLit.share.2019 <- sum((scf.data.2019$FinLit == 3) * scf.data.2019$wgt) / sum(scf.data.2019$wgt) * 100
scf.data.2019.wgt.FinLit_int_1 <- sum(scf.data.2019$FinLit_int * scf.data.2019$wgt)
scf.data.2019.wgt.FinLit_int_0 <- scf.data.2019.wgt - scf.data.2019.wgt.FinLit_int_1

# scf.data.2022$FinLit_sto <- as.integer(scf.data.2022$x7558 == 5)
# scf.data.2022$FinLit_int <- as.integer(scf.data.2022$x7559 == 1)
# scf.data.2022$FinLit_inf <- as.integer(scf.data.2022$x7560 == 5)
# scf.data.2022$FinLit <- scf.data.2022$FinLit_sto + scf.data.2022$FinLit_int + scf.data.2022$FinLit_inf
# FinLit_int.share.2022 <- sum(scf.data.2022$FinLit_int * scf.data.2022$wgt) / sum(scf.data.2022$wgt) * 100
# FinLit.share.2022 <- sum((scf.data.2022$FinLit == 3) * scf.data.2022$wgt) / sum(scf.data.2022$wgt) * 100

df_FinLit_share <- data.frame(
  year = c(2016, 2019, "Pool"),
  FinLit_share = c(
    FinLit.share.2016,
    FinLit.share.2019,
    scf.data.2016.wgt_share * FinLit.share.2016 + scf.data.2019.wgt_share * FinLit.share.2019
  ),
  FinLit_int_share = c(
    FinLit_int.share.2016,
    FinLit_int.share.2019,
    scf.data.2016.wgt_share * FinLit_int.share.2016 + scf.data.2019.wgt_share * FinLit_int.share.2019
  ),
  FinLit_inf_share = c(
    FinLit_inf.share.2016,
    FinLit_inf.share.2019,
    scf.data.2016.wgt_share * FinLit_inf.share.2016 + scf.data.2019.wgt_share * FinLit_inf.share.2019
  ),
  FinLit_sto_share = c(
    FinLit_sto.share.2016,
    FinLit_sto.share.2019,
    scf.data.2016.wgt_share * FinLit_sto.share.2016 + scf.data.2019.wgt_share * FinLit_sto.share.2019
  )
)
write.csv(df_FinLit_share, "FinLit_share.csv", row.names = FALSE)

# credit card holding ----------------------------------------------------------
scf.data.2016$CCH <- as.numeric(scf.data.2016$x7973 == 1)
CCH.weighted.FinLit_int_1.2016 <- sum(scf.data.2016$CCH * scf.data.2016$FinLit_int * scf.data.2016$wgt) / sum(scf.data.2016$FinLit_int * scf.data.2016$wgt) * 100
CCH.weighted.FinLit_int_0.2016 <- sum(scf.data.2016$CCH * (1.0 - scf.data.2016$FinLit_int) * scf.data.2016$wgt) / sum((1.0 - scf.data.2016$FinLit_int) * scf.data.2016$wgt) * 100

scf.data.2019$CCH <- as.numeric(scf.data.2019$x7973 == 1)
CCH.weighted.FinLit_int_1.2019 <- sum(scf.data.2019$CCH * scf.data.2019$FinLit_int * scf.data.2019$wgt) / sum(scf.data.2019$FinLit_int * scf.data.2019$wgt) * 100
CCH.weighted.FinLit_int_0.2019 <- sum(scf.data.2019$CCH * (1.0 - scf.data.2019$FinLit_int) * scf.data.2019$wgt) / sum((1.0 - scf.data.2019$FinLit_int) * scf.data.2019$wgt) * 100

CCH.weighted.FinLit_int_1.pool <- scf.data.2016.wgt_share * CCH.weighted.FinLit_int_1.2016 + scf.data.2019.wgt_share * CCH.weighted.FinLit_int_1.2019
CCH.weighted.FinLit_int_0.pool <- scf.data.2016.wgt_share * CCH.weighted.FinLit_int_0.2016 + scf.data.2019.wgt_share * CCH.weighted.FinLit_int_0.2019

# fraction of credit card borrowers --------------------------------------------
scf.data.2016$x413_pos <- as.numeric(scf.data.2016$x413 > 0)
frac_CCD.weighted.FinLit_int_1.2016 <- sum(
  scf.data.2016$x413_pos * scf.data.2016$CCH * scf.data.2016$FinLit_int * scf.data.2016$wgt
) / sum(scf.data.2016$CCH * scf.data.2016$FinLit_int * scf.data.2016$wgt) * 100
frac_CCD.weighted.FinLit_int_0.2016 <- sum(
  scf.data.2016$x413_pos * scf.data.2016$CCH * (1.0 - scf.data.2016$FinLit_int) * scf.data.2016$wgt
) / sum(scf.data.2016$CCH * (1.0 - scf.data.2016$FinLit_int) * scf.data.2016$wgt) * 100

scf.data.2019$x413_pos <- as.numeric(scf.data.2019$x413 > 0)
frac_CCD.weighted.FinLit_int_1.2019 <- sum(
  scf.data.2019$x413_pos * scf.data.2019$CCH * scf.data.2019$FinLit_int * scf.data.2019$wgt
) / sum(scf.data.2019$CCH * scf.data.2019$FinLit_int * scf.data.2019$wgt) * 100
frac_CCD.weighted.FinLit_int_0.2019 <- sum(
  scf.data.2019$x413_pos * scf.data.2019$CCH * (1.0 - scf.data.2019$FinLit_int) * scf.data.2019$wgt
) / sum(scf.data.2019$CCH * (1.0 - scf.data.2019$FinLit_int) * scf.data.2019$wgt) * 100

frac_CCD.weighted.FinLit_int_1.pool <- scf.data.2016.wgt_share * frac_CCD.weighted.FinLit_int_1.2016 + scf.data.2019.wgt_share * frac_CCD.weighted.FinLit_int_1.2019
frac_CCD.weighted.FinLit_int_0.pool <- scf.data.2016.wgt_share * frac_CCD.weighted.FinLit_int_0.2016 + scf.data.2019.wgt_share * frac_CCD.weighted.FinLit_int_0.2019

# credit card borrowing --------------------------------------------------------
scf.data.2016$CCD <- scf.data.2016$x413_pos * scf.data.2016$x413
CCD.weighted.2016 <- sum(scf.data.2016$CCD * scf.data.2016$x413_pos * scf.data.2016$CCH * scf.data.2016$wgt) / sum(scf.data.2016$x413_pos * scf.data.2016$CCH * scf.data.2016$wgt)
CCD.weighted.FinLit_int_1.2016 <- sum(
  scf.data.2016$CCD * scf.data.2016$x413_pos * scf.data.2016$CCH * scf.data.2016$FinLit_int * scf.data.2016$wgt
) / sum(
  scf.data.2016$x413_pos * scf.data.2016$CCH * scf.data.2016$FinLit_int * scf.data.2016$wgt
)
CCD.weighted.FinLit_int_0.2016 <- sum(
  scf.data.2016$CCD * scf.data.2016$x413_pos * scf.data.2016$CCH * (1.0 - scf.data.2016$FinLit_int) * scf.data.2016$wgt
) / sum(
  scf.data.2016$x413_pos * scf.data.2016$CCH * (1.0 - scf.data.2016$FinLit_int) * scf.data.2016$wgt
)

scf.data.2019$CCD <- scf.data.2019$x413_pos * scf.data.2019$x413
CCD.weighted.2019 <- sum(scf.data.2019$CCD * scf.data.2019$x413_pos * scf.data.2019$CCH * scf.data.2019$wgt) / sum(scf.data.2019$x413_pos * scf.data.2019$CCH * scf.data.2019$wgt)
CCD.weighted.FinLit_int_1.2019 <- sum(
  scf.data.2019$CCD * scf.data.2019$x413_pos * scf.data.2019$CCH * scf.data.2019$FinLit_int * scf.data.2019$wgt
) / sum(
  scf.data.2019$x413_pos * scf.data.2019$CCH * scf.data.2019$FinLit_int * scf.data.2019$wgt
)
CCD.weighted.FinLit_int_0.2019 <- sum(
  scf.data.2019$CCD * scf.data.2019$x413_pos * scf.data.2019$CCH * (1.0 - scf.data.2019$FinLit_int) * scf.data.2019$wgt
) / sum(
  scf.data.2019$x413_pos * scf.data.2019$CCH * (1.0 - scf.data.2019$FinLit_int) * scf.data.2019$wgt
)

# scf.data.2022$x413_pos <- as.numeric(scf.data.2022$x413 > 0)
# scf.data.2022$x413_debt <- scf.data.2022$x413_pos * scf.data.2022$x413
# scf.data.2022$CCD <- scf.data.2022$x413_debt
# CCD.weighted.2022 <- sum(scf.data.2022$CCD * scf.data.2022$wgt)
# CCD.weighted.FinLit_int_1.2022 <- sum(scf.data.2022$CCD * scf.data.2022$wgt * scf.data.2022$FinLit_int)
# CCD.weighted.FinLit_int_0.2022 <- sum(scf.data.2022$CCD * scf.data.2022$wgt * (1 - scf.data.2022$FinLit_int))

# aggregate debt-to-earnings ratio (D2E) [conditional] -----------------------------------
earnings.weighted.2016 <- sum(
  scf.data.2016$wageinc * scf.data.2016$x413_pos * scf.data.2016$CCH * scf.data.2016$wgt
) / sum(scf.data.2016$x413_pos * scf.data.2016$CCH * scf.data.2016$wgt)
earnings.weighted.FinLit_int_1.2016 <- sum(
  scf.data.2016$wageinc * scf.data.2016$x413_pos * scf.data.2016$CCH * scf.data.2016$FinLit_int * scf.data.2016$wgt
) / sum(
  scf.data.2016$x413_pos * scf.data.2016$CCH * scf.data.2016$FinLit_int * scf.data.2016$wgt
)
earnings.weighted.FinLit_int_0.2016 <- sum(
  scf.data.2016$wageinc * scf.data.2016$x413_pos * scf.data.2016$CCH * (1 - scf.data.2016$FinLit_int) * scf.data.2016$wgt
) / sum(
  scf.data.2016$x413_pos * scf.data.2016$CCH * (1 - scf.data.2016$FinLit_int) * scf.data.2016$wgt
)

earnings.weighted.2019 <- sum(
  scf.data.2019$wageinc * scf.data.2019$x413_pos * scf.data.2019$CCH * scf.data.2019$wgt
) / sum(scf.data.2019$x413_pos * scf.data.2019$CCH * scf.data.2019$wgt)
earnings.weighted.FinLit_int_1.2019 <- sum(
  scf.data.2019$wageinc * scf.data.2019$x413_pos * scf.data.2019$CCH * scf.data.2019$FinLit_int * scf.data.2019$wgt
) / sum(
  scf.data.2019$x413_pos * scf.data.2019$CCH * scf.data.2019$FinLit_int * scf.data.2019$wgt
)
earnings.weighted.FinLit_int_0.2019 <- sum(
  scf.data.2019$wageinc * scf.data.2019$x413_pos * scf.data.2019$CCH * (1 - scf.data.2019$FinLit_int) * scf.data.2019$wgt
) / sum(
  scf.data.2019$x413_pos * scf.data.2019$CCH * (1 - scf.data.2019$FinLit_int) * scf.data.2019$wgt
)

# earnings.weighted.2022 <- sum(scf.data.2022$wageinc * scf.data.2022$x413_pos * scf.data.2022$wgt)
# earnings.weighted.FinLit_int_1.2022 <- sum(
#   scf.data.2022$wageinc * scf.data.2022$x413_pos * scf.data.2022$wgt * scf.data.2022$FinLit_int
# )
# earnings.weighted.FinLit_int_0.2022 <- sum(
#   scf.data.2022$wageinc * scf.data.2022$x413_pos * scf.data.2022$wgt * (1 - scf.data.2022$FinLit_int)
# )

D2E.ratio.2016 <- (CCD.weighted.2016 / earnings.weighted.2016) * 100
D2E.ratio.FinLit_int_1.2016 <- (CCD.weighted.FinLit_int_1.2016 / earnings.weighted.FinLit_int_1.2016) * 100
D2E.ratio.FinLit_int_0.2016 <- (CCD.weighted.FinLit_int_0.2016 / earnings.weighted.FinLit_int_0.2016) * 100

D2E.ratio.2019 <- (CCD.weighted.2019 / earnings.weighted.2019) * 100
D2E.ratio.FinLit_int_1.2019 <- (CCD.weighted.FinLit_int_1.2019 / earnings.weighted.FinLit_int_1.2019) * 100
D2E.ratio.FinLit_int_0.2019 <- (CCD.weighted.FinLit_int_0.2019 / earnings.weighted.FinLit_int_0.2019) * 100

# D2E.ratio.2022 <- (CCD.weighted.2022 / earnings.weighted.2022) * 100
# D2E.ratio.FinLit_int_1.2022 <- (CCD.weighted.FinLit_int_1.2022 / earnings.weighted.FinLit_int_1.2022) * 100
# D2E.ratio.FinLit_int_0.2022 <- (CCD.weighted.FinLit_int_0.2022 / earnings.weighted.FinLit_int_0.2022) * 100

# individual debt-to-earnings ratio (D2E) [conditional] -----------------------------------
# scf.data.w <- scf.data[scf.data$wageinc > 0, ]
# scf.data.w$D2E <- (scf.data.w$CCD / scf.data.w$wageinc) * 100
#
# Ind.D2E.ratio <- weighted.mean(scf.data.w$D2E, scf.data.w$wgt)
# Ind.D2E.ratio.FinLit_int_1 <- weighted.mean(scf.data.w$D2E * scf.data.w$FinLit_int,
#                                             scf.data.w$wgt * scf.data.w$FinLit_int)
# Ind.D2E.ratio.FinLit_int_0 <- weighted.mean(
#   scf.data.w$D2E * (1 - scf.data.w$FinLit_int),
#   scf.data.w$wgt * (1 - scf.data.w$FinLit_int)
# )
#
# Ind.D2E.ratio.cond <- weighted.mean(scf.data.w$D2E * scf.data.w$x413_pos,
#                                     scf.data.w$wgt * scf.data.w$x413_pos)
# Ind.D2E.ratio.cond.FinLit_int_1 <- weighted.mean(
#   scf.data.w$D2E * scf.data.w$x413_pos * scf.data.w$FinLit_int,
#   scf.data.w$wgt * scf.data.w$x413_pos * scf.data.w$FinLit_int
# )
# Ind.D2E.ratio.cond.FinLit_int_0 <- weighted.mean(
#   scf.data.w$D2E * scf.data.w$x413_pos * (1 - scf.data.w$FinLit_int),
#   scf.data.w$wgt * scf.data.w$x413_pos * (1 - scf.data.w$FinLit_int)
# )

D2E.ratio.weighted.FinLit_int_1.pool <- scf.data.2016.wgt_share * D2E.ratio.FinLit_int_1.2016 + scf.data.2019.wgt_share * D2E.ratio.FinLit_int_1.2019
D2E.ratio.weighted.FinLit_int_0.pool <- scf.data.2016.wgt_share * D2E.ratio.FinLit_int_0.2016 + scf.data.2019.wgt_share * D2E.ratio.FinLit_int_0.2019

# credit card interest rate (CCIR) ---------------------------------------------
scf.data.2016$x7132pos <- as.numeric(scf.data.2016$x7132 > 0)
AvgCCrate.2016 <- sum(scf.data.2016$x7132 * scf.data.2016$x7132pos * scf.data.2016$x413_pos * scf.data.2016$CCH * scf.data.2016$wgt) / sum(scf.data.2016$x7132pos * scf.data.2016$x413_pos * scf.data.2016$CCH * scf.data.2016$wgt) / 100

AvgCCrate.FinLit_int_1_un.2016 <- sum(
  scf.data.2016$x7132 * scf.data.2016$x7132pos * scf.data.2016$CCH * scf.data.2016$FinLit_int * scf.data.2016$wgt 
) / sum(scf.data.2016$x7132pos * scf.data.2016$CCH * scf.data.2016$FinLit_int * scf.data.2016$wgt) / 100
AvgCCrate.FinLit_int_0_un.2016 <- sum(
  scf.data.2016$x7132 * scf.data.2016$x7132pos * scf.data.2016$CCH * (1.0 - scf.data.2016$FinLit_int) * scf.data.2016$wgt 
) / sum(scf.data.2016$x7132pos * scf.data.2016$CCH * (1.0 - scf.data.2016$FinLit_int) * scf.data.2016$wgt) / 100

AvgCCrate.FinLit_int_1.2016 <- sum(
  scf.data.2016$x7132 * scf.data.2016$x7132pos * scf.data.2016$x413_pos * scf.data.2016$CCH * scf.data.2016$FinLit_int * scf.data.2016$wgt 
) / sum(scf.data.2016$x7132pos * scf.data.2016$x413_pos * scf.data.2016$CCH * scf.data.2016$FinLit_int * scf.data.2016$wgt) / 100
AvgCCrate.FinLit_int_0.2016 <- sum(
  scf.data.2016$x7132 * scf.data.2016$x7132pos * scf.data.2016$x413_pos * scf.data.2016$CCH * (1.0 - scf.data.2016$FinLit_int) * scf.data.2016$wgt 
) / sum(scf.data.2016$x7132pos * scf.data.2016$x413_pos * scf.data.2016$CCH * (1.0 - scf.data.2016$FinLit_int) * scf.data.2016$wgt) / 100

scf.data.2019$x7132pos <- as.numeric(scf.data.2019$x7132 > 0)
AvgCCrate.2019 <- sum(scf.data.2019$x7132 * scf.data.2019$x7132pos * scf.data.2019$x413_pos * scf.data.2019$CCH * scf.data.2019$wgt) / sum(scf.data.2019$x7132pos * scf.data.2019$x413_pos * scf.data.2019$CCH * scf.data.2019$wgt) / 100

AvgCCrate.FinLit_int_1_un.2019 <- sum(
  scf.data.2019$x7132 * scf.data.2019$x7132pos * scf.data.2019$CCH * scf.data.2019$FinLit_int * scf.data.2019$wgt 
) / sum(scf.data.2019$x7132pos * scf.data.2019$CCH * scf.data.2019$FinLit_int * scf.data.2019$wgt) / 100
AvgCCrate.FinLit_int_0_un.2019 <- sum(
  scf.data.2019$x7132 * scf.data.2019$x7132pos * scf.data.2019$CCH * (1.0 - scf.data.2019$FinLit_int) * scf.data.2019$wgt 
) / sum(scf.data.2019$x7132pos * scf.data.2019$CCH * (1.0 - scf.data.2019$FinLit_int) * scf.data.2019$wgt) / 100

AvgCCrate.FinLit_int_1.2019 <- sum(
  scf.data.2019$x7132 * scf.data.2019$x7132pos * scf.data.2019$x413_pos * scf.data.2019$CCH * scf.data.2019$FinLit_int * scf.data.2019$wgt 
) / sum(scf.data.2019$x7132pos * scf.data.2019$x413_pos * scf.data.2019$CCH * scf.data.2019$FinLit_int * scf.data.2019$wgt) / 100
AvgCCrate.FinLit_int_0.2019 <- sum(
  scf.data.2019$x7132 * scf.data.2019$x7132pos * scf.data.2019$x413_pos * scf.data.2019$CCH * (1.0 - scf.data.2019$FinLit_int) * scf.data.2019$wgt 
) / sum(scf.data.2019$x7132pos * scf.data.2019$x413_pos * scf.data.2019$CCH * (1.0 - scf.data.2019$FinLit_int) * scf.data.2019$wgt) / 100

AvgCCrate.weighted.FinLit_int_1.pool <- scf.data.2016.wgt_share * AvgCCrate.FinLit_int_1.2016 + scf.data.2019.wgt_share * AvgCCrate.FinLit_int_1.2019
AvgCCrate.weighted.FinLit_int_0.pool <- scf.data.2016.wgt_share * AvgCCrate.FinLit_int_0.2016 + scf.data.2019.wgt_share * AvgCCrate.FinLit_int_0.2019

# scf.data.2022$x7132pos <- as.numeric(scf.data.2022$x7132 > 0)
# AvgCCrate.2022 <- sum(scf.data.2022$x7132 * scf.data.2022$x7132pos * scf.data.2022$wgt) / sum(scf.data.2022$x7132pos * scf.data.2022$wgt) / 100
# AvgCCrate.FinLit_int_1.2022 <- sum(
#   scf.data.2022$x7132 * scf.data.2022$x7132pos * scf.data.2022$wgt * scf.data.2022$FinLit_int
# ) / sum(scf.data.2022$x7132pos * scf.data.2022$wgt * scf.data.2022$FinLit_int) / 100
# AvgCCrate.FinLit_int_0.2022 <- sum(
#   scf.data.2022$x7132 * scf.data.2022$x7132pos * scf.data.2022$wgt * (1 - scf.data.2022$FinLit_int)
# ) / sum(scf.data.2022$x7132pos * scf.data.2022$wgt * (1 - scf.data.2022$FinLit_int)) / 100

df_D2E_CCrate_by_FinLit <- data.frame(
  year = c(2016, 2019, "Pool"),
  D2E_0 = c(
    D2E.ratio.FinLit_int_0.2016,
    D2E.ratio.FinLit_int_0.2019,
    scf.data.2016.wgt_share * D2E.ratio.FinLit_int_0.2016 + scf.data.2019.wgt_share * D2E.ratio.FinLit_int_0.2019
  ),
  CCrate_0 = c(
    AvgCCrate.FinLit_int_0.2016,
    AvgCCrate.FinLit_int_0.2019,
    scf.data.2016.wgt_share * AvgCCrate.FinLit_int_0.2016 + scf.data.2019.wgt_share * AvgCCrate.FinLit_int_0.2019
  ),
  D2E_1 = c(
    D2E.ratio.FinLit_int_1.2016,
    D2E.ratio.FinLit_int_1.2019,
    scf.data.2016.wgt_share * D2E.ratio.FinLit_int_1.2016 + scf.data.2019.wgt_share * D2E.ratio.FinLit_int_1.2019
  ),
  CCrate_1 = c(
    AvgCCrate.FinLit_int_1.2016,
    AvgCCrate.FinLit_int_1.2019,
    scf.data.2016.wgt_share * AvgCCrate.FinLit_int_1.2016 + scf.data.2019.wgt_share * AvgCCrate.FinLit_int_1.2019
  )
)
write.csv(df_D2E_CCrate_by_FinLit,
          "D2E_CCrate_by_FinLit.csv",
          row.names = FALSE)

# results by financial literacy group ------------------------------------------
# scf.data %>%
#   group_by(FinLit) %>%
#   summarise(weighted_FI = sum(wgt)) / sum(scf.data$wgt)
#
# scf.data %>%
#   group_by(FinLit) %>%
#   # summarise(weighted_CCrate = weighted.mean(x7132, wgt))
#   summarise(weighted_CCrate = weighted.mean(x7132 * x7132pos, wgt * x7132pos))
#
# scf.data %>%
#   group_by(FinLit) %>%
#   # summarise(weighted_CCD = weighted.mean(CCD, wgt))
#   summarise(weighted_CCD = weighted.mean(CCD * x413_pos, wgt * x413_pos))