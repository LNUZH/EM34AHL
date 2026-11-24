# =============================================================================
# PS3_Solution.R  -  Empirical Methods HS 2026, Problem Set 3
# Replication of Pawar & Munuswamy (2024), "Does environmental reporting of
# banks affect their financial performance? Evidence from India",
# International Journal of Bank Marketing 42(4), 745-767.
#
# One script, run top to bottom from the project folder (open the .Rproj file in this folder).
# Input : the two raw Excel files in the project folder (never modified)
# Output: output/tables/*.csv, output/figures/*.pdf
# Packages (install once in the console):
#   install.packages(c("readxl", "plm", "sandwich", "lmtest"))
# =============================================================================

rm(list = ls())
set.seed(12345)
library(readxl); library(plm); library(sandwich); library(lmtest)

## (a) Setup: folders and input files ----
for (dir in c("output/tables", "output/figures")) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}
f_panel  <- "Banks financial performance and Env score.xlsx"
f_coding <- "Environmenmental Reporting of Banks in India Coding and Results.xlsx"
stopifnot(file.exists(f_panel), file.exists(f_coding))   # error = wrong working directory
excel_sheets(f_panel)    # "Sheet1" "Sheet2"
excel_sheets(f_coding)   # "Results" "Coding in detail"


## (b) Import and rename ----
raw <- as.data.frame(read_excel(f_panel, sheet = "Sheet1"))
str(raw)                 # 50 obs. of 12 variables

d <- data.frame(
  bank   = raw$Bank,
  year   = as.integer(raw$Years),     # fiscal year ending in March (2021 = FY 2020-21)
  roa    = raw$ROA,                   # return on assets, %
  roe    = raw$ROE,                   # return on equity, %
  eps    = raw$EPS,                   # earnings per share, rupees
  env    = raw$`ENV SCR`,             # environmental reporting score (sum of 0-3 scores)
  size   = raw$`Bank Size`,           # ln(total assets)
  lev    = raw$Leverage,              # leverage
  age    = raw$`Bank Age`,            # years since incorporation
  capint = raw$`Capital Int`,         # capital intensity
  npa    = raw$`Net NPA`,             # net non-performing assets, %
  nim    = raw$NIM                    # net interest margin, %
)
d <- d[order(d$bank, d$year), ]       # file is sorted newest year first
rownames(d) <- NULL

# Sheet2: same numbers, bank name only in the first row of each block?
raw2 <- as.data.frame(read_excel(f_panel, sheet = "Sheet2"))
cat("Sheet2 numerically identical to Sheet1:",
    isTRUE(all.equal(raw[, -1], raw2[, -1], check.attributes = FALSE)), "\n")

# Coding file, sheet 'Results', rows 18-27: bank (col 1), disclosures (col 2),
# total score (col 3), categories ENV_1-ENV_15 (cols 7-21), score by year (cols 27-31)
res <- as.data.frame(read_excel(f_coding, sheet = "Results",
                                col_names = FALSE, .name_repair = "minimal"))
coding <- data.frame(
  bank_coding = res[18:27, 1],
  n_disc      = as.numeric(res[18:27, 2]),
  score_total = as.numeric(res[18:27, 3]),
  s2017 = as.numeric(res[18:27, 27]), s2018 = as.numeric(res[18:27, 28]),
  s2019 = as.numeric(res[18:27, 29]), s2020 = as.numeric(res[18:27, 30]),
  s2021 = as.numeric(res[18:27, 31])
)
coding$score_by_item <- rowSums(sapply(res[18:27, 7:21], as.numeric))
coding$score_by_year <- rowSums(coding[, paste0("s", 2017:2021)])
coding

## (c) Data checks (nothing is changed) ----
vars <- c("roa", "roe", "eps", "env", "size", "lev", "age", "capint", "npa", "nim")

# (c1) Panel structure, missing values, duplicates
cat("Banks:", length(unique(d$bank)), " Years:", paste(sort(unique(d$year)), collapse = ", "), "\n")
print(table(d$bank))                                     # every bank should have 5
cat("Duplicated bank-years:", sum(duplicated(d[, c("bank", "year")])), "\n")
print(colSums(is.na(d)))

# (c2) Ranges
print(round(t(sapply(d[, vars], range)), 2))

# (c3) Bank age must rise by exactly 1 per year
dage <- ave(d$age, d$bank, FUN = function(x) c(NA, diff(x)))
cat("\nBank-age jumps (change != 1):\n")
print(cbind(d[, c("bank", "year", "age")], dage)[!is.na(dage) & dage != 1, ])

# (c4) Identical five-year series across different banks (copy-paste errors?)
cat("\nIdentical five-year series across banks:\n")
banks <- unique(d$bank)
for (v in vars) {
  for (i in 1:(length(banks) - 1)) for (j in (i + 1):length(banks)) {
    if (isTRUE(all.equal(d[d$bank == banks[i], v], d[d$bank == banks[j], v])))
      cat(" ", v, ":", banks[i], "=", banks[j], "\n")
  }
}

# (c5) ROA, ROE and EPS all come from net income: their signs should agree
sgn <- sign(d[, c("roa", "roe", "eps")])
cat("\nRows where ROA, ROE and EPS have different signs:\n")
print(d[apply(sgn, 1, function(r) length(unique(r[r != 0])) > 1), c("bank", "year", "roa", "roe", "eps")])

# (c6) ENV score in the panel vs. the coding file (bank names differ -> mapping)
map <- c("State Bank of India" = "SBI", "Punjab National Bank" = "Punjab National Bank",
         "Axis Bank" = "Axis Bank", "HDFC Bank" = "HDFC Bank", "ICICI Bank" = "ICICI Bank",
         "Federal Bank" = "Federal Bank", "IDFC FIRST Bank" = "IDFC First Bank",
         "Kotak Mahindra Bank" = "Kotak Mahindra Bank Ltd.", "RBL Bank" = "RBL Bank",
         "IndusInd Bank" = "IndusInd Bank")
coding$bank <- map[coding$bank_coding]
long <- reshape(coding[, c("bank", paste0("s", 2017:2021))], direction = "long",
                varying = paste0("s", 2017:2021), v.names = "env_coding",
                timevar = "year", times = 2017:2021, idvar = "bank")
chk <- merge(d[, c("bank", "year", "env")], long, by = c("bank", "year"))
cat("\nENV in panel = ENV in coding file:", sum(chk$env == chk$env_coding), "of", nrow(chk), "\n")

# (c7) Coding file: total score by item vs. by year
cat("\nTotal score by item vs. by year (rows that differ):\n")
print(coding[coding$score_by_item != coding$score_by_year,
             c("bank_coding", "score_total", "score_by_item", "score_by_year")])

# (c8) Within-bank share of the variance (what FE can use for identification)
within_share <- sapply(vars, function(v) {
  dm <- d[[v]] - ave(d[[v]], d$bank)
  100 * sum(dm^2) / sum((d[[v]] - mean(d[[v]]))^2)
})
cat("\nWithin-bank share of total variance (%):\n")
print(round(within_share, 1))

# (c9) Means by year: common time trends?
cat("\nMeans by year:\n")
print(round(aggregate(d[, c("env", "roa", "roe", "eps")], by = list(year = d$year), FUN = mean), 2))


## (d) Descriptive statistics: Table 5, Table 6, figures ----
# (d1) Table 5: ranking by total environmental reporting score
t5 <- data.frame(bank = coding$bank_coding, disclosures = coding$n_disc,
                 total = coding$score_total, average = coding$score_total / 5)
t5 <- t5[order(-t5$total), ]
t5$rank <- 1:nrow(t5)
print(t5, row.names = FALSE)
write.csv(t5, "output/tables/table5_ours.csv", row.names = FALSE)

# (d2) Table 6 with EViews conventions (the authors used EViews):
#      skewness = m3 / m2^1.5, kurtosis = m4 / m2^2 (normal = 3, NOT excess),
#      Jarque-Bera = n/6 * (S^2 + (K-3)^2/4), p-value from chi^2(2)
desc <- function(x) {
  n  <- length(x); m <- mean(x)
  m2 <- mean((x - m)^2); m3 <- mean((x - m)^3); m4 <- mean((x - m)^4)
  S  <- m3 / m2^1.5; K <- m4 / m2^2
  JB <- n / 6 * (S^2 + (K - 3)^2 / 4)
  c(mean = m, sd = sd(x), min = min(x), max = max(x),
    skew = S, kurt = K, JB = JB, p_JB = 1 - pchisq(JB, df = 2))
}
labels <- c(env = "Environmental score", roa = "ROA", roe = "ROE", eps = "EPS",
            size = "Bank size", age = "Bank age", lev = "Leverage",
            capint = "Capital intensity", npa = "Net NPA", nim = "Net interest margin")
t6 <- t(sapply(names(labels), function(v) desc(d[[v]])))
rownames(t6) <- labels
print(round(t6, 3))
write.csv(round(t6, 3), "output/tables/table6_ours.csv")

# (d3) Figure 1: ENV score by bank over time
pdf("output/figures/fig1_env_by_bank.pdf", width = 7, height = 4.5)
par(mar = c(4, 4, 1, 9), xpd = TRUE)
bnk <- sort(unique(d$bank)); cols <- rainbow(length(bnk), v = 0.8)
plot(NA, xlim = range(d$year), ylim = range(d$env), xlab = "Fiscal year",
     ylab = "Environmental reporting score", xaxt = "n")
axis(1, at = 2017:2021)
for (i in seq_along(bnk)) {
  s <- d[d$bank == bnk[i], ]
  lines(s$year, s$env, col = cols[i], lwd = 2, type = "b", pch = 16)
}
legend("topright", inset = c(-0.38, 0), legend = bnk, col = cols, lwd = 2, bty = "n", cex = 0.75)
dev.off()

# (d4) Figure 2: ROA and ROE against ENV (pooled, with OLS line)
pdf("output/figures/fig2_scatter_env.pdf", width = 8, height = 4)
par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
for (y in c("roa", "roe")) {
  plot(d$env, d[[y]], pch = 16, col = "grey40", xlab = "Environmental reporting score",
       ylab = paste(toupper(y), "(%)"))
  abline(lm(d[[y]] ~ d$env), lwd = 2)
}
dev.off()


# (d5) Sign of the ENV slope by set of controls (Sections 3.2, 4.1; Table A.4)
#      none / one control only / all controls but one / all six (pooled OLS)
ctrl <- c("age", "size", "lev", "capint", "npa", "nim")
b_env <- function(y, w) coef(lm(reformulate(c("env", w), response = y), data = d))[["env"]]
sens <- data.frame(controls = c("none", paste("only", ctrl), paste("all but", ctrl), "all six"))
for (y in c("roa", "roe")) {
  sens[[y]] <- c(b_env(y, NULL), sapply(ctrl, function(w) b_env(y, w)),
                 sapply(ctrl, function(w) b_env(y, setdiff(ctrl, w))), b_env(y, ctrl))
}
print(sens, digits = 3)
cat("ROE: change from no controls to net NPA only:",
    round(b_env("roe", NULL) - b_env("roe", "npa"), 4), "\n")
cat("Correlation ENV - net NPA:", round(cor(d$env, d$npa), 3), "\n")
write.csv(sens, "output/tables/sign_by_controls.csv", row.names = FALSE)

# (d6) Omitted variable bias formula for net NPA (Section 4.1):
#      slope(ENV only) = slope(ENV + NPA) + beta_NPA * beta_(NPA on ENV)
ovb_long <- lm(roe ~ env + npa, data = d)   # long regression
ovb_aux  <- lm(npa ~ env, data = d)         # auxiliary regression
ovb <- data.frame(b_short   = b_env("roe", NULL),
                  b_long    = coef(ovb_long)[["env"]],
                  beta_npa  = coef(ovb_long)[["npa"]],
                  npa_on_env = coef(ovb_aux)[["env"]])
ovb$bias <- ovb$beta_npa * ovb$npa_on_env
ovb$check <- ovb$b_long + ovb$bias - ovb$b_short   # = 0 (exact in sample)
print(ovb, digits = 4)
write.csv(ovb, "output/tables/ovb_npa.csv", row.names = FALSE)


## (e) Panel regressions: Tables 8 (ROA), 9 (ROE), 10 (EPS) ----
p  <- pdata.frame(d, index = c("bank", "year"))   # unit = bank, time = year
xs <- c("env", "age", "size", "lev", "capint", "npa", "nim")   # order as in the paper
ys <- c("roa", "roe", "eps")

# (e1) Pooled OLS, random effects (Swamy-Arora, EViews default), fixed effects
fit_all <- function(y) {
  f  <- as.formula(paste(y, "~", paste(xs, collapse = " + ")))
  po <- plm(f, data = p, model = "pooling")
  re <- plm(f, data = p, model = "random", random.method = "swar")
  fe <- plm(f, data = p, model = "within", effect = "individual")
  lsdv <- lm(update(f, . ~ . + factor(bank)), data = d)   # FE with bank dummies
  list(f = f, po = po, re = re, fe = fe, lsdv = lsdv)
}
m <- lapply(ys, fit_all); names(m) <- ys
summary(m$roa$re)

# (e2) Check: FE 'within' slopes = LSDV slopes
for (y in ys) cat(y, ": max |b_within - b_LSDV| =",
                  max(abs(coef(m[[y]]$fe) - coef(m[[y]]$lsdv)[xs])), "\n")

# (e3) LM test (H0: no random effects -> pooled OLS) and
#      Hausman test (H0: bank effect uncorrelated with X -> RE consistent)
tests <- do.call(rbind, lapply(ys, function(y) {
  lm1 <- plmtest(m[[y]]$po, type = "bp", effect = "individual")
  lm2 <- plmtest(m[[y]]$po, type = "bp", effect = "twoways")
  h1  <- phtest(m[[y]]$fe, m[[y]]$re)                     # contrast form
  h2  <- phtest(m[[y]]$f, data = p, method = "aux")       # auxiliary-regression form
  data.frame(y = y,
             LM_bank = unname(lm1$statistic), p_LM_bank = lm1$p.value,
             LM_twoway = unname(lm2$statistic), p_LM_twoway = lm2$p.value,
             H_contrast = unname(h1$statistic), p_H_contrast = h1$p.value,
             H_aux = unname(h2$statistic), p_H_aux = h2$p.value)
}))
print(tests, digits = 4)

# (e4) Coefficients and p-values (conventional SE), R-squared, Durbin-Watson
#      FE constant = average of the bank effects (within_intercept, as EViews)
#      FE R-squared = R-squared of the LSDV regression (as EViews)
#      DW on the stacked residuals; does not reproduce the paper's DW exactly
dw <- function(e) sum(diff(as.numeric(e))^2) / sum(as.numeric(e)^2)
tidy <- function(mod, y, model) {
  ct <- coeftest(mod)
  b  <- ct[, 1]; pv <- ct[, 4]
  if (model == "FE") {
    wi <- within_intercept(mod)
    b  <- c("(Intercept)" = unname(wi), b)
    pv <- c("(Intercept)" = 2 * pt(-abs(wi / attr(wi, "se")), df.residual(mod)), pv)
    r2 <- summary(m[[y]]$lsdv)$r.squared
  } else r2 <- summary(mod)$r.squared[["rsq"]]
  rbind(data.frame(y = y, model = model, term = names(b), b = b, p = pv),
        data.frame(y = y, model = model, term = c("R2", "DW"),
                   b = c(r2, dw(residuals(mod))), p = NA))
}
ours <- do.call(rbind, lapply(ys, function(y) rbind(
  tidy(m[[y]]$po, y, "POLS"), tidy(m[[y]]$re, y, "RE"), tidy(m[[y]]$fe, y, "FE"))))
ours$term[ours$term == "(Intercept)"] <- "const"
rownames(ours) <- NULL
write.csv(ours, "output/tables/regressions_ours.csv", row.names = FALSE)

# (e5) p-value of ENV under four types of standard errors
se_types <- list(
  conventional = function(mod) vcov(mod),
  white        = function(mod) vcovHC(mod, method = "white1", type = "HC1"),
  newey_west   = function(mod) vcovNW(mod, type = "HC1"),
  cluster_bank = function(mod) vcovHC(mod, method = "arellano", cluster = "group", type = "HC1")
)
pe <- do.call(rbind, lapply(ys, function(y) do.call(rbind, lapply(c("POLS", "RE", "FE"), function(k) {
  mod <- m[[y]][[c(POLS = "po", RE = "re", FE = "fe")[[k]]]]
  data.frame(y = y, model = k, t(sapply(se_types, function(v) coeftest(mod, vcov = v(mod))["env", 4])))
}))))


## (f) Comparison with the paper and deviations ----
# (f1) Published values, typed from the PDF (pp. 755-758), signs checked on the page
terms <- c("env", "age", "size", "lev", "capint", "npa", "nim", "const", "R2", "DW")
pp <- function(y, model, b, p) data.frame(y = y, model = model, term = terms,
                                          b_paper = b, p_paper = c(p, NA, NA))
paper <- rbind(
  # Table 8: ROA
  pp("roa", "POLS", c(-0.007, -0.003, 0.236, 0.041, -1.247, -0.085, 0.595, -2.876, 0.667, 0.868),
     c(0.0850, 0.291, 0.011, 0.506, 0.009, 0.194, 0.069, 0.085)),
  pp("roa", "RE",   c(-0.008, -0.005, 0.102, 0.063, -0.293, -0.127, 0.307, 0.312, 0.269, 1.783),
     c(0.085, 0.507, 0.602, 0.364, 0.704, 0.061, 0.341, 0.877)),
  pp("roa", "FE",   c(-0.007, 0.149, -0.640, 0.049, 0.509, -0.101, 0.244, 2.801, 0.869, 2.022),
     c(0.241, 0.293, 0.409, 0.540, 0.654, 0.173, 0.490, 0.548)),
  # Table 9: ROE
  pp("roe", "POLS", c(-0.071, -0.023, 0.031, -0.519, -3.120, -2.193, 6.087, 3.498, 0.631, 1.476),
     c(0.029, 0.537, 0.974, 0.430, 0.529, 0.002, 0.081, 0.842)),
  pp("roe", "RE",   c(-0.099, -0.024, 0.120, -0.353, -1.789, -2.714, 4.479, 8.936, 0.432, 1.886),
     c(0.055, 0.732, 0.949, 0.673, 0.828, 0.003, 0.274, 0.768)),
  pp("roe", "FE",   c(-0.155, 0.391, -0.290, -0.193, -4.831, -2.949, 3.297, 2.575, 0.746, 2.072),
     c(0.073, 0.844, 0.979, 0.863, 0.763, 0.007, 0.510, 0.972)),
  # Table 10: EPS
  pp("eps", "POLS", c(-0.017, 0.046, 15.410, 1.398, -61.708, -1.931, 23.956, -225.423, 0.602, 0.575),
     c(0.885, 0.752, 0.000, 0.578, 0.002, 0.466, 0.072, 0.002)),
  pp("eps", "RE",   c(-0.071, -0.196, 7.795, 1.462, -15.756, -4.925, -2.272, -43.159, 0.281, 1.801),
     c(0.653, 0.544, 0.284, 0.515, 0.553, 0.021, 0.824, 0.679)),
  pp("eps", "FE",   c(-0.037, -0.438, 9.794, 0.977, -4.771, -4.966, -4.168, -53.284, 0.912, 2.015),
     c(0.842, 0.919, 0.679, 0.689, 0.891, 0.032, 0.700, 0.736))
)

# (f2) Merge and flag. Tolerances: coefficient within 0.001 (or 0.1 % if large),
#      because the paper prints 3 decimals and sometimes truncates; p within 0.002
cmp <- merge(paper, ours, by = c("y", "model", "term"), sort = FALSE)
cmp$match_b <- abs(cmp$b - cmp$b_paper) <= pmax(0.001, 0.001 * abs(cmp$b_paper))
cmp$match_p <- ifelse(is.na(cmp$p_paper), NA, abs(cmp$p - cmp$p_paper) <= 0.002)
cmp$y     <- factor(cmp$y, levels = ys)
cmp$model <- factor(cmp$model, levels = c("POLS", "RE", "FE"))
cmp$term  <- factor(cmp$term, levels = terms)
cmp <- cmp[order(cmp$y, cmp$model, cmp$term), ]
for (y in ys) {
  x <- cmp[cmp$y == y, ]; x$b <- round(x$b, 4); x$p <- round(x$p, 3)
  print(x[, c("model", "term", "b_paper", "b", "match_b", "p_paper", "p", "match_p")], row.names = FALSE)
}

# (f3) Summary of the match
slopes <- cmp[cmp$term %in% xs, ]
cat("\nSlope coefficients matched:", sum(slopes$match_b), "of", nrow(slopes), "\n")
cat("Slope p-values matched:    ", sum(slopes$match_p), "of", nrow(slopes), "\n")
print(aggregate(cbind(b = match_b, p = match_p) ~ model, data = slopes, FUN = sum))

# (f4) Specification tests: ours vs. paper
tp <- data.frame(y = ys, LM_paper = c(17.564, 1.160, 31.806), p_LM_paper = c(0.000, 0.281, 0.000),
                 H_paper = c(2.403, NA, 2.257), p_H_paper = c(0.934, NA, 0.944))
print(merge(tp, tests, by = "y", sort = FALSE)[, c("y", "LM_paper", "LM_bank", "LM_twoway",
                                                   "p_LM_paper", "p_LM_twoway", "H_paper", "H_contrast", "H_aux", "p_H_paper", "p_H_aux")], digits = 4)

# (f5) Which standard errors reproduce the paper's p-value of ENV?
print(merge(pe, cmp[cmp$term == "env", c("y", "model", "p_paper")], by = c("y", "model")), digits = 3)

# (f6) Table 6: ours minus paper (|diff| > 0.002 = deviation)
t6p <- rbind(
  "Environmental score" = c(60.56, 32.994, 15, 144, 0.649, 2.342, 0.109),
  "ROA"                 = c(0.91, 0.893, -1.6, 3.3, 0.039, 4.246, 0.197),
  "ROE"                 = c(7.303, 9.050, -32.85, 20.4, -2.510, 11.466, 0.000),
  "EPS"                 = c(22.83, 33.297, -54.63, 123.2, 1.042, 4.516, 0.000),
  "Bank size"           = c(12.314, 1.234, 9.95, 14.7, 0.072, 2.107, 0.426),
  "Bank age"            = c(47.34, 37.21, 2, 127, 0.816, 2.462, 0.048),
  "Leverage"            = c(8.759, 2.499, 4.37, 16.3, 0.512, 3.479, 0.264),
  "Capital intensity"   = c(0.517, 0.254, 0.2, 1.13, 0.643, 2.241, 0.098),
  "Net NPA"             = c(2.686, 2.240, 0.39, 11.24, 1.576, 5.735, 0.000),
  "Net interest margin" = c(3.433, 0.559, 2.16, 4.6, -0.008, 2.876, 0.984))
colnames(t6p) <- c("mean", "sd", "min", "max", "skew", "kurt", "p_JB")
diff6 <- round(t6[rownames(t6p), colnames(t6p)] - t6p, 3)
print(diff6)

# (f7) Deviations table for Appendix A.3 (fill in the 'reason' column by hand)
dev <- cmp[!cmp$match_b | (!is.na(cmp$match_p) & !cmp$match_p),
           c("y", "model", "term", "b_paper", "b", "p_paper", "p")]
dev$b <- round(dev$b, 4); dev$p <- round(dev$p, 3)
idx  <- which(abs(diff6) > 0.002, arr.ind = TRUE)
dev6 <- data.frame(y = "table6", model = "", term = paste(rownames(diff6)[idx[, 1]], colnames(diff6)[idx[, 2]]),
                   b_paper = t6p[idx], b = round(t6[rownames(t6p), colnames(t6p)][idx], 3),
                   p_paper = NA, p = NA)
dev <- rbind(dev, dev6)
dev$reason <- ""
print(dev, row.names = FALSE)
write.csv(dev, "output/tables/deviations.csv", row.names = FALSE)
write.csv(cmp, "output/tables/comparison_full.csv", row.names = FALSE)


## (g) Extension: two-way fixed effects, clustered SE, robustness ----
# (g1) Specifications for ROA and ROE
#   A  paper's choice (RE for ROA, pooled OLS for ROE), conventional SE
#   B  bank fixed effects, SE clustered by bank
#   C  bank + year fixed effects, SE clustered by bank      <- main extension
#   D  as C, without IDFC First Bank (suspected copied ROA/ROE data)
#   E  as C, with last year's ENV score instead of this year's (ENV_t-1)
#   F  first differences (as the paper claims), SE clustered by bank; bank age
#      dropped: its change is 1 in (almost) every year -> collinear with the constant
#   G  as E, but pooled OLS without bank/year FE (does the lag result need FE?)
#   p_conv: p-value with conventional SE, to compare with the clustered p
d$env_lag <- ave(d$env, d$bank, FUN = function(x) c(NA, head(x, -1)))
p_all  <- pdata.frame(d, index = c("bank", "year"))
p_noid <- pdata.frame(d[d$bank != "IDFC First Bank", ], index = c("bank", "year"))

clus <- function(mod) {                       # cluster-robust SE by bank,
  G <- length(unique(index(mod)[[1]]))        # t-distribution with G - 1 df
  coeftest(mod, vcov = vcovHC(mod, method = "arellano", cluster = "group", type = "sss"),
           df = G - 1)
}
ext_one <- function(y) {
  f  <- as.formula(paste(y, "~", paste(xs, collapse = " + ")))
  fl <- update(f, . ~ . - env + env_lag)
  ff <- update(f, . ~ . - age)
  A  <- if (y == "roa") plm(f, p_all, model = "random", random.method = "swar")
  else            plm(f, p_all, model = "pooling")
  mods <- list(A = A,
               B = plm(f,  p_all,  model = "within", effect = "individual"),
               C = plm(f,  p_all,  model = "within", effect = "twoways"),
               D = plm(f,  p_noid, model = "within", effect = "twoways"),
               E = plm(fl, p_all,  model = "within", effect = "twoways"),
               F = plm(ff, p_all,  model = "fd"),
               G = plm(fl, p_all,  model = "pooling"))
  do.call(rbind, lapply(names(mods), function(k) {
    ct <- if (k == "A") coeftest(mods[[k]]) else clus(mods[[k]])
    v  <- if (k %in% c("E", "G")) "env_lag" else "env"
    data.frame(y = y, spec = k, b = ct[v, 1], se = ct[v, 2], p = ct[v, 4],
               p_conv = coeftest(mods[[k]])[v, 4], n = nobs(mods[[k]]), banks = length(unique(index(mods[[k]])[[1]])))
  }))
}
ext <- rbind(ext_one("roa"), ext_one("roe"))
rownames(ext) <- NULL
print(ext, digits = 3)
write.csv(ext, "output/tables/extension.csv", row.names = FALSE)

# (g2) Size of the effect: change in ROA/ROE (percentage points) for a
#      one-standard-deviation higher ENV score (33 points)
sd_env <- sd(d$env)
print(data.frame(ext[, c("y", "spec")], effect_1sd = round(ext$b * sd_env, 2)))

sessionInfo()   # R and package versions for the README
