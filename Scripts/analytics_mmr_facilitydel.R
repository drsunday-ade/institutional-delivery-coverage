#!/usr/bin/env Rscript
# ============================================================
# Study 1 — Policy-Ready Analytics + Shiny Dashboard (WHO GHO)
#
# Primary hypothesis (H1):
#   Within-country increases in institutional delivery coverage (lag-1)
#   are associated with reductions in MMR, adjusting for adolescent fertility
#   and anaemia, with robust fixed effects inference.
#
# Secondary:
#   H4 effect modification (inst × anaemia)
#   Robustness: add SBA and ANC4; Region×Year FE; placebo lead test.
#
# INPUT:
#   ./data/ indicator CSVs (WHO GHO export format)
#
# OUTPUT:
#   ./output/data/    panel + policy translation datasets
#   ./output/tables/  5 publishable tables (csv/html)
#   ./output/figures/ 5 publishable figures (png)
#   ./output/shiny_app/ deployable Shiny app (app.R + www + data)
#   ./output/logs/    run log + sessionInfo + sha256 manifests
#
# ============================================================

options(stringsAsFactors = FALSE, scipen = 999)

# ---------------------------
# 0) Root + folders
# ---------------------------
get_script_dir <- function(){
  cmd <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd, value = TRUE)
  if(length(file_arg) == 1){
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
  return(getwd())
}
ROOT <- get_script_dir()

DATA_DIR <- Sys.getenv("DATA_DIR", unset = file.path(ROOT, "data"))
OUTDIR   <- Sys.getenv("OUTDIR",   unset = file.path(ROOT, "output"))

YEAR_MIN <- as.integer(Sys.getenv("YEAR_MIN", unset = "2000"))
YEAR_MAX <- as.integer(Sys.getenv("YEAR_MAX", unset = "2022"))
SEED     <- as.integer(Sys.getenv("SEED", unset = "12345"))

dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)
for(p in c("data","tables","figures","logs","shiny_app")){
  dir.create(file.path(OUTDIR, p), recursive = TRUE, showWarnings = FALSE)
}
dir.create(file.path(OUTDIR, "shiny_app", "www"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTDIR, "shiny_app", "data"), recursive = TRUE, showWarnings = FALSE)

# Log
log_path <- file.path(OUTDIR, "logs", "run_log_policy_pipeline.txt")
sink_con <- file(log_path, open = "wt")
sink(sink_con, split = TRUE)
on.exit({
  cat("\nRun ended:", as.character(Sys.time()), "\n")
  try(sink(), silent = TRUE)
  try(close(sink_con), silent = TRUE)
}, add = TRUE)

set.seed(SEED)

cat("Run started:", as.character(Sys.time()), "\n")
cat("ROOT:", ROOT, "\n")
cat("DATA_DIR:", DATA_DIR, "\n")
cat("OUTDIR:", OUTDIR, "\n")
cat("Year window:", YEAR_MIN, "to", YEAR_MAX, "\n")
cat("SEED:", SEED, "\n\n")

# ---------------------------
# 1) Packages (install if missing)
# ---------------------------
req <- c(
  "data.table","dplyr","tidyr","readr","lubridate","stringr","magrittr",
  "fixest","splines","ggplot2","scales","ggrepel","viridis",
  "modelsummary","knitr","kableExtra","digest",
  "shiny","bslib","DT","htmltools"
)
missing <- req[!req %in% rownames(installed.packages())]
if(length(missing)){
  install.packages(missing, dependencies = TRUE, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages(invisible(lapply(req, library, character.only = TRUE)))
if("setFixest_notes" %in% getNamespaceExports("fixest")) fixest::setFixest_notes(FALSE)

# ---------------------------
# 2) Helpers
# ---------------------------
assert_cols <- function(dt, cols, name="dataset"){
  miss <- cols[!cols %in% names(dt)]
  if(length(miss)) stop("Missing columns in ", name, ": ", paste(miss, collapse=", "))
}

read_gho <- function(path){
  dt <- data.table::fread(path, na.strings = c("","NA","NaN","."))
  if(!("NumericValue" %in% names(dt))){
    cand <- intersect(names(dt), c("Value","value","Numeric","numeric"))
    if(length(cand) >= 1) data.table::setnames(dt, cand[1], "NumericValue")
  }
  if(!("TimeDim" %in% names(dt))){
    cand <- intersect(names(dt), c("Year","year","Time","time"))
    if(length(cand) >= 1) data.table::setnames(dt, cand[1], "TimeDim")
  }
  dt
}

prep_country_year <- function(dt){
  assert_cols(dt, c("SpatialDimension","SpatialDimensionValueCode","TimeDim","NumericValue"), "GHO extract")
  dt %>%
    dplyr::filter(SpatialDimension == "COUNTRY") %>%
    dplyr::mutate(
      country_code = SpatialDimensionValueCode,
      year = suppressWarnings(as.integer(TimeDim)),
      NumericValue = suppressWarnings(as.numeric(NumericValue))
    ) %>%
    dplyr::filter(!is.na(year), year >= YEAR_MIN, year <= YEAR_MAX)
}

parse_date_any <- function(x){
  suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c("ymd HMS","ymd HM","ymd","Ymd HMS","Ymd","mdy HMS","mdy","dmy HMS","dmy"),
    tz = "UTC",
    quiet = TRUE
  ))
}

collapse_latest <- function(dt){
  if(!("Date" %in% names(dt))) dt$Date <- NA_character_
  if(!("ParentLocation" %in% names(dt))) dt$ParentLocation <- NA_character_
  if(!("ParentLocationCode" %in% names(dt))) dt$ParentLocationCode <- NA_character_
  
  dt %>%
    dplyr::mutate(Date_parsed = parse_date_any(Date)) %>%
    dplyr::arrange(country_code, year, dplyr::desc(Date_parsed)) %>%
    dplyr::group_by(country_code, year) %>%
    dplyr::summarise(
      value = dplyr::first(NumericValue),
      who_region = dplyr::first(ParentLocation),
      who_region_code = dplyr::first(ParentLocationCode),
      .groups = "drop"
    )
}

winsor <- function(x, p = c(0.01, 0.99)){
  qs <- stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE)
  pmin(pmax(x, qs[1]), qs[2])
}

sha256 <- function(path) digest::digest(file = path, algo = "sha256")

write_html_table <- function(df, out_html, caption){
  k <- knitr::kable(df, format = "html", caption = caption, digits = 4) %>%
    kableExtra::kable_styling(
      full_width = FALSE,
      bootstrap_options = c("striped","hover","condensed")
    ) %>%
    kableExtra::scroll_box(width = "100%")
  kableExtra::save_kable(k, out_html)
}

theme_pub <- function(){
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face="bold", size=15, margin=ggplot2::margin(b=6)),
      plot.subtitle = ggplot2::element_text(size=11, margin=ggplot2::margin(b=8)),
      plot.caption = ggplot2::element_text(size=9, hjust=0, margin=ggplot2::margin(t=10)),
      axis.title = ggplot2::element_text(face="bold"),
      axis.text.y = ggplot2::element_text(size=10),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(16, 18, 16, 18),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face="bold")
    )
}

ggsave_pub <- function(filename, plot, width=8.6, height=5.6){
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width, height = height, dpi = 300, units = "in",
    bg = "white", limitsize = FALSE
  )
}

# Predict + CI from fixest coefficients/vcov (FE set to 0)
predict_ci_fixest <- function(fit, newdata, rhs_formula){
  X <- stats::model.matrix(rhs_formula, data = newdata)
  if("(Intercept)" %in% colnames(X)) X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  b <- stats::coef(fit)
  V <- stats::vcov(fit)
  common <- intersect(names(b), colnames(X))
  Xc <- X[, common, drop = FALSE]
  bc <- b[common]
  Vc <- V[common, common, drop = FALSE]
  eta <- as.numeric(Xc %*% bc)
  se  <- sqrt(pmax(0, diag(Xc %*% Vc %*% t(Xc))))
  data.frame(eta = eta, se = se, lwr = eta - 1.96*se, upr = eta + 1.96*se)
}

mmr_from_eta <- function(eta){ pmax(0, exp(eta) - 1) }  # eta = log(MMR+1)

# Policy translation (coherent CI ordering)
policy_translate <- function(base_mmr, delta_pp, beta, beta_se){
  eta0 <- log(base_mmr + 1)
  
  d_eta <- beta * (delta_pp/10)
  d_eta_lo <- (beta - 1.96*beta_se) * (delta_pp/10)
  d_eta_hi <- (beta + 1.96*beta_se) * (delta_pp/10)
  
  mmr_new <- mmr_from_eta(eta0 + d_eta)
  mmr_new_lo <- mmr_from_eta(eta0 + d_eta_lo) # typically lower if beta negative
  mmr_new_hi <- mmr_from_eta(eta0 + d_eta_hi) # typically higher if beta negative
  
  delta <- base_mmr - mmr_new
  delta_lo <- base_mmr - mmr_new_hi  # smallest reduction
  delta_hi <- base_mmr - mmr_new_lo  # largest reduction
  
  pct <- 100 * (1 - (mmr_new + 1) / (base_mmr + 1))
  
  list(mmr_new = mmr_new, delta = delta, delta_lo = delta_lo, delta_hi = delta_hi, pct = pct)
}

# ---------------------------
# 3) Required files
# ---------------------------
need_files <- c(
  "MDG_0000000026.csv",
  "SRHINSTITUTIONALBIRTH.csv",
  "MDG_0000000025.csv",
  "MDG_0000000003.csv",
  "NUTRITION_ANAEMIA_REPRODUCTIVEAGE_PREV.csv",
  "WHS4_154.csv",
  "WHS4_115.csv"
)
missing_files <- need_files[!file.exists(file.path(DATA_DIR, need_files))]
if(length(missing_files)){
  stop("Missing files in ./data:\n- ", paste(missing_files, collapse = "\n- "))
}

# ---------------------------
# 4) Read + filter indicators
# ---------------------------
mmr_raw  <- prep_country_year(read_gho(file.path(DATA_DIR, "MDG_0000000026.csv")))
inst_raw <- prep_country_year(read_gho(file.path(DATA_DIR, "SRHINSTITUTIONALBIRTH.csv")))
sba_raw  <- prep_country_year(read_gho(file.path(DATA_DIR, "MDG_0000000025.csv")))
abr_raw  <- prep_country_year(read_gho(file.path(DATA_DIR, "MDG_0000000003.csv")))
an_raw   <- prep_country_year(read_gho(file.path(DATA_DIR, "NUTRITION_ANAEMIA_REPRODUCTIVEAGE_PREV.csv")))
anc4_raw <- prep_country_year(read_gho(file.path(DATA_DIR, "WHS4_154.csv")))
cs_raw   <- prep_country_year(read_gho(file.path(DATA_DIR, "WHS4_115.csv")))

abr_f <- abr_raw
if(all(c("DisaggregatingDimension1ValueCode","DisaggregatingDimension2ValueCode") %in% names(abr_raw))){
  abr_f <- abr_raw %>%
    dplyr::filter(DisaggregatingDimension1ValueCode == "SEX_FMLE") %>%
    dplyr::filter(DisaggregatingDimension2ValueCode == "AGEGROUP_YEARS15-19")
}

an_f <- an_raw
if(all(c("DisaggregatingDimension1ValueCode","DisaggregatingDimension2ValueCode") %in% names(an_raw))){
  an_f <- an_raw %>% dplyr::filter(DisaggregatingDimension1ValueCode == "SEX_FMLE")
  codes <- unique(na.omit(an_f$DisaggregatingDimension2ValueCode))
  if("PREGNANCYSTATUS_TOTAL" %in% codes){
    an_f <- an_f %>% dplyr::filter(DisaggregatingDimension2ValueCode == "PREGNANCYSTATUS_TOTAL")
  } else if("PREGNANCYSTATUS_NONPREGNANT" %in% codes){
    an_f <- an_f %>% dplyr::filter(DisaggregatingDimension2ValueCode == "PREGNANCYSTATUS_NONPREGNANT")
  } else if("PREGNANCYSTATUS_PREGNANT" %in% codes){
    an_f <- an_f %>% dplyr::filter(DisaggregatingDimension2ValueCode == "PREGNANCYSTATUS_PREGNANT")
  }
}

mmr  <- collapse_latest(mmr_raw)  %>% dplyr::rename(mmr = value)
inst <- collapse_latest(inst_raw) %>% dplyr::rename(inst_birth = value)
sba  <- collapse_latest(sba_raw)  %>% dplyr::rename(sba = value)
abr  <- collapse_latest(abr_f)    %>% dplyr::rename(abr_15_19 = value)
anem <- collapse_latest(an_f)     %>% dplyr::rename(anaemia = value)
anc4 <- collapse_latest(anc4_raw) %>% dplyr::rename(anc4 = value)
cs   <- collapse_latest(cs_raw)   %>% dplyr::rename(csection = value)

# ---------------------------
# 5) Build panel (country-year)
# ---------------------------
panel <- mmr %>%
  dplyr::select(country_code, year, who_region, who_region_code, mmr) %>%
  dplyr::left_join(inst %>% dplyr::select(country_code, year, inst_birth), by=c("country_code","year")) %>%
  dplyr::left_join(sba  %>% dplyr::select(country_code, year, sba), by=c("country_code","year")) %>%
  dplyr::left_join(anc4 %>% dplyr::select(country_code, year, anc4), by=c("country_code","year")) %>%
  dplyr::left_join(cs   %>% dplyr::select(country_code, year, csection), by=c("country_code","year")) %>%
  dplyr::left_join(abr  %>% dplyr::select(country_code, year, abr_15_19), by=c("country_code","year")) %>%
  dplyr::left_join(anem %>% dplyr::select(country_code, year, anaemia), by=c("country_code","year")) %>%
  dplyr::mutate(
    mmr = as.numeric(mmr),
    inst_birth = as.numeric(inst_birth),
    sba = as.numeric(sba),
    anc4 = as.numeric(anc4),
    csection = as.numeric(csection),
    abr_15_19 = as.numeric(abr_15_19),
    anaemia = as.numeric(anaemia),
    who_region = dplyr::if_else(is.na(who_region) | who_region=="", "UNKNOWN", who_region)
  ) %>%
  dplyr::mutate(
    inst_birth = dplyr::if_else(inst_birth < 0 | inst_birth > 100, NA_real_, inst_birth),
    sba = dplyr::if_else(sba < 0 | sba > 100, NA_real_, sba),
    anc4 = dplyr::if_else(anc4 < 0 | anc4 > 100, NA_real_, anc4),
    csection = dplyr::if_else(csection < 0 | csection > 100, NA_real_, csection),
    anaemia = dplyr::if_else(anaemia < 0 | anaemia > 100, NA_real_, anaemia)
  ) %>%
  dplyr::filter(!is.na(mmr)) %>%
  dplyr::mutate(
    mmr_w = winsor(mmr, c(0.01, 0.99)),
    lmmr = log(mmr_w + 1),
    lmmr_raw = log(mmr + 1)
  ) %>%
  dplyr::arrange(country_code, year) %>%
  dplyr::group_by(country_code) %>%
  dplyr::mutate(
    inst_l1 = dplyr::lag(inst_birth, 1),
    sba_l1  = dplyr::lag(sba, 1),
    anc4_l1 = dplyr::lag(anc4, 1),
    abr_l1  = dplyr::lag(abr_15_19, 1),
    an_l1   = dplyr::lag(anaemia, 1),
    inst_lead1 = dplyr::lead(inst_birth, 1)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    inst10 = inst_l1 / 10,
    sba10  = sba_l1 / 10,
    anc410 = anc4_l1 / 10,
    abr1   = abr_l1,
    an10   = an_l1 / 10,
    inst10_lead1 = inst_lead1 / 10,
    an10_c = an10 - mean(an10, na.rm = TRUE)
  )

panel_path <- file.path(OUTDIR, "data", "analysis_panel.csv")
readr::write_csv(panel, panel_path)
cat("Panel built. N rows:", nrow(panel),
    "| Countries:", dplyr::n_distinct(panel$country_code),
    "| Years:", dplyr::n_distinct(panel$year), "\n\n")

# ---------------------------
# 6) MODELS (H1 primary + defensibility suite)
# ---------------------------
dat_h1 <- panel %>% dplyr::filter(!is.na(inst10), !is.na(abr1), !is.na(an10), !is.na(lmmr))
m_h1_twfe <- fixest::feols(
  lmmr ~ inst10 + abr1 + an10 | country_code + year,
  data = dat_h1,
  vcov = ~ country_code + year,
  fixef.rm = "singleton"
)

m_h1_regionyear <- fixest::feols(
  lmmr ~ inst10 + abr1 + an10 | country_code + who_region^year,
  data = dat_h1,
  vcov = ~ country_code + year,
  fixef.rm = "singleton"
)

dat_cov <- panel %>% dplyr::filter(!is.na(inst10), !is.na(abr1), !is.na(an10), !is.na(sba10), !is.na(anc410), !is.na(lmmr))
m_h1_plus <- fixest::feols(
  lmmr ~ inst10 + abr1 + an10 + sba10 + anc410 | country_code + year,
  data = dat_cov,
  vcov = ~ country_code + year,
  fixef.rm = "singleton"
)

dat_h4 <- panel %>% dplyr::filter(!is.na(inst10), !is.na(an10_c), !is.na(abr1), !is.na(lmmr))
m_h4 <- fixest::feols(
  lmmr ~ inst10 + an10_c + inst10:an10_c + abr1 | country_code + year,
  data = dat_h4,
  vcov = ~ country_code + year,
  fixef.rm = "singleton"
)

m_spline <- fixest::feols(
  lmmr ~ splines::ns(inst_l1, df = 4) + abr1 + an10 | country_code + year,
  data = dat_h1 %>% dplyr::filter(!is.na(inst_l1)),
  vcov = ~ country_code + year,
  fixef.rm = "singleton"
)

dat_lead <- panel %>% dplyr::filter(!is.na(inst10_lead1), !is.na(abr1), !is.na(an10), !is.na(lmmr))
m_placebo_lead1 <- fixest::feols(
  lmmr ~ inst10_lead1 + abr1 + an10 | country_code + year,
  data = dat_lead,
  vcov = ~ country_code + year,
  fixef.rm = "singleton"
)

# ---------------------------
# 7) TABLES (5 total)
# ---------------------------

# Table 1: overall descriptives
t1 <- panel %>%
  dplyr::summarise(
    n_country_year = n(),
    countries = dplyr::n_distinct(country_code),
    years = dplyr::n_distinct(year),
    year_min = min(year, na.rm=TRUE),
    year_max = max(year, na.rm=TRUE),
    MMR_median = median(mmr, na.rm=TRUE),
    MMR_IQR = IQR(mmr, na.rm=TRUE),
    InstBirth_median = median(inst_birth, na.rm=TRUE),
    InstBirth_IQR = IQR(inst_birth, na.rm=TRUE),
    SBA_median = median(sba, na.rm=TRUE),
    ANC4_median = median(anc4, na.rm=TRUE),
    Anaemia_median = median(anaemia, na.rm=TRUE),
    ABR_median = median(abr_15_19, na.rm=TRUE)
  )
readr::write_csv(t1, file.path(OUTDIR,"tables","table1_overall_descriptives.csv"))
write_html_table(t1, file.path(OUTDIR,"tables","table1_overall_descriptives.html"),
                 "Table 1. Panel descriptives (overall).")

# Table 2: by region
t2 <- panel %>%
  dplyr::group_by(who_region) %>%
  dplyr::summarise(
    n = n(),
    countries = dplyr::n_distinct(country_code),
    MMR_median = median(mmr, na.rm=TRUE),
    InstBirth_median = median(inst_birth, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(desc(countries))
readr::write_csv(t2, file.path(OUTDIR,"tables","table2_by_region_descriptives.csv"))
write_html_table(t2, file.path(OUTDIR,"tables","table2_by_region_descriptives.html"),
                 "Table 2. Descriptives by WHO region (medians).")

# Table 3: H1 + robustness + placebo
models_tbl <- list(
  "H1 TWFE (primary)" = m_h1_twfe,
  "H1 + Region×Year FE" = m_h1_regionyear,
  "H1 + SBA + ANC4" = m_h1_plus,
  "Placebo: Lead(+1y) Inst" = m_placebo_lead1
)
modelsummary::msummary(
  models_tbl,
  gof_omit = "IC|Log|Adj|AIC|BIC",
  stars = TRUE,
  output = file.path(OUTDIR,"tables","table3_models_h1.html")
)
t3_df <- modelsummary::msummary(
  models_tbl,
  gof_omit = "IC|Log|Adj|AIC|BIC",
  stars = FALSE,
  output = "data.frame"
)
readr::write_csv(t3_df, file.path(OUTDIR,"tables","table3_models_h1.csv"))

# Table 4: H4 + spline joint test (FIXED HERE)
spline_terms <- grep("splines::ns\\(inst_l1, df = 4\\)", names(coef(m_spline)), value = TRUE)
jt <- tryCatch(fixest::wald(m_spline, spline_terms), error = function(e) NA)

# fixest::wald returns a named atomic vector: stat, p, df1, df2  (not a list)
jt_p <- NA_real_
if(is.atomic(jt) && !all(is.na(jt)) && !is.null(names(jt)) && ("p" %in% names(jt))){
  jt_p <- as.numeric(jt[["p"]])
}

ct_h4 <- fixest::coeftable(m_h4)
t4 <- data.frame(
  term = rownames(ct_h4),
  estimate = as.numeric(ct_h4[,"Estimate"]),
  std_error = as.numeric(ct_h4[,"Std. Error"]),
  p_value = as.numeric(ct_h4[,"Pr(>|t|)"]),
  stringsAsFactors = FALSE
)
t4_meta <- data.frame(
  spline_joint_p = jt_p,
  n_spline_terms_tested = length(spline_terms),
  note = "Spline joint test uses fixest::wald; p-value extracted from named vector element `p`.",
  stringsAsFactors = FALSE
)
readr::write_csv(t4, file.path(OUTDIR,"tables","table4_effect_modification_h4.csv"))
write_html_table(t4, file.path(OUTDIR,"tables","table4_effect_modification_h4.html"),
                 "Table 4. Effect modification (H4): inst10 × anaemia (centered) model coefficients.")
readr::write_csv(t4_meta, file.path(OUTDIR,"tables","table4_spline_joint_test.csv"))
write_html_table(t4_meta, file.path(OUTDIR,"tables","table4_spline_joint_test.html"),
                 "Table 4b. Spline joint test for nonlinearity in institutional delivery association.")

# Table 5: country policy translation (latest year per country)
b_inst <- as.numeric(coef(m_h1_twfe)["inst10"])
se_inst <- sqrt(as.numeric(vcov(m_h1_twfe)["inst10","inst10"]))

latest <- panel %>%
  dplyr::filter(!is.na(inst_birth), !is.na(mmr), !is.na(year)) %>%
  dplyr::arrange(country_code, year) %>%
  dplyr::group_by(country_code) %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::ungroup()

p10 <- lapply(latest$mmr, policy_translate, delta_pp=10, beta=b_inst, beta_se=se_inst)
p20 <- lapply(latest$mmr, policy_translate, delta_pp=20, beta=b_inst, beta_se=se_inst)

latest_policy <- latest %>%
  dplyr::mutate(
    mmr_plus10 = vapply(p10, function(x) x$mmr_new, numeric(1)),
    mmr_reduction10 = vapply(p10, function(x) x$delta, numeric(1)),
    mmr_red10_lwr = vapply(p10, function(x) x$delta_lo, numeric(1)),
    mmr_red10_upr = vapply(p10, function(x) x$delta_hi, numeric(1)),
    pct_red10 = vapply(p10, function(x) x$pct, numeric(1)),
    mmr_plus20 = vapply(p20, function(x) x$mmr_new, numeric(1)),
    mmr_reduction20 = vapply(p20, function(x) x$delta, numeric(1)),
    pct_red20 = vapply(p20, function(x) x$pct, numeric(1))
  ) %>%
  dplyr::arrange(desc(mmr_reduction10)) %>%
  dplyr::mutate(rank_priority_10pp = dplyr::row_number())

readr::write_csv(latest_policy, file.path(OUTDIR,"tables","table5_country_policy_translation.csv"))
write_html_table(
  latest_policy %>%
    dplyr::select(country_code, who_region, year, mmr, inst_birth,
                  mmr_reduction10, mmr_red10_lwr, mmr_red10_upr, pct_red10, rank_priority_10pp) %>%
    dplyr::slice_head(n=80),
  file.path(OUTDIR,"tables","table5_country_policy_translation_top80.html"),
  "Table 5. Policy translation (top 80 countries): predicted MMR reduction from +10pp institutional delivery coverage (associational, based on H1)."
)

# Save policy data for Shiny
saveRDS(
  list(panel = panel, latest_policy = latest_policy,
       coef_inst10 = b_inst, se_inst10 = se_inst),
  file.path(OUTDIR,"shiny_app","data","policy_data.rds")
)

# ---------------------------
# 8) FIGURES (5 PNG)
# ---------------------------

# Fig 1: spline dose–response
grid <- data.frame(
  inst_l1 = seq(0, 100, by = 1),
  abr1 = median(dat_h1$abr1, na.rm=TRUE),
  an10 = median(dat_h1$an10, na.rm=TRUE)
)
pred <- predict_ci_fixest(m_spline, grid, rhs_formula = ~ splines::ns(inst_l1, df=4) + abr1 + an10)
grid2 <- cbind(grid, pred) %>%
  dplyr::mutate(
    mmr_fit = mmr_from_eta(eta),
    mmr_lwr = mmr_from_eta(lwr),
    mmr_upr = mmr_from_eta(upr)
  )

p1 <- ggplot2::ggplot(grid2, ggplot2::aes(x = inst_l1, y = mmr_fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = mmr_lwr, ymax = mmr_upr), alpha = 0.18) +
  ggplot2::geom_line(linewidth = 1.1) +
  ggplot2::geom_rug(data = dat_h1, ggplot2::aes(x = inst_l1), sides = "b", alpha = 0.08, inherit.aes = FALSE) +
  ggplot2::coord_cartesian(clip="off") +
  ggplot2::labs(
    title = "Dose–response: institutional delivery coverage and predicted maternal mortality",
    subtitle = "Spline TWFE model; predictions at median adolescent birth rate and anaemia (FE set to 0 for shape)",
    x = "Institutional delivery coverage (%) (lagged 1 year)",
    y = "Predicted MMR (per 100,000 live births)",
    caption = "Ribbon = 95% CI from coefficient/vcov. Shape is associational (not causal)."
  ) +
  ggplot2::scale_y_continuous(labels = scales::label_number(big.mark=",")) +
  theme_pub()
ggsave_pub(file.path(OUTDIR,"figures","fig1_dose_response_spline.png"), p1, width=9.2, height=5.8)

# Fig 2: placebo lead vs lag coefficient
coef_df <- function(fit, term, label){
  ct <- fixest::coeftable(fit)
  if(!(term %in% rownames(ct))){
    return(data.frame(spec = label, term = term, estimate=NA, se=NA, lwr=NA, upr=NA))
  }
  est <- as.numeric(ct[term,"Estimate"])
  se <- as.numeric(ct[term,"Std. Error"])
  data.frame(spec = label, term = term, estimate = est, se = se,
             lwr = est - 1.96*se, upr = est + 1.96*se)
}
c_main <- coef_df(m_h1_twfe, "inst10", "Lagged inst (H1)")
c_lead <- coef_df(m_placebo_lead1, "inst10_lead1", "Lead(+1y) placebo")
c_plot <- dplyr::bind_rows(c_main, c_lead)

p2 <- ggplot2::ggplot(c_plot, ggplot2::aes(x = estimate, y = spec)) +
  ggplot2::geom_vline(xintercept = 0, linetype = 2, linewidth = 0.8, alpha = 0.8) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = lwr, xmax = upr), height = 0.18, linewidth = 0.9) +
  ggplot2::geom_point(size = 3.2) +
  ggplot2::coord_cartesian(clip="off") +
  ggplot2::labs(
    title = "Reverse-causality check: lagged effect vs placebo lead",
    subtitle = "Coefficient: change in log(MMR+1) per +10pp institutional delivery coverage",
    x = "Estimated coefficient (95% CI)",
    y = NULL
  ) +
  theme_pub()
ggsave_pub(file.path(OUTDIR,"figures","fig2_placebo_lead_vs_lag.png"), p2, width=8.8, height=4.8)

# Fig 3: global trends
trend <- panel %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    mmr_med = median(mmr, na.rm=TRUE),
    inst_med = median(inst_birth, na.rm=TRUE),
    .groups="drop"
  )
a <- (max(trend$mmr_med, na.rm=TRUE) - min(trend$mmr_med, na.rm=TRUE)) / 100
b0 <- min(trend$mmr_med, na.rm=TRUE)
trend <- trend %>% dplyr::mutate(inst_scaled = inst_med * a + b0)

p3 <- ggplot2::ggplot(trend, ggplot2::aes(x = year)) +
  ggplot2::geom_line(ggplot2::aes(y = mmr_med, linetype = "Median MMR"), linewidth = 1.1) +
  ggplot2::geom_line(ggplot2::aes(y = inst_scaled, linetype = "Median institutional delivery (scaled)"), linewidth = 1.1) +
  ggplot2::scale_linetype_manual(values = c("solid","dashed")) +
  ggplot2::coord_cartesian(clip="off") +
  ggplot2::labs(
    title = "Global trends (unweighted medians across countries)",
    subtitle = "Institutional delivery is scaled to share the MMR axis for visual comparability",
    x = "Year",
    y = "Median MMR (per 100,000)",
    caption = "Scaling is visual only."
  ) +
  theme_pub()
ggsave_pub(file.path(OUTDIR,"figures","fig3_global_trends.png"), p3, width=9.2, height=5.4)

# Fig 4: priority countries top 25
top25 <- latest_policy %>% dplyr::slice_head(n=25) %>%
  dplyr::mutate(country_code = factor(country_code, levels = rev(country_code)))

p4 <- ggplot2::ggplot(top25, ggplot2::aes(x = mmr_reduction10, y = country_code, color = who_region)) +
  ggplot2::geom_vline(xintercept = 0, linetype=2, linewidth=0.8, alpha=0.7) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = mmr_red10_lwr, xmax = mmr_red10_upr), height = 0.18, linewidth=0.8, alpha=0.9) +
  ggplot2::geom_point(size = 2.8) +
  ggplot2::coord_cartesian(clip="off") +
  ggplot2::scale_color_viridis_d(end=0.85) +
  ggplot2::labs(
    title = "Priority setting: largest predicted MMR reduction from +10pp coverage",
    subtitle = "Associational translation based on H1 coefficient applied to each country's latest MMR",
    x = "Predicted MMR reduction (deaths per 100,000) for +10pp institutional delivery",
    y = NULL,
    color = "WHO region"
  ) +
  theme_pub()
ggsave_pub(file.path(OUTDIR,"figures","fig4_priority_countries_top25.png"), p4, width=10.2, height=7.2)

# Fig 5: spec robustness forest for inst10
dat_raw <- panel %>% dplyr::filter(!is.na(inst10), !is.na(abr1), !is.na(an10), !is.na(lmmr_raw))
m_h1_raw <- fixest::feols(
  lmmr_raw ~ inst10 + abr1 + an10 | country_code + year,
  data = dat_raw,
  vcov = ~ country_code + year,
  fixef.rm = "singleton"
)

specs <- data.frame(
  spec = c("H1 TWFE (primary)","H1 + Region×Year FE","H1 + SBA + ANC4","H1 (no winsor)"),
  estimate = c(
    as.numeric(coef(m_h1_twfe)["inst10"]),
    as.numeric(coef(m_h1_regionyear)["inst10"]),
    as.numeric(coef(m_h1_plus)["inst10"]),
    as.numeric(coef(m_h1_raw)["inst10"])
  ),
  se = c(
    sqrt(as.numeric(vcov(m_h1_twfe)["inst10","inst10"])),
    sqrt(as.numeric(vcov(m_h1_regionyear)["inst10","inst10"])),
    sqrt(as.numeric(vcov(m_h1_plus)["inst10","inst10"])),
    sqrt(as.numeric(vcov(m_h1_raw)["inst10","inst10"]))
  )
) %>%
  dplyr::mutate(lwr = estimate - 1.96*se,
                upr = estimate + 1.96*se,
                spec = factor(spec, levels = rev(spec)))

p5 <- ggplot2::ggplot(specs, ggplot2::aes(x = estimate, y = spec)) +
  ggplot2::geom_vline(xintercept = 0, linetype=2, linewidth=0.8, alpha=0.8) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = lwr, xmax = upr), height = 0.18, linewidth=0.9) +
  ggplot2::geom_point(size=3.0) +
  ggplot2::coord_cartesian(clip="off") +
  ggplot2::labs(
    title = "Robustness: institutional delivery coefficient across specifications",
    subtitle = "Coefficient: change in log(MMR+1) per +10pp institutional delivery (lagged 1 year)",
    x = "Estimated coefficient (95% CI)",
    y = NULL
  ) +
  theme_pub()
ggsave_pub(file.path(OUTDIR,"figures","fig5_spec_robustness_forest.png"), p5, width=9.6, height=5.4)

# ---------------------------
# 9) Shiny app scaffold (app.R + CSS + JS)
# ---------------------------
app_dir <- file.path(OUTDIR, "shiny_app")

css <- "
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; }
.small-note { font-size: 0.92rem; opacity: 0.92; }
.kpi { padding: 12px 14px; border-radius: 14px; background: #ffffff; box-shadow: 0 8px 22px rgba(0,0,0,0.06); }
.kpi h4 { margin: 0; font-weight: 700; }
.kpi p { margin: 6px 0 0 0; }
hr.soft { border: none; height: 1px; background: rgba(0,0,0,0.08); margin: 14px 0; }
"
js <- "document.addEventListener('DOMContentLoaded', function(){ document.querySelectorAll('a').forEach(e => e.setAttribute('rel','noopener')); });"
writeLines(css, con = file.path(app_dir, "www", "style.css"))
writeLines(js,  con = file.path(app_dir, "www", "custom.js"))

appR <- '
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)

DATA <- readRDS(file.path("data","policy_data.rds"))
panel <- DATA$panel
latest <- DATA$latest_policy
b <- DATA$coef_inst10
se <- DATA$se_inst10

mmr_from_eta <- function(eta){ pmax(0, exp(eta) - 1) }

policy_translate <- function(base_mmr, delta_pp, beta, beta_se){
  eta0 <- log(base_mmr + 1)
  d_eta <- beta * (delta_pp/10)
  d_eta_lo <- (beta - 1.96*beta_se) * (delta_pp/10)
  d_eta_hi <- (beta + 1.96*beta_se) * (delta_pp/10)

  mmr_new <- mmr_from_eta(eta0 + d_eta)
  mmr_new_lo <- mmr_from_eta(eta0 + d_eta_lo)
  mmr_new_hi <- mmr_from_eta(eta0 + d_eta_hi)

  delta <- base_mmr - mmr_new
  delta_lo <- base_mmr - mmr_new_hi
  delta_hi <- base_mmr - mmr_new_lo

  pct <- 100 * (1 - (mmr_new + 1) / (base_mmr + 1))
  list(mmr_new = mmr_new, delta = delta, delta_lo = delta_lo, delta_hi = delta_hi, pct = pct)
}

theme_pub <- function(){
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face="bold", size=15, margin=margin(b=6)),
      plot.subtitle = element_text(size=11, margin=margin(b=8)),
      plot.caption = element_text(size=9, hjust=0, margin=margin(t=10)),
      axis.title = element_text(face="bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

ui <- page_navbar(
  title = "Facility Delivery Coverage & Maternal Mortality (Policy Explorer)",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  header = tags$head(
    tags$link(rel="stylesheet", type="text/css", href="style.css"),
    tags$script(src="custom.js")
  ),

  nav_panel(
    "Country Explorer",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("cc", "Select country", choices = sort(unique(latest$country_code))),
        sliderInput("delta", "Assumed increase in institutional delivery coverage (percentage points)",
                    min=0, max=30, value=10, step=1),
        helpText(class="small-note",
                 "Associational translation from a fixed-effects model; not causal proof.")
      ),
      card(
        card_header("Policy impact snapshot"),
        uiOutput("kpis"),
        tags$hr(class="soft"),
        plotOutput("ts", height = 320),
        tags$hr(class="soft"),
        DTOutput("country_table")
      )
    )
  ),

  nav_panel(
    "Global Evidence",
    layout_columns(
      col_widths = c(6,6),
      card(card_header("Dose–response shape (spline)"),
           tags$img(src="fig1_dose_response_spline.png", style="width:100%; border-radius:14px;")),
      card(card_header("Robustness across specifications"),
           tags$img(src="fig5_spec_robustness_forest.png", style="width:100%; border-radius:14px;"))
    ),
    layout_columns(
      col_widths = c(6,6),
      card(card_header("Placebo lead vs lag"),
           tags$img(src="fig2_placebo_lead_vs_lag.png", style="width:100%; border-radius:14px;")),
      card(card_header("Global trends"),
           tags$img(src="fig3_global_trends.png", style="width:100%; border-radius:14px;"))
    ),
    layout_columns(
      col_widths = c(12),
      card(card_header("Priority setting (top 25)"),
           tags$img(src="fig4_priority_countries_top25.png", style="width:100%; border-radius:14px;"))
    )
  ),

  nav_panel(
    "Download",
    card(
      card_header("Download policy table"),
      downloadButton("dl_table", "Download country policy table (CSV)")
    )
  )
)

server <- function(input, output, session){

  output$kpis <- renderUI({
    row <- latest %>% filter(country_code == input$cc) %>% slice(1)
    if(nrow(row)==0) return(NULL)
    base_mmr <- row$mmr
    base_cov <- row$inst_birth
    out <- policy_translate(base_mmr, input$delta, b, se)

    tagList(
      div(class="kpi",
          h4(sprintf("Baseline MMR: %.1f", base_mmr)),
          p(sprintf("Baseline institutional delivery: %.1f%%", base_cov))),
      div(style="height:10px;"),
      div(class="kpi",
          h4(sprintf("Projected MMR after +%dpp: %.1f", input$delta, out$mmr_new)),
          p(sprintf("Reduction: %.1f (95%% CI %.1f to %.1f) per 100,000 | %.2f%% (MMR+1)",
                    out$delta, out$delta_lo, out$delta_hi, out$pct)))
    )
  })

  output$ts <- renderPlot({
    df <- panel %>% filter(country_code == input$cc) %>% arrange(year)
    ggplot(df, aes(x=year)) +
      geom_line(aes(y=mmr), linewidth=1.05) +
      geom_line(aes(y=inst_birth * (max(mmr, na.rm=TRUE)/100)), linetype=2, linewidth=1.05) +
      coord_cartesian(clip="off") +
      labs(
        title = "Country time series",
        subtitle = "Solid: MMR; Dashed: institutional delivery (scaled to MMR axis)",
        x = "Year", y = "MMR (per 100,000)",
        caption = "Scaling is visual only."
      ) +
      scale_y_continuous(labels = label_number(big.mark=",")) +
      theme_pub()
  })

  output$country_table <- renderDT({
    df <- latest %>% filter(country_code == input$cc) %>%
      select(country_code, who_region, year, mmr, inst_birth,
             mmr_reduction10, pct_red10, mmr_reduction20, pct_red20)
    datatable(df, options = list(dom="t", scrollX = TRUE))
  })

  output$dl_table <- downloadHandler(
    filename = function(){ "country_policy_translation.csv" },
    content = function(file){
      write.csv(latest, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
'
writeLines(appR, con = file.path(app_dir, "app.R"))

# Copy figures into Shiny www
file.copy(file.path(OUTDIR,"figures","fig1_dose_response_spline.png"), file.path(app_dir,"www","fig1_dose_response_spline.png"), overwrite=TRUE)
file.copy(file.path(OUTDIR,"figures","fig2_placebo_lead_vs_lag.png"), file.path(app_dir,"www","fig2_placebo_lead_vs_lag.png"), overwrite=TRUE)
file.copy(file.path(OUTDIR,"figures","fig3_global_trends.png"), file.path(app_dir,"www","fig3_global_trends.png"), overwrite=TRUE)
file.copy(file.path(OUTDIR,"figures","fig4_priority_countries_top25.png"), file.path(app_dir,"www","fig4_priority_countries_top25.png"), overwrite=TRUE)
file.copy(file.path(OUTDIR,"figures","fig5_spec_robustness_forest.png"), file.path(app_dir,"www","fig5_spec_robustness_forest.png"), overwrite=TRUE)

cat("\nShiny app created at:", app_dir, "\n")
cat("To run locally: shiny::runApp('", app_dir, "')\n\n", sep="")

# ---------------------------
# 10) Reproducibility logs (sha256 + sessionInfo)
# ---------------------------
in_manifest <- data.frame(
  path = file.path(DATA_DIR, need_files),
  sha256 = vapply(file.path(DATA_DIR, need_files), sha256, character(1)),
  stringsAsFactors = FALSE
)
readr::write_csv(in_manifest, file.path(OUTDIR,"logs","input_sha256_manifest.csv"))

si <- capture.output(sessionInfo())
writeLines(si, con = file.path(OUTDIR,"logs","sessionInfo.txt"))

out_files <- c(
  list.files(file.path(OUTDIR,"tables"), full.names = TRUE),
  list.files(file.path(OUTDIR,"figures"), full.names = TRUE),
  list.files(file.path(OUTDIR,"shiny_app"), full.names = TRUE, recursive = TRUE),
  file.path(OUTDIR,"data","analysis_panel.csv")
)
out_files <- out_files[file.exists(out_files)]
out_manifest <- data.frame(
  path = out_files,
  sha256 = vapply(out_files, sha256, character(1)),
  stringsAsFactors = FALSE
)
readr::write_csv(out_manifest, file.path(OUTDIR,"logs","output_sha256_manifest.csv"))

cat("Outputs written under:", OUTDIR, "\n")
cat("Figures:", length(list.files(file.path(OUTDIR,"figures"), pattern="\\.png$", full.names=TRUE)), "PNG\n")
cat("Tables :", length(list.files(file.path(OUTDIR,"tables"), full.names=TRUE)), "files\n")
cat("Done.\n")