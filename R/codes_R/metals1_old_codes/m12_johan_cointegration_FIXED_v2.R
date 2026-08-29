# =====================================================================
#  JOHANSEN COINTEGRATION - RAW vs BUBBLE-FILTERED
#  Wersja poprawiona. Zmiany wzgledem oryginalu oznaczone [ZMIANA n].
#  Dwie wersje Tabeli 4: A (8 kolumn, bez alfy) i B (10 kolumn, z alfa).
# =====================================================================

library(vars)
library(urca)
library(here)
library(tidyverse)
library(kableExtra)


# ---------------------------------------------------------------------
#  KONFIGURACJA  [ZMIANA 1]
# ---------------------------------------------------------------------
# SPEC = "transitory" -> Pi mnozy y_{t-1}, wiec alpha = predkosc dostosowania
#   do nierownowagi z POPRZEDNIEGO okresu. Tak opisujesz alpha w artykule.
# SPEC = "longrun" (domyslne w ca.jo) -> Pi mnozy y_{t-K}. Inna interpretacja.
SPEC <- "transitory"

# TRUE  -> ten sam rzad opoznien w modelu raw i filtered (tak mowi artykul)
# FALSE -> osobny dobor AIC dla kazdego modelu (tak bylo w oryginale)
SAME_LAG_BOTH_MODELS <- TRUE

MAX_LAG <- 10

OUT_DIR <- "R/results_R"


# ---------------------------------------------------------------------
#  DANE  [ZMIANA 2]  - laczenie po Date zamiast po pozycji wiersza
# ---------------------------------------------------------------------
df_raw <- read.csv2(file.path(OUT_DIR, "series_and_bubble_up.csv"))
df_raw$Date <- as.Date(df_raw$Date, format = "%Y-%m-%d")

price_cols <- names(df_raw)[2:17]
dummy_cols <- names(df_raw)[18:33]

stopifnot(length(price_cols) == 16, length(dummy_cols) == 16)


# ---------------------------------------------------------------------
#  FUNKCJA GLOWNA
# ---------------------------------------------------------------------
run_metal_analysis <- function(metal_name, columns) {

  message("\n=== Analyzing: ", metal_name, " ===")

  tryCatch({

    # --- [ZMIANA 2] ceny i dummy wycinane RAZEM, po Date ---
    cols_bd <- paste0(columns, "_BD")

    dat <- df_raw %>%
      dplyr::select(Date, dplyr::all_of(columns), dplyr::all_of(cols_bd)) %>%
      tidyr::drop_na()

    price_raw <- as.matrix(dat[, columns, drop = FALSE])

    if (any(price_raw <= 0, na.rm = TRUE)) {
      stop("Non-positive prices found - log() would give NaN/-Inf.")
    }

    metal_data <- log(price_raw)
    metal_dummies <- as.matrix(dat[, cols_bd, drop = FALSE])
    colnames(metal_dummies) <- paste0(columns, "_DV")

    stopifnot(nrow(metal_data) == nrow(metal_dummies))
    message("  Data dimensions: ", nrow(metal_data), " x ", ncol(metal_data))
    message("  Sample: ", min(dat$Date), " to ", max(dat$Date))

    # --- usuniecie stalych kolumn dummy (serie bez babli) ---
    dummy_vars <- apply(metal_dummies, 2, var, na.rm = TRUE)
    constant_cols <- which(dummy_vars < 1e-10)

    if (length(constant_cols) > 0) {
      message("  Removing ", length(constant_cols), " constant dummy column(s): ",
              paste(colnames(metal_dummies)[constant_cols], collapse = ", "))
      metal_dummies <- metal_dummies[, -constant_cols, drop = FALSE]
    }

    has_dummies <- ncol(metal_dummies) > 0
    if (!has_dummies) {
      message("  WARNING: No bubbles detected - skipping exogenous model")
    } else {
      message("  Using ", ncol(metal_dummies), " bubble dummy variable(s)")
    }

    # --- dobor opoznien  [ZMIANA 3] guard K >= 2 + wspolny lag ---
    lags_basic <- VARselect(metal_data, lag.max = MAX_LAG, type = "const")
    best_lag_basic <- max(2, as.integer(lags_basic$selection["AIC(n)"]))

    if (has_dummies) {
      lags_ex <- VARselect(metal_data, lag.max = MAX_LAG, type = "const",
                           exogen = metal_dummies)
      best_lag_ex <- max(2, as.integer(lags_ex$selection["AIC(n)"]))
    } else {
      best_lag_ex <- best_lag_basic
    }

    lag_ex_aic <- best_lag_ex
    if (SAME_LAG_BOTH_MODELS) best_lag_ex <- best_lag_basic

    if (lag_ex_aic != best_lag_basic) {
      message("  NOTE: AIC lag differs (raw = ", best_lag_basic,
              ", filtered = ", lag_ex_aic, "). Using ",
              ifelse(SAME_LAG_BOTH_MODELS, "the raw lag for both.",
                     "separate lags."))
    }

    # --- test Johansena  [ZMIANA 4] spec = SPEC w obu modelach ---
    jo_basic <- ca.jo(metal_data, type = "trace", ecdet = "const",
                      K = best_lag_basic, spec = SPEC)

    jo_ex <- if (has_dummies) {
      ca.jo(metal_data, type = "trace", ecdet = "const",
            K = best_lag_ex, spec = SPEC, dumvar = metal_dummies)
    } else NULL

    # --- sekwencyjne ustalenie rzedu ---
    get_rank <- function(jo) {
      ts <- jo@teststat; cv <- jo@cval[, 2]
      r <- 0
      for (i in length(ts):1) {
        if (ts[i] > cv[i]) r <- r + 1 else break
      }
      r
    }

    r_basic <- get_rank(jo_basic)
    r_ex    <- if (!is.null(jo_ex)) get_rank(jo_ex) else NA_integer_

    results <- tibble(
      Metal         = metal_name,
      N_Series      = ncol(metal_data),
      N_Obs         = nrow(metal_data),
      N_Bubble_Vars = if (has_dummies) ncol(metal_dummies) else 0,
      Lag_Basic     = best_lag_basic,
      Lag_Ex        = best_lag_ex,
      Lag_Ex_AIC    = lag_ex_aic,
      Rank_Basic    = r_basic,
      Rank_Ex       = r_ex,
      Trace_r0_Basic = jo_basic@teststat[length(jo_basic@teststat)],
      Trace_r0_Ex    = if (!is.null(jo_ex)) jo_ex@teststat[length(jo_ex@teststat)] else NA_real_,
      Normalised_On  = columns[1],                 # [ZMIANA 5]
      Status = if (!has_dummies) "No bubbles - Basic only" else "Complete"
    )

    message("  OK - ", metal_name)

    list(results = results, jo_basic = jo_basic, jo_ex = jo_ex,
         data = metal_data, dates = dat$Date,
         dummies = if (has_dummies) metal_dummies else NULL,
         n_bubbles_removed = length(constant_cols))

  }, error = function(e) {
    message("  ERROR in ", metal_name, ": ", e$message)
    list(results = tibble(Metal = metal_name, N_Series = length(columns),
                          Status = paste("ERROR:", e$message)),
         jo_basic = NULL, jo_ex = NULL, data = NULL, dates = NULL,
         dummies = NULL, n_bubbles_removed = NA)
  })
}


# ---------------------------------------------------------------------
#  EKSTRAKCJA WYNIKOW
# ---------------------------------------------------------------------
extract_full_results <- function(metal_name, jo_obj, model_name, price_data) {

  if (is.null(jo_obj)) return(NULL)

  trace_stats  <- jo_obj@teststat
  n_tests      <- length(trace_stats)
  crit_vals_10 <- jo_obj@cval[, 1]
  crit_vals_5  <- jo_obj@cval[, 2]
  crit_vals_1  <- jo_obj@cval[, 3]

  r <- 0
  for (i in n_tests:1) {
    if (trace_stats[i] > crit_vals_5[i]) r <- r + 1 else break
  }

  k_ser  <- ncol(price_data)
  safe_r <- max(1, min(r, k_ser - 1))
  if (r == 0) message("  WARNING: r = 0; alpha reported for r = 1 only illustratively.")

  vecm <- cajorls(jo_obj, r = safe_r)
  exchange_names <- colnames(price_data)

  # =============================================================
  #  [ZMIANA 6] POPRAWNA EKSTRAKCJA ALPHA
  # =============================================================
  cf        <- coef(vecm$rlm)
  ect_rows  <- grep("^ect", rownames(cf))
  alpha_mat <- t(cf[ect_rows, , drop = FALSE])          # k x r
  rownames(alpha_mat) <- exchange_names
  colnames(alpha_mat) <- paste0("ect", seq_len(safe_r))

  # [ZMIANA 7] laczna predkosc dostosowania = norma wiersza
  adj_speed <- sqrt(rowSums(alpha_mat^2))

  leader_idx   <- which.min(adj_speed)
  follower_idx <- which.max(adj_speed)

  leader_name    <- exchange_names[leader_idx]
  follower_name  <- exchange_names[follower_idx]
  leader_alpha   <- adj_speed[leader_idx]
  follower_alpha <- adj_speed[follower_idx]

  # [ZMIANA 5] normalizacja beta
  norm_series <- exchange_names[1]
  own_alpha   <- alpha_mat[1, 1]
  if (own_alpha > 0) {
    message("  NOTE [", metal_name, " ", model_name, "]: alpha na serii ",
            "normalizujacej (", norm_series, ") = ", round(own_alpha, 4),
            " (dodatnia) - sprawdz konwencje znaku.")
  }

  # =============================================================
  #  [ZMIANA 8] TEST SLABEJ EGZOGENICZNOSCI (LR)
  # =============================================================
  wx_stat <- vapply(seq_len(k_ser), function(i) {
    A   <- diag(k_ser)[, -i, drop = FALSE]
    out <- try(urca::alrtest(z = jo_obj, A = A, r = safe_r), silent = TRUE)
    if (inherits(out, "try-error")) return(NA_real_)
    as.numeric(out@teststat)[1]
  }, numeric(1))

  wx_pval <- pchisq(wx_stat, df = safe_r, lower.tail = FALSE)   # df = r
  names(wx_stat) <- names(wx_pval) <- exchange_names

  wx_leader <- if (all(is.na(wx_pval))) NA_character_ else
    exchange_names[which.max(replace(wx_pval, is.na(wx_pval), -Inf))]

  # =============================================================
  #  [ZMIANA 9] WKLAD POSZCZEGOLNYCH WARTOSCI WLASNYCH
  # =============================================================
  Tobs      <- nrow(jo_obj@Z0)
  lam       <- head(jo_obj@lambda, k_ser)
  trace_con <- -Tobs * log(1 - lam)

  message("  [", metal_name, " ", model_name, "] T = ", Tobs)
  message("    lambda:    ", paste(sprintf("%.4f", lam), collapse = " | "))
  message("    wklad do trace: ", paste(sprintf("%.2f", trace_con), collapse = " | "))

  # --- gwiazdki dla statystyk trace ---
  add_stars <- function(stat, c10, c5, c1) {
    if (stat > c1) return(paste0(sprintf("%.2f", stat), "***"))
    if (stat > c5) return(paste0(sprintf("%.2f", stat), "**"))
    if (stat > c10) return(paste0(sprintf("%.2f", stat), "*"))
    sprintf("%.2f", stat)
  }

  trace_formatted <- character(n_tests)
  for (i in 1:n_tests) {
    if (i <= r + 1) {
      j <- n_tests - i + 1
      trace_formatted[i] <- add_stars(trace_stats[j], crit_vals_10[j],
                                      crit_vals_5[j], crit_vals_1[j])
    } else trace_formatted[i] <- ""
  }

  result <- tibble(Metal = metal_name, Model = model_name, Rank = r)

  for (i in 0:(n_tests - 1)) {
    col_name <- if (i == 0) "r=0" else paste0("r<=", i)
    result[[col_name]] <- trace_formatted[i + 1]
  }

  result$`Max |a|`     <- sprintf("%.3f", follower_alpha)
  result$Follower      <- follower_name
  result$`Min |a|`     <- sprintf("%.3f", leader_alpha)
  result$Leader        <- leader_name
  result$WX_Leader     <- wx_leader
  result$WX_p          <- sprintf("%.3f", suppressWarnings(max(wx_pval, na.rm = TRUE)))
  result$WX_p_leader   <- sprintf("%.3f", wx_pval[leader_idx])
  result$WX_p_follower <- sprintf("%.3f", wx_pval[follower_idx])   # [NOWE] do gwiazdek w wersji B
  result$Normalised    <- norm_series

  attr(result, "alpha_mat") <- alpha_mat
  attr(result, "wx_pval")   <- wx_pval
  attr(result, "beta")      <- vecm$beta
  attr(result, "trace_con") <- trace_con

  result
}


# ---------------------------------------------------------------------
#  URUCHOMIENIE
# ---------------------------------------------------------------------
metal_configs <- list(
  "Cobalt"  = c("COCOMX", "CODALY", "COLMEX", "COWUXI"),
  "Copper"  = c("CUCOMX", "CUDALY", "CULMEX", "CUSHFE"),
  "Lithium" = c("LICOMX", "LIDALY", "LILMEX", "LIANTH"),
  "Nickel"  = c("NIDALY", "NILMEX", "NISHFE", "NIWUXI")
)

all_results <- map(names(metal_configs),
                   ~ run_metal_analysis(.x, metal_configs[[.x]]))
names(all_results) <- names(metal_configs)

summary_table <- map_dfr(all_results, ~ .x$results)
print(summary_table)

# [ZMIANA 3] KONTROLA: czy AIC chcial innego opoznienia dla modelu filtered?
message("\n=== Lag check ===")
summary_table %>%
  dplyr::select(Metal, Lag_Basic, Lag_Ex, Lag_Ex_AIC, Rank_Basic, Rank_Ex) %>%
  print()

# test najwyzszego opoznienia dla niklu:
# ni  <- metal_configs[["Nickel"]]
# dat <- df_raw %>% dplyr::select(Date, all_of(ni)) %>% tidyr::drop_na()
# X   <- log(as.matrix(dat[, ni]))
# sel <- VARselect(X, lag.max = 20, type = "const")
# sel$selection
# round(sel$criteria["AIC(n)", ], 3)


# ---------------------------------------------------------------------
#  ZEBRANIE WYNIKOW
# ---------------------------------------------------------------------
raw_data <- map_dfr(names(all_results), function(metal) {
  res <- all_results[[metal]]
  extract_full_results(toupper(metal), res$jo_basic, "Raw", res$data)
})

filtered_data <- map_dfr(names(all_results), function(metal) {
  res <- all_results[[metal]]
  if (!is.null(res$jo_ex))
    extract_full_results(toupper(metal), res$jo_ex, "Filtered", res$data)
  else NULL
})

latex_table_data <- bind_rows(raw_data, filtered_data)
print(latex_table_data)

# --- podglad: pelna macierz alpha i testy WX ---
message("\n=== Alpha matrices (raw) ===")
walk(names(metal_configs), function(m) {
  res <- all_results[[m]]
  if (is.null(res$jo_basic)) return(invisible(NULL))
  out <- extract_full_results(toupper(m), res$jo_basic, "Raw", res$data)
  cat("\n--", m, "-- normalised on", head(rownames(attr(out, "beta")), 1), "\n")
  print(round(attr(out, "alpha_mat"), 4))
  cat("weak exogeneity p-values:\n")
  print(round(attr(out, "wx_pval"), 4))
})


# ---------------------------------------------------------------------
#  WSPOLNE ELEMENTY OBU WERSJI TABELI
# ---------------------------------------------------------------------
trace_cols   <- names(latex_table_data)[grepl("^r", names(latex_table_data))]
trace_labels <- ifelse(trace_cols == "r=0", "$r=0$",
                       paste0("$r \\leq ", sub("^r<=", "", trace_cols), "$"))

n_raw      <- sum(latex_table_data$Model == "Raw")
n_filtered <- sum(latex_table_data$Model == "Filtered")

# pelny zapis diagnostyczny - wspolny dla obu wersji
write.csv2(latex_table_data,
           file.path(OUT_DIR, "metals1_tab4_full.csv"), row.names = FALSE)


# =====================================================================
#  WERSJA A - 8 kolumn, bez alfy, lider z testu WX
# =====================================================================
table_A <- latex_table_data %>%
  mutate(
    Leader_Disp = if_else(as.numeric(WX_p) < 0.05,
                          "none",
                          paste0(WX_Leader, " [",
                                 sprintf("%.2f", as.numeric(WX_p)), "]"))
  ) %>%
  dplyr::select(Metal, Model, Rank, all_of(trace_cols), Follower, Leader_Disp)

message("\n=== WERSJA A ===")
print(table_A)

write.csv2(table_A, file.path(OUT_DIR, "metals1_tab4_A.csv"), row.names = FALSE)

latex_A <- table_A %>%
  dplyr::select(-Model) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      align = c("l", "c", rep("c", length(trace_cols)), "l", "l"),
      col.names = c("Metal", "$r$", trace_labels, "Follower", "Leader [$p$]"),
      caption = "Results of the Johansen Cointegration Tests",
      label = "tab:johansen") %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  column_spec(1, bold = TRUE) %>%
  add_header_above(c(" " = 2,
                     "Trace statistics" = length(trace_cols),
                     "Price discovery" = 2)) %>%
  pack_rows("Raw series", 1, n_raw, bold = TRUE, indent = FALSE) %>%
  pack_rows("Filtered", n_raw + 1, n_raw + n_filtered, bold = TRUE, indent = FALSE)

cat(latex_A)
writeLines(as.character(latex_A), file.path(OUT_DIR, "metals1_tab4_A.tex"))


# =====================================================================
#  WERSJA B - 10 kolumn, z alfa i gwiazdkami z testu WX
# =====================================================================
# Gwiazdki oznaczaja ODRZUCENIE slabej egzogenicznosci, czyli:
#   przy Max |a| -> seria istotnie sie dostosowuje (oczekiwane dla followera)
#   przy Min |a| -> nawet najwolniejsza seria sie dostosowuje,
#                   wiec NIE jest liderem cenowym
add_wx_stars <- function(val, p) {
  p <- as.numeric(p)
  v <- sprintf("%.3f", as.numeric(val))
  ifelse(is.na(p), v,
  ifelse(p < 0.01, paste0(v, "***"),
  ifelse(p < 0.05, paste0(v, "**"),
  ifelse(p < 0.10, paste0(v, "*"), v))))
}

table_B <- latex_table_data %>%
  mutate(
    MaxA = add_wx_stars(`Max |a|`, WX_p_follower),
    MinA = add_wx_stars(`Min |a|`, WX_p_leader)
  ) %>%
  dplyr::select(Metal, Model, Rank, all_of(trace_cols),
                MaxA, Follower, MinA, Leader)

message("\n=== WERSJA B ===")
print(table_B)

write.csv2(table_B, file.path(OUT_DIR, "metals1_tab4_B.csv"), row.names = FALSE)

latex_B <- table_B %>%
  dplyr::select(-Model) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      align = c("l", "c", rep("c", length(trace_cols)), "c", "l", "c", "l"),
      col.names = c("Metal", "$r$", trace_labels,
                    "Max $|\\alpha|$", "Follower",
                    "Min $|\\alpha|$", "Leader"),
      caption = "Results of the Johansen Cointegration Tests",
      label = "tab:johansen") %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  column_spec(1, bold = TRUE) %>%
  add_header_above(c(" " = 2,
                     "Trace statistics" = length(trace_cols),
                     "Adjustment speed" = 4), escape = FALSE) %>%
  pack_rows("Raw series", 1, n_raw, bold = TRUE, indent = FALSE) %>%
  pack_rows("Filtered", n_raw + 1, n_raw + n_filtered, bold = TRUE, indent = FALSE)

cat(latex_B)
writeLines(as.character(latex_B), file.path(OUT_DIR, "metals1_tab4_B.tex"))


# ---------------------------------------------------------------------
#  KONTROLA: nikiel - gdzie Leader i WX_Leader sie roznia
# ---------------------------------------------------------------------
latex_table_data %>%
  dplyr::filter(Metal == "NICKEL") %>%
  dplyr::select(Model, Leader, WX_Leader, WX_p, WX_p_leader, WX_p_follower)
