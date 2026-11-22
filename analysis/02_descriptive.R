# 02_descriptive.R
# dataset.csv -> descriptive tables, plots and the parametric /
# non-parametric decision for each question

d <- read.csv("analysis/dataset.csv", stringsAsFactors = FALSE)
dir.create("report/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("report/figures", recursive = TRUE, showWarnings = FALSE)

f <- d[d$offers_filter == 1, ]    # cafes with both prices


# ---- Helper functions ----

skewness <- function(x) {
  x <- x[!is.na(x)]
  mean((x - mean(x))^3) / (mean((x - mean(x))^2))^1.5
}

mean_ci <- function(x) {
  x <- x[!is.na(x)]
  se <- sd(x) / sqrt(length(x))
  mean(x) + c(-1, 1) * qt(0.975, length(x) - 1) * se
}

num <- function(x, digits = 1) formatC(x, format = "f", digits = digits)

write_tex <- function(df, file, align) {
  header <- paste(names(df), collapse = " & ")
  body   <- apply(df, 1, function(row) paste(row, collapse = " & "))
  lines  <- c(paste0("\\begin{tabular}{", align, "}"),
              "\\toprule",
              paste(header, "\\\\"),
              "\\midrule",
              paste(body, "\\\\"),
              "\\bottomrule",
              "\\end{tabular}")
  writeLines(lines, file)
}


# ---- 1. Numeric variables ----

f$diff <- f$filter - f$espresso

numeric_vars <- list(
  "Espresso price (CZK)"           = d$espresso,
  "Filter coffee price (CZK)"      = d$filter,
  "Filter minus espresso (CZK)"    = f$diff,
  "Distance from Orloj (km)"       = d$distance_km,
  "East of Orloj (km)"             = d$east_km
)

desc <- data.frame()
for (v in names(numeric_vars)) {
  x  <- numeric_vars[[v]]
  ci <- mean_ci(x)
  q  <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  desc <- rbind(desc, data.frame(
    Variable  = v,
    n         = sum(!is.na(x)),
    Missing   = sum(is.na(x)),
    Mean      = num(mean(x, na.rm = TRUE)),
    `95\\% CI` = paste0("[", num(ci[1]), ", ", num(ci[2]), "]"),
    SD        = num(sd(x, na.rm = TRUE)),
    Median    = num(median(x, na.rm = TRUE)),
    IQR       = paste0(num(q[1]), "--", num(q[2])),
    Range     = paste0(num(min(x, na.rm = TRUE)), "--", num(max(x, na.rm = TRUE))),
    check.names = FALSE))
}
desc
write_tex(desc, "report/tables/D_descriptives.tex", "lrrrrrrrr")


# ---- 2. Categorical variables ----

cat_table <- function(x, label, levels, names) {
  counts <- table(factor(x, levels = levels), useNA = "ifany")
  pct <- 100 * as.numeric(counts) / sum(counts)
  lab <- c(names, rep("Missing", length(counts) - length(names)))
  data.frame(Variable = c(label, rep("", length(counts) - 1)),
             Category = lab,
             n = as.integer(counts),
             `\\%` = num(pct), check.names = FALSE)
}

cats <- rbind(
  cat_table(d$offers_filter, "Offers filter coffee", c(0, 1), c("No", "Yes")),
  cat_table(d$is_roastery,   "Roastery",             c(0, 1), c("No", "Yes")),
  cat_table(d$n_features,    "Specialty features",   c(0, 1, 2), c("0", "1", "2")),
  cat_table(d$cafe_type,     "Cafe type",
            c("Independent", "Local Chain", "International Chain"),
            c("Independent", "Local chain", "International chain")),
  cat_table(f$chain, "Chain (cafes with filter)", c(0, 1),
            c("Independent", "Chain"))
)
cats
write_tex(cats, "report/tables/D_categorical.tex", "llrr")


# ---- 3. Variables and groups that are tested ----
# A7-A10 are regressions: residuals are checked in 03_analysis.R

checks <- list(
  list(var = "Filter price",       group = "All with filter",  used = "A1, A6",
       x = f$filter),
  list(var = "Espresso price",     group = "Offers filter",    used = "A2, A6",
       x = d$espresso[d$offers_filter == 1]),
  list(var = "Espresso price",     group = "No filter",        used = "A2",
       x = d$espresso[d$offers_filter == 0]),
  list(var = "Filter - espresso",  group = "Same cafe",        used = "A3",
       x = f$diff),
  list(var = "Espresso price",     group = "0 features",       used = "A4",
       x = d$espresso[d$n_features == 0]),
  list(var = "Espresso price",     group = "1 feature",        used = "A4",
       x = d$espresso[d$n_features == 1]),
  list(var = "Espresso price",     group = "2 features",       used = "A4",
       x = d$espresso[d$n_features == 2])
)


# ---- 4. Plots ----

pdf("report/figures/D_histograms.pdf", width = 11, height = 6)
par(mfrow = c(2, 4))
for (ch in checks) {
  x <- ch$x[!is.na(ch$x)]
  hist(x, freq = FALSE, col = "grey85", border = "white",
       main = paste0(ch$var, "\n", ch$group, " (n = ", length(x), ")"),
       xlab = "CZK", cex.main = 0.95)
  curve(dnorm(x, mean(ch$x, na.rm = TRUE), sd(ch$x, na.rm = TRUE)),
        add = TRUE, lwd = 2)
}
dev.off()

pdf("report/figures/D_qqplots.pdf", width = 11, height = 6)
par(mfrow = c(2, 4))
for (ch in checks) {
  x <- ch$x[!is.na(ch$x)]
  qqnorm(x, main = paste0(ch$var, "\n", ch$group, " (n = ", length(x), ")"),
         cex.main = 0.95, pch = 19, col = "grey30")
  qqline(x, lwd = 2)
}
dev.off()

pdf("report/figures/D_boxplots.pdf", width = 11, height = 4)
par(mfrow = c(1, 3))
boxplot(espresso ~ offers_filter, data = d,
        names = c("No filter", "Offers filter"),
        ylab = "Espresso price (CZK)", xlab = "", main = "A2: espresso price")
boxplot(espresso ~ n_features, data = d,
        names = c("0", "1", "2"),
        ylab = "Espresso price (CZK)", xlab = "Number of specialty features",
        main = "A4: espresso price")
boxplot(f$diff, ylab = "Filter minus espresso (CZK)",
        main = paste0("A3: differences (n = ", nrow(f), ")"))
abline(h = 0, lty = 2)
dev.off()

pdf("report/figures/D_distance.pdf", width = 9, height = 4)
par(mfrow = c(1, 2))
hist(d$distance_km, col = "grey85", border = "white",
     main = paste0("Distance from Orloj (n = ", sum(!is.na(d$distance_km)), ")"),
     xlab = "km")
hist(d$east_km, col = "grey85", border = "white",
     main = paste0("East of Orloj (n = ", sum(!is.na(d$east_km)), ")"),
     xlab = "km (negative = west)")
dev.off()


# ---- 5. Normality and CLT ----
# normal: |skewness| < 0.5 (and plots agree)
# CLT: n >= 30 and |skewness of bootstrapped means| < 0.5
# parametric if normal or CLT

set.seed(2026)
B <- 5000

decision <- data.frame()
boot_means <- list()
for (k in seq_along(checks)) {
  x <- checks[[k]]$x
  x <- x[!is.na(x)]
  bm <- replicate(B, mean(sample(x, replace = TRUE)))
  boot_means[[k]] <- bm

  sk        <- skewness(x)
  normal    <- abs(sk) < 0.5
  boot_sk   <- skewness(bm)
  clt       <- length(x) >= 30 && abs(boot_sk) < 0.5
  shapiro_p <- shapiro.test(x)$p.value

  decision <- rbind(decision, data.frame(
    Variable    = checks[[k]]$var,
    Group       = checks[[k]]$group,
    Used        = checks[[k]]$used,
    n           = length(x),
    Skewness    = num(sk, 2),
    `Shapiro $p$` = ifelse(shapiro_p < 0.001, "$<$.001",
                           sub("^0", "", num(shapiro_p, 3))),
    Normal      = ifelse(normal, "Yes", "No"),
    `Boot. skew` = num(boot_sk, 2),
    CLT         = ifelse(clt, "Yes", "No"),
    Method      = ifelse(normal || clt, "Parametric", "Non-parametric"),
    check.names = FALSE))
}
decision

pdf("report/figures/D_bootstrap.pdf", width = 11, height = 6)
par(mfrow = c(2, 4))
for (k in seq_along(checks)) {
  hist(boot_means[[k]], freq = FALSE, col = "grey85", border = "white",
       main = paste0(checks[[k]]$var, "\n", checks[[k]]$group,
                     " (", B, " resamples)"),
       xlab = "Bootstrapped mean (CZK)", cex.main = 0.95)
  curve(dnorm(x, mean(boot_means[[k]]), sd(boot_means[[k]])),
        add = TRUE, lwd = 2)
}
dev.off()

write_tex(decision, "report/tables/D_table.tex", "lllrrrcrcl")


# ---- 6. Method per question ----
# questions with groups need every group to pass

ok <- decision$Method == "Parametric"
method <- c(
  A1 = ifelse(ok[1],             "parametric: one-sample t-test",
                                 "non-parametric: Wilcoxon signed-rank"),
  A2 = ifelse(all(ok[2:3]),      "parametric: Welch t-test",
                                 "non-parametric: Wilcoxon rank-sum"),
  A3 = ifelse(ok[4],             "parametric: paired t-test",
                                 "non-parametric: Wilcoxon signed-rank"),
  A4 = ifelse(all(ok[5:7]),      "parametric: Welch ANOVA + Games-Howell",
                                 "non-parametric: Kruskal-Wallis + Dunn"),
  A6 = ifelse(all(ok[1:2]),      "parametric: Pearson r",
                                 "non-parametric: Spearman rho")
)
method
