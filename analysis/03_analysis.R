# 03_analysis.R
# A1-A10 for the Speciality Cafes topic.
# dataset.csv -> report/tables/A*_table.tex, report/figures/A*_figure.pdf,
# summary.tex (all ten results) and the appendix tables.

library(sandwich)   # robust (HC3) standard errors
library(lmtest)     # coeftest, coefci, bptest

d <- read.csv("analysis/dataset.csv", stringsAsFactors = FALSE)
f <- d[d$offers_filter == 1, ]    # cafes with both prices
f$esp10 <- f$espresso / 10        # espresso price in units of 10 CZK

dir.create("report/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("report/figures", recursive = TRUE, showWarnings = FALSE)

set.seed(2026)
B <- 5000


# ---- Helpers ----

num    <- function(x, digits = 1) formatC(x, format = "f", digits = digits)
pfmt   <- function(p) ifelse(p < .001, "$<$.001", sub("^0", "", num(p, 3)))
ci_txt <- function(lo, hi, digits = 1) paste0("[", num(lo, digits), ", ", num(hi, digits), "]")

mean_ci <- function(x) {
  se <- sd(x) / sqrt(length(x))
  mean(x) + c(-1, 1) * qt(0.975, length(x) - 1) * se
}

write_tex <- function(df, file, align, extra = NULL) {
  header <- paste(names(df), collapse = " & ")
  body   <- apply(df, 1, function(row) paste(row, collapse = " & "))
  lines  <- c(paste0("\\begin{tabular}{", align, "}"), "\\toprule",
              paste(header, "\\\\"), "\\midrule", paste(body, "\\\\"))
  if (!is.null(extra)) {
    lines <- c(lines, "\\midrule",
               paste0("\\multicolumn{", ncol(df), "}{l}{", extra, "} \\\\"))
  }
  writeLines(c(lines, "\\bottomrule", "\\end{tabular}"), file)
}

# data points per group, with the mean and its 95% CI next to them
grey_pts <- rgb(0, 0, 0, 0.3)
plot_groups <- function(y, g, labels, ylab, file, ref = NULL) {
  g <- factor(g)
  pdf(file, width = 6, height = 4.5)
  par(mar = c(4, 4.5, 1, 1))
  stripchart(y ~ g, vertical = TRUE, method = "jitter", jitter = 0.12,
             pch = 19, col = grey_pts, group.names = labels, ylab = ylab,
             xlim = c(0.5, nlevels(g) + 0.5))
  for (i in seq_len(nlevels(g))) {
    yi <- y[g == levels(g)[i]]
    ci <- mean_ci(yi)
    arrows(i + 0.25, ci[1], i + 0.25, ci[2], angle = 90, code = 3,
           length = 0.06, lwd = 2)
    points(i + 0.25, mean(yi), pch = 18, cex = 1.8)
  }
  if (!is.null(ref)) abline(h = ref, lty = 2)
  dev.off()
}

# regression diagnostics: residuals vs fitted, Q-Q, Cook's distance
diag_plot <- function(m, file) {
  pdf(file, width = 10, height = 3.5)
  par(mfrow = c(1, 3))
  plot(fitted(m), resid(m), pch = 19, col = grey_pts,
       xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs fitted")
  abline(h = 0, lty = 2)
  qqnorm(resid(m), pch = 19, col = grey_pts, main = "Normal Q-Q of residuals")
  qqline(resid(m))
  cd <- cooks.distance(m)
  plot(cd, type = "h", xlab = "Observation", ylab = "Cook's distance",
       main = "Influential observations")
  abline(h = 4 / length(cd), lty = 2)
  dev.off()
}

term_labels <- c("(Intercept)"   = "Intercept",
                 "esp10"         = "Espresso price (per 10 CZK)",
                 "offers_filter" = "Offers filter coffee",
                 "distance_km"   = "Distance from Orloj (km)",
                 "chain"         = "Chain",
                 "esp10:chain"   = "Espresso $\\times$ chain")

reg_col <- function(m, vc) {
  ct <- coeftest(m, vcov. = vc)
  ci <- coefci(m, vcov. = vc)
  data.frame(term = rownames(ct),
             est  = paste0(num(ct[, 1], 2), " (", num(ct[, 2], 2), ")"),
             ci   = ci_txt(ci[, 1], ci[, 2], 2),
             p    = pfmt(ct[, 4]))
}

# one or more models side by side, robust SE in brackets
reg_table <- function(models, vcs, file) {
  cols  <- Map(reg_col, models, vcs)
  terms <- unique(unlist(lapply(cols, function(x) x$term)))
  out   <- data.frame(Term = unname(term_labels[terms]), check.names = FALSE)
  for (k in seq_along(cols)) {
    idx <- match(terms, cols[[k]]$term)
    pre <- if (length(models) > 1) paste0("M", k, " ") else ""
    out[[paste0(pre, "$b$ (SE)")]] <- ifelse(is.na(idx), "", cols[[k]]$est[idx])
    out[[paste0(pre, "95\\% CI")]] <- ifelse(is.na(idx), "", cols[[k]]$ci[idx])
    out[[paste0(pre, "$p$")]]      <- ifelse(is.na(idx), "", cols[[k]]$p[idx])
  }
  n_row  <- c("$n$",   rep("", ncol(out) - 1))
  r2_row <- c("$R^2$", rep("", ncol(out) - 1))
  for (k in seq_along(models)) {
    n_row[2 + 3 * (k - 1)]  <- nobs(models[[k]])
    r2_row[2 + 3 * (k - 1)] <- num(summary(models[[k]])$r.squared, 2)
  }
  out <- rbind(out, n_row, r2_row)
  write_tex(out, file, paste0("l", strrep("rrr", length(models))))
  out
}

summary_rows <- list()
robust_rows  <- list()


# =============================================================
# A1. Is the average filter coffee price different from 4 USD?
# H0: mean = 88.7 CZK   H1: mean != 88.7 CZK (two-tailed)
# =============================================================

mu  <- 4 * 22.182    # 4 USD at the CNB rate of 16 Nov 2026
a1  <- t.test(f$filter, mu = mu)
a1d <- (mean(f$filter) - mu) / sd(f$filter)
a1

a1_tab <- data.frame(
  "$n$" = nrow(f), "Mean (CZK)" = num(mean(f$filter)),
  "95\\% CI" = ci_txt(a1$conf.int[1], a1$conf.int[2]),
  "Benchmark (CZK)" = num(mu, 2), "$t$" = num(a1$statistic, 2),
  "$df$" = a1$parameter, "$p$" = pfmt(a1$p.value), "$d$" = num(a1d, 2),
  check.names = FALSE)
a1_tab
write_tex(a1_tab, "report/tables/A1_table.tex", "rrrrrrrr")

plot_groups(f$filter, rep(1, nrow(f)),
            paste0("Cafes with filter coffee (n = ", nrow(f), ")"),
            "Filter coffee price (CZK)", "report/figures/A1_figure.pdf", ref = mu)

summary_rows$A1 <- c("A1 Filter price vs 4 USD", nrow(f),
  paste(num(mean(f$filter)), "CZK", ci_txt(a1$conf.int[1], a1$conf.int[2])),
  paste0("$d$ = ", num(a1d, 2)),
  paste0("$t$(", a1$parameter, ") = ", num(a1$statistic, 2)), pfmt(a1$p.value))

w1 <- suppressWarnings(wilcox.test(f$filter, mu = mu, exact = FALSE))
robust_rows$A1 <- c("A1", "Wilcoxon signed-rank",
  paste0("$V$ = ", w1$statistic, ", $p$ = ", pfmt(w1$p.value)))


# =============================================================
# A2. Do cafes with filter coffee charge more for an espresso?
# H0: same mean   H1: higher with filter (one-tailed)
# Difference = offers filter minus no filter
# =============================================================

x1 <- d$espresso[d$offers_filter == 1]
x0 <- d$espresso[d$offers_filter == 0]
n1 <- length(x1); n0 <- length(x0)

vt2 <- var.test(x1, x0)    # equal variances? Welch's test is used either way
vt2

a2   <- t.test(x1, x0, alternative = "greater")   # Welch, one-tailed p
a2ci <- t.test(x1, x0)$conf.int                    # two-sided CI
a2

sp     <- sqrt(((n1 - 1) * var(x1) + (n0 - 1) * var(x0)) / (n1 + n0 - 2))
a2d    <- (mean(x1) - mean(x0)) / sp
a2d_se <- sqrt((n1 + n0) / (n1 * n0) + a2d^2 / (2 * (n1 + n0)))
a2diff <- mean(x1) - mean(x0)

a2_tab <- data.frame(
  "Group" = c("Offers filter", "No filter"), "$n$" = c(n1, n0),
  "Mean (CZK)" = num(c(mean(x1), mean(x0))),
  "95\\% CI" = c(ci_txt(mean_ci(x1)[1], mean_ci(x1)[2]),
                 ci_txt(mean_ci(x0)[1], mean_ci(x0)[2])),
  check.names = FALSE)
a2_extra <- c(
  paste0("Difference (offers filter minus no filter): ", num(a2diff), " CZK, 95\\% CI ",
         ci_txt(a2ci[1], a2ci[2])),
  paste0("Cohen's $d$ = ", num(a2d, 2), ", 95\\% CI ",
         ci_txt(a2d - 1.96 * a2d_se, a2d + 1.96 * a2d_se, 2)),
  paste0("Welch $t$(", num(a2$parameter), ") = ", num(a2$statistic, 2),
         ", one-tailed $p$ = ", pfmt(a2$p.value)))
a2_tab
write_tex(a2_tab, "report/tables/A2_table.tex", "lrrr", extra = a2_extra)

plot_groups(d$espresso, factor(d$offers_filter, levels = c(0, 1)),
            c(paste0("No filter (n = ", n0, ")"), paste0("Offers filter (n = ", n1, ")")),
            "Espresso price (CZK)", "report/figures/A2_figure.pdf")

summary_rows$A2 <- c("A2 Espresso: filter minus no filter", n1 + n0,
  paste(num(a2diff), "CZK", ci_txt(a2ci[1], a2ci[2])),
  paste0("$d$ = ", num(a2d, 2)),
  paste0("$t$(", num(a2$parameter), ") = ", num(a2$statistic, 2)),
  paste(pfmt(a2$p.value), "(one-tailed)"))

w2 <- wilcox.test(x1, x0, alternative = "greater", exact = FALSE)
robust_rows$A2 <- c("A2", "Wilcoxon rank-sum (one-tailed)",
  paste0("$W$ = ", w2$statistic, ", $p$ = ", pfmt(w2$p.value)))


# =============================================================
# A3. How much more does a filter coffee cost than an espresso
#     in the same cafe?
# H0: mean difference = 0   H1: difference > 0 (one-tailed)
# Difference = filter minus espresso
# =============================================================

diff3 <- f$filter - f$espresso
n3    <- length(diff3)

a3   <- t.test(f$filter, f$espresso, paired = TRUE, alternative = "greater")
a3ci <- t.test(f$filter, f$espresso, paired = TRUE)$conf.int
a3

dz    <- mean(diff3) / sd(diff3)
dz_se <- sqrt(1 / n3 + dz^2 / (2 * n3))

a3_tab <- data.frame(
  "$n$" = n3, "Mean difference (CZK)" = num(mean(diff3)),
  "95\\% CI" = ci_txt(a3ci[1], a3ci[2]),
  "$d_z$" = num(dz, 2), "95\\% CI ($d_z$)" = ci_txt(dz - 1.96 * dz_se, dz + 1.96 * dz_se, 2),
  "$t$" = num(a3$statistic, 2), "$df$" = a3$parameter,
  "$p$ (one-tailed)" = pfmt(a3$p.value), check.names = FALSE)
a3_tab
write_tex(a3_tab, "report/tables/A3_table.tex", "rrrrrrrr")

plot_groups(diff3, rep(1, n3), paste0("Cafes with both prices (n = ", n3, ")"),
            "Filter minus espresso price (CZK)", "report/figures/A3_figure.pdf", ref = 0)

summary_rows$A3 <- c("A3 Filter minus espresso, same cafe", n3,
  paste(num(mean(diff3)), "CZK", ci_txt(a3ci[1], a3ci[2])),
  paste0("$d_z$ = ", num(dz, 2)),
  paste0("$t$(", a3$parameter, ") = ", num(a3$statistic, 2)),
  paste(pfmt(a3$p.value), "(one-tailed)"))

w3 <- suppressWarnings(wilcox.test(f$filter, f$espresso, paired = TRUE,
                                   alternative = "greater", exact = FALSE))
robust_rows$A3 <- c("A3", "Wilcoxon signed-rank (one-tailed)",
  paste0("$V$ = ", w3$statistic, ", $p$ = ", pfmt(w3$p.value)))


# =============================================================
# A4. Does the espresso price differ between cafes with 0, 1 or 2
#     specialty features (roastery, filter coffee)?
# H0: same mean in all groups   H1: at least one differs
# =============================================================

g4 <- factor(d$n_features, levels = 0:2)

fl4 <- fligner.test(d$espresso ~ g4)    # equal variances? Welch ANOVA either way
fl4

a4   <- oneway.test(d$espresso ~ g4, var.equal = FALSE)
ss4  <- summary(aov(d$espresso ~ g4))[[1]][["Sum Sq"]]
eta2 <- ss4[1] / sum(ss4)
a4

# Games-Howell pairwise comparisons (unequal variances, Tukey-adjusted)
games_howell <- function(y, g) {
  lev <- levels(g); k <- length(lev)
  m <- as.numeric(tapply(y, g, mean))
  v <- as.numeric(tapply(y, g, var))
  n <- as.numeric(tapply(y, g, length))
  out <- data.frame()
  for (i in 1:(k - 1)) for (j in (i + 1):k) {
    dif  <- m[j] - m[i]
    se2  <- v[i] / n[i] + v[j] / n[j]
    df   <- se2^2 / ((v[i] / n[i])^2 / (n[i] - 1) + (v[j] / n[j])^2 / (n[j] - 1))
    t    <- dif / sqrt(se2)
    p    <- ptukey(abs(t) * sqrt(2), k, df, lower.tail = FALSE)
    crit <- qtukey(0.95, k, df) / sqrt(2)
    out  <- rbind(out, data.frame(pair = paste(lev[j], "minus", lev[i]),
                                  diff = dif, lo = dif - crit * sqrt(se2),
                                  hi = dif + crit * sqrt(se2), t = t, df = df, p = p))
  }
  out
}
gh4 <- games_howell(d$espresso, g4)
gh4

a4_tab <- data.frame(
  "Specialty features" = levels(g4),
  "$n$" = as.numeric(table(g4)),
  "Mean (CZK)" = num(as.numeric(tapply(d$espresso, g4, mean))),
  "95\\% CI" = sapply(levels(g4), function(l) {
    ci <- mean_ci(d$espresso[g4 == l]); ci_txt(ci[1], ci[2]) }),
  check.names = FALSE)
a4_extra <- paste0("Welch $F$(", a4$parameter[1], ", ", num(a4$parameter[2]), ") = ",
                   num(a4$statistic, 2), ", $p$ = ", pfmt(a4$p.value),
                   ", $\\eta^2$ = ", num(eta2, 2))
a4_tab
write_tex(a4_tab, "report/tables/A4_table.tex", "lrrr", extra = a4_extra)

gh4_tab <- data.frame(
  "Comparison" = paste(gh4$pair, "features"),
  "Difference (CZK)" = num(gh4$diff), "95\\% CI" = ci_txt(gh4$lo, gh4$hi),
  "$t$" = num(gh4$t, 2), "$df$" = num(gh4$df), "Adjusted $p$" = pfmt(gh4$p),
  check.names = FALSE)
write_tex(gh4_tab, "report/tables/A4_posthoc.tex", "lrrrrr")

plot_groups(d$espresso, g4,
            paste0(levels(g4), " (n = ", as.numeric(table(g4)), ")"),
            "Espresso price (CZK)", "report/figures/A4_figure.pdf")

big <- gh4[gh4$pair == "2 minus 0", ]
summary_rows$A4 <- c("A4 Espresso by number of features", nrow(d),
  paste("2 vs 0:", num(big$diff), "CZK", ci_txt(big$lo, big$hi)),
  paste0("$\\eta^2$ = ", num(eta2, 2)),
  paste0("$F$(", a4$parameter[1], ", ", num(a4$parameter[2]), ") = ", num(a4$statistic, 2)),
  pfmt(a4$p.value))

kw4 <- kruskal.test(d$espresso ~ g4)
robust_rows$A4 <- c("A4", "Kruskal-Wallis",
  paste0("$H$(", kw4$parameter, ") = ", num(kw4$statistic, 2), ", $p$ = ", pfmt(kw4$p.value)))


# =============================================================
# A5. Is a roastery more likely to offer filter coffee?
# H0: same share   H1: higher share for roasteries (one-tailed)
# =============================================================

tab5 <- table(factor(d$is_roastery, 0:1, c("Other cafe", "Roastery")),
              factor(d$offers_filter, 0:1, c("No filter", "Offers filter")))
tab5

a5 <- chisq.test(tab5, correct = FALSE)
a5
a5$expected
min(a5$expected)     # all >= 5, so the chi-square test is valid
a5$stdres            # |value| > 2 = cells that drive the association

# one-tailed p: odds of offering filter higher for roasteries
fis5 <- fisher.test(tab5, alternative = "greater")
fis5

# difference in percentage points: roastery minus other
pr5 <- prop.test(x = c(tab5["Roastery", "Offers filter"], tab5["Other cafe", "Offers filter"]),
                 n = c(sum(tab5["Roastery", ]), sum(tab5["Other cafe", ])),
                 correct = FALSE)
pp5    <- 100 * (pr5$estimate[1] - pr5$estimate[2])
pp5_ci <- 100 * pr5$conf.int

# Cramer's V (for a 2x2 table it equals |phi|) with a bootstrap CI
cramer_v <- function(r, fl) {
  t <- table(factor(r, 0:1), factor(fl, 0:1))
  abs(t[1, 1] * t[2, 2] - t[1, 2] * t[2, 1]) /
    sqrt(sum(t[1, ]) * sum(t[2, ]) * sum(t[, 1]) * sum(t[, 2]))
}
v5 <- cramer_v(d$is_roastery, d$offers_filter)
v5_boot <- replicate(B, {
  i <- sample(nrow(d), replace = TRUE)
  cramer_v(d$is_roastery[i], d$offers_filter[i])
})
v5_ci <- quantile(v5_boot, c(0.025, 0.975), na.rm = TRUE)

a5_tab <- data.frame(
  " " = rownames(tab5),
  "No filter" = tab5[, 1], "Offers filter" = tab5[, 2],
  "\\% offering filter" = num(100 * tab5[, 2] / rowSums(tab5)),
  "Std. residual" = num(a5$stdres[, 2], 2),
  check.names = FALSE)
a5_extra <- c(
  paste0("Difference (roastery minus other): ", num(pp5), " pp, 95\\% CI ",
         ci_txt(pp5_ci[1], pp5_ci[2])),
  paste0("$\\chi^2$(1, $N$ = ", sum(tab5), ") = ", num(a5$statistic, 2),
         ", $p$ = ", pfmt(a5$p.value), "; smallest expected count ", num(min(a5$expected))),
  paste0("Cram\\'er's $V$ = ", num(v5, 2), ", 95\\% bootstrap CI ", ci_txt(v5_ci[1], v5_ci[2], 2)),
  paste0("Fisher's exact test, one-tailed $p$ = ", pfmt(fis5$p.value)))
a5_tab
write_tex(a5_tab, "report/tables/A5_table.tex", "lrrrr", extra = a5_extra)

# share offering filter coffee with 95% CI, per group
pdf("report/figures/A5_figure.pdf", width = 6, height = 4.5)
par(mar = c(4, 4.5, 1, 1))
shares <- 100 * tab5[, 2] / rowSums(tab5)
cis <- sapply(rownames(tab5), function(r)
  100 * prop.test(tab5[r, 2], sum(tab5[r, ]), correct = FALSE)$conf.int)
mids <- barplot(shares, ylim = c(0, 100), col = "grey80", border = NA,
                names.arg = paste0(rownames(tab5), " (n = ", rowSums(tab5), ")"),
                ylab = "Cafes offering filter coffee (%)")
arrows(mids, cis[1, ], mids, cis[2, ], angle = 90, code = 3, length = 0.06, lwd = 2)
dev.off()

summary_rows$A5 <- c("A5 Filter coffee: roastery minus other", sum(tab5),
  paste(num(pp5), "pp", ci_txt(pp5_ci[1], pp5_ci[2])),
  paste0("$V$ = ", num(v5, 2)),
  paste0("$\\chi^2$(1, $N$ = ", sum(tab5), ") = ", num(a5$statistic, 2)),
  paste(pfmt(fis5$p.value), "(Fisher, one-tailed)"))

fis5b <- fisher.test(tab5)
robust_rows$A5 <- c("A5", "Fisher's exact (two-tailed)",
  paste0("OR = ", num(fis5b$estimate, 2), ", $p$ = ", pfmt(fis5b$p.value)))


# =============================================================
# A6. Is the filter price correlated with the espresso price?
# H0: r = 0   H1: r != 0
# =============================================================

a6 <- cor.test(f$espresso, f$filter)    # Pearson
a6

rho <- cor(f$espresso, f$filter, method = "spearman")
rho_boot <- replicate(B, {
  i <- sample(nrow(f), replace = TRUE)
  cor(f$espresso[i], f$filter[i], method = "spearman")
})
rho_ci <- quantile(rho_boot, c(0.025, 0.975), na.rm = TRUE)

a6_tab <- data.frame(
  "$n$" = nrow(f), "$r$" = num(a6$estimate, 2),
  "95\\% CI" = ci_txt(a6$conf.int[1], a6$conf.int[2], 2),
  "$r^2$" = num(a6$estimate^2, 2), "$t$" = num(a6$statistic, 2),
  "$df$" = a6$parameter, "$p$" = pfmt(a6$p.value),
  "Spearman $\\rho$ [bootstrap CI]" = paste(num(rho, 2), ci_txt(rho_ci[1], rho_ci[2], 2)),
  check.names = FALSE)
a6_tab
write_tex(a6_tab, "report/tables/A6_table.tex", "rrrrrrrr")

pdf("report/figures/A6_figure.pdf", width = 6, height = 4.5)
par(mar = c(4, 4.5, 1, 1))
plot(jitter(f$espresso, 0.5), jitter(f$filter, 0.5), pch = 19, col = grey_pts,
     xlab = "Espresso price (CZK)", ylab = "Filter coffee price (CZK)")
abline(lm(filter ~ espresso, data = f), lwd = 2)
legend("topleft", bty = "n",
       legend = paste0("r = ", num(a6$estimate, 2), ", 95% CI ",
                       ci_txt(a6$conf.int[1], a6$conf.int[2], 2), ", n = ", nrow(f)))
dev.off()

summary_rows$A6 <- c("A6 Filter vs espresso price", nrow(f),
  paste("$r$ =", num(a6$estimate, 2), ci_txt(a6$conf.int[1], a6$conf.int[2], 2)),
  paste0("$r^2$ = ", num(a6$estimate^2, 2)),
  paste0("$t$(", a6$parameter, ") = ", num(a6$statistic, 2)), pfmt(a6$p.value))

robust_rows$A6 <- c("A6", "Spearman $\\rho$ (bootstrap CI)",
  paste(num(rho, 2), ci_txt(rho_ci[1], rho_ci[2], 2)))


# =============================================================
# A7. By how many CZK does the filter price change per 10 CZK of
#     espresso price?
# H0: slope = 0   H1: slope != 0
# =============================================================

m7  <- lm(filter ~ esp10, data = f)
vc7 <- vcovHC(m7, type = "HC3")
summary(m7)
bp7 <- bptest(m7)             # constant variance? robust SE used anyway
bp7
coeftest(m7, vcov. = vc7)

a7_tab <- reg_table(list(m7), list(vc7), "report/tables/A7_table.tex")
a7_tab

diag_plot(m7, "report/figures/A7_diagnostics.pdf")
infl7 <- which(cooks.distance(m7) > 4 / nobs(m7))
f[infl7, c("cafe", "espresso", "filter")]    # influential cafes

# fitted line with a robust 95% confidence band
pdf("report/figures/A7_figure.pdf", width = 6, height = 4.5)
par(mar = c(4, 4.5, 1, 1))
plot(jitter(f$espresso, 0.5), jitter(f$filter, 0.5), pch = 19, col = grey_pts,
     xlab = "Espresso price (CZK)", ylab = "Filter coffee price (CZK)")
grid7 <- data.frame(esp10 = seq(min(f$esp10), max(f$esp10), length.out = 100))
X7    <- model.matrix(~ esp10, grid7)
fit7  <- X7 %*% coef(m7)
se7   <- sqrt(rowSums((X7 %*% vc7) * X7))
tq7   <- qt(0.975, df.residual(m7))
polygon(c(grid7$esp10, rev(grid7$esp10)) * 10,
        c(fit7 - tq7 * se7, rev(fit7 + tq7 * se7)),
        col = rgb(0, 0, 0, 0.12), border = NA)
lines(grid7$esp10 * 10, fit7, lwd = 2)
dev.off()

ct7 <- coeftest(m7, vcov. = vc7)
ci7 <- coefci(m7, vcov. = vc7)
summary_rows$A7 <- c("A7 Filter price per 10 CZK espresso", nobs(m7),
  paste(num(ct7["esp10", 1], 2), "CZK", ci_txt(ci7["esp10", 1], ci7["esp10", 2], 2)),
  paste0("$R^2$ = ", num(summary(m7)$r.squared, 2)),
  paste0("$t$(", df.residual(m7), ") = ", num(ct7["esp10", 3], 2)),
  pfmt(ct7["esp10", 4]))

if (length(infl7) > 0) {
  m7b  <- lm(filter ~ esp10, data = f[-infl7, ])
  ct7b <- coeftest(m7b, vcov. = vcovHC(m7b, type = "HC3"))
  robust_rows$A7 <- c("A7", paste("Without", length(infl7), "influential cafes"),
    paste0("$b$ = ", num(ct7b["esp10", 1], 2), " (", num(ct7b["esp10", 2], 2),
           "), $p$ = ", pfmt(ct7b["esp10", 4]), ", $n$ = ", nobs(m7b)))
} else {
  robust_rows$A7 <- c("A7", "Influential cafes", "none above 4/n")
}


# =============================================================
# A8. Does the espresso price differ between cafes with and without
#     filter coffee once distance from the Orloj is accounted for?
# H0: b(offers filter) = 0 given distance   H1: != 0
# =============================================================

m8a  <- lm(espresso ~ offers_filter, data = d)
m8b  <- lm(espresso ~ offers_filter + distance_km, data = d)
vc8a <- vcovHC(m8a, type = "HC3")
vc8b <- vcovHC(m8b, type = "HC3")
summary(m8b)
bp8 <- bptest(m8b)
bp8
an8 <- anova(m8a, m8b)     # nested F-test: does distance improve the model?
an8

vif8 <- 1 / (1 - summary(lm(offers_filter ~ distance_km, data = d))$r.squared)
vif8

change8 <- coef(m8b)["offers_filter"] - coef(m8a)["offers_filter"]
change8    # how much the filter coefficient moved when distance entered

a8_tab <- reg_table(list(m8a, m8b), list(vc8a, vc8b), "report/tables/A8_table.tex")
a8_tab

diag_plot(m8b, "report/figures/A8_diagnostics.pdf")
infl8 <- which(cooks.distance(m8b) > 4 / nobs(m8b))
d[infl8, c("cafe", "espresso", "offers_filter", "distance_km")]

pdf("report/figures/A8_figure.pdf", width = 6, height = 4.5)
par(mar = c(4, 4.5, 1, 1))
cols8 <- ifelse(d$offers_filter == 1, "black", "grey60")
plot(d$distance_km, jitter(d$espresso, 0.5), pch = 19, col = adjustcolor(cols8, 0.5),
     xlab = "Distance from Orloj (km)", ylab = "Espresso price (CZK)")
xs <- seq(min(d$distance_km), max(d$distance_km), length.out = 100)
for (g in 0:1) {
  nd <- data.frame(offers_filter = g, distance_km = xs)
  X  <- model.matrix(~ offers_filter + distance_km, nd)
  ft <- X %*% coef(m8b)
  se <- sqrt(rowSums((X %*% vc8b) * X))
  tq <- qt(0.975, df.residual(m8b))
  polygon(c(xs, rev(xs)), c(ft - tq * se, rev(ft + tq * se)),
          col = adjustcolor(ifelse(g == 1, "black", "grey60"), 0.15), border = NA)
  lines(xs, ft, lwd = 2, col = ifelse(g == 1, "black", "grey60"))
}
legend("topright", bty = "n", pch = 19, lwd = 2, col = c("black", "grey60"),
       legend = c(paste0("Offers filter (n = ", sum(d$offers_filter == 1), ")"),
                  paste0("No filter (n = ", sum(d$offers_filter == 0), ")")))
dev.off()

ct8 <- coeftest(m8b, vcov. = vc8b)
ci8 <- coefci(m8b, vcov. = vc8b)
summary_rows$A8 <- c("A8 Filter effect on espresso, given distance", nobs(m8b),
  paste(num(ct8["offers_filter", 1], 2), "CZK",
        ci_txt(ci8["offers_filter", 1], ci8["offers_filter", 2], 2)),
  paste0("$R^2$ = ", num(summary(m8b)$r.squared, 2)),
  paste0("$t$(", df.residual(m8b), ") = ", num(ct8["offers_filter", 3], 2)),
  pfmt(ct8["offers_filter", 4]))

ct8c <- summary(m8b)$coefficients
robust_rows$A8 <- c("A8", "Classical (non-robust) SE",
  paste0("$b$ = ", num(ct8c["offers_filter", 1], 2), " (", num(ct8c["offers_filter", 2], 2),
         "), $p$ = ", pfmt(ct8c["offers_filter", 4])))


# =============================================================
# A9. Does the espresso-filter price relationship differ between
#     independent cafes and chains?
# H0: interaction = 0 (same slope)   H1: != 0
# =============================================================

table(f$chain)

m9a  <- lm(filter ~ esp10 + chain, data = f)
m9b  <- lm(filter ~ esp10 * chain, data = f)
vc9a <- vcovHC(m9a, type = "HC3")
vc9b <- vcovHC(m9b, type = "HC3")
summary(m9b)
bp9 <- bptest(m9b)
bp9
anova(m9a, m9b)     # nested F-test for the interaction

# slope per 10 CZK of espresso in each group, with robust CI
b9 <- coef(m9b)
slope9 <- function(L) {
  est <- sum(L * b9)
  se  <- sqrt(as.numeric(t(L) %*% vc9b %*% L))
  tq  <- qt(0.975, df.residual(m9b))
  c(est = est, lo = est - tq * se, hi = est + tq * se)
}
L_ind   <- c("(Intercept)" = 0, esp10 = 1, chain = 0, "esp10:chain" = 0)[names(b9)]
L_chain <- c("(Intercept)" = 0, esp10 = 1, chain = 0, "esp10:chain" = 1)[names(b9)]
s_ind   <- slope9(L_ind)
s_chain <- slope9(L_chain)
s_ind; s_chain

a9_tab <- reg_table(list(m9a, m9b), list(vc9a, vc9b), "report/tables/A9_table.tex")
a9_tab

a9_slopes <- data.frame(
  "Group" = c("Independent", "Chain"),
  "$n$" = c(sum(f$chain == 0), sum(f$chain == 1)),
  "Slope (CZK per 10 CZK)" = num(c(s_ind["est"], s_chain["est"]), 2),
  "95\\% CI" = c(ci_txt(s_ind["lo"], s_ind["hi"], 2), ci_txt(s_chain["lo"], s_chain["hi"], 2)),
  check.names = FALSE)
a9_slopes
write_tex(a9_slopes, "report/tables/A9_slopes.tex", "lrrr")

diag_plot(m9b, "report/figures/A9_diagnostics.pdf")
infl9 <- which(cooks.distance(m9b) > 4 / nobs(m9b))
f[infl9, c("cafe", "espresso", "filter", "chain")]

pdf("report/figures/A9_figure.pdf", width = 6, height = 4.5)
par(mar = c(4, 4.5, 1, 1))
cols9 <- ifelse(f$chain == 1, "black", "grey60")
plot(jitter(f$espresso, 0.5), jitter(f$filter, 0.5), pch = 19,
     col = adjustcolor(cols9, 0.6),
     xlab = "Espresso price (CZK)", ylab = "Filter coffee price (CZK)")
for (g in 0:1) {
  sub <- f[f$chain == g, ]
  xs  <- seq(min(sub$esp10), max(sub$esp10), length.out = 100)
  nd  <- data.frame(esp10 = xs, chain = g)
  X   <- model.matrix(~ esp10 * chain, nd)
  ft  <- X %*% coef(m9b)
  se  <- sqrt(rowSums((X %*% vc9b) * X))
  tq  <- qt(0.975, df.residual(m9b))
  colg <- ifelse(g == 1, "black", "grey60")
  polygon(c(xs, rev(xs)) * 10, c(ft - tq * se, rev(ft + tq * se)),
          col = adjustcolor(colg, 0.15), border = NA)
  lines(xs * 10, ft, lwd = 2, col = colg)
}
legend("topleft", bty = "n", pch = 19, lwd = 2, col = c("grey60", "black"),
       legend = c(paste0("Independent (n = ", sum(f$chain == 0), ")"),
                  paste0("Chain (n = ", sum(f$chain == 1), ")")))
dev.off()

ct9 <- coeftest(m9b, vcov. = vc9b)
ci9 <- coefci(m9b, vcov. = vc9b)
summary_rows$A9 <- c("A9 Slope difference: chain minus independent", nobs(m9b),
  paste(num(ct9["esp10:chain", 1], 2), "CZK",
        ci_txt(ci9["esp10:chain", 1], ci9["esp10:chain", 2], 2)),
  "--",
  paste0("$t$(", df.residual(m9b), ") = ", num(ct9["esp10:chain", 3], 2)),
  pfmt(ct9["esp10:chain", 4]))

an9 <- anova(m9a, m9b)
robust_rows$A9 <- c("A9", "Nested $F$-test (classical)",
  paste0("$F$(", an9$Df[2], ", ", an9$Res.Df[2], ") = ", num(an9$F[2], 2),
         ", $p$ = ", pfmt(an9$`Pr(>F)`[2])))


# =============================================================
# A10. Does the probability of offering filter coffee change with
#      each km east of the Orloj?
# H0: b = 0 (AME = 0 pp per km)   H1: != 0
# =============================================================

m10 <- glm(offers_filter ~ east_km, family = binomial, data = d)
summary(m10)
b10 <- coef(m10)
V10 <- vcov(m10)

# average marginal effect: mean of b * p * (1 - p), delta-method SE
p10   <- fitted(m10)
x10   <- d$east_km
ame   <- mean(b10[2] * p10 * (1 - p10))
grad  <- c(mean(b10[2] * p10 * (1 - p10) * (1 - 2 * p10)),
           mean(p10 * (1 - p10) + b10[2] * x10 * p10 * (1 - p10) * (1 - 2 * p10)))
ame_se <- sqrt(as.numeric(t(grad) %*% V10 %*% grad))
ame_pp <- 100 * ame
ame_ci <- 100 * (ame + c(-1, 1) * 1.96 * ame_se)

or10    <- exp(b10[2])
or10_ci <- exp(confint.default(m10)[2, ])
z10     <- summary(m10)$coefficients[2, ]

m10_0 <- glm(offers_filter ~ 1, family = binomial, data = d)
lr10  <- anova(m10_0, m10, test = "LRT")
lr10

a10_tab <- data.frame(
  "$n$" = nobs(m10), "Events" = sum(d$offers_filter),
  "AME (pp per km)" = num(ame_pp, 2), "95\\% CI" = ci_txt(ame_ci[1], ame_ci[2], 2),
  "Odds ratio" = num(or10, 2), "95\\% CI (OR)" = ci_txt(or10_ci[1], or10_ci[2], 2),
  "$z$" = num(z10[3], 2), "$p$" = pfmt(z10[4]), check.names = FALSE)
a10_tab
write_tex(a10_tab, "report/tables/A10_table.tex", "rrrrrrrr")

# predicted probability with 95% CI, data shown at 0 and 1
pdf("report/figures/A10_figure.pdf", width = 6, height = 4.5)
par(mar = c(4, 4.5, 1, 1))
xs   <- seq(min(x10), max(x10), length.out = 200)
pr   <- predict(m10, data.frame(east_km = xs), type = "link", se.fit = TRUE)
inv  <- function(z) 1 / (1 + exp(-z))
plot(x10, jitter(d$offers_filter, 0.15), pch = 19, col = grey_pts, ylim = c(-0.05, 1.05),
     xlab = "Distance east of Orloj (km, negative = west)",
     ylab = "Probability of offering filter coffee")
polygon(c(xs, rev(xs)), c(inv(pr$fit - 1.96 * pr$se.fit), rev(inv(pr$fit + 1.96 * pr$se.fit))),
        col = rgb(0, 0, 0, 0.12), border = NA)
lines(xs, inv(pr$fit), lwd = 2)
dev.off()

summary_rows$A10 <- c("A10 P(filter) per km east", nobs(m10),
  paste(num(ame_pp, 2), "pp", ci_txt(ame_ci[1], ame_ci[2], 2)),
  paste0("OR = ", num(or10, 2)),
  paste0("$z$ = ", num(z10[3], 2)), pfmt(z10[4]))

robust_rows$A10 <- c("A10", "Likelihood-ratio test",
  paste0("$\\chi^2$(1) = ", num(lr10$Deviance[2], 2), ", $p$ = ", pfmt(lr10$`Pr(>Chi)`[2])))


# =============================================================
# Summary of all ten analyses and the robustness table
# =============================================================

summary_tab <- as.data.frame(do.call(rbind, summary_rows), stringsAsFactors = FALSE)
names(summary_tab) <- c("Analysis", "$n$", "Estimate [95\\% CI]", "Effect size",
                        "Test", "$p$")
summary_tab
write_tex(summary_tab, "report/tables/summary.tex", "lrllll")

robust_tab <- as.data.frame(do.call(rbind, robust_rows), stringsAsFactors = FALSE)
names(robust_tab) <- c("Analysis", "Alternative / check", "Result")
robust_tab
write_tex(robust_tab, "report/tables/A_robustness.tex", "lll")


# ---- Assumption checks (appendix) ----

bp_txt <- function(bp) paste0("BP = ", num(bp$statistic, 2), ", $p$ = ", pfmt(bp$p.value))
cafes  <- function(x) gsub("&", "\\\\&", paste(x, collapse = ", "))

checks_tab <- data.frame(rbind(
  c("A2", "Equal variances ($F$-test)",
    paste0("$F$(", vt2$parameter[1], ", ", vt2$parameter[2], ") = ", num(vt2$statistic, 2),
           ", $p$ = ", pfmt(vt2$p.value))),
  c("A4", "Equal variances (Fligner-Killeen)",
    paste0("$\\chi^2$(", fl4$parameter, ") = ", num(fl4$statistic, 2), ", $p$ = ", pfmt(fl4$p.value))),
  c("A5", "Smallest expected count", num(min(a5$expected))),
  c("A7", "Constant variance (Breusch-Pagan)", bp_txt(bp7)),
  c("A7", "Influential (Cook's $D$ $>$ 4/$n$)", cafes(f$cafe[infl7])),
  c("A8", "Constant variance (Breusch-Pagan)", bp_txt(bp8)),
  c("A8", "Variance inflation factor", num(vif8, 2)),
  c("A8", "Nested $F$-test (adding distance)",
    paste0("$F$(", an8$Df[2], ", ", an8$Res.Df[2], ") = ", num(an8$F[2], 2),
           ", $p$ = ", pfmt(an8$`Pr(>F)`[2]))),
  c("A8", "Influential (Cook's $D$ $>$ 4/$n$)", paste(length(infl8), "cafes")),
  c("A9", "Constant variance (Breusch-Pagan)", bp_txt(bp9)),
  c("A9", "Influential (Cook's $D$ $>$ 4/$n$)", cafes(f$cafe[infl9]))
), stringsAsFactors = FALSE)
names(checks_tab) <- c("Analysis", "Check", "Result")
checks_tab
write_tex(checks_tab, "report/tables/A_checks.tex", "lll")

# software versions for the report
writeLines(paste0(R.version.string, "; packages sandwich ", packageVersion("sandwich"),
                  " and lmtest ", packageVersion("lmtest")),
           "report/tables/versions.tex")
