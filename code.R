

## Setup ------------------------------------------------------------------------------------------

# packages
pkgs <- c("ggplot2", "scales", "sandwich", "quadprog")

# install if missing
missing_pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs, repos = "https://cloud.r-project.org")

# load packages
invisible(lapply(pkgs, library, character.only = TRUE))

invisible(Sys.setlocale("LC_TIME", "C"))   # English month names regardless of system locale
options(width = 150)

# print a table with a title above it
show_table <- function(x, title) {
  cat("\n", title, "\n", sep = "")
  print(x, row.names = FALSE)
}

# data.RData holds four objects. Panel has all monthly series from November 1996 to September 2026.
load("data.RData")


# colours, labels and the recession shading used in the charts ------------------------------------------------------------------------------------------

# short labels and one fixed colour per series (validated categorical palette, assigned in fixed order)
short_names <- c(us_equity = "US equity", euro_equity = "Euro area equity",
                 dev_equity = "Dev. ex US ex euro area equity", em_equity = "Emerging markets equity",
                 euro_govt = "Euro government bonds", us_treasuries = "US Treasuries",
                 euro_corp = "Euro IG corporate bonds", currency = "Currency excess return x")
series_colour <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")
names(series_colour) <- short_names

# equities in one panel, bonds and currency in the other
panel_of <- c(us_equity = "Equities", euro_equity = "Equities", dev_equity = "Equities", em_equity = "Equities",
              euro_govt = "Bonds and currency", us_treasuries = "Bonds and currency",
              euro_corp = "Bonds and currency", currency = "Bonds and currency")

# stack a data frame of series (one column each) into long format for ggplot
to_long <- function(df, dates) {
  data.frame(date   = rep(dates, ncol(df)),
             series = factor(rep(short_names[names(df)], each = nrow(df)), levels = short_names),
             panel  = factor(rep(panel_of[names(df)], each = nrow(df)), levels = c("Equities", "Bonds and currency")),
             value  = unlist(df, use.names = FALSE))
}

# recessions since 1999 (used to shade in figures)
# US: NBER Business Cycle Dating Committee
# euro area: CEPR Euro Area Business Cycle Dating Committee (quarters as months)
recessions <- data.frame(
  source = rep(c("US recession (NBER)", "Euro area recession (CEPR)"), each = 3),
  start  = as.Date(c("2001-03-01", "2007-12-01", "2020-02-01", "2008-01-01", "2011-07-01", "2019-10-01")),
  end    = as.Date(c("2001-11-30", "2009-06-30", "2020-04-30", "2009-06-30", "2013-03-31", "2020-06-30")))
recessions$source <- factor(recessions$source, levels = c("US recession (NBER)", "Euro area recession (CEPR)"))

recession_layer <- function(ymin = -Inf, data = recessions) list(
  geom_rect(data = data, aes(xmin = start, xmax = end, fill = source), ymin = ymin, ymax = Inf,
            inherit.aes = FALSE, alpha = 0.5),
  scale_fill_manual(values = c("US recession (NBER)" = "#d6d5d0", "Euro area recession (CEPR)" = "#9e9d97")))

theme_case <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e6e5e1", linewidth = 0.3),
        axis.text = element_text(colour = "#52514e"),
        axis.title = element_text(colour = "#52514e"),
        strip.text = element_text(colour = "#0b0b0b", face = "bold", hjust = 0),
        legend.position = "bottom", legend.title = element_blank(),
        plot.title.position = "plot")

# recessions since 1881 for figures with long histories
month_end <- function(ym) as.Date(sapply(ym, function(m) seq(as.Date(paste0(m, "-01")), by = "month", length.out = 2)[2] - 1))
us_peak   <- c("1882-03", "1887-03", "1890-07", "1893-01", "1895-12", "1899-06", "1902-09", "1907-05", "1910-01",
               "1913-01", "1918-08", "1920-01", "1923-05", "1926-10", "1929-08", "1937-05", "1945-02", "1948-11",
               "1953-07", "1957-08", "1960-04", "1969-12", "1973-11", "1980-01", "1981-07", "1990-07", "2001-03",
               "2007-12", "2020-02")
us_trough <- c("1885-05", "1888-04", "1891-05", "1894-06", "1897-06", "1900-12", "1904-08", "1908-06", "1912-01",
               "1914-12", "1919-03", "1921-07", "1924-07", "1927-11", "1933-03", "1938-06", "1945-10", "1949-10",
               "1954-05", "1958-04", "1961-02", "1970-11", "1975-03", "1980-07", "1982-11", "1991-03", "2001-11",
               "2009-06", "2020-04")
ea_peak   <- c("1974-07", "1980-01", "1992-01", "2008-01", "2011-07", "2019-10")
ea_trough <- c("1975-03", "1982-09", "1993-09", "2009-06", "2013-03", "2020-06")
recessions_long <- data.frame(
  source = factor(rep(levels(recessions$source), c(length(us_peak), length(ea_peak))), levels = levels(recessions$source)),
  start  = as.Date(paste0(c(us_peak, ea_peak), "-01")),
  end    = month_end(c(us_trough, ea_trough)))


# Task 1: Data ------------------------------------------------------------------------------------

# Since the product measures everything in euro, so we first need to turn the index levels into monthly euro excess returns under the current currency policy
# We do this together with the currency excess return x_t that the hedge is built from. 


## Returns in local currency ----------------------------------------------------------------------

# monthly simple return of a series of index levels
simple_return <- function(level) level[-1] / level[-length(level)] - 1

# annualized volatility of monthly returns
vol_annual <- function(r) sd(r) * sqrt(12)

# format a decimal as a percentage string, e.g. 0.0712 -> "7.1%"
pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), 100 * x)

asset_names <- c(us_equity     = "US equity",
                 euro_equity   = "Euro area equity",
                 dev_equity    = "Developed markets equity ex US and ex euro area",
                 em_equity     = "Emerging markets equity",
                 euro_govt     = "Euro government bonds",
                 us_treasuries = "US Treasuries",
                 euro_corp     = "Euro investment-grade corporate bonds")
asset_currency <- c(us_equity = "USD", euro_equity = "EUR", dev_equity = "USD", em_equity = "USD",
                    euro_govt = "EUR", us_treasuries = "USD", euro_corp = "EUR")

# returns of the indices in their quotation currency
returns_local <- data.frame(date          = core$date[-1],
                            us_equity     = simple_return(core$eq_us_usd),
                            euro_equity   = simple_return(core$eq_emu_eur),
                            em_equity     = simple_return(core$eq_em_usd),
                            euro_govt     = simple_return(core$bd_egov_eur),
                            us_treasuries = simple_return(core$bd_ust_usd),
                            euro_corp     = simple_return(core$bd_ecorp_eur))

# There is no standard index for developed markets ex US and ex euro area, so we build it. 
# MSCI World ex USA is a market-cap weighted mix of the euro area and the rest
# so with w the euro area's share at the start of the month: r_rest = (r_world - w * r_emu) / (1 - w), with both returns in USD.

# euro area share of MSCI World ex USA at each month-end
euro_area_weight <- core$mcap_emu / core$mcap_wxus

# weight at the start of each month = previous month-end
weight_start <- euro_area_weight[-length(euro_area_weight)]

# returns of MSCI World ex USA and MSCI EMU, both in USD
world_ex_us_return <- simple_return(core$eq_wxus_usd)
emu_usd_return     <- simple_return(core$eq_emu_usd)

# remove the euro area from World ex USA
returns_local$dev_equity <- (world_ex_us_return - weight_start * emu_usd_return) / (1 - weight_start)

# put the columns in SAA order
returns_local <- returns_local[, c("date", names(asset_names))]


## Currency excess return and USD cash rate -------------------------------------------------------

# The euro return on one dollar is e_t, positive when the dollar appreciates. 
# A long dollar position financed in euro then earns x_t = e_t + (i_USD - i_EUR) / 12, using last month-end's 3-month rates.

# month t runs from the end of month t-1 to the end of month t
start <- 1:(nrow(core) - 1)
end   <- 2:nrow(core)

# euro price of one dollar (Bloomberg quotes USD per EUR)
eur_per_usd <- 1 / core$eurusd

# e_t: euro return on holding one dollar during month t
usd_return <- eur_per_usd[end] / eur_per_usd[start] - 1

# 3-month rates at the start of each month
euribor_start  <- core$euribor3m[start] / 100
usd_rate_start <- core$usd3m[start] / 100

# x_t: currency excess return of a long USD position financed in EUR
currency_excess <- usd_return + (usd_rate_start - euribor_start) / 12

# The USD rate in core is 3-month USD LIBOR until it ended in September 2024, and 3-month Term SOFR
# plus 0.26161 percentage points afterwards. SOFR is a nearly risk-free rate, while LIBOR contains
# bank credit risk, and 0.26161 is the official ISDA fallback spread that bridges the two. The chart
# shows that the switch causes no jump.

usd_rate <- panel[panel$date >= as.Date("2021-01-01"), c("date", "usd_libor3m", "usd_tsofr3m")]
usd_rate_long <- rbind(
  data.frame(date = usd_rate$date, series = "3M USD LIBOR", value = usd_rate$usd_libor3m),
  data.frame(date = usd_rate$date, series = "3M Term SOFR", value = usd_rate$usd_tsofr3m),
  data.frame(date = usd_rate$date, series = "Term SOFR + 0.26", value = usd_rate$usd_tsofr3m + 0.26161))
usd_rate_long$series <- factor(usd_rate_long$series, levels = unique(usd_rate_long$series))

fig_usd_rate <- ggplot(usd_rate_long[!is.na(usd_rate_long$value), ], aes(date, value / 100, colour = series, linetype = series, linewidth = series)) +
  geom_vline(xintercept = as.Date("2024-09-30"), colour = "#52514e", linewidth = 0.3, linetype = "dotted") +
  annotate("text", x = as.Date("2024-10-31"), y = 0.012, label = "Switch:\nno jump", hjust = 0, size = 3, colour = "#52514e") +
  geom_line() +
  scale_colour_manual(values = c("#2a78d6", "#52514e", "#eb6834")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  scale_linewidth_manual(values = c(1.4, 0.6, 0.6)) +
  scale_y_continuous(labels = function(x) pct(x, 0)) +
  labs(x = NULL, y = NULL) +
  theme_case + theme(legend.text = element_text(size = 8), legend.key.width = unit(0.6, "cm"))
print(fig_usd_rate)


## Euro excess returns under the currency policy --------------------------------------------------

# With covered interest parity, hedging a dollar asset with a forward adds a short position in x_t,
# so the return under the policy is r_unhedged - h * x_t. 
# The policy sets h = 1 for US Treasuries, h = 1/2 for the equities quoted in dollars and h = 0 for euro assets. 
# Excess returns then subtract last month-end's Euribor divided by twelve.

# SAA weights and hedge ratios of the current currency policy
saa_weight  <- c(us_equity = 0.18, euro_equity = 0.14, dev_equity = 0.07, em_equity = 0.06,
                 euro_govt = 0.25, us_treasuries = 0.10, euro_corp = 0.20)
hedge_ratio <- c(us_equity = 0.5, euro_equity = 0, dev_equity = 0.5, em_equity = 0.5,
                 euro_govt = 0, us_treasuries = 1, euro_corp = 0)

# short USD position of the SAA under the policy (the case states 25.5%)
usd_hedge_saa <- sum(saa_weight * hedge_ratio)

# unhedged euro returns: convert the USD assets with the dollar's move
usd_assets <- names(asset_names)[asset_currency == "USD"]
returns_eur_unhedged <- returns_local
for (a in usd_assets) {
  returns_eur_unhedged[[a]] <- (1 + returns_local[[a]]) * (1 + usd_return) - 1
}

# euro returns under the currency policy: subtract h * x_t
returns_eur_policy <- returns_eur_unhedged
for (a in names(asset_names)) {
  returns_eur_policy[[a]] <- returns_eur_unhedged[[a]] - hedge_ratio[a] * currency_excess
}

# return of euro cash over month t
cash_return <- euribor_start / 12

# excess returns under the currency policy (task 1 to 5) and unhedged (task 6)
excess_policy   <- returns_eur_policy
excess_unhedged <- returns_eur_unhedged
excess_policy[, names(asset_names)]   <- returns_eur_policy[, names(asset_names)] - cash_return
excess_unhedged[, names(asset_names)] <- returns_eur_unhedged[, names(asset_names)] - cash_return

# the currency excess return is reported alongside the seven asset classes
excess_policy$currency <- currency_excess



## Summary statistics -----------------------------------------------------------------------------

# Means are annualized by 12 and volatilities by the square root of 12. The 95% confidence interval of
# the mean is +/- 1.96 * volatility / sqrt(years), and the drawdown is the fall of the cumulative
# excess value from its previous peak.

# the seven asset classes plus the currency excess return x_t
stat_series <- c(names(asset_names), "currency")
excess      <- excess_policy[, stat_series]
n_years     <- nrow(excess) / 12

# cumulative excess return (growth of 1 EUR) and drawdown from the previous peak
cumulative <- as.data.frame(lapply(excess, function(r) cumprod(1 + r)))
drawdown   <- as.data.frame(lapply(cumulative, function(w) w / cummax(w) - 1))

stats <- data.frame(
  mean       = 12 * colMeans(excess),
  volatility = sapply(excess, vol_annual))
stats$sharpe       <- stats$mean / stats$volatility
stats$ci_low       <- stats$mean - 1.96 * stats$volatility / sqrt(n_years)
stats$ci_high      <- stats$mean + 1.96 * stats$volatility / sqrt(n_years)
stats$worst        <- sapply(excess, min)
stats$worst_date   <- excess_policy$date[sapply(excess, which.min)]
stats$max_drawdown <- sapply(drawdown, min)

stats_table <- data.frame(
  asset        = short_names[stat_series],
  mean         = pct(stats$mean),
  ci           = paste0("[", pct(stats$ci_low), ", ", pct(stats$ci_high), "]"),
  volatility   = pct(stats$volatility),
  sharpe       = sprintf("%.2f", stats$sharpe),
  worst        = pct(stats$worst),
  worst_date   = format(stats$worst_date, "%b %Y"),
  max_drawdown = pct(stats$max_drawdown))
show_table(stats_table, "Monthly euro excess returns under the current currency policy, annualized, Jan 1999 to Sep 2026")

# Equities earned about five times the bond premium at about four times the risk. US equity has the
# highest Sharpe ratio (0.46) and euro area equity the lowest among equities (0.28), hit by both the
# 2000 to 2003 bear market and the sovereign debt crisis. Among bonds, euro corporates have the best
# Sharpe ratio (0.40), partly because of their shorter duration and not only because of credit. The
# worst months cluster in crises: October 2008 for three of the four equity classes, March 2020 for
# euro corporates and August 2022 for euro government bonds.

cumulative_long <- to_long(cumulative, excess_policy$date)

fig_cumulative <- ggplot(cumulative_long, aes(x = date, y = value, colour = series)) +
  recession_layer(ymin = 0) +
  geom_hline(yintercept = 1, colour = "#52514e", linewidth = 0.3) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ panel, ncol = 2) +
  scale_y_log10() +
  scale_colour_manual(values = series_colour) +
  labs(x = NULL, y = "Value of 1 EUR (log scale)") +
  guides(colour = guide_legend(nrow = 2, order = 1), fill = guide_legend(nrow = 1, order = 2)) +
  theme_case +
  theme(legend.box = "vertical", legend.spacing.y = unit(0, "pt"))
print(fig_cumulative)

drawdown_long <- to_long(drawdown, excess_policy$date)

fig_drawdown <- ggplot(drawdown_long, aes(x = date, y = value, colour = series)) +
  recession_layer() +
  geom_hline(yintercept = 0, colour = "#52514e", linewidth = 0.3) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ panel, ncol = 2) +
  scale_y_continuous(labels = function(x) pct(x, 0)) +
  scale_colour_manual(values = series_colour) +
  labs(x = NULL, y = "Drawdown") +
  guides(colour = guide_legend(nrow = 2, order = 1), fill = guide_legend(nrow = 1, order = 2)) +
  theme_case +
  theme(legend.box = "vertical", legend.spacing.y = unit(0, "pt"))
print(fig_drawdown)

# Equities lost 54% to 66% of their cumulative excess value, in both 2000 to 2003 and 2007 to 2009.
# Government bonds had their largest drawdown not in a crisis but in the rate rises from 2022: by
# September 2026 euro government bonds are still 25% and US Treasuries 27% below their 2020 peaks.


# excess return of each USD asset for hedge ratios 0, 0.5 and 1: unhedged excess return minus h * x_t
hedge_levels <- c("Unhedged" = 0, "Half hedged" = 0.5, "Fully hedged" = 1)
hedging <- do.call(rbind, lapply(usd_assets, function(a) {
  do.call(rbind, lapply(names(hedge_levels), function(h) {
    r <- excess_unhedged[[a]] - hedge_levels[h] * currency_excess
    data.frame(series = unname(short_names[a]), hedge = h,
               metric = c("Volatility p.a.", "Sharpe ratio"),
               value  = c(vol_annual(r), 12 * mean(r) / vol_annual(r)))
  }))
}))
hedging$series <- factor(hedging$series, levels = rev(short_names[usd_assets]))
hedging$hedge  <- factor(hedging$hedge, levels = rev(names(hedge_levels)))
hedging$metric <- factor(hedging$metric, levels = c("Volatility p.a.", "Sharpe ratio"))
hedging$label  <- ifelse(hedging$metric == "Volatility p.a.", pct(hedging$value), sprintf("%.2f", hedging$value))

fig_hedging <- ggplot(hedging, aes(x = value, y = series, fill = hedge)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = label), position = position_dodge(width = 0.8), hjust = -0.15, size = 2.6, colour = "#0b0b0b") +
  facet_wrap(~ metric, scales = "free_x") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.35))) +
  scale_y_discrete(labels = scales::label_wrap(17)) +
  scale_fill_manual(values = c("Unhedged" = "#86b6ef", "Half hedged" = "#2a78d6", "Fully hedged" = "#104281"),
                    breaks = names(hedge_levels)) +
  labs(x = NULL, y = NULL) +
  theme_case +
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank(), panel.spacing = unit(1.5, "lines"))
print(fig_hedging)

# Hedge ratios matter far more for bonds than for equities. Hedging cuts the volatility of US
# Treasuries from 9.5% to 4.6%. The half hedge barely moves US equity (15.5% to 14.7%), and for
# developed and emerging markets equity it even raises volatility slightly (13.3% to 13.5%, 19.0% to
# 19.5%). Task 6 explains why.


## Historical means against the long run ----------------------------------------------------------

# SAA over the sample, and today's index yields minus Euribor as a forward-looking anchor for the bonds
saa_hist_mean <- sum(saa_weight * stats[names(asset_names), "mean"])
last_row   <- nrow(panel)
carry_govt <- (panel$ytw_egov[last_row]  - panel$euribor3m[last_row]) / 100
carry_corp <- (panel$ytw_ecorp[last_row] - panel$euribor3m[last_row]) / 100
bund_start <- panel$y10_de[panel$date == min(excess_policy$date)]
print(round(c(saa_hist_mean = saa_hist_mean, carry_govt = carry_govt, carry_corp = carry_corp), 4))
print(c(bund_start = bund_start, bund_low = min(panel$y10_de, na.rm = TRUE), bund_today = panel$y10_de[last_row]))

# We would not take these means as long-run expectations. The confidence intervals for equities are
# 10 to 15 percentage points wide, and those of euro area equity, developed equity, both government
# bond classes and the currency include zero. Bonds also had a somewhat abnormal tailwind:
# the 10-year Bund yield fell from 3.6% in January 1999 to -0.7% in August 2019, gains that are very unlikely to repeat from today's 3.6%.
# Today's index yield minus Euribor is a better anchor, at 1.3% for euro government bonds and 1.6%
# for euro corporates. The currency excess return should be about zero in the long run, since there
# is no reason for a lasting premium on holding dollars. 


# Task 2: Risk ------------------------------------------------------------------------------------

# The excess returns give us the risk model that the equilibrium and the volatility limit then rely on.
# Our baseline is the sample covariance over the full common sample, annualized by 12.
# It is the longest history available for all seven classes and spans several regimes.

# monthly excess returns of the seven asset classes as a matrix (columns in SAA order)
excess_matrix <- as.matrix(excess_policy[, names(asset_names)])

# annualized sample covariance over full sample
sigma_full <- cov(excess_matrix) * 12

# ex-ante volatility of a portfolio with weights w
portfolio_vol <- function(w, sigma) sqrt(drop(t(w) %*% sigma %*% w))
saa_vol_full  <- portfolio_vol(saa_weight, sigma_full)

# As alternatives we use two estimates that lean on recent data: the last five years, and an
# exponentially weighted covariance, which uses all months but gives a month k months back the
# weight (1 - lambda) * lambda^k. 
# With lambda = 0.97, the RiskMetrics standard for monthly data, a month's weight halves after about 23 months (half life).

# last five years: sample covariance of the last 60 months
recent_months <- tail(seq_len(nrow(excess_matrix)), 60)
sigma_5y <- cov(excess_matrix[recent_months, ]) * 12

# exponentially weighted covariance
# weight (1 - lambda) * lambda^k for the month k months before the last
ewma_cov <- function(returns, lambda) {
  k <- rev(seq_len(nrow(returns)) - 1)            # 0 for the last month, 1 for the month before, ...
  weight <- (1 - lambda) * lambda^k
  weight <- weight / sum(weight)                   # weights sum to one
  demeaned <- sweep(returns, 2, colSums(returns * weight))
  t(demeaned) %*% (demeaned * weight)
}
sigma_ewma <- ewma_cov(excess_matrix, lambda = 0.97) * 12

sigmas <- list(full = sigma_full, last_5y = sigma_5y, ewma = sigma_ewma)

vol_compare <- rbind(
  data.frame(item = short_names[names(asset_names)], sapply(sigmas, function(s) pct(sqrt(diag(s))))),
  data.frame(item = "SAA (ex-ante)", t(sapply(sigmas, function(s) pct(portfolio_vol(saa_weight, s), 2)))))
show_table(vol_compare, "Annualized volatilities of the asset classes and of the SAA under three covariance estimates")

corr_of <- function(sigma, a, b) cov2cor(sigma)[a, b]
pairs <- list(c("euro_equity", "euro_govt"), c("us_equity", "us_treasuries"), c("euro_equity", "euro_corp"),
              c("euro_govt", "euro_corp"), c("us_equity", "euro_equity"))

corr_compare <- data.frame(
  item = sapply(pairs, function(p) paste(short_names[p[1]], "/", short_names[p[2]])),
  sapply(sigmas, function(s) sapply(pairs, function(p) sprintf("%.2f", corr_of(s, p[1], p[2])))))
show_table(corr_compare, "Selected correlations under three covariance estimates")

# lower triangle of a correlation matrix in long format
corr_long <- function(sigma, label) {
  corr <- cov2cor(sigma)
  keep <- lower.tri(corr)
  data.frame(sample = label,
             row = factor(short_names[rownames(corr)[row(corr)[keep]]], levels = rev(short_names[names(asset_names)])),
             col = factor(short_names[colnames(corr)[col(corr)[keep]]], levels = short_names[names(asset_names)]),
             value = corr[keep])
}
heat <- rbind(corr_long(sigma_full, "Full sample"), corr_long(sigma_5y, "Last five years"))
heat$sample <- factor(heat$sample, levels = c("Full sample", "Last five years"))

fig_corr_heatmap <- ggplot(heat, aes(x = col, y = row, fill = value)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", value)), size = 2.4, colour = "#0b0b0b") +
  facet_wrap(~ sample) +
  scale_fill_gradient2(low = "#2a78d6", mid = "#f0efec", high = "#e34948", midpoint = 0, limits = c(-1, 1),
                       name = "Correlation") +
  labs(x = NULL, y = NULL) +
  theme_case +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_blank(),
        legend.position = "right", legend.title = element_text(colour = "#52514e", size = 8))
print(fig_corr_heatmap)

# correlation over the 36 months ending in each month
rolling_corr <- function(a, b, window = 36) {
  sapply(seq_along(a), function(i) if (i < window) NA else cor(a[(i - window + 1):i], b[(i - window + 1):i]))
}
rolling <- data.frame(
  date  = rep(excess_policy$date, 2),
  pair  = rep(c("Euro area equity / euro government bonds", "US equity / US Treasuries"), each = nrow(excess_policy)),
  value = c(rolling_corr(excess_policy$euro_equity, excess_policy$euro_govt),
            rolling_corr(excess_policy$us_equity, excess_policy$us_treasuries)))
rolling <- rolling[!is.na(rolling$value), ]

fig_rolling_corr <- ggplot(rolling, aes(x = date, y = value, colour = pair)) +
  recession_layer() +
  geom_hline(yintercept = 0, colour = "#52514e", linewidth = 0.3) +
  geom_line(linewidth = 0.6) +
  scale_colour_manual(values = c("Euro area equity / euro government bonds" = "#2a78d6",
                                 "US equity / US Treasuries" = "#eb6834")) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = NULL, y = "Correlation (36 months)") +
  guides(colour = guide_legend(nrow = 1, order = 1), fill = guide_legend(nrow = 1, order = 2)) +
  theme_case +
  theme(legend.box = "vertical", legend.spacing.y = unit(0, "pt"))
print(fig_rolling_corr)

# The SAA's ex-ante volatility is 7.23% with the full sample, 7.38% with EWMA and 8.28% with the
# last five years: below the 9% limit in all cases, but with less room for tactical risk in the
# recent estimates.

# There are three things we would not extrapolate into the coming year:
# First, the stock-bond correlation: the full sample gives 0.05 in the euro area and -0.19 in the US, reflecting long
# periods when bonds rallied in equity sell-offs. The US correlation has been positive since 2022
# and the euro area's mostly since 2015, and the last five years give 0.59 and 0.42, so that
# diversification may not be there while inflation risk stays elevated. 
# Second, bond volatilities:
# the 4.5% of euro government bonds was held down by zero rates and ECB purchases (6.5% over the
# last five years). 
# Third, the correlation of government and corporate bonds, which rose from 0.74
# to 0.92 because rate risk dominated recently and would likely fall in a credit event. Equity
# volatilities are less of a concern: the full sample includes crises, which are rare but do occur.

# We keep the full-sample covariance as the baseline, since the equilibrium stands for the board's
# long-run view. Because next year may look more like the recent regime, we use the EWMA estimate to
# check the equilibrium and the volatility limit in the later tasks.


# Task 3: Equilibrium returns ---------------------------------------------------------------------

# The board considers the SAA optimal, so we can read expected returns out of it. An investor
# maximizing w'mu - (A/2) w'Sigma w holds weights with Sigma w = mu / A, and turning this around
# gives the implied excess returns Pi = A Sigma w_SAA. We set A so that the SAA earns the board's
# 2.7 percentage points and then we can solve for the risk aversion A.

# board's long-run assumption for the SAA excess return
saa_premium <- 0.027

# risk aversion that makes the SAA earn exactly this premium
risk_aversion <- saa_premium / portfolio_vol(saa_weight, sigma_full)^2

# From above, we get A = 0.027 / sigma_SAA^2 = 5.17.

# implied equilibrium excess returns
pi_eq <- risk_aversion * drop(sigma_full %*% saa_weight)

# implied Sharpe ratios and the pieces that explain them
vol_full      <- sqrt(diag(sigma_full))
sharpe_eq     <- pi_eq / vol_full
corr_with_saa <- drop(sigma_full %*% saa_weight) / (vol_full * saa_vol_full)
risk_share    <- saa_weight * pi_eq / saa_premium     # share of the SAA variance each asset contributes

# sensitivity: the same calibration with the EWMA covariance from task 2
risk_aversion_ewma <- saa_premium / portfolio_vol(saa_weight, sigma_ewma)^2
pi_ewma <- risk_aversion_ewma * drop(sigma_ewma %*% saa_weight)

equity <- c("us_equity", "euro_equity", "dev_equity", "em_equity")

eq_table <- data.frame(
  asset      = short_names[names(asset_names)],
  weight     = pct(saa_weight, 0),
  pi         = pct(pi_eq, 2),
  pi_ewma    = pct(pi_ewma, 2),
  hist       = pct(stats[names(asset_names), "mean"], 2),
  vol        = pct(vol_full),
  sharpe     = sprintf("%.2f", sharpe_eq),
  corr       = sprintf("%.2f", corr_with_saa),
  risk_share = pct(risk_share))
show_table(eq_table, "Implied equilibrium excess returns with the full-sample and the EWMA covariance (A calibrated to 2.7 pp in both cases)")

eq_types <- c("Implied (full sample)", "Implied (EWMA)", "Historical mean")
eq_plot <- data.frame(
  series = factor(rep(short_names[names(asset_names)], 3), levels = rev(short_names[names(asset_names)])),
  type   = factor(rep(eq_types, each = length(asset_names)), levels = rev(eq_types)),   # dodge draws the first level at the bottom
  value  = c(pi_eq, pi_ewma, stats[names(asset_names), "mean"]))

fig_equilibrium <- ggplot(eq_plot, aes(x = value, y = series, fill = type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = pct(value)), position = position_dodge(width = 0.8), hjust = -0.15, size = 2.6, colour = "#0b0b0b") +
  scale_x_continuous(breaks = seq(0, 0.08, by = 0.02), labels = function(x) pct(x, 0),
                     expand = expansion(mult = c(0, 0.12))) +
  scale_fill_manual(values = c("Implied (full sample)" = "#2a78d6", "Implied (EWMA)" = "#1baf7a", "Historical mean" = "#eb6834"),
                    breaks = eq_types) +
  scale_y_discrete(labels = scales::label_wrap(17)) +
  labs(x = "Excess return p.a.", y = NULL) +
  theme_case
print(fig_equilibrium)

# Dividing Pi_i = A cov(r_i, r_SAA) by sigma_i shows what drives the implied Sharpe ratio: 
# it equals the SAA's own Sharpe ratio (0.37) times the asset's correlation with the SAA, because in
# equilibrium an asset is only paid for the risk it adds. 
# The equity classes therefore have the highest implied Sharpe ratios (0.30 to 0.34), 
# with 45% of the weight they make up 87% of the SAA's variance, so they are highly correlated with it (0.80 to 0.90). 
# Bonds rank low, and US Treasuries are the extreme case (correlation 0.04, Sharpe ratio 0.01), because they rallied when equities fell and so act as insurance.

# The bond premia depend heavily on the stock-bond correlation. 
# With the EWMA covariance, US Treasuries rise from 0.06% to 0.96% and euro government bonds from 0.58% to 1.48%.



# Task 4: Views -----------------------------------------------------------------------------------

# The equilibrium is the long-run anchor. For the coming year we add four views, using data up to
# the end of September 2026. Each comes from a predictive regression of the excess return over the
# next twelve months on a predictor known today, r(t to t+12) = a + b x_t + e. 

# We note 3 points of extra care here:
# 1. Consecutive observations share eleven months of returns, so the errors are autocorrelated and we use Newey-West standard errors with 12 lags. 
# 2. Predictive regressions also often fit well in-sample but fail in real time, so we re-estimate every month with only the data known then and compute R2_OS = 1 - MSE_model / MSE_mean against the historical mean. 
# 3. The uncertainty omega of a view is the variance of its fitted value, scaled up by MSE_model / MSE_mean if the model lost to the mean out of sample.

# excess return over the next 12 months: sum of the monthly excess returns of months t+1 to t+12
next_12m <- function(r) {
  n <- length(r)
  sapply(seq_len(n), function(t) if (t + 12 > n) NA else sum(r[(t + 1):(t + 12)]))
}

# out-of-sample comparison of the model's forecast errors with those of the historical mean
oos_summary <- function(oos) {
  mse_ratio <- sum((oos$actual - oos$model)^2) / sum((oos$actual - oos$hist_mean)^2)
  list(mse_ratio = mse_ratio, r2_oos = 1 - mse_ratio)
}

# predictive regression on consecutive months with Newey-West standard errors
predictive_regression <- function(y, x, dates, oos_start) {
  usable <- which(!is.na(y) & !is.na(x))
  fit <- lm(y[usable] ~ x[usable])
  cov_nw <- sandwich::NeweyWest(fit, lag = 12, prewhite = FALSE)

  # forecast for the next 12 months and its standard error
  now <- max(which(!is.na(x)))
  c_now <- c(1, x[now])
  forecast    <- sum(c_now * coef(fit))
  forecast_se <- sqrt(drop(t(c_now) %*% cov_nw %*% c_now))

  oos <- do.call(rbind, lapply(usable[dates[usable] >= oos_start], function(s) {
    known <- usable[usable <= s - 12]
    b <- coef(lm(y[known] ~ x[known]))
    data.frame(date = dates[s], model = b[1] + b[2] * x[s], hist_mean = mean(y[known]), actual = y[s])
  }))

  c(list(slope = unname(coef(fit)[2]), t_slope = unname(coef(fit)[2] / sqrt(cov_nw[2, 2])),
         r2 = summary(fit)$r.squared, sample = dates[range(usable)], x_now = x[now], x_mean = mean(x[usable]),
         forecast = forecast, forecast_se = forecast_se, oos = oos,
         data = data.frame(date = dates[usable], x = x[usable], y = y[usable])),
    oos_summary(oos))
}

# predictor from the Bloomberg panel, aligned with the months of the excess returns
at_return_dates <- function(series) panel[[series]][match(excess_policy$date, panel$date)]

# first out-of-sample forecast of the Bloomberg-based views: the first estimation window then contains the 2008/09 crisis
oos_start_bloomberg <- as.Date("2010-01-01")

# scatter of predictor against the next 12 months, with today's predictor (dashed) and the forecast (red dot)
scatter_plot <- function(reg_list, labels) {
  points <- do.call(rbind, Map(function(r, label) data.frame(r$data, view = label), reg_list, labels))
  today  <- data.frame(view = labels, x = sapply(reg_list, `[[`, "x_now"), y = sapply(reg_list, `[[`, "forecast"))
  points$view <- factor(points$view, levels = labels)
  today$view  <- factor(today$view, levels = labels)

  ggplot(points, aes(x = x, y = y)) +
    geom_hline(yintercept = 0, colour = "#52514e", linewidth = 0.3) +
    geom_point(colour = "#2a78d6", size = 0.5, alpha = 0.3) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "#0b0b0b", linewidth = 0.6) +
    geom_vline(data = today, aes(xintercept = x), colour = "#e34948", linetype = "dashed", linewidth = 0.4) +
    geom_point(data = today, colour = "#e34948", size = 2.2) +
    facet_wrap(~ view, ncol = 2, scales = "free") +
    scale_x_continuous(labels = function(x) pct(x, 1)) +
    scale_y_continuous(labels = function(x) pct(x, 0)) +
    labs(x = "Predictor", y = "Excess return, next 12 months") +
    theme_case
}


## View 1: absolute view on US equity -------------------------------------------------------------

# When stocks are expensive, later returns tend to be low, because a valuation ratio can only
# normalize through lower prices or higher fundamentals, and historically it has mostly been prices.
# We use the dividend yield and the CAPE yield (1/CAPE), since expected returns move with yields
# rather than price multiples. CAPE smooths the cycle by dividing the price by ten years of real
# earnings.

# For a long sample we use Shiller's S&P Composite, a broad market-cap weighted index standing in
# for MSCI USA. The monthly total return is r_t = (P_t + D_t / 12) / P_t-1 - 1, since D_t is a
# trailing twelve-month amount. 
# We subtract the 1-month T-bill return, whose series sets the start of the sample in July 1926. 
# Shiller's dividends end in June 2026, so we keep the June value forthe last months; dividends barely change month to month.

shiller <- long_history[!is.na(long_history$sp_price), ]

last_dividend <- max(which(!is.na(shiller$sp_dividend)))
shiller$sp_dividend[last_dividend:nrow(shiller)] <- shiller$sp_dividend[last_dividend]

n_shiller <- nrow(shiller)
shiller$return <- c(NA, (shiller$sp_price[-1] + shiller$sp_dividend[-1] / 12) / shiller$sp_price[-n_shiller] - 1)
us_long <- shiller[!is.na(shiller$tbill1m_return), ]
us_long$excess_next_12m <- next_12m(us_long$return - us_long$tbill1m_return / 100)

# predictors: CAPE yield and dividend yield
us_long$cape_yield     <- 1 / us_long$cape
us_long$dividend_yield <- us_long$sp_dividend / us_long$sp_price

reg_cape <- predictive_regression(us_long$excess_next_12m, us_long$cape_yield, us_long$date, as.Date("1956-01-01"))
reg_dp   <- predictive_regression(us_long$excess_next_12m, us_long$dividend_yield, us_long$date, as.Date("1956-01-01"))

# one row per regression: slope and Newey-West t-stat, fit, today's predictor against its average, and the forecast
regression_results <- function(regs, predictors, with_omega = TRUE) {
  results <- data.frame(
    predictor   = predictors,
    sample      = sapply(regs, function(r) paste(format(r$sample, "%Y"), collapse = "-")),
    beta        = sapply(regs, function(r) sprintf("%.2f", r$slope)),
    t_nw        = sapply(regs, function(r) sprintf("%.2f", r$t_slope)),
    r2          = sapply(regs, function(r) pct(r$r2)),
    r2_oos      = sapply(regs, function(r) pct(r$r2_oos)),
    x_now       = sapply(regs, function(r) pct(r$x_now, 2)),
    x_avg       = sapply(regs, function(r) pct(r$x_mean, 2)),
    forecast    = sapply(regs, function(r) pct(r$forecast, 2)),
    forecast_se = sapply(regs, function(r) pct(r$forecast_se, 2)))
  if (with_omega) results$sqrt_omega <- sapply(regs, function(r) pct(r$forecast_se * sqrt(max(1, r$mse_ratio)), 2))
  results
}

show_table(regression_results(list(reg_cape, reg_dp), c("CAPE yield 1/CAPE", "Dividend yield D/P"), with_omega = FALSE),
           "View 1: regressions of the 12-month US equity excess return in USD over T-bills")

cape_history <- long_history[!is.na(long_history$cape), c("date", "cape")]
cape_now <- tail(cape_history, 1)

fig_cape <- ggplot(cape_history, aes(x = date, y = cape)) +
  geom_hline(yintercept = median(cape_history$cape), colour = "#52514e", linewidth = 0.4, linetype = "dashed") +
  geom_line(colour = "#2a78d6", linewidth = 0.5) +
  geom_point(data = cape_now, colour = "#e34948", size = 2) +
  annotate("text", x = cape_now$date, y = cape_now$cape, label = sprintf("Sep 2026: %.1f", cape_now$cape),
           hjust = 1.1, vjust = -0.6, size = 3, colour = "#0b0b0b") +
  annotate("text", x = as.Date("2000-06-30"), y = median(cape_history$cape),   # below the line, where the CAPE stayed above 19 from 1992 to 2008
           label = sprintf("Median\n%.1f", median(cape_history$cape)), hjust = 0.5, vjust = 1.25, lineheight = 0.9, size = 3, colour = "#52514e") +
  labs(x = NULL, y = "CAPE") +
  theme_case
print(fig_cape)

# share of months since 1881 with a CAPE at or below today's
cape_rank <- mean(cape_history$cape <= cape_now$cape)

print(round(c(cape_now = cape_now$cape, cape_rank = cape_rank), 2))

fig_scatter_us_equity <- scatter_plot(list(reg_cape, reg_dp), c("CAPE yield 1/CAPE", "Dividend yield D/P"))
print(fig_scatter_us_equity)

# Both slopes are positive and significant at the 5% level (t = 2.96 and 2.04): a higher yield goes
# with a higher return over the next year. The R2 of 6.7% and 4.2% shows that most of next year's
# return is noise, so the view is a statement about the expected return, not the realized one.
# Today's CAPE of 40.6 is above 99% of months since 1881, and its yield of 2.46% and the D/P of
# 1.07% are far below their averages (6.23% and 3.63%). That gives low forecasts of 0.85% and 2.08%,
# at the edge of the sample, so the line is being extrapolated. Neither regression beat the
# historical mean out of sample (-30.5% and -5.3%).

# The two predictors are highly correlated (0.84), so two separate views would count the same
# information twice. We thus average their forecasts into one view, and average their standard
# errors. Averaging also helps because buybacks have replaced part of dividends since the 1980s,
# which makes the D/P forecast too pessimistic, while the CAPE yield is based on earnings.

# The regressions give an excess return in USD over T-bills, which we convert to euro under the
# policy: r_EUR - h x - i_EUR/12 = (r_USD - rf) - (i_USD/12 - rf) + (1 - h) x + r_USD e. For E[x] we
# let the dollar follow a random walk, so it earns today's rate differential.

# average of the two forecasts (and of their out-of-sample forecasts)
us_equity_oos <- data.frame(date = reg_cape$oos$date, model = (reg_cape$oos$model + reg_dp$oos$model) / 2,
                            hist_mean = reg_cape$oos$hist_mean, actual = reg_cape$oos$actual)
us_equity_local    <- (reg_cape$forecast + reg_dp$forecast) / 2
us_equity_local_se <- (reg_cape$forecast_se + reg_dp$forecast_se) / 2

# from USD to euro under the currency policy
expected_x <- (tail(core$usd3m, 1) - tail(core$euribor3m, 1)) / 100    
cross_term <- 12 * cov(returns_local$us_equity, usd_return)            # covariance of US equity with the dollar, p.a.

cash_gap <- (tail(core$usd3m, 1) - panel$tbill3m[nrow(panel)]) / 100
us_equity_view <- us_equity_local - cash_gap + (1 - hedge_ratio[["us_equity"]]) * expected_x + cross_term

# from the two regressions to the view in euro
us_equity_mse_ratio <- oos_summary(us_equity_oos)$mse_ratio
view_1_steps <- data.frame(
  step  = c("CAPE forecast (USD over T-bills)", "D/P forecast (USD over T-bills)", "Average of the two",
            "Minus USD cash rate over T-bill", "Plus unhedged half of E[x]", "Plus covariance with the dollar",
            "View in euro", "Standard error (average of the two)", "Out-of-sample R2 of the average", "sqrt(omega)"),
  value = pct(c(reg_cape$forecast, reg_dp$forecast, us_equity_local,
                -cash_gap, (1 - hedge_ratio[["us_equity"]]) * expected_x, cross_term,
                us_equity_view, us_equity_local_se, 1 - us_equity_mse_ratio,
                us_equity_local_se * sqrt(max(1, us_equity_mse_ratio))), 2))
show_table(view_1_steps, "View 1: from the regressions to the expected euro excess return")

# The average is 1.46% in USD. Subtracting the gap between the USD cash rate and the T-bill (0.24%),
# adding the unhedged half of the expected currency return (0.86%) and adding the covariance of US
# equity with the dollar (-0.41%) gives a view of 1.68% in euro, against an equilibrium of 4.94%.
# The rate differential behind E[x] is 1.73%. This one-year assumption differs from the zero we
# expect in the long run: over short horizons high-rate currencies have not depreciated as uncovered
# interest parity predicts.


## View 2: absolute view on euro government bonds -------------------------------------------------

# The shape of the yield curve predicts bond excess returns. 
# Cochrane and Piazzesi combine the forward rates into a single factor.
# We build from German zero-coupon yields in two steps:
# 1. compute the forwards and the one-year excess returns on 2- to 5-year bonds.
# 2. regress the average excess return on the five forwards. 
# The fitted value is the CP factor. 
# Our index is not a German zero, so we regress the index's own 12-month excess return on the factor. 
# For simplicity the out-of-sample check keeps the factor estimated on the full German sample, which slightly flatters this view.

german <- long_history[!is.na(long_history$zero_de_1y), c("date", paste0("zero_de_", 1:5, "y"))]
n_german <- nrow(german)

# log yields and log bond prices p(n) = -n * y(n), maturities 1 to 5 years in the columns
log_yield <- as.matrix(german[, -1]) / 100
log_price <- -sweep(log_yield, 2, 1:5, "*")

# forward rates: f(1) = y(1), f(n) = p(n-1) - p(n)
forwards <- cbind(log_yield[, 1], log_price[, 1:4] - log_price[, 2:5])

# excess return of buying an n-year bond today and selling it as an (n-1)-year bond in 12 months
bought <- 1:(n_german - 12)
sold   <- 13:n_german
bond_excess <- matrix(NA, n_german, 4)
for (n in 2:5) bond_excess[bought, n - 1] <- log_price[sold, n - 1] - log_price[bought, n] - log_yield[bought, 1]

# CP factor: fitted value of the average excess return regressed on the five forward rates
cp_fit <- lm(rowMeans(bond_excess) ~ forwards)
german$cp_factor <- drop(cbind(1, forwards) %*% coef(cp_fit))

# view: 12-month excess return of the euro government bond index on the CP factor
govt_bonds <- data.frame(date = excess_policy$date, y = next_12m(excess_policy$euro_govt))
govt_bonds$cp_factor <- german$cp_factor[match(govt_bonds$date, german$date)]
reg_cp <- predictive_regression(govt_bonds$y, govt_bonds$cp_factor, govt_bonds$date, oos_start_bloomberg)

# first step: how the forward rates map into the CP factor
print(round(setNames(coef(cp_fit), c("intercept", paste0("f", 1:5))), 2))

show_table(regression_results(list(reg_cp), "CP factor (German curve)"),
           "View 2: regression of the 12-month euro government bond excess return")
print(round(c(zero_1y = log_yield[n_german, 1], zero_5y = log_yield[n_german, 5], cp_avg_since_1972 = mean(german$cp_factor)) * 100, 2))

# The slope of 1.59 has the expected sign but is not significant at the 5% level (t = 1.75).
# The R2 is 5.9%, and the regression lost to the historical mean out of sample (-7.1%), so its omega is scaled up. 
# The German curve is almost flat from 1 to 5 years (3.04% to 3.36%), which puts today's CP factor of 0.30% far below its 1.09% average since 1972, but close to its 0.34% average over the regression sample. So the view of 1.50% is roughly the index's historical mean. 
# That mean includes the gains from falling yields, so we treat it as an upper bound.


## View 3: absolute view on US Treasuries ---------------------------------------------------------

# A steep curve contains a term premium that is earned on average. 
# We use the 10-year yield minus the 3-month T-bill, both government rates, so the short leg has no bank credit risk to distort the spread in crises. 
#Treasuries are fully hedged, so the view in euro and in local currency are the same up to a negligible cross term.

treasuries <- data.frame(date = excess_policy$date, y = next_12m(excess_policy$us_treasuries),
                         term_spread = (at_return_dates("y10_us") - at_return_dates("tbill3m")) / 100)
reg_term <- predictive_regression(treasuries$y, treasuries$term_spread, treasuries$date, oos_start_bloomberg)

show_table(regression_results(list(reg_term), "10y yield minus 3m T-bill"),
           "View 3: regression of the 12-month US Treasury excess return (euro, fully hedged)")

# The slope of 0.98 is significant (t = 2.24) with an R2 of 6.8%, and unlike the CP factor this
# regression beat the historical mean out of sample (5.3%), so its omega is not scaled up. The
# spread is 1.18% against a 1.36% average, so the view of 0.92% is also close to the historical mean
# and also an upper bound.


## View 4: relative view on euro corporate vs. government bonds -----------------------------------

# The credit spread is the price of credit risk, and spreads predict bond returns rather than later defaults
# a wide spread means a high expected return of corporates over government bonds, a tight spread a low one. 

# 12-month excess return of euro corporate over euro government bonds, and the credit spread
credit <- data.frame(date = excess_policy$date,
                     y    = next_12m(excess_policy$euro_corp - excess_policy$euro_govt),
                     oas  = at_return_dates("oas_ecorp") / 100)
reg_oas <- predictive_regression(credit$y, credit$oas, credit$date, oos_start_bloomberg)

show_table(regression_results(list(reg_oas), "Corporate OAS"),
           "View 4: regression of the 12-month return of euro corporates minus euro government bonds")

fig_scatter_credit <- scatter_plot(list(reg_oas), "Option-adjusted spread")
print(fig_scatter_credit)

# history of the spread with the level where the regression line crosses zero
oas_history <- panel[!is.na(panel$oas_ecorp) & panel$date >= as.Date("2000-08-31"), c("date", "oas_ecorp")]
break_even  <- reg_oas$x_now - reg_oas$forecast / reg_oas$slope

fig_oas_history <- ggplot(oas_history, aes(date, oas_ecorp / 100)) +
  recession_layer() +
  geom_hline(yintercept = break_even, colour = "#52514e", linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = as.Date("2004-06-30"), y = break_even, label = sprintf("Break-even %.1f%%", 100 * break_even),
           vjust = -0.6, size = 3, colour = "#52514e") +
  geom_line(colour = "#2a78d6", linewidth = 0.5) +
  geom_point(data = tail(oas_history, 1), colour = "#e34948", size = 2) +
  annotate("text", x = max(oas_history$date), y = tail(oas_history$oas_ecorp, 1) / 100,
           label = sprintf("Sep 2026: %.2f%%", tail(oas_history$oas_ecorp, 1)), hjust = 1.05, vjust = 1.8, size = 3, colour = "#0b0b0b") +
  scale_y_continuous(labels = function(x) pct(x, 0)) +
  labs(x = NULL, y = "Corporate OAS") +
  theme_case + theme(legend.position = "none")
print(fig_oas_history)

print(round(c(break_even = break_even, dur_corp = panel$dur_ecorp[nrow(panel)] / 100, dur_govt = panel$dur_egov[nrow(panel)] / 100) * 100, 2))

# This is the best in-sample fit of the four views: a slope of 2.07 (t = 3.69) and an R2 of 19.6%.
# It also beat the historical mean out of sample (4.4%). 
# The regression line crosses zero at a spread of about 1.1%: below that, the spread barely covers expected losses and the risk of widening from tight levels. 
# Today's 0.91% (average 1.28%) is below it, so we expect corporates to trail government bonds by 0.47%. The corporate index has a shorter duration (4.3 vs. 6.6 years),
# so the view also contains a rate bet and is not pure credit.


## All four views in the form P mu = v + e --------------------------------------------------------

regs <- list(reg_cape, reg_dp, reg_cp, reg_term, reg_oas)
reg_table <- data.frame(
  view      = c("1 US equity", "1 US equity", "2 Euro govt bonds", "3 US Treasuries",
                "4 Euro corp minus govt bonds"),
  predictor = c("CAPE yield 1/CAPE", "Dividend yield D/P", "CP factor (German curve)", "10y yield minus 3m T-bill",
                "Corporate OAS"),
  sample    = sapply(regs, function(r) paste(format(r$sample, "%Y"), collapse = "-")),
  slope     = sapply(regs, function(r) sprintf("%.2f", r$slope)),
  t_nw      = sapply(regs, function(r) sprintf("%.2f", r$t_slope)),
  r2        = sapply(regs, function(r) pct(r$r2)),
  r2_oos    = sapply(regs, function(r) pct(r$r2_oos)),
  now       = sapply(regs, function(r) pct(r$x_now, 2)),
  avg       = sapply(regs, function(r) pct(r$x_mean, 2)),
  forecast  = sapply(regs, function(r) pct(r$forecast, 2)))

show_table(reg_table, "Predictive regressions of the 12-month excess return on each predictor")

# Each row of P says which assets a view is about: a single 1 for an absolute view, and +1 and -1
# for the relative view, so its row sums to zero. v holds the expected euro excess returns, and the
# diagonal matrix Omega holds the uncertainty of each view. For comparison we also compute the
# equilibrium return of each view portfolio and its prior uncertainty.

tau <- 0.05     # scales Sigma into the uncertainty of the equilibrium returns (task 5)

view_ids <- c("us_equity", "euro_govt", "us_treasuries", "corp_minus_govt")

# P: one row per view; an absolute view has a single 1, a relative view goes long one asset and short another
P <- matrix(0, length(view_ids), length(asset_names), dimnames = list(view_ids, names(asset_names)))
P["us_equity", "us_equity"]         <- 1
P["euro_govt", "euro_govt"]         <- 1
P["us_treasuries", "us_treasuries"] <- 1
P["corp_minus_govt", c("euro_corp", "euro_govt")]  <- c(1, -1)

# v: expected euro excess returns (US equity converted from USD, the others estimated in euro)
v <- c(us_equity = us_equity_view, euro_govt = reg_cp$forecast, us_treasuries = reg_term$forecast,
       corp_minus_govt = reg_oas$forecast)

# omega: variance of the estimated expected return, scaled up if the model lost to the historical mean out of sample
view_se    <- c(us_equity_local_se, reg_cp$forecast_se, reg_term$forecast_se, reg_oas$forecast_se)
view_oos   <- list(us_equity_oos, reg_cp$oos, reg_term$oos, reg_oas$oos)
mse_ratio  <- sapply(view_oos, function(o) oos_summary(o)$mse_ratio)
omega      <- view_se^2 * pmax(1, mse_ratio)
names(omega) <- view_ids
Omega <- diag(omega, nrow = length(view_ids))

# for comparison: the equilibrium return of each view portfolio and its prior uncertainty tau * p' Sigma p
pi_view   <- drop(P %*% pi_eq)
prior_var <- tau * diag(P %*% sigma_full %*% t(P))

view_labels <- c(us_equity = "US equity", euro_govt = "Euro government bonds", us_treasuries = "US Treasuries",
                 corp_minus_govt = "Euro corp. minus euro govt bonds")
view_predictor <- c(sprintf("CAPE %.1f, D/P %s", tail(us_long$cape, 1), pct(reg_dp$x_now, 2)),
                    sprintf("CP factor %s", pct(reg_cp$x_now, 2)), sprintf("Term spread %s", pct(reg_term$x_now, 2)),
                    sprintf("OAS %s", pct(reg_oas$x_now, 2)))
views_table <- data.frame(
  view      = view_labels,
  type      = c("Absolute", "Absolute", "Absolute", "Relative"),
  predictor = view_predictor,
  v         = pct(v, 2),
  pi        = pct(pi_view, 2),
  se        = pct(sqrt(omega), 2),
  omega     = sprintf("%.6f", omega),
  prior_se  = pct(sqrt(prior_var), 2))
show_table(views_table, "Views for Oct 2026 to Sep 2027 against the equilibrium returns of task 3")

P_show <- data.frame(view = view_labels, round(P, 2), v = pct(v, 2), check.names = FALSE)
show_table(P_show, "View matrix P and view vector v")

# The views are most negative on US equity (1.68% against an equilibrium of 4.94%). Both bond views
# lie above equilibrium (1.50% vs. 0.58%, 0.92% vs. 0.06%), because the equilibrium gives bonds
# little return for adding little risk. The relative view is mildly negative (-0.47% vs. 0.37%).


## Reliability of the views --------------------------------------------------------------------

# view uncertainty relative to prior uncertainty
se_ratio <- sqrt(omega / prior_var)
print(round(se_ratio, 2))

# robustness: the same out-of-sample check starting in January 2009
r2os_2009 <- c(cp     = predictive_regression(govt_bonds$y, govt_bonds$cp_factor, govt_bonds$date, as.Date("2009-01-01"))$r2_oos,
               term   = predictive_regression(treasuries$y, treasuries$term_spread, treasuries$date, as.Date("2009-01-01"))$r2_oos,
               credit = predictive_regression(credit$y, credit$oas, credit$date, as.Date("2009-01-01"))$r2_oos)
spread_pre_crisis <- range(credit$oas[credit$date < as.Date("2008-01-01")], na.rm = TRUE)
print(round(r2os_2009, 3))
print(round(spread_pre_crisis, 4))

oos_labels <- c("1 US equity (CAPE + D/P)", "2 Euro govt bonds (CP)", "3 US Treasuries (term spread)",
                "4 Euro corp - govt bonds (OAS)")
oos_plot <- do.call(rbind, Map(function(o, label) {
  data.frame(date = o$date, view = label, value = cumsum((o$actual - o$hist_mean)^2 - (o$actual - o$model)^2))
}, view_oos, oos_labels))
oos_plot$view <- factor(oos_plot$view, levels = oos_labels)

# recessions within each panel's out-of-sample period (the US equity panel starts in 1956)
oos_recessions <- do.call(rbind, lapply(oos_labels, function(label) {
  span <- range(oos_plot$date[oos_plot$view == label])
  r <- recessions_long[recessions_long$end >= span[1] & recessions_long$start <= span[2], ]
  r$start <- pmax(r$start, span[1])
  r$end   <- pmin(r$end, span[2])
  r$view  <- factor(rep(label, nrow(r)), levels = oos_labels)
  r
}))

fig_oos <- ggplot(oos_plot, aes(x = date, y = value)) +
  recession_layer(data = oos_recessions) +
  geom_hline(yintercept = 0, colour = "#52514e", linewidth = 0.3) +
  geom_line(colour = "#2a78d6", linewidth = 0.6) +
  facet_wrap(~ view, ncol = 2, scales = "free") +
  labs(x = NULL, y = "Cumulative SSE difference") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_case +
  theme(strip.text = element_text(size = 8))
print(fig_oos)

# When the chart rises, the model is beating the historical mean. 
# The term spread (5.3%) and the credit spread (4.4%) beat it out of sample. 
# The US valuation regressions (-12.2%) and the CP factor (-7.1%) did not. 
# We keep them because their economic logic is clear, and their larger omega already reflects the weak record.

# The Bloomberg forecasts start in January 2010, so the first estimation window includes the 2008/09 crisis. 
# Starting in January 2009 would put credit's R2_OS at -96.8%, because a model fitted only on the calm years before 2008 had not seen wide spreads. 
#CP (-6.2%) and the term spread (5.6%) barely change.


compare_types <- c("Equilibrium (prior)", "View")
compare <- data.frame(
  view  = factor(rep(view_labels, 2), levels = rev(view_labels)),
  type  = factor(rep(compare_types, each = length(view_ids)), levels = rev(compare_types)),
  value = c(pi_view, v),
  se    = c(sqrt(prior_var), sqrt(omega)))

fig_views_vs_prior <- ggplot(compare, aes(x = value, y = view, colour = type)) +
  geom_vline(xintercept = 0, colour = "#52514e", linewidth = 0.3) +
  geom_errorbar(aes(xmin = value - 1.96 * se, xmax = value + 1.96 * se), width = 0.25, linewidth = 0.5,
                position = position_dodge(width = 0.5), orientation = "y") +
  geom_point(size = 2.2, position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = c("Equilibrium (prior)" = "#2a78d6", "View" = "#eb6834"), breaks = compare_types) +
  scale_x_continuous(labels = function(x) pct(x, 0)) +
  labs(x = "Expected euro excess return p.a.", y = NULL) +
  theme_case
print(fig_views_vs_prior)

# After the scaling, sqrt(omega) is 0.78 to 0.92 times the prior uncertainty for every view. 
# Views and equilibrium therefore get similar weight, and the posterior in task 5 will lie between them
# rather than jump to the views.


# Task 5: Black-Litterman and tactical asset allocation -------------------------------------------


## Posterior expected returns ---------------------------------------------------------------------

# Black-Litterman treats the equilibrium as a prior, mu ~ N(Pi, tau Sigma), and the views as a noisy
# signal, P mu = v + e with e ~ N(0, Omega). The posterior mean is a precision-weighted average of
# the two. We use tau = 0.05 and the full-sample Sigma, the same risk model that produced Pi, so a
# different Sigma here cannot shift returns for reasons unrelated to the views.

# The same formula can be written as mu_bar = Pi + tau Sigma P' (P tau Sigma P' + Omega)^-1 (v - P
# Pi). Each view's surprise v - P Pi is weighted by its confidence and passed to every asset through
# its covariance with the view portfolio. That is how assets without a view move too, and it splits
# mu_bar - Pi exactly into one contribution per view.

tau_sigma <- tau * sigma_full

# precision-weighted average of the equilibrium (prior) and the views
posterior_precision <- solve(tau_sigma) + t(P) %*% solve(Omega) %*% P
mu_bar <- drop(solve(posterior_precision, solve(tau_sigma) %*% pi_eq + t(P) %*% solve(Omega) %*% v))

# surprise v - P Pi, weighted by its confidence
view_weight  <- drop(solve(P %*% tau_sigma %*% t(P) + Omega, v - drop(P %*% pi_eq)))
contribution <- tau_sigma %*% t(P) %*% diag(view_weight)     # rows: asset classes, columns: views
colnames(contribution) <- view_ids

posterior_table <- data.frame(
  asset  = short_names[names(asset_names)],
  pi     = pct(pi_eq, 2),
  mu_bar = pct(mu_bar, 2),
  change = pct(mu_bar - pi_eq, 2),
  sharpe_pi = sprintf("%.2f", pi_eq / vol_full),
  sharpe_mu = sprintf("%.2f", mu_bar / vol_full))
show_table(posterior_table, "Equilibrium and posterior expected euro excess returns (tau = 0.05, full-sample Sigma)")

posterior_types <- c("Equilibrium", "Posterior")
posterior_plot <- data.frame(
  series = factor(rep(short_names[names(asset_names)], 2), levels = rev(short_names[names(asset_names)])),
  type   = factor(rep(posterior_types, each = length(asset_names)), levels = rev(posterior_types)),
  value  = c(pi_eq, mu_bar))

fig_posterior <- ggplot(posterior_plot, aes(x = value, y = series, fill = type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = pct(value, 2)), position = position_dodge(width = 0.8), hjust = -0.15, size = 2.6, colour = "#0b0b0b") +
  scale_x_continuous(labels = function(x) pct(x, 0), expand = expansion(mult = c(0, 0.12))) +
  scale_fill_manual(values = c("Equilibrium" = "#2a78d6", "Posterior" = "#eb6834"), breaks = posterior_types) +
  labs(x = "Expected euro excess return p.a.", y = NULL) +
  theme_case
print(fig_posterior)

view_short <- c(us_equity = "1 US equity", euro_govt = "2 Euro govt bonds", us_treasuries = "3 US Treasuries",
                corp_minus_govt = "4 Euro corp minus govt")
contribution_plot <- data.frame(
  series = factor(rep(short_names[names(asset_names)], length(view_ids)), levels = rev(short_names[names(asset_names)])),
  view   = factor(rep(view_short, each = length(asset_names)), levels = view_short),
  value  = as.vector(contribution))
total_change <- data.frame(series = factor(short_names[names(asset_names)], levels = rev(short_names[names(asset_names)])),
                           value = mu_bar - pi_eq)

fig_view_contributions <- ggplot(contribution_plot, aes(x = value, y = series)) +
  geom_vline(xintercept = 0, colour = "#52514e", linewidth = 0.4) +
  geom_col(aes(fill = view), width = 0.6) +
  geom_point(data = total_change, colour = "#0b0b0b", size = 2) +
  scale_fill_manual(values = setNames(series_colour[seq_along(view_short)], view_short)) +
  scale_x_continuous(labels = function(x) pct(x, 1)) +
  labs(x = "Change in expected excess return (posterior minus equilibrium)", y = NULL) +
  guides(fill = guide_legend(nrow = 2)) +
  theme_case
print(fig_view_contributions)

# All four equity classes lose 1.8 to 2.3 percentage points although only US equity has a view. The
# US equity view is the main driver: equities are highly correlated, so euro area equity, which has
# no view of its own, falls by 2.22, more than US equity itself (2.07). The credit view accounts for
# 0.6 to 0.9 of the fall, because credit risk moves with equities, and it hits the other equity
# classes harder than US equity. Government bonds are its short leg, so it also raises euro
# government bonds (0.25) and, through their correlation, US Treasuries (0.24).

# The two bond views raise both government bond classes and, through correlation, euro corporates,
# though the other views offset this and corporates end up almost unchanged (+0.03). The posterior
# lies between equilibrium and views (US equity: 2.87%, between 4.94% and 1.68%), and euro
# government and corporate bonds now have higher Sharpe ratios than any equity class.


## Tactical asset allocation ----------------------------------------------------------------------

# We maximize w'mu_bar - (A/2) w'Sigma w subject to the tactical bands (each SAA weight +/- 3 to 5
# pp), total equity of 45 +/- 8 pp, 0.95 <= sum(w) <= 1 with the rest in cash, and a volatility of
# at most 9%. The problem is quadratic, so we use solve.QP, which minimizes 1/2 w'Dw - d'w subject
# to C'w >= b: we set D = A Sigma and d = mu_bar and write every constraint as "greater or equal".
# The volatility limit is imposed by raising A in the optimization, not in the calibration of Pi,
# until it holds.

tactical_band <- c(us_equity = 0.05, euro_equity = 0.05, dev_equity = 0.03, em_equity = 0.03,
                   euro_govt = 0.05, us_treasuries = 0.04, euro_corp = 0.05)
is_equity <- as.numeric(names(saa_weight) %in% equity)

# optimal TAA weights for a given risk aversion
taa_weights <- function(risk_aversion_taa) {
  n_assets <- length(saa_weight)
  
  # each column of constraint_matrix is one constraint: constraint_matrix' w >= lower_limit
  constraint_matrix <- cbind(diag(n_assets), -diag(n_assets),   
                             is_equity, -is_equity,             
                             1, -1)                             
  lower_limit <- c(saa_weight - tactical_band, -(saa_weight + tactical_band),
                   0.45 - 0.08, -(0.45 + 0.08),
                   0.95, -1)
  solution <- quadprog::solve.QP(Dmat = risk_aversion_taa * sigma_full, dvec = mu_bar,
                                 Amat = constraint_matrix, bvec = lower_limit)$solution
  setNames(solution, names(saa_weight))
}

# start at the calibrated risk aversion and raise it by 5% steps until the volatility limit holds
risk_aversion_taa <- risk_aversion
w_taa <- taa_weights(risk_aversion_taa)
while (portfolio_vol(w_taa, sigma_full) > 0.09) {
  risk_aversion_taa <- risk_aversion_taa * 1.05
  w_taa <- taa_weights(risk_aversion_taa)
}

# portfolio statistics of the SAA and the TAA under the posterior returns
portfolio_stats <- function(w) {
  c(expected = sum(w * mu_bar), vol = portfolio_vol(w, sigma_full), vol_ewma = portfolio_vol(w, sigma_ewma),
    equity = sum(w[equity]), cash = max(0, 1 - sum(w)))      # max(): rounding noise of the optimizer
}
stats_saa <- portfolio_stats(saa_weight)
stats_taa <- portfolio_stats(w_taa)

at_bound <- ifelse(abs(w_taa - (saa_weight - tactical_band)) < 1e-6, "lower",
                   ifelse(abs(w_taa - (saa_weight + tactical_band)) < 1e-6, "upper", ""))
taa_table <- data.frame(
  asset     = asset_names,
  saa       = pct(saa_weight, 0),
  band      = paste0(pct(saa_weight - tactical_band, 0), " to ", pct(saa_weight + tactical_band, 0)),
  taa       = pct(w_taa, 1),
  deviation = sprintf("%+.1f pp", 100 * (w_taa - saa_weight)),
  bound     = at_bound,
  mu_bar    = pct(mu_bar, 2))
taa_table <- rbind(taa_table,
  data.frame(asset = c("Total equity", "Cash"), saa = pct(c(stats_saa["equity"], stats_saa["cash"]), 0),
             band = c("37% to 53%", "0% to 5%"), taa = pct(c(stats_taa["equity"], stats_taa["cash"]), 1),
             deviation = sprintf("%+.1f pp", 100 * c(stats_taa["equity"] - stats_saa["equity"], stats_taa["cash"] - stats_saa["cash"])),
             bound = c(ifelse(abs(stats_taa["equity"] - 0.37) < 1e-6, "lower", ""), ""), mu_bar = c("", "0.00%")))
show_table(taa_table, "Tactical asset allocation for Oct 2026 to Sep 2027")

taa_stats <- data.frame(
  item = c("Expected excess return (posterior)", "Volatility (full-sample Sigma)", "Volatility (EWMA Sigma)",
           "Sharpe ratio", "Utility w'mu_bar - (A/2) w'Sigma w"),
  saa  = c(pct(stats_saa["expected"], 2), pct(stats_saa["vol"], 2), pct(stats_saa["vol_ewma"], 2),
           sprintf("%.2f", stats_saa["expected"] / stats_saa["vol"]),
           pct(stats_saa["expected"] - risk_aversion / 2 * stats_saa["vol"]^2, 2)),
  taa  = c(pct(stats_taa["expected"], 2), pct(stats_taa["vol"], 2), pct(stats_taa["vol_ewma"], 2),
           sprintf("%.2f", stats_taa["expected"] / stats_taa["vol"]),
           pct(stats_taa["expected"] - risk_aversion / 2 * stats_taa["vol"]^2, 2)))
show_table(taa_stats, "Expected excess return and risk of the SAA and the TAA")

taa_plot <- data.frame(series = factor(short_names[names(asset_names)], levels = rev(short_names[names(asset_names)])),
                       low = saa_weight - tactical_band, high = saa_weight + tactical_band, saa = saa_weight, taa = w_taa)

fig_taa <- ggplot(taa_plot, aes(y = series)) +
  geom_linerange(aes(xmin = low, xmax = high), colour = "#d6d5d0", linewidth = 6) +
  geom_point(aes(x = saa, shape = "SAA"), colour = "#2a78d6", size = 3) +
  geom_point(aes(x = taa, shape = "TAA"), colour = "#eb6834", size = 3) +
  scale_shape_manual(values = c("SAA" = 124, "TAA" = 16), name = NULL) +
  scale_x_continuous(labels = function(x) pct(x, 0)) +
  guides(shape = guide_legend(override.aes = list(colour = c("#2a78d6", "#eb6834")))) +
  labs(x = "Portfolio weight", y = NULL) +
  theme_case
print(fig_taa)

# The volatility limit does not bind: the TAA has 6.11% (6.65% even with the EWMA covariance), so A
# stays at 5.17.

# The TAA de-risks. With the posterior returns, equity risk no longer pays at the board's risk
# aversion, so the optimizer cuts total equity to its 37% floor. Most of the cut is in US equity
# (-4.9 pp), which carries the most negative view and ends just above its lower band, and the rest
# in euro area (-3.1 pp) and emerging markets equity (-3.0 pp). Developed equity goes to its upper
# band, since it has the lowest volatility of the equity classes and is the cheapest way to hold the
# minimum equity share. The money goes to euro government bonds and US Treasuries, both at their
# upper bands, while euro corporates give up 1.0 pp because the credit view lowered their advantage.
# The TAA is fully invested.

# The TAA expects 1.81% against 1.99% for the SAA, at 6.11% instead of 7.23% volatility. Giving up
# 0.19 pp of return for 1.1 pp less volatility raises both the Sharpe ratio (0.30 vs. 0.28) and the
# board's utility. Most weights sit at a band, so the allocation is driven by the direction of the
# views more than by their exact size.


# Task 6: Currency policy -------------------------------------------------------------------------

# Whether a currency exposure is risky depends on how an asset moves with the exchange rate, 
# not on its quotation currency, so we regress each unhedged euro excess return on x_t. 
# Hedging a fraction h changes the variance to Var(r) + Var(x) (h^2 - 2 h beta), which is smallest at h = beta,
# so the slope is the variance-minimizing hedge ratio. The same formula shows that a hedge adds risk once h > 2 beta. 
# The months do not overlap, so OLS standard errors are appropriate.

# slope of each unhedged euro excess return on the currency excess return x_t
hedge_beta <- t(sapply(names(asset_names), function(a) {
  fit <- lm(excess_unhedged[[a]] ~ currency_excess)
  c(beta = unname(coef(fit)[2]), se = unname(sqrt(vcov(fit)[2, 2])), r2 = summary(fit)$r.squared)
}))

hedge_table <- data.frame(
  asset  = short_names[names(asset_names)],
  saa    = pct(saa_weight, 0),
  policy = sprintf("%.2f", hedge_ratio),
  beta   = sprintf("%.2f", hedge_beta[, "beta"]),
  t      = sprintf("%.2f", hedge_beta[, "beta"] / hedge_beta[, "se"]),
  r2     = pct(hedge_beta[, "r2"]),
  position = pct(-saa_weight * hedge_beta[, "beta"], 1))
show_table(hedge_table, "Regressions of the unhedged euro excess returns on x_t, Jan 1999 to Sep 2026")

# US Treasuries (0.90) are close to the policy's full hedge: their euro return is almost entirely
# bond return plus dollar. US equity (0.52) is very close to the half hedge, and well below 1
# because US stocks tend to fall when the dollar rises, which partly offsets the exposure. Developed
# (0.21) and emerging markets equity (0.03) are far below 0.5. These indices are quoted in dollars,
# but the stocks are priced in yen, pounds and so on, so when the dollar rises their USD returns
# fall. Their real exposure is to those currencies, which only partly move with the dollar, and the
# policy's 0.5 exceeds 2 beta for both, which is why their volatility rose in task 1. Euro assets
# have small, mostly negative betas, euro area equity in particular (-0.25).

# For the whole SAA, the variance-minimizing position from an all-unhedged start is h* = -Cov(r_SAA,
# x) / Var(x) = -sum(w_i beta_i).

# unhedged excess return of the SAA
saa_unhedged <- drop(as.matrix(excess_unhedged[, names(asset_names)]) %*% saa_weight)

# variance-minimizing USD position, measured from the all-unhedged SAA
usd_position_opt <- -cov(saa_unhedged, currency_excess) / var(currency_excess)

# annualized volatility of the SAA for a given USD position h
saa_vol_with_usd <- function(h) sd(saa_unhedged + h * currency_excess) * sqrt(12)

usd_positions <- c(unhedged = 0, policy = -usd_hedge_saa, optimal = usd_position_opt)
usd_vols      <- sapply(usd_positions, saa_vol_with_usd)

usd_table <- data.frame(
  item = c("All unhedged", "Current policy", "Variance-minimizing"),
  position = pct(usd_positions, 1),
  vol = pct(usd_vols, 2))
show_table(usd_table, "Volatility of the SAA for different USD positions, Jan 1999 to Sep 2026")

position_grid <- seq(-0.5, 0.1, by = 0.005)
curve <- data.frame(position = position_grid, vol = sapply(position_grid, saa_vol_with_usd))
marks <- data.frame(position = usd_positions, vol = usd_vols,
                    label = c("Unhedged", "Current policy", "Minimum"),
                    label_hjust = c(1.06, 1.06, 0.5), label_vjust = c(-0.7, 1.5, 1.8))   # labels placed off the curve

fig_usd_position <- ggplot(curve, aes(x = position, y = vol)) +
  geom_line(colour = "#2a78d6", linewidth = 0.7) +
  geom_point(data = marks, colour = "#eb6834", size = 2.5) +
  geom_text(data = marks, aes(label = paste0(label, ": ", pct(vol, 2)), hjust = label_hjust, vjust = label_vjust), size = 3, colour = "#0b0b0b") +
  scale_x_continuous(labels = function(x) pct(x, 0)) +
  scale_y_continuous(breaks = seq(0.072, 0.078, by = 0.002), labels = function(x) pct(x, 1), expand = expansion(mult = c(0.12, 0.05))) +
  labs(x = "USD position (share of assets)", y = "SAA volatility p.a.") +
  theme_case
print(fig_usd_position)

# h* is -14.3% of assets, only a little more than half the policy's short of 25.5%, and most of it
# comes from US Treasuries and US equity. But the SAA's volatility is 7.27% unhedged, 7.23% under
# the policy and 7.15% at h*: a range of only 0.12 pp, because the curve is flat around its minimum.
# For this SAA the currency hedge is a minor risk decision, so the choice of policy should rest on
# cost and robustness, which we take up in task 7(d).


# Task 7: Recommendations to the board ------------------------------------------------------------


## (a) Is euro inflation plus 2 percentage points attainable? -------------------------------------

# The target is market-implied inflation (5y5y swap, 2.13%) plus 2 pp, a nominal 4.13% per year. The
# SAA earns the euro cash rate plus its premium, and we assume Euribor stays at today's 2.62%. The
# 2.7 pp premium applies to today's 45% equity and falls along the glide path as equity goes to 25%,
# which we compute with the implied premia of the equity and bond blocks from task 3.
# we decided to use the 5y5y swap as a proxy for anchored long-run inflation. 

# (a) is euro inflation + 2 pp attainable with the SAA over the remaining nine years?
horizon       <- 9
cash_now      <- panel$euribor3m[nrow(panel)] / 100   # expected euro cash rate: random walk from today's Euribor
inflation_exp <- panel$infl_5y5y[nrow(panel)] / 100   # market-implied euro inflation (5y5y swap)
target        <- inflation_exp + 0.02

# implied premia of the equity and bond blocks of the SAA (equilibrium of task 3)
bonds          <- setdiff(names(saa_weight), equity)
equity_premium <- sum(saa_weight[equity] * pi_eq[equity]) / sum(saa_weight[equity])
bond_premium   <- sum(saa_weight[bonds] * pi_eq[bonds]) / sum(saa_weight[bonds])

# glide path: equity share falls linearly from 45% to 25% over the nine years
equity_path  <- seq(0.45, 0.25, length.out = horizon)
premium_path <- equity_path * equity_premium + (1 - equity_path) * bond_premium

# expected compound return: cash plus average premium minus volatility drag (today's SAA volatility, conservative)
expected_compound <- cash_now + mean(premium_path) - 0.5 * saa_vol_full^2
shortfall_sd      <- saa_vol_full / sqrt(horizon)          # volatility of the nine-year average return
prob_target       <- pnorm((expected_compound - target) / shortfall_sd)

# same without the glide path (premium stays at 2.7 pp) as an upper bound
expected_no_glide <- cash_now + saa_premium - 0.5 * saa_vol_full^2
prob_no_glide     <- pnorm((expected_no_glide - target) / shortfall_sd)

# The premium averages 2.24% over the nine years, and after a volatility drag of 0.26% the expected
# compound return is 4.59% per year. So the objective is attainable in expectation, but with a thin
# margin: the nine-year average return has a standard deviation of about 2.41%, so the probability
# of reaching the target is only about 58% (65% without the glide path). The target is in real terms
# while 55% of the SAA is in nominal bonds, which lose when inflation surprises upwards, as in 2022.
# We recommend presenting it to savers as an ambition, not as an objective the product can be
# expected to meet.


## (b) Do the SAA and the pace of de-risking fit savers nine years from retirement? ---------------

# A loss hurts more the closer it comes to retirement: savings are largest then, there is little
# time to recover, and from 2035 savers withdraw or buy annuities, which locks the loss in. So we
# compare today's SAA with the portfolio at the target date.

# (b) risk of today's SAA and of the portfolio at the target date (25% equity, same mix within equities and bonds)
w_target <- c(saa_weight[equity] / sum(saa_weight[equity]) * 0.25,
              saa_weight[bonds]  / sum(saa_weight[bonds])  * 0.75)[names(saa_weight)]

year <- format(excess_policy$date, "%Y")
risk_profile <- function(w) {
  r   <- drop(excess_matrix %*% w)              # monthly euro excess return under the currency policy
  cum <- cumprod(1 + r)
  c(premium = sum(w * pi_eq),
    vol     = portfolio_vol(w, sigma_full),
    max_dd  = min(cum / cummax(cum) - 1),
    y2008   = prod(1 + r[year == "2008"]) - 1,
    y2022   = prod(1 + r[year == "2022"]) - 1)
}
risk_now    <- risk_profile(saa_weight)
risk_target <- risk_profile(w_target)
govt_2008   <- prod(1 + excess_policy$euro_govt[year == "2008"]) - 1
govt_2022   <- prod(1 + excess_policy$euro_govt[year == "2022"]) - 1

risk_table <- data.frame(
  item = c("Expected excess return (equilibrium)", "Volatility", "Maximum drawdown since 1999",
           "Excess return in 2008", "Excess return in 2022"),
  now    = pct(risk_now, 1),
  target = pct(risk_target, 1))
show_table(risk_table, "Risk of the SAA today and at the end of the glide path, Jan 1999 to Sep 2026")
print(round(c(govt_2008 = govt_2008, govt_2022 = govt_2022), 3))

# Moving to 25% equity lowers volatility from 7.2% to 5.0%, the largest drawdown from 30.0% to 18.2%
# and the 2008 loss from 22.3% to 12.7%. But the protection depends on what bonds do in a crisis. In
# 2008 euro government bonds earned 4.3% above Euribor, while in 2022 they lost 18.6% together with
# equities, and the 25% equity portfolio lost as much as today's SAA (15.9% vs. 15.8%). De-risking
# into nominal government bonds protects against recessions, not against inflation shocks. They
# still suit savers who buy annuities, since annuities get cheaper when yields rise, but there is no
# such offset for savers who withdraw their capital.

# We recommend keeping the pace of de-risking, since faster de-risking would lower expected returns
# further and part (a) is already only narrowly attainable. Within the bond allocation, the product
# should not rely on euro government bonds alone as the safe asset. We recommend either inflation adjusted
# bonds or short duration bonds.


## (c) Should hedge funds replace part of the equity allocation? ----------------------------------

# The board wants to move 10 pp from equities into hedge funds to reduce risk. We use the Credit
# Suisse Hedge Fund Index (net of fees, fully hedged into euro) as a proxy, and compare the proposal
# with the simplest alternative: the same 10 pp into the existing bonds.

# (c) hedge fund index (Credit Suisse, net of fees): fully hedged euro excess return, months of task 1
hf_level <- panel$hf_hedgnav_usd[match(core$date, panel$date)]
hf_usd   <- simple_return(hf_level)
hf_r_eur <- (1 + hf_usd) * (1 + usd_return) - 1 - currency_excess - cash_return   # unhedged, minus full hedge, minus euro cash
common   <- !is.na(hf_r_eur)

# monthly excess return of the SAA equity block (same mix as in the SAA)
equity_block <- drop(excess_matrix[, equity] %*% (saa_weight[equity] / sum(saa_weight[equity])))

# equity beta: contemporaneous, and summed with last month's equity return (stale pricing)
ok      <- which(common)[-1]
beta_hf <- coef(lm(hf_r_eur[ok] ~ equity_block[ok] + equity_block[ok - 1]))
hf_beta_now <- unname(beta_hf[2])
hf_beta_sum <- unname(beta_hf[2] + beta_hf[3])

hf_stats <- c(mean  = 12 * mean(hf_r_eur[common]), vol = vol_annual(hf_r_eur[common]),
              y2008 = prod(1 + hf_r_eur[common & year == "2008"]) - 1,
              y2022 = prod(1 + hf_r_eur[common & year == "2022"]) - 1)

# the board's proposal: 10 pp from equities into hedge funds, against the same 10 pp into the existing bonds
w_eq_cut <- saa_weight
w_eq_cut[equity] <- saa_weight[equity] * (0.35 / 0.45)   # equity from 45% to 35%, same mix within equities
w_alt <- w_eq_cut
w_alt[bonds] <- saa_weight[bonds] * (0.65 / 0.55)        # bonds from 55% to 65%, same mix within bonds

path_stats <- function(r) {
  r <- r[common]
  cum <- cumprod(1 + r)
  c(vol = vol_annual(r), max_dd = min(cum / cummax(cum) - 1),
    y2008 = prod(1 + r[year[common] == "2008"]) - 1, y2022 = prod(1 + r[year[common] == "2022"]) - 1)
}
proposal <- sapply(list(saa         = drop(excess_matrix %*% saa_weight),
                        hedge_funds = drop(excess_matrix %*% w_eq_cut) + 0.10 * hf_r_eur,
                        bonds       = drop(excess_matrix %*% w_alt)), path_stats)

# return the hedge fund index would need to earn to be worth holding at the board's risk aversion
hf_required <- risk_aversion * 12 * cov(hf_r_eur[common], drop(excess_matrix %*% saa_weight)[common])
hf_period   <- format(range(excess_policy$date[common]), "%b %Y")

hf_table <- data.frame(
  item   = c("Volatility", "Maximum drawdown", "Excess return in 2008", "Excess return in 2022"),
  saa    = pct(proposal[, "saa"], 1),
  hedge  = pct(proposal[, "hedge_funds"], 1),
  bonds  = pct(proposal[, "bonds"], 1))
show_table(hf_table, paste("The board's proposal against the same de-risking with bonds,", hf_period[1], "to", hf_period[2]))
print(round(c(hf_stats, hf_beta_now = hf_beta_now, hf_beta_sum = hf_beta_sum, hf_required = hf_required), 4))

# We recommend against it. Hedge funds failed when it mattered most: in 2008 the index lost 22.4%
# above Euribor, as much as the SAA itself (22.3%). Its equity beta of 0.25 rises to 0.31 once last
# month's equity return is included, a sign that reported returns are smoothed. Moving the 10 pp
# into bonds lowers risk more: volatility 6.04% against 6.21%, maximum drawdown 24.3% against 26.2%,
# and a 2008 loss of 17.6% against 19.8%. Hedge funds did better in 2022 (1.1% loss), when stocks
# and bonds fell together, which is their main argument.

# The index earned 3.8% per year above Euribor, more than the 1.35% it would need to be worth
# holding at the board's risk aversion (A times its covariance with the SAA). But index returns are
# biased upwards, since funds that close or stop reporting drop out, and the board's aim is risk,
# not return. Fees, lock-ups and possible suspended redemptions also fit poorly with a retail
# pension product that has outflows from 2035. Given part (b) we see no need to de-risk faster than
# the glide path, and if the board wants to, the existing bonds do it more cheaply.


## (d) Should the currency policy be changed? -----------------------------------------------------

# Task 6 showed that the policy fits US Treasuries and US equity but not developed and emerging
# markets equity. So we test a small change: keep those two hedges and stop dollar-hedging the other
# foreign equities. With the dollar following a random walk, a short dollar position loses the rate
# differential each year, which gives the expected cost of each option. As a stability check we
# repeat the hedge regressions on the last five years.

# (d) proposed policy: hedge US Treasuries fully and US equity by half, stop hedging the other foreign equities
hedge_ratio_new <- hedge_ratio
hedge_ratio_new[c("dev_equity", "em_equity")] <- 0
usd_hedge_new <- sum(saa_weight * hedge_ratio_new)

# USD positions measured from the all-unhedged SAA (negative = short USD)
policy_options <- c(unhedged = 0, current = -usd_hedge_saa, proposed = -usd_hedge_new, min_variance = usd_position_opt)
policy_vol     <- sapply(policy_options, saa_vol_with_usd)
policy_cost    <- abs(policy_options) * expected_x      # expected carry lost per year by the short USD position

# stability check: hedge ratios over the last five years
last_60 <- tail(seq_along(currency_excess), 60)
beta_5y <- sapply(usd_assets, function(a) unname(coef(lm(excess_unhedged[[a]][last_60] ~ currency_excess[last_60]))[2]))

policy_table <- data.frame(
  item     = c("All unhedged", "Current policy", "Proposed policy", "Variance-minimizing"),
  position = pct(policy_options, 1),
  vol      = pct(policy_vol, 2),
  cost     = pct(policy_cost, 2))
show_table(policy_table, "Currency policy options for the SAA")
print(round(beta_5y, 2))

# We recommend the small change. The currency risk of developed and emerging markets equity is in
# yen, pounds and emerging market currencies, not in the dollar, so a dollar hedge adds risk. Over
# the last five years their hedge ratios were even negative (-0.28 and -0.37). US equity (0.25) and
# US Treasuries (0.57) were also lower than over the full sample, but they remain positive, so we
# keep those two hedges. US equity's half hedge sits right at 2 beta in that period, so it removed
# almost no risk then: this is the part of the policy to watch.

# The change reduces the dollar short from 25.5% to 19.0% of assets, closer to the variance-
# minimizing 14.3%, and lowers volatility from 7.23% to 7.16%. The risk difference between all
# options is small, so the main gain is cost: the short dollar position loses 1.73% per year on the
# differential, and the change saves 0.11% of expected return per year, which helps with part (a).
# This saving should disappear over the long run if uncovered interest parity holds, so it is a
# benefit of the change rather than the reason for it. We do not recommend hedging exactly at the
# variance-minimizing ratios: they are estimated with error and change over time, while a rule based
# on which currency an asset is actually exposed to is more robust and easier to explain to savers.
