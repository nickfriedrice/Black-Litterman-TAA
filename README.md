# Black-Litterman Tactical Asset Allocation

**A quantitative tactical overlay for a euro-based multi-asset target-date fund.** The model combines equilibrium returns, return forecasts from predictive regressions and a Black-Litterman update to tilt a seven-asset strategic allocation within risk limits.

![R](https://img.shields.io/badge/R-4.5-276DC3?logo=r&logoColor=white)
![Method](https://img.shields.io/badge/method-Black--Litterman-0b3d66)
![Data](https://img.shields.io/badge/data-monthly%201999--2026-555)

---

## Overview

The portfolio is a euro-based target-date fund ("Target 2035") with a strategic asset allocation (SAA) across seven asset classes:

| Equities | Fixed income |
|---|---|
| US · Euro area · Developed ex US ex euro area · Emerging markets | Euro government · US Treasuries · Euro investment-grade corporate |

The mandate:
- targets a long-run premium of **2.7 pp over cash**
- caps portfolio volatility at **9%**
- allows tactical deviations within fixed bands around the SAA.

The project answers four questions:
1. What returns does the current allocation imply?
2. Where do forecasts disagree with them?
3. How should the portfolio be tilted as a result?
4. Is the currency hedge set efficiently?

## Key results

| | SAA | Tactical allocation |
|---|---:|---:|
| Expected excess return | 1.99% | 1.81% |
| Volatility | 7.23% | 6.11% |
| Sharpe ratio | 0.28 | 0.30 |

- **Equities.** Forecasts cut expected equity returns by about 2 pp. The tactical portfolio holds equities at their minimum weight and moves government bonds to their upper bands.
- **Risk.** The 9% volatility limit does not bind.
- **Currency.** Developed and emerging market equities partly hedge the fund's dollar exposure. Unhedging them lowers the hedge ratio from −25.5% to −19%, which reduces volatility and saves about 0.11% a year in hedging costs.
- **Target.** The SAA has a ~58% probability of meeting its 2035 return target.

## Methodology

**1. Return construction.** Monthly euro excess returns over 3M Euribor, net of the fund's currency hedge.
- **Developed ex US ex euro area equity:** no standard index exists, so we back it out from MSCI World ex USA and MSCI EMU using lagged market-cap weights.
- **USD cash:** 3M LIBOR, continued with Term SOFR plus the ISDA fallback spread.

**2. Risk model.**
- **Baseline:** a full-sample covariance matrix spanning several regimes and crises.
- **Robustness checks:** EWMA (λ = 0.97) and a five-year window.
- **Not extrapolated:** recent shifts in the stock-bond correlation.

**3. Equilibrium returns.** Reverse optimisation, $\Pi = A\Sigma w_{SAA}$, with risk aversion $A$ calibrated to the 2.7 pp premium. Each asset's implied Sharpe ratio is proportional to its correlation with the portfolio.

**4. Views.** Predictive regressions of 12-month-ahead excess returns, with Newey-West standard errors:

| View | Type | Predictor |
|---|---|---|
| US equity | Absolute | CAPE yield and dividend yield (long history from 1926) |
| Euro government bonds | Absolute | Cochrane-Piazzesi factor on German zero yields |
| US Treasuries | Absolute | 10y minus 3M term spread |
| Euro corporates vs. government bonds | Relative | Option-adjusted credit spread |

Each view's uncertainty is the forecast variance. It is scaled up when the model failed to beat the historical mean in an expanding-window out-of-sample test.

**5. Black-Litterman update.**

$$\bar\mu = \Pi + \tau\Sigma P'\left(P\tau\Sigma P' + \Omega\right)^{-1}(v - P\Pi)$$

The posterior returns feed a constrained mean-variance optimisation with the mandate's bands, equity limits and volatility cap.

**6. Currency overlay.** Regressing each asset's return on the currency return gives its dollar exposure. We compare the minimum-variance hedge ratio with the current policy hedge.

**7. Long-horizon analysis.**
- the probability of reaching the 2035 target
- de-risking along the glide path
- the case against adding hedge funds.

## Repository

| File | Contents |
|---|---|
| [`slides.pdf`](slides.pdf) | Presentation of the approach and results |
| [`report.pdf`](report.pdf) | Full technical write-up with all tables and charts |
| [`code.R`](code.R) | Complete, commented analysis |
| [`data.RData`](data.RData) | Monthly input data |

## Getting started

Place `code.R` and `data.RData` in the same folder and run:

```r
source("code.R")
```

Missing CRAN packages (`ggplot2`, `scales`, `sandwich`, `quadprog`) are installed automatically. The script prints every table and chart and writes no files.

## Data

`data.RData` contains four objects:
- `panel`: monthly series, Nov 1996 to Sep 2026
- `core`: the main return and rate series
- `long_history`: long-run US equity and German yield data
- `series_info`: the description and source of each series.

| Source | Series |
|---|---|
| Bloomberg | MSCI equity and Bloomberg bond indices, Euribor, USD rates, EUR/USD, yields, spreads, durations, inflation |
| Robert Shiller | S&P 500 prices, dividends, earnings and CAPE from 1871 |
| Deutsche Bundesbank | German zero-coupon yields, 1 to 5 years |
| Kenneth French Data Library | 1-month T-bill return |

The data is provided only to reproduce the results. Bloomberg data remains subject to Bloomberg's terms of use.

## Authors

Nicholas Friedrich · Boldizsár Boross
