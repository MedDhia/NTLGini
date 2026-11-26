# Does nighttime-light spatial inequality predict political outcomes?

A robustness exercise linking `ntlgini` country-year Gini measures (1992–2024) to
cross-national political outcomes. All the outcome data come from political-science
R packages:

| Source (R package) | Outcomes used |
|---|---|
| **V-Dem v16** (`vdemdata`) | Urban–rural exclusion index and its components (power, public-service access, civil liberties); polyarchy; liberal democracy; political violence; mass mobilization; state authority over territory; corruption; rule of law; fiscal capacity; Powell & Thyne coup attempts |
| **`democracyData`** | Polity5 `polity2`; Freedom House (reversed total); Extended UDS; LIED lexical index |
| **`peacesciencer`** | UCDP/PRIO intrastate armed-conflict incidence and onset (1992–2024); CREG ethnic fractionalization; ruggedness (both used as controls) |
| **`WDI`** | Controls: log GDP pc (PPP), log population, % urban, resource rents |

**Panel:** 5,682 country-years, 173 V-Dem polities. The main specification uses
4,324 country-years from 148 countries.

## Bottom line

**Spatial inequality, as measured here, does not robustly predict political
outcomes once you account for how developed a country is.**

1. **Bivariate associations are strong and point the way theory expects.**
 also needs WDI electrification (cached by first run) More spatially unequal countries have more urban–rural political exclusion,
 also needs WDI electrification (cached by first run) less democracy, more conflict and political violence, more corruption, and
 also needs WDI electrification (cached by first run) weaker rule of law and fiscal capacity. 52 of 76 predictor × outcome pairs
 also needs WDI electrification (cached by first run) are significant (49 after Benjamini–Hochberg correction). The pattern holds
 also needs WDI electrification (cached by first run) on the main-spec sample too (47/76), so sample composition doesn't explain it.
2. **These associations go away with standard controls.** With controls plus
 also needs WDI electrification (cached by first run) year and region fixed effects, 3 of 76 pairs are p < .05 and **0 of 76 survive
 also needs WDI electrification (cached by first run) FDR correction**. Under the main specification **no pair counts as robust**
 also needs WDI electrification (cached by first run) (robust here means BH p < .05 in the main spec and same-sign p < .05 in at
 also needs WDI electrification (cached by first run) least 75% of the 10–12 non-bivariate specifications).
3. **The confounder is development.** Log GDP per capita on its own removes about
 also needs WDI electrification (cached by first run) 75% of the bivariate coefficient. The light Ginis correlate at −0.55 to −0.76
 also needs WDI electrification (cached by first run) with log GDP pc and mean radiance: poor countries have many unlit cells, and
 also needs WDI electrification (cached by first run) that mechanically pushes the Gini toward 1. Total Gini has a median of 0.96
 also needs WDI electrification (cached by first run) at ADM1.
4. **Adding the inequality measures does not improve prediction.** With 10×
 also needs WDI electrification (cached by first run) repeated, country-blocked 10-fold CV, adding all four measures to the control
 also needs WDI electrification (cached by first run) model *lowers* out-of-sample R² for 15 of 19 outcomes (median −0.018). The
 also needs WDI electrification (cached by first run) gains are tiny: conflict-incidence AUC +0.013, fiscal capacity R² +0.009, and
 also needs WDI electrification (cached by first run) territorial authority and exclusion each under +0.015.
5. **Within countries, nothing systematic shows up.** Country + year FE estimates
 also needs WDI electrification (cached by first run) are near zero and mixed in sign. Within-region (ADM1) Gini is the exception:
 also needs WDI electrification (cached by first run) it is negatively associated with every regime measure within countries
 also needs WDI electrification (cached by first run) (e.g., polyarchy −0.26 SD, p < .001). That result is not significant in the
 also needs WDI electrification (cached by first run) main pooled spec, though, and only 1 of 76 within-FE tests survives FDR.

### One lead worth pursuing: between-region Gini at **ADM2**

Measured across ADM2 rather than ADM1 units, between-region inequality is
negatively associated with **all six democracy measures**, even with the full
controls (≈ −0.18 SD per SD, BH p < .05 for all six). The follow-up checks
(`04_adm2_followup.R`) show:

- **It comes from the measure, not the sample.** ADM1 between-Gini on the same
 also needs WDI electrification (cached by first run)140 countries is null (≈ −0.05).
- It gets **stronger** when ADM1 between-Gini and total Gini are also controlled
 also needs WDI electrification (cached by first run)(≈ −0.30 to −0.41). The relevant variation is the inequality *across districts
 also needs WDI electrification (cached by first run)within provinces*.
- It holds up in the country-means cross-section (n = 140, ≈ −0.28), without
 also needs WDI electrification (cached by first run)region FE, when each of the six world regions is dropped in turn, and in a
 also needs WDI electrification (cached by first run)country jackknife (range −0.23 to −0.35).
- **It is zero within countries** (country + year FE ≈ 0).

So this is a persistent **cross-sectional** difference, not a dynamic predictor.
There is also a plausible artefact to rule out. ADM2 boundaries are political
objects: autocracies tend to proliferate and redraw districts, and GADM ADM2
granularity varies across countries. The unit-count control doesn't change the
result, but unit-*size* heterogeneity hasn't been addressed. A natural next step
is to recompute between-unit Gini on a regular grid or on population-equal
zones, which the subunit Parquet tables would allow.

### Other things to note

- **Total Gini → state authority over territory** is *positive* (+0.38 SD,
 also needs WDI electrification (cached by first run)BH p = .07, 7/10 specs), which is the opposite of the "harmful" hypothesis.
 also needs WDI electrification (cached by first run)The most likely cause is large, mostly empty but state-controlled territories
 also needs WDI electrification (cached by first run)such as deserts and the Gulf.
- **Within-region Gini → lower fiscal capacity** (−0.26 SD, BH p = .07, 7/12
 also needs WDI electrification (cached by first run)specs) is the closest any ADM1 measure comes to robustness.
- **There is a visible DMSP→VIIRS seam around 2012.** Germany's total Gini, for
 also needs WDI electrification (cached by first run)example, jumps from 0.72 to 0.84. Results are similar in DMSP-only and
 also needs WDI electrification (cached by first run)VIIRS-only subsamples and with country×era FE. Still, users of the package's
 also needs WDI electrification (cached by first run)within-country time series should be aware of it.

### Fixed-effects battery (`06_fixed_effects.R`)

This section asks whether within-country *changes* in spatial inequality track
political change. It runs 5 predictors (the four ADM1 measures plus ADM2
between-Gini) × 19 outcomes under 9 FE designs:

- F1: country + year FE, no controls
- F2: F1 plus time-varying controls (log GDP pc, log population, % urban, log mean radiance, resource rents)
- F3: region×year FE
- F4: country-specific linear trends
- F5: country×era FE
- F6 / F7: predictor lagged 3 / 5 years
- F8: placebo using inequality 3 years *in the future*
- F9: 5-year-period panel

| | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 placebo | F9 |
|---|---|---|---|---|---|---|---|---|---|
| p < .05 (of 95) | 13 | 20 | 19 | 12 | 15 | 15 | 11 | 4 | 10 |
| BH-significant | 0 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 0 |

- **There is little within-country variation to use.** After country and year
 also needs WDI electrification (cached by first run)FE, only 1.4% (within-region Gini) to 9% (ADM2 between Gini) of each
 also needs WDI electrification (cached by first run)measure's variance is left. A large share of that residual likely reflects
 also needs WDI electrification (cached by first run)sensor noise and the 2012 seam rather than real redistribution of activity.
- **There is still enough power for moderate effects.** The median F2 SE is
 also needs WDI electrification (cached by first run)0.045 outcome-SD per pooled SD of inequality, so the minimum detectable effect
 also needs WDI electrification (cached by first run)at 80% power is ≈ 0.13. The nulls are not just noise.
- **The significant results don't point one way.** Of the 20 F2 hits, 8 are in
 also needs WDI electrification (cached by first run)the "inequality is harmful" direction and 12 go the other way.
- **The only BH-significant within-country result fails the identification
 also needs WDI electrification (cached by first run)checks.** Within-region Gini → lower polyarchy (−0.26 SD, BH p = .04) is
 also needs WDI electrification (cached by first run)stable across F1–F3, F5 and F9. But it vanishes with country trends
 also needs WDI electrification (cached by first run)(F4: −0.07), and the *future* value of inequality predicts current democracy
 also needs WDI electrification (cached by first run)about as well (F8: −0.21, p = .03). That pattern means slow co-moving trends
 also needs WDI electrification (cached by first run)(democratic erosion coinciding with secular lighting trends), not inequality
 also needs WDI electrification (cached by first run)preceding political change.
- **The coup associations fail the placebo too.** Future between-region
 also needs WDI electrification (cached by first run)inequality "predicts" today's coups (p ≈ .001), which points to reverse
 also needs WDI electrification (cached by first run)causation: political shocks change the light distribution.
- **No predictor × outcome pair is robust within countries.** Robust here means
 also needs WDI electrification (cached by first run)BH p < .05 in F2, same-sign p < .05 in at least 6 of the 7 non-placebo
 also needs WDI electrification (cached by first run)alternatives, and a null placebo.

### Reverse-direction test (`07_reverse_direction.R`)

The question here is whether regime change reshapes how light is distributed.
The prior is the democracy-and-electrification argument (Brown & Mobarak 2009;
Min 2015).

**A. Cross-lagged panel.** Both directions are estimated in the same FE models:
4 inequality measures × 5 democracy measures × horizons of 1, 3 and 5 years.
Effects are in within-country SD units, using the residual SDs after country
and year FE, so the two directions are comparable.

| Polyarchy × within-region Gini, k = 1 | Democracy → future inequality | Inequality → future democracy |
|---|---|---|
| TWFE + controls | −0.093 (p = .002) | −0.090 (p = .001) |
| + lagged dependent variable | −0.034 (p = .04) | −0.007 (p = .47) |
| + lagged DV + country trends | −0.008 (p = .57) | −0.018 (p = .21) |

- In plain TWFE the association is the same size in both directions. That is
 also needs WDI electrification (cached by first run)what a shared trend produces, not one variable driving the other.
- With a lagged dependent variable, a small democracy → inequality effect
 also needs WDI electrification (cached by first run)remains at one year only. It is no longer significant at 3 or 5 years.
- With country trends, both directions are null.
- Across all 20 measure pairs, the share of p < .05 results stays near chance
 also needs WDI electrification (cached by first run)in every variant and direction (0–6 of 20), and almost nothing survives BH.

**B. Event studies** (Sun & Abraham 2021 estimator; window −6 to +8; controls
include GDP pc, population, % urban and resource rents). Events are the first
sustained crossing of the Regimes of the World (RoW) democracy threshold in
1995–2021: 45 democratizations against 58 always-autocracies, and 32
breakdowns against 52 always-democracies. LIED transitions are used as a
robustness check (44 events).

- There are no pre-trends: the average t−6 to t−2 deviation has p > .1 for
 also needs WDI electrification (cached by first run)every outcome.
- **No distribution measure moves** after democratization or breakdown. For
 also needs WDI electrification (cached by first run)example, the ATT of RoW democratization on within-region Gini is −0.007
 also needs WDI electrification (cached by first run)(SE 0.017); for LIED it is −0.028 (SE 0.026).
- The design can rule out effects larger than about 0.4 within-country SD of
 also needs WDI electrification (cached by first run)within-region Gini.
- The only near-signal is a *level* effect: mean radiance rises after
 also needs WDI electrification (cached by first run)breakdowns (+0.064 log points, p = .07).

**C. Mechanism.** Polyarchy doesn't predict WDI rural electricity access 3 years
later (−0.24 pp, SE 0.67). Rural access doesn't predict within-region Gini, and
controlling for its change leaves the democracy coefficient unchanged. WDI
access figures are partly modelled and smoothed, so this is a weak test.

**Verdict.** Country-level regime change doesn't reshape the distribution of
light measured by these area-weighted Ginis, in either direction. The
within-country co-movement is best read as country-specific trends. The
electrification mechanism may still operate at a scale these measures can't
see, such as villages in competitive constituencies. Testing that needs
population-weighted or unit-level (ADM2/ADM3) outcomes matched to subnational
electoral data.

![Coefficient plot](output/coefplot.png)

## Specification grid (`02_models.R`)

Predictors (each standardized; one predictor per model): total Gini; ADM1
between-region Gini; ADM1 within-region Gini; ADM1 between share. Continuous
outcomes are standardized. Binary outcomes are linear probability models.
SEs are clustered by country.

| ID | Specification |
|---|---|
| S01 / S01b | Bivariate + year FE (full sample / main-spec sample) |
| **S02** | **Controls + year & region FE (main)**: log GDP pc, log pop, % urban, log mean radiance, resource rents, log land area, log # ADM1 units, ethnic fractionalization, ruggedness |
| S03 | S02 without the mean-radiance control |
| S04 | S02 with predictor lagged 3 years |
| S05 | Outcome at t+3, conditional on outcome at t |
| S06 | Between-country: country means, region FE, HC1 SEs |
| S07 | Country + year FE (time-varying controls) |
| S08 | Country×era + year FE (absorbs the DMSP/VIIRS seam) |
| S09 / S10 | S02 in DMSP era (1992–2012) / VIIRS era (2013–2024) |
| S11 | S02 excluding states < 20,000 km² or with < 5 ADM1 units |
| S12 | S02 with ADM2-level measures (ADM2 unit count control) |
| S13 | ADM2-level measures, country + year FE |

## Reproduce

From the package root (requires `fixest`, `dplyr`, `tidyr`, `countrycode`,
`peacesciencer`, `WDI`, `ggplot2`, and `ntlgini` installed):

```sh
Rscript analysis/spatial-inequality-politics/00_get_data.R also needs WDI electrification (cached by first run)   # downloads V-Dem, democracyData .rda files, WDI
Rscript analysis/spatial-inequality-politics/01_build_panel.R
Rscript analysis/spatial-inequality-politics/02_models.R
Rscript analysis/spatial-inequality-politics/03_diagnostics.R also needs WDI electrification (cached by first run)# attenuation + cross-validation (~2 min)
Rscript analysis/spatial-inequality-politics/04_adm2_followup.R
Rscript analysis/spatial-inequality-politics/05_figures.R
Rscript analysis/spatial-inequality-politics/06_fixed_effects.R
Rscript analysis/spatial-inequality-politics/07_reverse_direction.R
```

`vdemdata` and `democracyData` are GitHub-only packages. `00_get_data.R` fetches
only their bundled data files at pinned commits (V-Dem v16 `f4dd269`,
democracyData `84290a7`), so neither package needs to be installed.

## Outputs (`output/`)

- `spec_grid.csv`: every estimate (spec × predictor × outcome) with SE, p, BH/Holm p, N, clusters
- `robustness_summary.csv`: per predictor × outcome robustness tally
- `attenuation.csv`: bivariate coefficient after adding each control one at a time
- `cv_prediction.csv`: country-blocked CV, out-of-sample R² / AUC with and without the inequality measures
- `adm2_followup.csv`, `adm2_leave_one_region_out.csv`, `adm2_jackknife.csv`
- `fe_battery.csv`, `fe_robustness_summary.csv`, `fe_within_variation.csv`: fixed-effects battery
- `reverse_crosslag.csv`, `reverse_event_study.csv`, `reverse_events.csv`, `reverse_mechanism.csv`, `reverse_within_sd.csv`: reverse-direction tests
- `predictor_correlations.csv`, `coefplot.png`
