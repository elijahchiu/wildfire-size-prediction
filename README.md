# Wildfire Size Prediction

A reproducible R analysis of whether discovery date and recorded location help classify the eventual size of an already-recorded wildfire.

The positive outcome is a final reported size of at least 300 acres. This is an event-level retrospective classification study. It does not predict fire occurrence and does not infer fire-free county-months.

## Results

The analysis uses a 100,000-row extract of the FPA-FOD 4th edition. Its exact extraction and sampling method are unknown, so findings describe this extract and should not be treated as national estimates. The held-out test results, split counts, data audit, and threshold choices are recorded in [`results/`](results/). The full report is available at [`docs/index.html`](docs/index.html) after building.

## Rebuild

Requires R 4.1 or later and packages `data.table`, `randomForest`, `rmarkdown`, and `knitr`.

```sh
Rscript scripts/run_all.R
Rscript tests/check_pipeline.R
```

The first script rebuilds machine-readable results and the self-contained HTML report. The data file is included here as the supplied coursework extract; its undocumented selection method is disclosed in [`DATA_SOURCES.md`](DATA_SOURCES.md).

## Project files

- `data/wildfires_sample_100k.csv` — supplied 100,000-row source extract.
- `scripts/pipeline.R` — audit, cleaning, modeling, evaluation, and result export.
- `report/wildfire_analysis.Rmd` — 11-section report source.
- `docs/index.html` — generated standalone report for GitHub Pages.
- `results/` — CSV audits, summaries, predictions, and test metrics.
- `tests/check_pipeline.R` — structural, split, metric, and confusion-count checks.

## License and citation

The FPA-FOD source citation and extract provenance caveat are in [`DATA_SOURCES.md`](DATA_SOURCES.md). The data remains attributed to the USDA Forest Service Research Data Archive; the project code and prose are provided under the MIT license in `LICENSE`.
