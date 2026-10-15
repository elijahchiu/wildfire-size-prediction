# Data sources and provenance

## Supplied extract

`data/wildfires_sample_100k.csv` is the 100,000-row file supplied with the STA 141A coursework materials. The columns and record format match the Fire Program Analysis Fire-Occurrence Database (FPA-FOD), 4th edition, but the coursework file does not include an extraction script, selection rule, random seed, download date, checksum, or source URL. The precise origin and sampling mechanism therefore cannot be verified from the provided materials. The 100,000 rows are a subset of the roughly 1.88 million records in that edition; do not interpret prevalence or state/month shares as population estimates.

## Canonical source

Short, Karen C. 2017. *Spatial wildfire occurrence data for the United States, 1992–2015 [FPA_FOD_20170508], 4th Edition*. USDA Forest Service Research Data Archive. https://doi.org/10.2737/RDS-2013-0009.4

The archive dataset is compiled from federal, state, and local reporting sources. See the archive landing page and its metadata for terms, field definitions, and source limitations.

## Feature timing

The outcome (`FIRE_SIZE`) is the final recorded acreage. `FIRE_SIZE_CLASS`, containment fields, incident identifiers added after discovery, cause, ownership, county, and reporting-agency variables are excluded from model predictors. Discovery year/month and recorded coordinates/state are treated as available near discovery, although their actual reporting timestamps and positional precision cannot be verified in this extract.
