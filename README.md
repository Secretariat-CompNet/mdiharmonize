# mdiharmonize

**Harmonize confidential firm-level microdata into a common format**

`mdiharmonize` turns the firm-level microdata that national statistical institutes (NSIs) hold into one common structure, the MD format, so that the same analysis code runs unchanged in every country. It is the harmonization engine of the CompNet Micro Data Infrastructure (MDI), and builds on the analysis tools in [`mditools`](https://github.com/Secretariat-CompNet/mditools).

The data never leaves the institute: each NSI runs the package inside its own secure environment, on its own data, and only outputs that pass disclosure control are exported.

## Installation

Once on CRAN:

```r
install.packages("mdiharmonize")
```

Development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("Secretariat-CompNet/mdiharmonize")
```

## How it works

Two sets of metadata tables drive a run: one describes the NSI's raw files and variables, the other maps each NSI variable to a common MD variable (the concordance).

| Step | Functions |
|---|---|
| Configuration and metadata | `mdi_config()`, `mdi_load_md_metadata()`, `mdi_load_nsi_metadata()`, `mdi_disclosure_config()`, `mdi_load_deflators()` |
| Preparing raw files | `mdi_preprocess()` |
| Harmonization | `mdi_harmonize()`, `mdi_classify_conc()`, `mdi_coerce_md_types()` |
| Classifications over time | `mdi_concord_nace()`, `mdi_harmonize_codes_over_time()` |
| Firm identifier levels and hierarchies | `mdi_key_id_switch()`, `hier_col()` |
| Capital stocks | `mdi_build_pim_capital()` |
| Quality checks | `mdi_post_harmonization_checks()`, `mdi_check_harmonized()`, `mdi_plot_breaks()`, `mdi_plot_na_share()` |
| Disclosure control and export | `mdi_disclose()`, `mdi_export()` |
| The whole run | `mdi_run()`, `log_warnings()` |

## Usage example

```r
library(mdiharmonize)
library(data.table)

# a raw NSI file and its concordance rows: turnover is reported in
# thousands, so it is revalued and renamed; employment is only renamed
raw <- data.table(TURN = c(1, 2), EMP = c(3, 4))
conc <- data.table(
  NSI_dataset = "sbs", year = 2020,
  NSI_varname = c("TURN", "TURN", "EMP"),
  MD_varname  = c("turnover", "turnover", "emp"),
  method      = c("revalue", "remap", "remap"),
  detail      = c("x*1000", NA, NA)
)

mdi_harmonize(raw, conc, "FI")
#>    turnover   emp  year
#>       <num> <num> <int>
#> 1:     1000     3  2020
#> 2:     2000     4  2020
```

In a full run, `mdi_config()` sets up the folders and `mdi_run()` reads every
raw file the metadata lists, harmonizes it, and saves one dataset per MD
dataset, which the post-harmonization checks then review.

## Acknowledgements

We thank Jorge de la Cal Medina, Ashim Dubey, Dominik Frommeyer (Statistisches Bundesamt, Destatis), Adam Haunch and Rachel Kühn (Statistisches Bundesamt, Destatis).

Funded by the European Union under the Technical Support Instrument (TSI). This package was developed as part of the project ProdTool (TSI-2025-MULTIMSPROD-IBA, TSI Project Number: 101261159). Views and opinions expressed are however those of the author(s) only and do not necessarily reflect those of the European Union or the European Commission. Neither the European Union nor the granting authority can be held responsible for them.

## License

GPL-3.

## Related

- [`mditools`](https://github.com/Secretariat-CompNet/mditools): data, analysis and disclosure tools for firm-level microdata research.
