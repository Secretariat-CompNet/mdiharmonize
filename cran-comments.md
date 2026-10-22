## Resubmission

This is a resubmission. In response to the review:

- **References**: the Description now cites OECD (2009)
  <doi:10.1787/9789264068476-en> for the perpetual inventory method behind
  the capital stocks, and Hundepool et al. (2012)
  <doi:10.1002/9781118348239> for the disclosure-control rules.
- **\dontrun{}**: removed. The examples of `mdi_key_id_switch()` and
  `mdi_export()` now run on small example data (in under a second);
  `mdi_export()` writes to `tempdir()`.
- **Console output**: the package code never calls `print()` or `cat()` to
  write to the console. The two `print()` calls were inside
  `utils::capture.output()`: one turned a table into the text of a
  `message()` and is now a plain `paste()`; the other
  (`mdi_post_harmonization_checks()`) writes result tables to its report
  file and never reaches the console. Progress messages ("Step 1: revalue",
  "Datafile ... harmonized", where the report was saved, ...) are now only
  shown with `verbose = TRUE` (new argument of `mdi_harmonize()`,
  `mdi_post_harmonization_checks()` and `mdi_run()`; default `FALSE`, except
  for `mdi_run()`, which runs a whole harmonization at a statistical
  institute and reports its progress by default). The remaining
  `message()`s report something that happened to the user's data (rows
  dropped for lack of a concordance match, rows masked by disclosure
  control, variables ignored, a step skipped for lack of inputs) and can be
  suppressed with `suppressMessages()`.

## New submission

This is the first submission of mdiharmonize. It builds on mditools
(on CRAN since 0.1.0), from the same team.

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.
* "Possibly misspelled words in DESCRIPTION: Microdata, microdata": a domain
  term (firm-level data on individual units, as held by national statistical
  institutes), spelled as intended.

## Check environments

- local macOS (x86_64), R 4.6.0: 0 errors | 0 warnings | 1 note, local only
  (README.md and NEWS.md can't be checked without pandoc installed)
- win-builder, R-release 4.6.1 (2026-06-24 ucrt): 1 note (above)
- win-builder, R-devel (2026-10-21 r90733 ucrt): 1 note (above)
- GitHub Actions: ubuntu-latest (R release and R devel), macos-latest and
  windows-latest (R release): all passing

## Examples

Every exported function has a runnable example except `mdi_run()`, which runs
a full harmonization from a country's metadata folders, raw data files and
research modules; there is no meaningful self-contained example. Its steps are
exported and exemplified individually (`mdi_config()`, `mdi_harmonize()`,
`mdi_post_harmonization_checks()`, ...). Examples that write files write to
`tempdir()`.

## Notes on suggested packages

- `ggplot2` and `gridExtra` are only used to draw the plots of
  `mdi_plot_breaks()`, `mdi_plot_na_share()` and
  `mdi_post_harmonization_checks()`, behind `requireNamespace()`; the tests
  that draw use `skip_if_not_installed()`.
- `readr` is only used by `mdi_preprocess()`'s date-parsing step, behind
  `requireNamespace()`.
- `RODBC` and `askpass` are only used to read data files stored in an Oracle
  database (`format = "oracle"`), behind `requireNamespace()`; no test needs a
  database.
- `bit64` is only used in a test (`skip_if_not_installed("bit64")`); the
  package code checks for its class with `inherits()` and doesn't call it.

## Reverse dependencies

There are no reverse dependencies on CRAN.
