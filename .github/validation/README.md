# OpenMalaria integration validation

The `OpenMalaria validation` workflow runs independent v44.0 and v49.0 jobs on
Windows Server 2022. Each job uses the official Windows release archive, with
its SHA256 checksum pinned in the workflow. It installs this checkout so R
sessions and workers load the same OMU version.

The checks cover two RNG seeds and include:

- Complete XML-schema validation and executable version verification.
- Successful simulation with nonempty survey and continuous outputs.
- All expected survey dates and finite survey values.
- V49 clinical and infection-origin measures and their translated labels.
- Serial and batch SQLite collection matching the translated raw outputs,
  with unchanged result columns, a TEXT measure column, and valid foreign keys.

For v44, generated XML is compared with `reference-v44.xml`. That frozen
template was generated using the original `defineDemography()`,
`defineEntomology()` and `write_end_compat()` implementations from commit
`b24e1d6`, applied to `makeValidationScenario(44L)`. Only `@seed@` is substituted
for each reference run. Both survey and continuous outputs must match exactly
using the same v44 executable. Do not regenerate this reference with the
updated builders: that would weaken its regression protection.

V44 and v49 results are not compared numerically with each other. Upstream
model changes can produce different trajectories even with matching seeds.
The v49 scenario deliberately requests additional monitoring measures, so its
output has more rows.

## Run locally

From the repository root, install the checkout and then invoke the validation
script with the matching executable. Use a new output directory for each run;
existing experiments and databases are not overwritten.

```r
install.packages(".", repos = NULL, type = "source")
source(".github/validation/run.R")
validateOpenMalaria(44L, "C:/path/to/v44/openMalaria.exe", "C:/validation/run-v44")
validateOpenMalaria(49L, "C:/path/to/v49/openMalaria.exe", "C:/validation/run-v49")
```

The same script can be run from a terminal:

```text
Rscript .github/validation/run.R 49 /path/to/openMalaria /path/to/new-output-directory
```

For input-generation checks without a simulator:

```text
Rscript .github/validation/run.R 49 - /path/to/new-output-directory --prepare-only
```

On GitHub, logs and results are uploaded even when a job fails. Each job's
artifact includes the release URL/checksum, schemas, expanded XML, simulation
logs, raw outputs, SQLite databases and a summary on success.

These small scenarios check compatibility and data handling; they do not
establish scientific equivalence for all parameterizations or validate external
companion packages. The separate R-CMD-check workflow retains its platform
matrix. Hosted workflow execution must be confirmed after pushing the branch.
