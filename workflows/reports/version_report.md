version_report
================

``` r
source("/mnt/ecocast/projects/koliveira/subprojects/carcharodon/setup.R")
```

    ## here() starts at /mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon

``` r
cfg = charlier::read_config("version_report.yaml")
# cfg$version = version
vpars = charlier::parse_version(cfg$version)

m_vpars = charlier::parse_version(cfg$m_version)
m_path = file.path(cfg$root_path, cfg$m_workflow_path, "versions", m_vpars[["major"]], m_vpars[["minor"]], cfg$m_version)

f_vpars = charlier::parse_version(cfg$f_version)
f_path = file.path(cfg$root_path, cfg$f_workflow_path, "versions", f_vpars[["major"]], f_vpars[["minor"]], cfg$f_version)
```

``` r
file = file.path(f_path, paste0(cfg$f_version, "_prediction.png"))
```

<figure>
<img
src="/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon//workflows/forecast_workflow/versions/v01/000/v01.000.12/v01.000.12_prediction.png"
alt="Prediction image not found" />
<figcaption aria-hidden="true">Prediction image not found</figcaption>
</figure>

``` r
file2 = file.path(f_path, "pauc.png")
```

<figure>
<img
src="/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon//workflows/forecast_workflow/versions/v01/000/v01.000.12/pauc.png"
alt="pAUC image not found" />
<figcaption aria-hidden="true">pAUC image not found</figcaption>
</figure>

``` r
file3 = file.path(m_path, "variable_importance.csv")
x = readr::read_csv(file3)
```

    ## New names:
    ## Rows: 12 Columns: 10
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (1): var dbl (9): ...1, importance, mean, sd, min, q25, med, q75, max
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

``` r
print(x)
```

    ## # A tibble: 12 × 10
    ##     ...1 var          importance  mean       sd   min   q25   med   q75   max
    ##    <dbl> <chr>             <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1     1 sst               38.8  0.425 0.0411   0.359 0.410 0.448 0.449 0.457
    ##  2     2 v                 12.6  0.813 0.0129   0.795 0.811 0.811 0.816 0.831
    ##  3     3 xbtm              11.4  0.831 0.0126   0.815 0.819 0.834 0.842 0.843
    ##  4     4 tbtm               8.95 0.867 0.00971  0.861 0.862 0.863 0.867 0.884
    ##  5     5 dfs                6.32 0.906 0.00341  0.901 0.905 0.906 0.909 0.910
    ##  6     6 u                  6.12 0.909 0.00275  0.906 0.908 0.909 0.911 0.913
    ##  7     7 fish_biomass       4.84 0.928 0.00426  0.922 0.927 0.929 0.931 0.933
    ##  8     8 sbtm               3.98 0.941 0.00822  0.928 0.938 0.945 0.945 0.949
    ##  9     9 sss                3.36 0.950 0.00573  0.943 0.947 0.948 0.956 0.957
    ## 10    10 mld                2.46 0.964 0.00332  0.958 0.963 0.964 0.965 0.967
    ## 11    11 log_depth          0.61 0.991 0.000320 0.991 0.991 0.991 0.991 0.991
    ## 12    12 depth              0.47 0.993 0.000655 0.993 0.993 0.993 0.993 0.994

<figure>
<embed
src="/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon//workflows/modeling_workflow/versions/v01/000/v01.000.12/variable_importance.csv" />
<figcaption aria-hidden="true">variable importance table not
found</figcaption>
</figure>

``` r
file4 = file.path(m_path, "variable_likelihood.png")
```

<figure>
<img
src="/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon//workflows/modeling_workflow/versions/v01/000/v01.000.12/variable_likelihood.png"
alt="variable likelihood curves not found" />
<figcaption aria-hidden="true">variable likelihood curves not
found</figcaption>
</figure>
