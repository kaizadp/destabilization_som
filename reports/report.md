SOM destabilization
================

<details>

<summary>experimental info, click here</summary> two treatments:

1.  desorption: 13C oxalic acid adsorbed to goethite, 5 g goethite+OA
    added to soil
2.  priming: 13C oxalic acid added to soil as solution, 5 g goethite
    (without substrate) added to soil
3.  control: 5 g goethite added to soil, but no OA, only water

Jars were sealed for 48 hours, after which headspace samples were
collected and analyzed for CO2 concentration and 13C/12C composition of
CO2.

-----

How much substrate was added?

``` r
substrate = "OXALIC ACID"
A_labelled = 0.99
A_unlabelled = 0.0121 

mole_substrate = (12*2) + (1*2) + (16*4) # molar weight of substrate
mole_carbon = 12* 2 # molar weight of carbon in substrate
carbon_atoms = 2
c_fraction = mole_carbon/mole_substrate
```

279.384 mg unlabelled oxalic acid (enrichment = 0.0121) was mixed with
58.700 mg labelled oxalic acid (enrichment = 0.99) in 600 mL deionized
water.

``` r
M_labelled = 58.700 # mg
M_unlabelled = 279.384 # mg

# A_mixture * (M_labelled + M_unlabelled) = (A_unlabelled * M_unlabelled) + (A_labelled + M_labelled)
A_mixture = ((A_unlabelled * M_unlabelled) + (A_labelled + M_labelled)) / (M_labelled + M_unlabelled)
print(A_mixture)
#> [1] 0.1865529
```

|                        | value      |
| ---------------------- | ---------- |
| total enrichment       | 0.1866     |
| total volume (mL)      | 600        |
| total oxalic acid (mg) | 338.084    |
| total C (mg)           | 90.1557333 |

**desorption**

20 mL of this solution was added to 5 g goethite.

**priming**

16 mL of this solution was added to the soil + 5 g goethite.

|                        | value per 16 mL |
| ---------------------- | --------------- |
| total enrichment       | 0.1866          |
| total volume (mL)      | 16              |
| total oxalic acid (mg) | 9.0155733       |
| total C (mg)           | 2.4041529       |
| 13C (mg)               | 0.4174222       |

</details>

-----

<details>

<summary>overall, preliminary plots</summary>

### Respiration

![](markdown-figs/report/unnamed-chunk-3-1.png)<!-- -->![](markdown-figs/report/unnamed-chunk-3-2.png)<!-- -->![](markdown-figs/report/unnamed-chunk-3-3.png)<!-- -->

### Soil and WEOC

![](markdown-figs/report/unnamed-chunk-4-1.png)<!-- -->![](markdown-figs/report/unnamed-chunk-4-2.png)<!-- -->![](markdown-figs/report/unnamed-chunk-4-3.png)<!-- -->

</details>

-----

## DESORPTION SAMPLES

![](markdown-figs/report/unnamed-chunk-7-1.png)<!-- -->![](markdown-figs/report/unnamed-chunk-7-2.png)<!-- -->![](markdown-figs/report/unnamed-chunk-7-3.png)<!-- -->

  - wetting vs. drying-rewetting:
      - respiration: similar enrichment. drying-rewetting showed
        significantly greater CO2 evolution.
      - WEOC: similar enrichment and similar amount of WEOC
  - drying increased the WEOC pool and also the WEOC enrichment – (a)
    desorption of labelled C, (b) negative enrichment: labelled C
    accumulated in the WEOC because it was not consumed/respired. the
    enrichment and size of WEOC pool decreased when the soil was rewet,
    indicating that it was consumed.

### mass balance

![](markdown-figs/report/unnamed-chunk-8-1.png)<!-- -->

## PRIMING SAMPLES

![](markdown-figs/report/unnamed-chunk-9-1.png)<!-- -->

-----

<details>

<summary>Session Info</summary>

**Kaizad F. Patel**

Date Run: 2021-01-08

    #> R version 4.0.2 (2020-06-22)
    #> Platform: x86_64-apple-darwin17.0 (64-bit)
    #> Running under: macOS Catalina 10.15.7
    #> 
    #> Matrix products: default
    #> BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
    #> LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    #> 
    #> locale:
    #> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #>  [1] agricolae_1.3-3 PNWColors_0.1.0 forcats_0.5.0   stringr_1.4.0  
    #>  [5] dplyr_1.0.1     purrr_0.3.4     readr_1.3.1     tidyr_1.1.1    
    #>  [9] tibble_3.0.3    ggplot2_3.3.2   tidyverse_1.3.0 drake_7.12.4   
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] nlme_3.1-148      fs_1.5.0          lubridate_1.7.9   filelock_1.0.2   
    #>  [5] progress_1.2.2    httr_1.4.2        tools_4.0.2       backports_1.1.8  
    #>  [9] R6_2.4.1          AlgDesign_1.2.0   DBI_1.1.0         questionr_0.7.1  
    #> [13] colorspace_1.4-1  withr_2.2.0       tidyselect_1.1.0  prettyunits_1.1.1
    #> [17] klaR_0.6-15       compiler_4.0.2    cli_2.0.2         rvest_0.3.6      
    #> [21] xml2_1.3.2        labeling_0.3      scales_1.1.1      digest_0.6.25    
    #> [25] txtq_0.2.3        rmarkdown_2.3     pkgconfig_2.0.3   htmltools_0.5.0  
    #> [29] labelled_2.5.0    dbplyr_1.4.4      fastmap_1.0.1     highr_0.8        
    #> [33] rlang_0.4.7       readxl_1.3.1      rstudioapi_0.11   shiny_1.5.0      
    #> [37] farver_2.0.3      generics_0.0.2    combinat_0.0-8    jsonlite_1.7.0   
    #> [41] magrittr_1.5      Rcpp_1.0.5        munsell_0.5.0     fansi_0.4.1      
    #> [45] lifecycle_0.2.0   stringi_1.4.6     yaml_2.2.1        MASS_7.3-51.6    
    #> [49] storr_1.2.1       grid_4.0.2        blob_1.2.1        parallel_4.0.2   
    #> [53] promises_1.1.1    crayon_1.3.4      lattice_0.20-41   miniUI_0.1.1.1   
    #> [57] haven_2.3.1       hms_0.5.3         knitr_1.29        pillar_1.4.6     
    #> [61] igraph_1.2.5      base64url_1.4     reprex_0.3.0      glue_1.4.1       
    #> [65] evaluate_0.14     modelr_0.1.8      vctrs_0.3.2       httpuv_1.5.4     
    #> [69] cellranger_1.1.0  gtable_0.3.0      assertthat_0.2.1  xfun_0.16        
    #> [73] mime_0.9          xtable_1.8-4      broom_0.7.0       later_1.1.0.1    
    #> [77] cluster_2.1.0     ellipsis_0.3.1

</details>

-----
