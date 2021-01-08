DOM destabilization: respiration
================

two treatments:

1.  desorption: 13C oxalic acid adsorbed to goethite, 5 g goethite+OA
    added to soil
2.  priming: 13C oxalic acid added to soil as solution, 5 g goethite
    (without substrate) added to soil
3.  control: 5 g goethite added to soil, but no OA, only water

Jars were sealed for 48 hours, after which headspace samples were
collected and analyzed for CO2 concentration and 13C/12C composition of
CO2.

-----

![](markdown-figs/respiration/resp_plots-1.png)<!-- -->![](markdown-figs/respiration/resp_plots-2.png)<!-- -->![](markdown-figs/respiration/resp_plots-3.png)<!-- -->![](markdown-figs/respiration/resp_plots-4.png)<!-- -->

-----

### priming

How much priming was seen in the treatments?

Priming calculated as: *(CO2C\_umol - 13CO2C\_umol) -
control\_CO2C\_umol*

[from Bastida et al. Nat.
Comms.](https://doi.org/10.1038/s41467-019-11472-7)

| treatment          | desorption\_umolC | priming\_umolC |
| :----------------- | ----------------: | -------------: |
| 1-time-zero        |           \-0.331 |        \-2.271 |
| 2-wetting          |           \-2.228 |          0.279 |
| 4-drying-rewetting |           \-0.770 |             NA |

-----

<details>

<summary>Session Info</summary>

Date Run: 2020-12-30

    #> R version 4.0.2 (2020-06-22)
    #> Platform: x86_64-apple-darwin17.0 (64-bit)
    #> Running under: macOS Catalina 10.15.7
    #> 
    #> Matrix products: default
    #> BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
    #> LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    #> 
    #> locale:
    #> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils    
    #> [5] datasets  methods   base     
    #> 
    #> other attached packages:
    #>  [1] outliers_0.14   PNWColors_0.1.0
    #>  [3] drake_7.12.4    forcats_0.5.0  
    #>  [5] stringr_1.4.0   dplyr_1.0.1    
    #>  [7] purrr_0.3.4     readr_1.3.1    
    #>  [9] tidyr_1.1.1     tibble_3.0.3   
    #> [11] ggplot2_3.3.2   tidyverse_1.3.0
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] fs_1.5.0          lubridate_1.7.9  
    #>  [3] filelock_1.0.2    progress_1.2.2   
    #>  [5] httr_1.4.2        tools_4.0.2      
    #>  [7] backports_1.1.8   utf8_1.1.4       
    #>  [9] R6_2.4.1          DBI_1.1.0        
    #> [11] colorspace_1.4-1  withr_2.2.0      
    #> [13] tidyselect_1.1.0  prettyunits_1.1.1
    #> [15] curl_4.3          compiler_4.0.2   
    #> [17] cli_2.0.2         rvest_0.3.6      
    #> [19] gWidgets2_1.0-8   xml2_1.3.2       
    #> [21] labeling_0.3      scales_1.1.1     
    #> [23] digest_0.6.25     foreign_0.8-80   
    #> [25] txtq_0.2.3        rmarkdown_2.3    
    #> [27] rio_0.5.16        pkgconfig_2.0.3  
    #> [29] htmltools_0.5.0   highr_0.8        
    #> [31] dbplyr_1.4.4      rlang_0.4.7      
    #> [33] readxl_1.3.1      rstudioapi_0.11  
    #> [35] farver_2.0.3      generics_0.0.2   
    #> [37] jsonlite_1.7.0    zip_2.1.0        
    #> [39] car_3.0-9         magrittr_1.5     
    #> [41] Rcpp_1.0.5        munsell_0.5.0    
    #> [43] fansi_0.4.1       abind_1.4-5      
    #> [45] lifecycle_0.2.0   stringi_1.4.6    
    #> [47] yaml_2.2.1        carData_3.0-4    
    #> [49] storr_1.2.1       grid_4.0.2       
    #> [51] blob_1.2.1        parallel_4.0.2   
    #> [53] crayon_1.3.4      haven_2.3.1      
    #> [55] hms_0.5.3         knitr_1.29       
    #> [57] pillar_1.4.6      igraph_1.2.5     
    #> [59] base64url_1.4     reprex_0.3.0     
    #> [61] glue_1.4.1        packrat_0.5.0    
    #> [63] evaluate_0.14     data.table_1.13.0
    #> [65] modelr_0.1.8      vctrs_0.3.2      
    #> [67] cellranger_1.1.0  gtable_0.3.0     
    #> [69] assertthat_0.2.1  xfun_0.16        
    #> [71] openxlsx_4.1.5    broom_0.7.0      
    #> [73] rsconnect_0.8.16  tinytex_0.25     
    #> [75] memoise_1.1.0     ellipsis_0.3.1

</details>
