Pilot Tests
================

# Experimental

<details>

<summary>click to expand</summary>

## I. Soil Processing and Intitial Characterization

### sieving

sieve through 4 mm mesh.

### gravimetric moisture

``` r
wt_tin_g
wt_tin_fmsoil_g
wt_tin_airdried_g

moisture
```

### saturation water content

``` r
wt_setup_g # empty setup of ring+ mesh + saturation plate
wt_setup_fmsoil_g
wt_setup_fmsoil_clay_g # 5 g clay packet added
wt_setup_saturated_g # setup + saturated soil+clay system
wt_added_water_g = wt_setup_saturated_g - wt_setup_fmsoil_clay_g
```

## II. Preparing Oxalic Acid

### oxalic acid mixture

mix ?? unlabelled with ?? labelled oxalic acid add ?? water to dissolve
and make a ?? concentration solution

### adsorbing OA onto goethite

total clay needed = 40 g

1.  add ?? clay and ?? OA solution to each of ?? 50-mL omics-safe tubes
2.  shake on vortexer for ?? minutes
3.  centrifuge
4.  decant supernatant
5.  add ?? mL water (\#1)
6.  shake on vortexer for ?? minutes
7.  centrifuge
8.  decant supernatant
9.  add ?? mL water (\#2)
10. shake on vortexer for ?? minutes
11. centrifuge
12. decant supernatant
13. freeze-dry
14. gently shake to break up large sheets of dried clay
15. weigh 5 g of OA-clay into packets made of 100 um mesh.

## III. incubation setup

### requirements

  - sieved soil
  - prepared oxalic acid-goethite packets
  - pint-size Mason jars
      - lids fitted with (a) Swagelok/compression unions, (b) rubber
        washers, (c) septa
  - 50 mL syringe + needle
  - 50 mL collection bottle + stopper + crimp seal (evacuate and seal)
  - labels for jars and bottles

### procedure

**set A: wetting** (n=3)

1.  weigh 50 g ODE soil into pint-size Mason jars
2.  place the oxalic acid-goethite packet in the top 1 cm of the soil.
3.  add ?? mL Milli-Q water from above
4.  seal the jars for 48 hours at room temperature
5.  at the end of the incubation, pull 50 mL headspace and transfer to
    pre-evacuated bottles
      - ship gas samples to MSL
6.  remove the oxalic acid-goethite packet
      - transfer to 15 mL omics-safe tube
      - freeze-dry
      - grind
      - analyze on Minerva for TC, 13C
7.  mix the soil well
8.  subsample for moisture
      - weigh into aluminum tin
      - dry in oven at 105 C for 24 hours
9.  subsample for TC, 13C
      - weigh into 15 mL omics-safe tube
      - freeze-dry
      - grind
      - analyze on Minerva

**set B: drying** (n=3)

1.  weigh 50 g ODE soil into pint-size Mason jars
2.  place the oxalic acid-goethite packet in the top 1 cm of the soil.
3.  air-dry until constant weight
4.  add ?? mL Milli-Q water from above
5.  seal the jars and repeat steps described above

### experimental details

</details>

# Results

-----

<details>

<summary>Session Info</summary>

Date: 2020-07-25

    ## R version 4.0.2 (2020-06-22)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Catalina 10.15.6
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] forcats_0.5.0   stringr_1.4.0   dplyr_1.0.0     purrr_0.3.4    
    ## [5] readr_1.3.1     tidyr_1.1.0     tibble_3.0.3    ggplot2_3.3.2  
    ## [9] tidyverse_1.3.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.5       cellranger_1.1.0 pillar_1.4.6     compiler_4.0.2  
    ##  [5] dbplyr_1.4.4     tools_4.0.2      digest_0.6.25    lubridate_1.7.9 
    ##  [9] jsonlite_1.7.0   evaluate_0.14    lifecycle_0.2.0  gtable_0.3.0    
    ## [13] pkgconfig_2.0.3  rlang_0.4.7      reprex_0.3.0     cli_2.0.2       
    ## [17] rstudioapi_0.11  DBI_1.1.0        yaml_2.2.1       haven_2.3.1     
    ## [21] xfun_0.15        withr_2.2.0      xml2_1.3.2       httr_1.4.2      
    ## [25] knitr_1.29       fs_1.4.2         hms_0.5.3        generics_0.0.2  
    ## [29] vctrs_0.3.2      grid_4.0.2       tidyselect_1.1.0 glue_1.4.1      
    ## [33] R6_2.4.1         fansi_0.4.1      readxl_1.3.1     rmarkdown_2.3   
    ## [37] modelr_0.1.8     blob_1.2.1       magrittr_1.5     backports_1.1.8 
    ## [41] scales_1.1.1     ellipsis_0.3.1   htmltools_0.5.0  rvest_0.3.5     
    ## [45] assertthat_0.2.1 colorspace_1.4-1 stringi_1.4.6    munsell_0.5.0   
    ## [49] broom_0.7.0      crayon_1.3.4

</details>
