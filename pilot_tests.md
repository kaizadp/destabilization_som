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
wt_tin_g = 0.81 
wt_tin_fmsoil_g = 9.08
wt_tin_airdried_g = 7.95

fieldmoisture = round(((wt_tin_fmsoil_g - wt_tin_airdried_g) / (wt_tin_airdried_g - wt_tin_g)) * 100, 2) 
```

field moisture = 15.83 %

### saturation water content

``` r
wt_setup_g = 240.3# empty setup of ring+ mesh + saturation plate
wt_setup_fmsoil_g = 291.9
wt_setup_saturated_g = 320.7 # setup + saturated soil+clay system
wt_added_water_g = wt_setup_saturated_g - wt_setup_fmsoil_g

wt_fm_g = wt_setup_fmsoil_g - wt_setup_g
wt_od_g = wt_fm_g/((fieldmoisture/100)+1)
wt_moisture_g = wt_fm_g - wt_od_g
wt_saturation_water_g = wt_added_water_g + wt_moisture_g
```

saturation water content: 8.05 g water for 10 g ODE soil

## II. Preparing Oxalic Acid

### oxalic acid mixture

from the [isotope-calculations
file](https://github.com/kaizadp/destabilization_som/blob/master/tests/isotope_enrichment_calculations.md),

> we will mix 1.94 mg of 0.99 labelled substrate with 9.31 mg of
> unlabelled substrate (0.0121 enriched), to get 11.25 mg of 0.1809
> enriched substrate – for a 30-g soil core.

for 10 cores,  
\- mix 19.4 mg of labelled substrate with 93.1 mg unlabelled substrate
to get 112.5 mg of enriched substrate.  
\- dissolve in 150 mL deionized mQ water

*112.780 mg unlabelled + 20.199 mg labelled OA weighed and dissolved in
150 mL water*

### adsorbing OA onto goethite

total clay needed = 50 g

~~add 10 g clay + 30 mL OA solution to each 50-mL omics-safe tube (5
tubes total)~~

1.  add 5 g clay + 15 mL OA solution (+ 5 mL water) to each 50-mL
    omics-safe tube (4 tubes total)  
2.  shake overnight at 200 rpm at room temperature
3.  centrifuge for 20 minutes at 2000g
4.  decant and collect supernatant (“shake”)
5.  add 20 mL water (\#1)
6.  shake on vortexer for 10 minutes
7.  centrifuge for 20 minutes at 2000g
8.  decant and collect supernatant (“rinse1”)
9.  add 20 mL water (\#2)
10. shake on vortexer for 10 minutes
11. centrifuge for 20 minutes at 2000g
12. decant and collect supernatant (“rinse2”)
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

1.  weigh ~~30~~ 50 g ODE (~~35~~ \~58 g FM) soil into pint-size Mason
    jars
2.  place the oxalic acid-goethite packet in the top 1 cm of the soil.
3.  add 25 mL Milli-Q water from above
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
      - weigh \~5 g into 15 mL omics-safe tube
      - freeze-dry
      - grind in mortar-pestle
      - analyze on Minerva
10. subsample for WEOC
      - weigh 2.5 g ODE (4.5 g) soil into a 50-mL omics-safe tube
      - add 25 mL milli-Q water
      - shake at 200 rpm for 30 minutes
      - centrifuge at 2000g for 20 minutes
      - filter through 0.4 um PES filters
      - freeze-dry
      - reconstitute with 0.2 uL milli-Q water
      - transfer to 0.3 ul capsules
      - allow to evaporate completely
      - run on IRMS
11. repeat the same steps with control samples (soil without clay/OA)

~~**set B: drying** (n=3)~~

1.  weigh 50 g ODE soil into pint-size Mason jars
2.  place the oxalic acid-goethite packet in the top 1 cm of the soil.
3.  air-dry until constant weight
4.  add ?? mL Milli-Q water from above
5.  seal the jars and repeat steps described above

### experimental details

The clay-amended soils were at \~80% gravimetric moisture.  
Therefore, 2.5 g ODE = 4.5 g wet soil

</details>

# Results

-----

<details>

<summary>Session Info</summary>

Date: 2020-08-07

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
    ## [1] forcats_0.5.0   stringr_1.4.0   dplyr_1.0.1     purrr_0.3.4    
    ## [5] readr_1.3.1     tidyr_1.1.1     tibble_3.0.3    ggplot2_3.3.2  
    ## [9] tidyverse_1.3.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.5       cellranger_1.1.0 pillar_1.4.6     compiler_4.0.2  
    ##  [5] dbplyr_1.4.4     tools_4.0.2      digest_0.6.25    lubridate_1.7.9 
    ##  [9] jsonlite_1.7.0   evaluate_0.14    lifecycle_0.2.0  gtable_0.3.0    
    ## [13] pkgconfig_2.0.3  rlang_0.4.7      reprex_0.3.0     cli_2.0.2       
    ## [17] rstudioapi_0.11  DBI_1.1.0        yaml_2.2.1       haven_2.3.1     
    ## [21] xfun_0.16        withr_2.2.0      xml2_1.3.2       httr_1.4.2      
    ## [25] knitr_1.29       fs_1.5.0         hms_0.5.3        generics_0.0.2  
    ## [29] vctrs_0.3.2      grid_4.0.2       tidyselect_1.1.0 glue_1.4.1      
    ## [33] R6_2.4.1         fansi_0.4.1      readxl_1.3.1     rmarkdown_2.3   
    ## [37] modelr_0.1.8     blob_1.2.1       magrittr_1.5     backports_1.1.8 
    ## [41] scales_1.1.1     ellipsis_0.3.1   htmltools_0.5.0  rvest_0.3.6     
    ## [45] assertthat_0.2.1 colorspace_1.4-1 stringi_1.4.6    munsell_0.5.0   
    ## [49] broom_0.7.0      crayon_1.3.4

</details>
