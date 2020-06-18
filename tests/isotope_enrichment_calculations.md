isotope calculations
================

Calculations for isotope enrichment.  
Change input parameters for soil (step 1) and for substrate (step 3).

soil name: Palouse  
45/44-R: 0.0188  
TC = 2 %

``` r
## INPUT VARIABLES FOR SOIL 

soil_name = "MOPANG"
R_45_44 = 0.0191 # 45/44 isotope ratio
core_wt = 100 # grams
soil_tc = 0.09 # total carbon 9 %
soil_weoc = 0.06 # water-extractable organic carbon, mg C/g soil
```

#### step 1: how much carbon to add?

in a 100-g core,  
total carbon = 9 g.  
WEOC = 6 mgC.

if we add C substrate equivalent to the WEOC,  
we **add 6 mg C** to the 100-g core.

-----

#### step 2: what enrichment do we want for the added carbon?

We want our final enrichmment in the soil = 0.1 (i.e. 10 %).  
So how much enriched substrate do we need to add?

``` r
AT = 0.10 # target enrichment
A1 = R_45_44 # R-value for initial soil
# A2 = ?

M1 = soil_weoc * core_wt # initial C, grams
M2 = soil_weoc*core_wt # added C, grams
```

AT = ((A1 x M1) + (A2 x M2)) / (M1 + M2)  
so A2 = ((AT x (M1+M2)) - (A1 x M1)) / M2

``` r
A2 = ((AT *(M1+M2)) - (A1*M1))/M2
print(A2)
```

    ## [1] 0.1809

**so we need to add 6 mg C at 0.1809 enrichment.**

-----

#### step 3: how will we prepare this enriched substrate?

we will mix labelled + unlabelled substrate to get this value

## substrate 1: dextrose

``` r
## INPUT VARIABLES FOR SUBSTRATE
substrate = "DEXTROSE"
A_labelled = 0.99
A_unlabelled = 0.0191 # assuming for now

mole_substrate = (12*6) + (1*12) + (16*6) # molar weight of substrate
mole_carbon = 12* 6 # molar weight of carbon in substrate
carbon_atoms = 6
c_fraction = mole_carbon/mole_substrate
```

How much labelled + unlabelled substrate should we mix to get the target
enrichment?

M\_labelled + M\_unlabelled = M2

M\_unlabelled = 6 - M\_labelled

solve for M\_labelled

A2 = ((A\_labelled x M\_labelled) + (A\_unlabelled x M\_unlabelled)) /
(M\_labelled + M\_unlabelled)

``` r
M_labelled = ((A2*M2) - (A_unlabelled*M2)) / (A_labelled - A_unlabelled)
M_unlabelled = M2 - M_labelled
print(M_labelled)
```

    ## [1] 0.999897

``` r
print(M_unlabelled)
```

    ## [1] 5.000103

**so, we will mix 1 mg of 0.99 enriched CARBON with 5 mg of unlabelled
CARBON (0.0191 enriched),**

our substrate is DEXTROSE,  
where 72 g C corresponds to 180 g substrate.

-----

#### conclusion

**so, we will mix 2.5 mg of 0.99 enriched substrate with 12.5 mg of
unlabelled substrate (0.0191 enriched),**  
**to get 15 mg of 0.1809 enriched substrate – for a 100-g soil core.**

-----

## substrate 2: salicylic acid

``` r
## INPUT VARIABLES FOR SUBSTRATE
substrate = "SALICYLIC ACID"
A_labelled = 0.99
A_unlabelled = 0.0191 # assuming for now

mole_substrate = (12*7) + (1*6) + (16*3) # molar weight of substrate
mole_carbon = 12* 7 # molar weight of carbon in substrate
carbon_atoms = 7
c_fraction = mole_carbon/mole_substrate
```

How much labelled + unlabelled substrate should we mix to get the target
enrichment?

M\_labelled + M\_unlabelled = M2

M\_unlabelled = 6 - M\_labelled

solve for M\_labelled

A2 = ((A\_labelled x M\_labelled) + (A\_unlabelled x M\_unlabelled)) /
(M\_labelled + M\_unlabelled)

``` r
M_labelled = ((A2*M2) - (A_unlabelled*M2)) / (A_labelled - A_unlabelled)
M_unlabelled = M2 - M_labelled
print(M_labelled)
```

    ## [1] 0.999897

``` r
print(M_unlabelled)
```

    ## [1] 5.000103

**so, we will mix 1 mg of 0.99 enriched CARBON with 5 mg of unlabelled
CARBON (0.0191 enriched),**

our substrate is SALICYLIC ACID,  
where 84 g C corresponds to 138 g substrate.

-----

#### conclusion

**so, we will mix 1.64 mg of 0.99 enriched substrate with 8.21 mg of
unlabelled substrate (0.0191 enriched),**  
**to get 9.8571429 mg of 0.1809 enriched substrate – for a 100-g soil
core.**

-----

## substrate 3: oxalic acid

``` r
## INPUT VARIABLES FOR SUBSTRATE
substrate = "OXALIC ACID"
A_labelled = 0.99
A_unlabelled = 0.0191 # assuming for now

mole_substrate = (12*2) + (1*2) + (16*4) # molar weight of substrate
mole_carbon = 12* 2 # molar weight of carbon in substrate
carbon_atoms = 2
c_fraction = mole_carbon/mole_substrate
```

How much labelled + unlabelled substrate should we mix to get the target
enrichment?

M\_labelled + M\_unlabelled = M2

M\_unlabelled = 6 - M\_labelled

solve for M\_labelled

A2 = ((A\_labelled x M\_labelled) + (A\_unlabelled x M\_unlabelled)) /
(M\_labelled + M\_unlabelled)

``` r
M_labelled = ((A2*M2) - (A_unlabelled*M2)) / (A_labelled - A_unlabelled)
M_unlabelled = M2 - M_labelled
print(M_labelled)
```

    ## [1] 0.999897

``` r
print(M_unlabelled)
```

    ## [1] 5.000103

**so, we will mix 1 mg of 0.99 enriched CARBON with 5 mg of unlabelled
CARBON (0.0191 enriched),**

our substrate is OXALIC ACID,  
where 24 g C corresponds to 90 g substrate.

-----

#### conclusion

**so, we will mix 3.75 mg of 0.99 enriched substrate with 18.75 mg of
unlabelled substrate (0.0191 enriched),**  
**to get 22.5 mg of 0.1809 enriched substrate – for a 100-g soil core.**

-----

**Session Info:**

last run: 2020-06-18

``` r
sessionInfo()
```

    ## R version 3.6.0 (2019-04-26)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS Mojave 10.14.6
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_3.6.0  magrittr_1.5    tools_3.6.0     htmltools_0.4.0
    ##  [5] yaml_2.2.0      Rcpp_1.0.2      stringi_1.4.3   rmarkdown_2.1  
    ##  [9] knitr_1.25      stringr_1.4.0   xfun_0.10       digest_0.6.25  
    ## [13] rlang_0.4.6     evaluate_0.14
