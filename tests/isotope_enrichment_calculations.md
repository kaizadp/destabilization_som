isotope calculations
================

Calculations for isotope enrichment.  
Change input parameters for soil (step 1) and for substrate (step 3).

soil name: Palouse  
45/44-R: 0.0188  
TC = 2 %

``` r
## INPUT VARIABLES FOR SOIL 
soil_name = "PALOUSE"
R_45_44 = 0.0191 # 45/44 isotope ratio
core_wt = 50 # grams
soil_tc = mean(2.84, 2.96, 2.77)/100 # total carbon 2.8 %
soil_weoc = 0.06 # water-extractable organic carbon, mg C/g soil # ASSUMING FOR NOW
```

#### step 1: how much carbon to add?

in a 50-g core,  
total carbon = 1.42 g.  
WEOC = 3 mgC.

if we add C substrate equivalent to the WEOC,  
we **add 3 mg C** to the 50-g core.

-----

#### step 2: what enrichment do we want for the added carbon?

We want our final enrichmment in the soil = 0.1 (i.e. 10 %).  
So how much enriched substrate do we need to add?

``` r
AT = 0.10 # target enrichment
A1 = R_45_44 # R-value for initial soil
# A2 = ?

M1 = soil_weoc * core_wt # initial C, grams
M2 = soil_weoc * core_wt # added C, grams
```

AT = ((A1 x M1) + (A2 x M2)) / (M1 + M2)  
so  
A2 = ((AT x (M1+M2)) - (A1 x M1)) / M2

``` r
A2 = ((AT *(M1+M2)) - (A1*M1))/M2
print(A2)
```

    ## [1] 0.1809

**so we need to add 3 mg C at 0.1809 enrichment.**

-----

#### step 3: how will we prepare this enriched substrate?

we will mix labelled + unlabelled substrate to get this value

## other substrates

<details>

<summary>click to expand</summary>

### substrate 1: dextrose

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

M\_unlabelled = 3 - M\_labelled

solve for M\_labelled

A2 = ((A\_labelled x M\_labelled) + (A\_unlabelled x M\_unlabelled)) /
(M\_labelled + M\_unlabelled)

``` r
M_labelled = ((A2*M2) - (A_unlabelled*M2)) / (A_labelled - A_unlabelled)
M_unlabelled = M2 - M_labelled
print(M_labelled)
```

    ## [1] 0.4999485

``` r
print(M_unlabelled)
```

    ## [1] 2.500051

**so, we will mix 0.5 mg of 0.99 enriched CARBON with 2.5 mg of
unlabelled CARBON (0.0191 enriched),**

our substrate is DEXTROSE,  
where 72 g C corresponds to 180 g substrate.

-----

#### conclusion

**so, we will mix 1.25 mg of 0.99 enriched substrate with 6.25 mg of
unlabelled substrate (0.0191 enriched),**  
**to get 7.5 mg of 0.1809 enriched substrate – for a 50-g soil core.**

-----

### substrate 2: salicylic acid

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

M\_unlabelled = 3 - M\_labelled

solve for M\_labelled

A2 = ((A\_labelled x M\_labelled) + (A\_unlabelled x M\_unlabelled)) /
(M\_labelled + M\_unlabelled)

``` r
M_labelled = ((A2*M2) - (A_unlabelled*M2)) / (A_labelled - A_unlabelled)
M_unlabelled = M2 - M_labelled
print(M_labelled)
```

    ## [1] 0.4999485

``` r
print(M_unlabelled)
```

    ## [1] 2.500051

**so, we will mix 0.5 mg of 0.99 enriched CARBON with 2.5 mg of
unlabelled CARBON (0.0191 enriched),**

our substrate is SALICYLIC ACID,  
where 84 g C corresponds to 138 g substrate.

-----

#### conclusion

**so, we will mix 0.82 mg of 0.99 enriched substrate with 4.11 mg of
unlabelled substrate (0.0191 enriched),**  
**to get 4.9285714 mg of 0.1809 enriched substrate – for a 50-g soil
core.**

-----

</details>

-----

## substrate 3: oxalic acid

``` r
## INPUT VARIABLES FOR SUBSTRATE
substrate = "OXALIC ACID"
A_labelled = 0.99
A_unlabelled = 0.0121 # assuming for now

mole_substrate = (12*2) + (1*2) + (16*4) # molar weight of substrate
mole_carbon = 12* 2 # molar weight of carbon in substrate
carbon_atoms = 2
c_fraction = mole_carbon/mole_substrate
```

How much labelled + unlabelled substrate should we mix to get the target
enrichment?

M\_labelled + M\_unlabelled = M2

M\_unlabelled = 3 - M\_labelled

solve for M\_labelled

A2 = ((A\_labelled x M\_labelled) + (A\_unlabelled x M\_unlabelled)) /
(M\_labelled + M\_unlabelled)

``` r
M_labelled = ((A2*M2) - (A_unlabelled*M2)) / (A_labelled - A_unlabelled)
M_unlabelled = M2 - M_labelled
print(M_labelled)
```

    ## [1] 0.5178444

``` r
print(M_unlabelled)
```

    ## [1] 2.482156

**so, we will mix 0.52 mg of 0.99 enriched CARBON with 2.48 mg of
unlabelled CARBON (0.0121 enriched),**

our substrate is OXALIC ACID,  
where 24 g C corresponds to 90 g substrate.

-----

#### conclusion

**so, we will mix 1.94 mg of 0.99 enriched substrate with 9.31 mg of
unlabelled substrate (0.0121 enriched),**  
**to get 11.25 mg of 0.1809 enriched substrate – for a 50-g soil core.**

-----

**Session Info:**

last run: 2020-07-29

``` r
sessionInfo()
```

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
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_4.0.2  magrittr_1.5    tools_4.0.2     htmltools_0.5.0
    ##  [5] yaml_2.2.1      stringi_1.4.6   rmarkdown_2.3   knitr_1.29     
    ##  [9] stringr_1.4.0   xfun_0.15       digest_0.6.25   rlang_0.4.7    
    ## [13] evaluate_0.14
