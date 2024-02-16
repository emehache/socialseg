
<!-- README.md is generated from README.Rmd. Please edit that file -->

# socialseg

# Usage

``` r
data(input)
input %>% 
  distribute(Ngrid = 50, vars = c("HExtendido", "HNuclear")) %>% 
  segsmooth(sigma = 5) %>% 
  segplot(input)
```
