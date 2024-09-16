
<!-- README.md is generated from README.Rmd. Please edit that file -->

# socialseg

# Usage

``` r
data(input)

vars <- c("nivel_edu_alto", "nivel_edu_bajo")

distribuido <- input %>% 
  distribute(lx = 100, vars = vars) 

plot(distribuido, var = vars[1])

suavizado <- smoothgrid(distribuido, sigma = 200)
plot(suavizado, var = vars[1])

entornos <- environments(suavizado, gamma = 500, vars = vars)
plot(entornos, var = "Hp")

coef(distribuido)
coef(suavizado)
coef(entornos)
```
