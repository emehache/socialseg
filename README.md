
<!-- README.md is generated from README.Rmd. Please edit that file -->

# socialseg

## Installation

Install `socialseg` using the following code:

``` r
install.packages("remotes")
remotes::install_github("https://github.com/emehache/socialseg")
```

## Usage

``` r
data(input)

vars <- c("nivel_edu_alto", "nivel_edu_bajo")
lx <- 100

distribuido <- input %>% 
  distribute(lx = lx, vars = vars) 

plot(distribuido, var = vars[1])

suavizado <- smoothgrid(distribuido, sigma = 200)
plot(suavizado, var = vars[1])

entornos <- environments(suavizado, gamma = 500, vars = vars)
plot(entornos, var = "Hp")

coef(distribuido)
coef(suavizado)
coef(entornos)
```

### Differences between maps

``` r
vars <- c("nivel_edu_alto", "nivel_edu_bajo")
data("input1985")
input1985 <- st_transform(input1985, crs=32721)
bb <- st_bbox(input)
distribuido1985 <- input1985 %>% 
  distribute(lx = lx, vars = vars, bbox = bb)
diferencia <- diff_grid(distribuido, distribuido1985, var = vars[1])
plot(diferencia)

diferencia2 <- diff_grid(distribuido1985, distribuido, var = vars[1])
plot(diferencia2)
```

### Segregation profile with confidence bands

``` r
perfil <- seg_profile(suavizado, N = 100, vars = vars)
plot(perfil)

perfil2 <- seg_profile(suavizado, N = 0, vars = vars)
plot(perfil2)
```
