
<!-- README.md is generated from README.Rmd. Please edit that file -->

# socialseg

## Installation

Install `socialseg` using the command
`devtools::install_github("https://github.com/emehache/socialseg")`.

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

### Segregation profile

``` r
grid_gamma <- seq(lx, 3e3, length.out = 11)
perfil <- seg_profile(gridmap = suavizado, grid_gamma = grid_gamma, vars = vars)

plot(H ~ gamma, data = perfil, type = "o", pch = 20)
abline(h = 0, col = 2)
```

### Differences between maps (en proceso)

``` r
lx <- 100
distribuido <- distribute(input = input, lx = lx, vars = vars) 

data(input1985)
input1985 <- st_transform(input1985, crs = st_crs(input))
distribuido1985 <- distribute(input1985, lx = lx, vars = vars)

# grid2011 <- st_make_grid(input, cellsize = c(lx, lx))
# grid1985 <- st_make_grid(input1985, cellsize = c(lx, lx))
# 
# st_bbox(grid2011) %>% plot
# st_bbox(grid1985) %>% points(col = 2)
```
