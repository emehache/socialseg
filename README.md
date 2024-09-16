
<!-- README.md is generated from README.Rmd. Please edit that file -->

# socialseg

# Usage

``` r
data(input)

vars <- c("nivel_edu_alto", "nivel_edu_bajo")
lx <- 100


st_bbox(input)

st_make_grid(input, cellsize = c(lx, lx)) %>% 
  st_centroid() %>% 
  st_coordinates()

distribuido <- input %>% 
  distribute(lx = lx, vars = vars) 

suavizado <- smoothgrid(distribuido, sigma = 200)

plot(distribuido, var = vars[1])
plot(suavizado, var = vars[1])


suavizado$values
```
