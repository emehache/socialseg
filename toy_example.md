
<!-- README.md is generated from README.Rmd. Please edit that file -->

# socialseg

### Toy example

``` r
theme_set(theme_bw())
centers <- expand.grid(long = c(-3,0,3), lat = c(-3,0,3)) %>%
  as.data.table
centers <- st_as_sf(centers, coords = c("long", "lat"), crs = 32721)
circles <- st_buffer(centers, dist = c(.1, .3, .5, .2)*3)
circles <- st_intersection(circles)[, "geometry"]
box <- st_as_sf(data.table(long = 0, lat = 0),  coords = c("long", "lat"), crs = 32721) %>%
  st_buffer(2*3)
box <- st_difference(box, st_union(circles))
polygon <- rbind(circles, box)
polygon$id <- 1:nrow(polygon)

ggplot(polygon) +
  geom_sf(aes(fill = factor(id)))
```

``` r
set.seed(123)
n <- nrow(polygon)
polygon$x <- sample(1:50, size = n)
polygon$x[10] <- 1e3

ggplot(polygon) +
  geom_sf(aes(fill = factor(x)))
```

``` r
distributed <- distribute(polygon, vars = "x", lx = .2)
plot(distributed)

plot(distributed$grid[,"x"], reset = F)
```

### Ejemplo mejor

``` r
theme_set(theme_bw())

box <- st_as_sf(data.table(long = 0, lat = 0),  coords = c("long", "lat"), crs = 32721) %>%
  st_buffer(6)

sl <- data.table(long = seq(-6,6,length = 101)) %>%
  .[, lat := (long-2)^2/4] %>%
  .[] %>%
  st_as_sf(coords = c("long", "lat"), crs = 32721) %>%
  st_union() %>%
  st_cast('LINESTRING') %>%
  st_sf()

sl2 <- data.table(long = seq(-6,6,length = 101)) %>%
  .[, lat := -(long+4)^2] %>%
  .[] %>%
  st_as_sf(coords = c("long", "lat"), crs = 32721) %>%
  st_union() %>%
  st_cast('LINESTRING') %>%
  st_sf()

sl3 <- data.table(long = seq(-6,6,length = 101)) %>%
  .[, lat := long*2 - 5] %>%
  .[] %>%
  st_as_sf(coords = c("long", "lat"), crs = 32721) %>%
  st_union() %>%
  st_cast('LINESTRING') %>%
  st_sf()

sl <- rbind(sl, sl2, sl3)

sl <- sl %>%
  st_cast("MULTILINESTRING") %>%
  st_union
polygon <- lwgeom::st_split(box, sl) %>%
  st_collection_extract("POLYGON")

plot(polygon, col = 1:6)

set.seed(1234)
n <- nrow(polygon)
polygon$id <- 1:n
polygon$x <- sample(10:50, size = n)

ggplot(polygon) +
  # geom_sf(aes(fill = factor(id))) +
  geom_sf(aes(fill = x)) +
  scale_fill_gradient2()
```

``` r
roxygen2::roxygenise()
distributed <- distribute(polygon, vars = "x", lx = .3)
plot(distributed)

distributed %>%
  smoothgrid(sigma = 1) %>%
  plot

distributed$grid %>%
  as.data.table
```
