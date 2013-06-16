test_region_sizes_positionnal <- function() {
  x <- c(3, 13);
  c <- corpus("TEST");
  y <- region_sizes(c$p_id);
  checkEquals(y, x);
}


