
test_region_sizes <- function() {
  c <- corpus("TEST");
  x <- region_sizes(c$p_id);
  checkEquals(c(3,13), x);
}
