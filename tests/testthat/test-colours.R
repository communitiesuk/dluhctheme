test_that("palettes are suitable for the colourblind", {
  checks <- lapply(dluhc_palettes, colorblindcheck::palette_check)
  for (check in checks){
    expect_true(all(check$min_dist > 5)) 
  }
})

test_that("palettes are distinct in greyscale", {
  for (palette in dluhc_palettes){
    greys <- unlist(lapply(palette, .to_grey))
    contrasts <- abs(diff(greys))
  expect_true(min(contrasts) >= 20)
  }
})