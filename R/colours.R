# These are the palettes from the DLUHC brand guidelines


# These colours are all in the DLUHC brand guidelines and 
# a) Look distinct to people with colour blindess
# b) Are distinct from their neighbors in greyscale
#
# (There are surprisingly few combinations of the colours in
# the guidelines where this is true)
dluhc_palettes <- list(
  categorical = c(
    "#012169", # Union Blue
    "#C8102E", # Union Red
    "#96BC3D", # Light Green
    "#6C558F", # Mid Purple
    "#4A2E29",  # Light Brown
    "#4C4C4F" # Dark Grey
  ),

  # Our colours are red and blue
  duo = c(
    "#012169", # Union Blue
    "#C8102E" # Union Red
  ),

  # This is the old colour palette, which is prettier and 
  # sequential, but doesn't use the brand colours
  sequential = c(
    "#012169",
    "#7B2876",
    "#C5406E",
    "#F4745D",
    "#FFB454",
    "#F9F871"
  )
)

dluhc_blue <-  "#012169"

# Utility functions for displaying palettes
.display_palette <- function(palette) {
  image(1:length(palette), 1, as.matrix(1:length(palette)),
    col = palette, xlab="", ylab = "", xaxt="n", yaxt = "n", bty="n")
}

.to_grey <-  function(colour) {
  return(rbind(c(0.3, 0.59, 0.11)) %*% col2rgb(colour))
}

.display_grey_palette <- function(palette){
  greys <- unlist(lapply(palette, .to_grey))
  greys_rgb <- unlist(unlist(lapply(sort(greys), function(x) {rgb(x, x, x, maxColorValue = 255)})))
.display_palette(greys_rgb)
}
