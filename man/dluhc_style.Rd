% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dluhc_style.R
\name{dluhc_style}
\alias{dluhc_style}
\title{ggplot function to create standard background and axis labels for a DLUHC style graph}
\usage{
dluhc_style(size = 2)
}
\arguments{
\item{size}{a variable used to edit the text size. This is not the font size, but works as a multiplier for font size, default is 2}
}
\value{
a ggplot object
}
\description{
ggplot function to create standard background and axis labels for a DLUHC style graph
}
\examples{
df <- dluhctheme::Net_Additions_Regional
df$year <- as.numeric(substr(df$Year,7,10))
df <- dplyr::filter(df,Region \%in\% c("North East","North West","South East"))
a <- ggplot2::ggplot(df, ggplot2::aes(x = year, y=Net_Additions)) +
ggplot2::geom_line(ggplot2::aes(colour = Region),linewidth = 2)
a
a + dluhc_style()
}
