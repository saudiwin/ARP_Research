# Define ggplot_theme objects 

my_theme <- theme_bw() + theme(
  panel.grid.major=element_blank(), # Remove all gridlines
  panel.grid.minor=element_blank(),# Remove all gridlines
  strip.background = element_blank(), # Remove gray background from facet_wrap()
  strip.text = element_text(face="bold",size=12), # Facet_wrap titles
  axis.title = element_text(face="bold",size=12), # All axis titles
  axis.text.x = element_text(face="bold",size=10)
)
