install.packages(c("ggplot2", "tidyr"))
library(ggplot2)
setwd("/Users/ashleybelknap/Library/CloudStorage/OneDrive-TexasA&MAgriLife/Barboza Lab Shared/PCH_CaribouForage/NIR_CBforage/R files")
spectra_dir <- "/Users/ashleybelknap/Library/CloudStorage/OneDrive-TexasA&MAgriLife/Barboza Lab Shared/PCH_CaribouForage/NIR_CBforage/R files/spectra/"
output_dir  <- "/Users/ashleybelknap/Library/CloudStorage/OneDrive-TexasA&MAgriLife/Barboza Lab Shared/PCH_CaribouForage/NIR_CBforage/R files/outputs/plots/"

# List species files and their display names
species_list <- data.frame(
  filename    = c(
    "bena_spectra.csv",
    "caaq_spectra.csv",
    "cabi_spectra.csv",
    "erva_spectra.csv",   
    "pela_spectra.csv",
    "sapu_spectra.csv"
  ),
  latin_name  = c(
    "Betula glandulosa",
    "Carex aquatilis",
    "Carex bigalowii",
    "Eriophorum vaginatum",
    "Pedicularis",
    "Salix pulchra"
  )
)
for (i in 1:nrow(species_list)) {
  
  cat("Processing:", species_list$latin_name[i], "\n")
  
  # Read file
  dataset <- read.csv(paste0(spectra_dir, species_list$filename[i]))
  
  # Extract wavelengths and sample columns
  wavelengths  <- dataset$wavelength
  sample_cols  <- dataset[, 8:ncol(dataset)]
  
  # Reshape to long format
  spectra_long <- data.frame(
    Wavelength = rep(wavelengths, times = ncol(sample_cols)),
    Sample     = rep(colnames(sample_cols), each = nrow(dataset)),
    Absorbance = unlist(sample_cols)
  )
  
  # Mean spectrum
  mean_spectrum <- data.frame(
    Wavelength = wavelengths,
    Absorbance = rowMeans(sample_cols, na.rm = TRUE)
  )
  
  # Build plot
  p <- ggplot(spectra_long, aes(x = Wavelength, y = Absorbance, group = Sample)) +
    geom_line(linewidth = 0.3, alpha = 0.4, color = "gray60") +
    geom_line(data = mean_spectrum,
              aes(x = Wavelength, y = Absorbance),
              color = "black", linewidth = 1.0, inherit.aes = FALSE) +
    annotate("rect", xmin = 650,  xmax = 700,  ymin = -Inf, ymax = Inf,
             alpha = 0.08, fill = "green4") +
    annotate("rect", xmin = 1420, xmax = 1480, ymin = -Inf, ymax = Inf,
             alpha = 0.08, fill = "blue") +
    annotate("rect", xmin = 1900, xmax = 1960, ymin = -Inf, ymax = Inf,
             alpha = 0.08, fill = "blue") +
    annotate("rect", xmin = 2040, xmax = 2100, ymin = -Inf, ymax = Inf,
             alpha = 0.08, fill = "red") +
    annotate("text", x = 675,  y = 0.78, label = "Chlorophyll",
             size = 3, angle = 90, hjust = 1, color = "green4") +
    annotate("text", x = 1450, y = 0.78, label = "O-H (water)",
             size = 3, angle = 90, hjust = 1, color = "blue") +
    annotate("text", x = 1930, y = 0.78, label = "O-H (water)",
             size = 3, angle = 90, hjust = 1, color = "blue") +
    annotate("text", x = 2070, y = 0.78, label = "N-H / C-H",
             size = 3, angle = 90, hjust = 1, color = "red") +
    labs(
      title   = bquote(italic(.(species_list$latin_name[i])) ~ "NIR Spectra"),
      x       = "Wavelength (nm)",
      y       = "Absorbance (log 1/R)",
      caption = paste0("Gray lines = individual samples (n = ",
                       ncol(sample_cols), ") | Black line = mean spectrum")
    ) +
    theme_bw(base_size = 13) +
    theme(
      plot.caption = element_text(color = "gray50", size = 9)
    )
  
  # Save plot — named automatically after the species
  outfile <- paste0(output_dir,
                    gsub(" ", "_", species_list$latin_name[i]),
                    "_NIR_spectra.png")
  ggsave(outfile, plot = p, width = 10, height = 5, dpi = 300)
  cat("  Saved:", outfile, "\n")
}

cat("\nAll species plots complete.\n")

# Find which wavelengths have negative values
neg_wavelengths <- wavelengths[rowMeans(sample_cols, na.rm = TRUE) < 0]
cat("Wavelengths with negative mean absorbance:", neg_wavelengths, "\n")

# How many values total are negative?
cat("Total negative values:", sum(sample_cols < 0, na.rm = TRUE), "\n")

# Where are the negatives concentrated?
neg_by_wavelength <- rowSums(sample_cols < 0, na.rm = TRUE)
neg_df <- data.frame(
  Wavelength   = wavelengths,
  Neg_count    = neg_by_wavelength
)

# Show only wavelengths that have at least one negative
neg_df[neg_df$Neg_count > 0, ]

# Which samples have the most negatives?
neg_by_sample <- colSums(sample_cols < 0, na.rm = TRUE)
sort(neg_by_sample[neg_by_sample > 0], decreasing = TRUE)
