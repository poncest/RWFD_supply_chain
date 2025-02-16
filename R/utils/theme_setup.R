# R/utils/theme_setup.R

# Color palette ----
palette_main <- c(
  "#2C3E50", # Dark Blue-Gray (primary)
  "#E74C3C", # Coral Red
  "#3498DB", # Bright Blue
  "#2ECC71", # Emerald Green
  "#F1C40F"  # Sun Yellow
)

# Plot aesthetics ----
bkg_col      <- "#f5f5f2"
title_col    <- "gray20"
subtitle_col <- "gray20"
caption_col  <- "gray30"
text_col     <- "gray30"

# Theme setup ----
theme_custom <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = bkg_col, color = NA),
      panel.background = element_rect(fill = bkg_col, color = NA),
      plot.title = element_text(
        color = title_col,
        size = 16,
        face = "bold",
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        color = subtitle_col,
        size = 12,
        margin = margin(b = 10)
      ),
      plot.caption = element_text(
        color = caption_col,
        size = 8,
        margin = margin(t = 10)
      ),
      axis.text = element_text(color = text_col, size = 10),
      axis.title = element_text(color = text_col, size = 11),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_text(color = text_col, size = 10),
      legend.text = element_text(color = text_col, size = 9)
    )
}

# Set as default theme
theme_set(theme_custom())


