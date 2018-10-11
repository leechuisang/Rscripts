## ==== EXPORT COLOURS ====
# If export==TRUE, then colour palettes will be saved to the Colour directory
# Set to FALSE by default, and only to TRUE when it is the first time exporting
# palettes, or new ones are added and you wish to save them
export <- FALSE

# Input custom palettes between "USER INPUT HERE" and "INPUT END" sections
# Palettes prepared by CSL and CT are found in respective colour palettes below

## ==== Install and load libraries required ====
req_packages <- c("stringr",
                  "colourpicker",     # For colour formatting
                  "RColorBrewer", "wesanderson")     # For colour formatting
for(pkg in req_packages){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    require(pkg, character.only = TRUE)
  }
}

## ==== Functions for visualising colours ====
# Use the show_palette function to view any set of colours
show_palette <- function(colours) {
  n <- length(colours)
  i <- deparse(substitute(colours))
  image(1:n, 1, as.matrix(1:n), col = colours, 
        xlab = "", ylab = "", xaxt = "n", 
        yaxt = "n", bty = "n")
  text(x=1:n, y=1,labels=str_c(colours), srt=90)
  mtext(as.name(i), side=3)
}
# Or optionally use a pie chart, ideal for larger sets
pie_palette <- function(colours){
  n <- length(colours)
  i <- deparse(substitute(colours))
  pie(rep(1, n), col=colours)
  # To add the names of the colours (not recommended for large sets), include:
  # text(x=1.5, y=(n:1)/n,labels=str_c(colours))
  mtext(as.name(i), side=3)
}

## ==== CSL colour palettes ====
## == USER INPUT HERE ==
# Define colour palettes here
CSL4_basic_cols <- c("red", "blue", "green", "yellow")
CSL5_grey_cols <- c("#030303", "#333333", "#595959", "#7F7F7F", "#A6A6A6")
# Add palette names manually to the list below
CSL_palname <- c("CSL4_basic_cols",
                 "CSL5_grey_cols")
## == INPUT END ==
# Tell R to create a  directory of palette names with their respective colours
CSL_palettes <- sapply(CSL_palname, get)
# Then save all palettes to folder
for(i in names(CSL_palettes)){
  if(length(CSL_palettes[[i]]) >= 3){
    # To assign the name of the palette with the prefix
    # "WA" followed by length of the palette, and
    # the suffix "_cols", e.g. Darjeeling1_cols
    show_palette(get(i))
    mtext(as.name(i), side=3, line=1)
    if(export==TRUE){
      dev.copy(png, str_c("Colours/", i, ".png"))
      dev.off()
    }
  }
}

## ==== CT colour palettes ====
## == USER INPUT HERE ==
# Define colour palettes here
CT5_Sunset_cols <- c("grey52", "olivedrab", "gold2", "darkorange2", "darkred")
CT5_autumnRainbow_cols <- c("olivedrab", "darkred", "coral2", "goldenrod2", "slateblue3")
CT10_RG_cols <- c('grey52', 'olivedrab1', 'coral', 'olivedrab2', 'coral3', 
                  'olivedrab3',  'orangered', 'springgreen3', 'coral2', 'olivedrab')
# Add palette names manually to the list below
CT_palettes <- c("CT5_Sunset_cols",
                 "CT5_autumnRainbow_cols",
                 "CT10_RG_cols")
## == INPUT END ==
# Condensed code to save all palettes to folder without a need for a directory
for(i in CT_palettes){
  show_palette(get(i))
  mtext(as.name(i), side=3, line=1)
  if(export==TRUE){
    dev.copy(png, str_c("Colours/", i, ".png"))
    dev.off()
  }
}

## ==== Wes Anderson palettes ====
# Goes through the list of palettes in wes_palettes and assigns the name and length
# of each palette in the format WA#_Name_cols
# Example: Darjeeling1 becomes WA5_Darjeeling1_cols
for(i in names(wes_palettes)){
  if(length(wes_palette(i)) >= 1){
    # To assign the name of the palette with the prefix
    # "WA" followed by length of the palette, and
    # the suffix "_cols", e.g. Darjeeling1_cols
    assign(str_c("WA",length(wes_palette(i)), "_", i, "_cols"), 
           get_palette(wes_palette(i), length(wes_palette(i))))
    show_palette(wes_palette(i))
    mtext(as.name(i), side=3, line=1)
    if(export==TRUE){
      dev.copy(png, str_c("Colours/WA", length(wes_palette(i)), "_", i, ".png"))
      dev.off()
    }
  }
}

## ==== Other palettes ====
# A colour palette containing 25 high contrast colours
c25_cols <- 
  c("#E31A1C", # red
    "dodgerblue2",
    "green4",
    "#6A3D9A", # purple
    "#FF7F00", # orange
    "black",
    "gold1",
    "skyblue2",
    "#FB9A99", # lt pink
    "palegreen2",
    "#CAB2D6", # lt purple
    "#FDBF6F", # lt orange
    "gray70",
    "khaki2",
    "maroon",
    "orchid1",
    "deeppink1",
    "blue1",
    "steelblue4",
    "darkturquoise",
    "green1",
    "yellow4",
    "yellow3",
    "darkorange4",
    "brown")
# Display palette as a pie chart
pie_palette(c25_cols)
# Export manually as a single palette
if(export==TRUE){
  dev.copy(png, str_c("Colours/c25_cols.png"))
  dev.off()
}
