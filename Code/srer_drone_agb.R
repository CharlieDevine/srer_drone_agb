library(ggplot2)
library(ggpubr)
library(ggh4x)
library(cowplot)

# Set filepaths
code.fp = getwd()
setwd('../')
data.fp = paste(getwd(), 'Data', sep = '/')
figs.fp = paste(getwd(), 'Figures', sep = '/')

# Get files
files = list.files(data.fp, pattern = '.csv$', full.names = TRUE, recursive = FALSE)

# Read files
pre.2019 = read.csv(files[3], header = TRUE)
post.2019 = read.csv(files[1], header = TRUE)
pre.2023 = read.csv(files[4], header = TRUE)
post.2023 = read.csv(files[2], header = TRUE)

# Create function to split numeric values from crown area field
ca.extract = function(ca, year) {
  ca.split = strsplit(ca, split = ' ')
  ca.out = vector(mode = 'numeric', length = length(ca.split))
  for (i in 1 : length(ca.split)) {
    ca.i = ca.split[[i]]
    if (year == '2019') { ca.i.val = as.numeric(ca.i[2]) }
    if (year == '2023') { ca.i.val = as.numeric(ca.i[1]) }
    ca.out[i] = ca.i.val 
  }
  return(ca.out)
}

# Create function to assign species name to codes
assign.sp.name = function(sp.codes) {
  sp.names = vector(mode = 'character', length = length(sp.codes))
  for (i in 1 : length(sp.codes)) {
    if (isTRUE(sp.codes[i] == 1)) { sp.names[i] = 'NA' }
    if (isTRUE(sp.codes[i] == 2)) { sp.names[i] = 'NA' }
    if (isTRUE(sp.codes[i] == 3)) { sp.names[i] = 'Mesquite' }
    if (isTRUE(sp.codes[i] == 4)) { sp.names[i] = 'Cactus' }
    if (isTRUE(sp.codes[i] == 5)) { sp.names[i] = 'Creosote' }
    if (isTRUE(sp.codes[i] == 6)) { sp.names[i] = 'Lotebush' }
    if (isTRUE(sp.codes[i] == 7)) { sp.names[i] = 'Palo Verde' }
    if (isTRUE(is.na(sp.codes[i]))) { sp.names[i] = 'NA' }
  }
  return(sp.names)
}

# Extract metrics and species info
pre.2019.ca = ca.extract(pre.2019[,5],'2019') * 0.092903 # convert from ft2 to m2
pre.2019.hmean = pre.2019[,7] * 0.3048 # convert from ft to m
pre.2019.species = assign.sp.name(pre.2019[,9])

post.2019.ca = ca.extract(post.2019[,5],'2019') * 0.092903
post.2019.hmean = post.2019[,7] * 0.3048
post.2019.species = assign.sp.name(post.2019[,9])

pre.2023.ca = ca.extract(pre.2023[,5],'2023') * 0.092903
pre.2023.hmean = pre.2023[,7] * 0.3048
pre.2023.species = assign.sp.name(pre.2023[,9])

post.2023.ca = ca.extract(post.2023[,5],'2023') * 0.092903
post.2023.hmean = post.2023[,7] * 0.3048
post.2023.species = assign.sp.name(post.2023[,9])

# Combine as data frame
df = rbind(data.frame('CrownArea' = pre.2019.ca,
                      'MeanHeight' = pre.2019.hmean,
                      'PrePostMonsoon' = 'PreMonsoon',
                      'Year' = '2019',
                      'Species' = pre.2019.species),
           data.frame('CrownArea' = post.2019.ca,
                      'MeanHeight' = post.2019.hmean,
                      'PrePostMonsoon' = 'PostMonsoon',
                      'Year' = '2019',
                      'Species' = post.2019.species),
           data.frame('CrownArea' = pre.2023.ca,
                      'MeanHeight' = pre.2023.hmean,
                      'PrePostMonsoon' = 'PreMonsoon',
                      'Year' = '2023',
                      'Species' = pre.2023.species),
           data.frame('CrownArea' = post.2023.ca,
                      'MeanHeight' = post.2023.hmean,
                      'PrePostMonsoon' = 'PostMonsoon',
                      'Year' = '2023',
                      'Species' = post.2023.species))

# Remove outlier values
remove.outliers = function() {
  for (i in 1 : nrow(df)) {
    # Remove height outliers (mean height > 2.5 m)
    if (isTRUE(df[i,2] < 2.5)) { df[i,2] = df[i,2] }
    if (isTRUE(df[i,2] > 2.5)) { df[i,2] = NA }
    # Remove crown area outliers (crown area < 0.929174)
    if (isTRUE(df[i,1] < 0.929174)) { df[i,1] = NA }
  }
  return(df)
}

df = remove.outliers()

# Remove NA values
df = na.omit(df)

# Filter by species (exclude cactus and non-identified species)
df = df[df$Species %in% c('Mesquite','Creosote','Palo Verde','Lotebush'),]

# Factor pre/post monsoon, years, and species
df$PrePostMonsoon = factor(df$PrePostMonsoon, 
                           levels = c('PreMonsoon','PostMonsoon'),
                           labels = c('Pre-Monsoon','Post-Monsoon'))
df$Year = factor(df$Year, levels = c('2019','2023'))
df$Species = factor(df$Species, levels = c('Mesquite','Creosote','Palo Verde','Lotebush'))

# Set facet strip colors
facet.cols = strip_themed(background_x = elem_list_rect(fill = c('darkorange4','forestgreen')),
                          text_x = elem_list_text(color = 'white', face = 'bold'))

# Set species colors
species.cols = c('green4','tomato','dodgerblue4','darkorange1')

# ------------------------ Plot crown area/height scatterplot relationships
sp = ggplot(data = df,
            aes(x = CrownArea, y = MeanHeight, color = Species)) +
  geom_point(size = 1, alpha = 0.25, pch = 19) +
  #scale_color_manual(values = c('darkorange4','forestgreen'), guide = 'none') +
  scale_color_manual(values = species.cols,
                     guide = guide_legend(override.aes = list(size = 4, alpha = 1))) +
  stat_smooth(formula = y ~ x, method = 'lm', color = 'red', se = TRUE, na.rm = TRUE) +
  stat_cor(aes(x = CrownArea, y = MeanHeight), 
           method = 'pearson', 
           label.x.npc = 0.45,
           size = 5.5, na.rm = TRUE) +
  ylab('Mean Height [ m ]') +
  xlab(expression(Crown~Area~'['~m^2~']')) +
  theme_bw() +
  facet_grid2(Year~PrePostMonsoon, strip = facet.cols, scales = 'fixed') +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 17, face = 'bold'),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 1),
        strip.background.y = element_rect(fill = 'white', color = 'transparent'),
        strip.background.x = element_rect(color = 'black'),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 15))

ggsave(file = paste(figs.fp, 'PrePost_2019_2023_CrownArea_vs_HeightMean.png', sep = '/'),
       sp,
       width = 9, height = 9,
       bg = 'white')

# ------------------------ Plot box/whisker figures

# Reshape data frame
df.2 = reshape2::melt(df,
                      id.vars = c('PrePostMonsoon','Year','Species'),
                      variable.name = 'Metric',
                      value.name = 'Value')

# Create function to extract statistics for box/whisker plots
get_box_stats = function(y) {
  return(data.frame(
    y = 3,
    label = paste(
      "Max =", round(max(y), 2), "\n",
      "Min =", round(min(y), 2), "\n",
      "Mean =", round(mean(y), 2), "\n",
      "Median =", round(median(y), 2), "\n",
      "STD =", round(sd(y), 2), "\n"
    )
  ))
}

# Generate box/whisker plots for crown area and mean height, then combine into single figure
bw.plot.fun = function() {
  
  ca = df.2[df.2$Metric == 'CrownArea',]
  hmean = df.2[df.2$Metric == 'MeanHeight',]
  
  # Crown area
  bw.ca = ggplot(data = ca,
                 aes(x = PrePostMonsoon, y = Value, color = Species)) +
    geom_boxplot(linewidth = 1) +
    scale_color_manual(values = species.cols,
                       guide = guide_legend(override.aes = list(size = 0, linewidth = 1))) +
    stat_summary(fun.data = get_box_stats,
                 inherit.aes = TRUE,
                 position = 'identity',
                 geom = 'text',
                 hjust = rep(c(1,1,0,0),4),
                 vjust = rep(c(-2.5,-1.5,-2.5,-1.5),4),
                 size = 5) +
    theme_bw() +
    xlab('') +
    ylab(expression(Crown~Area~'['~m^2~']')) +
    facet_grid(~Year,
               scales = 'fixed') +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          strip.text = element_text(size = 25, face = 'bold'),
          panel.border = element_rect(color = 'black', fill = NA, linewidth = 1),
          strip.background.x = element_rect(fill = 'white', color = 'transparent'),
          legend.position = 'bottom',
          legend.title = element_blank(),
          legend.text = element_text(size = 20))
  
  ggsave(paste(figs.fp, 'CA_PrePostMonsoon_Species_BWP.png', sep = '/'),
         bw.ca,
         width = 15, height = 8.5,
         bg = 'white')
  
  # Mean height
  bw.hmean = ggplot(data = hmean,
                    aes(x = PrePostMonsoon, y = Value, color = Species)) +
    geom_boxplot(linewidth = 1) +
    scale_color_manual(values = species.cols,
                       guide = guide_legend(override.aes = list(size = 0, linewidth = 1))) +
    stat_summary(fun.data = get_box_stats,
                 inherit.aes = TRUE,
                 position = 'identity',
                 geom = 'text',
                 hjust = rep(c(1,1,0,0),4),
                 vjust = rep(c(0.85,1.85,0.85,1.85),4),
                 size = 5) +
    theme_bw() +
    xlab('') +
    ylab('Mean Height [ m ]') +
    facet_grid(~Year,
               scales = 'fixed') +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          strip.text = element_text(size = 25, face = 'bold'),
          panel.border = element_rect(color = 'black', fill = NA, linewidth = 1),
          strip.background.x = element_rect(fill = 'white', color = 'transparent'),
          legend.position = 'bottom',
          legend.title = element_blank(),
          legend.text = element_text(size = 20))
  
  ggsave(paste(figs.fp, 'HMEAN_PrePostMonsoon_Species_BWP.png', sep = '/'),
         bw.hmean,
         width = 15, height = 8.5,
         bg = 'white')
  
  # Combine plots
  ca.hmean.plot = plot_grid(plotlist = list(bw.ca + theme(legend.position = 'none'),
                                            bw.hmean),
                            ncol = 1,
                            align = 'v',
                            axis = 't',
                            labels = c('a)','b)'),
                            label_size = 20)
  
  ggsave(paste(figs.fp, 'CA_HMEAN_PrePostMonsoon_Species_BWP.png', sep = '/'),
         ca.hmean.plot,
         width = 16, height = 16,
         bg = 'white')
  
  #return(ca.hmean.plot)
}

bw.plot.fun()


# ------------------------------------------- Compute biomass
agb.fun = function(year, monsoon.phase) {
  
  ca = df.2[df.2$Metric == 'CrownArea' & df.2$Year == year & df.2$PrePostMonsoon == monsoon.phase,5]
  hmean = df.2[df.2$Metric == 'MeanHeight' & df.2$Year == year & df.2$PrePostMonsoon == monsoon.phase,5]
  
  # Convert height mean (m) and canopy area (m2) to units of cm and cm2, respectively
  #hmean = hmean * 100
  ca = ca * 10000
  
  # Set coefficients
  b0 = -304.77
  b1 = 0.061
  b2 = 4137.34
  
  # Compute AGB 
  agb = b0 + (b1 * ca) + (b2 * hmean)
  
  return(agb)
}

pre.2019.agb = agb.fun('2019','Pre-Monsoon')
post.2019.agb = agb.fun('2019','Post-Monsoon')
pre.2023.agb = agb.fun('2023','Pre-Monsoon')
post.2023.agb = agb.fun('2023','Post-Monsoon')

# Create AGB data frame
agb.df = rbind(data.frame('AGB' = pre.2019.agb,
                          'Year' = '2019',
                          'PrePostMonsoon' = 'PreMonsoon'),
               data.frame('AGB' = post.2019.agb,
                          'Year' = '2019',
                          'PrePostMonsoon' = 'PostMonsoon'),
               data.frame('AGB' = pre.2023.agb,
                          'Year' = '2023',
                          'PrePostMonsoon' = 'PreMonsoon'),
               data.frame('AGB' = post.2023.agb,
                          'Year' = '2023',
                          'PrePostMonsoon' = 'PostMonsoon'))

# Remove NA values
agb.df = na.omit(agb.df)

# Factor pre/post monsoon and years
agb.df$PrePostMonsoon = factor(agb.df$PrePostMonsoon, 
                               levels = c('PreMonsoon','PostMonsoon'),
                               labels = c('Pre-Monsoon','Post-Monsoon'))

agb.df$Year = factor(agb.df$Year, levels = c('2019','2023'))

# Plot AGB histograms and box/whisker plots
hist.bw.plot.fun = function() {
  
  hist = ggplot(data = agb.df,
                aes(x = AGB, color = PrePostMonsoon, fill = PrePostMonsoon)) +
    geom_histogram(bins = 100, show.legend = FALSE) +
    #geom_density(linewidth = 1, show.legend = FALSE) +
    scale_color_manual(values = c('darkorange4','forestgreen'), guide = 'none') +
    scale_fill_manual(values = c('darkorange4','forestgreen'), guide = 'none') +
    ylab('Count') +
    xlab('AGB [ g ]') +
    xlim(c(2000,9000)) +
    theme_bw() +
    facet_grid2(Year~PrePostMonsoon, strip = facet.cols, scales = 'fixed') +
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          strip.text = element_text(size = 17, face = 'bold'),
          panel.border = element_rect(color = 'black', fill = NA, linewidth = 1),
          strip.background.y = element_rect(fill = 'white', color = 'transparent'),
          strip.background.x = element_rect(fill =, color = 'black'))
  
}


