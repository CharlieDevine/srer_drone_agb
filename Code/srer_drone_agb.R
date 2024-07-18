library(ggplot2)
library(ggpubr)
library(ggh4x)

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

# Extract metrics
pre.2019.ca = ca.extract(pre.2019[,5],'2019')
pre.2019.hmean = pre.2019[,7] * 0.3048
post.2019.ca = ca.extract(post.2019[,5],'2019')
post.2019.hmean = post.2019[,7] * 0.3048

pre.2023.ca = ca.extract(pre.2023[,5],'2023')
pre.2023.hmean = pre.2023[,7] * 0.3048
post.2023.ca = ca.extract(post.2023[,5],'2023')
post.2023.hmean = post.2023[,7] * 0.3048

# Combine as data frame
df = rbind(data.frame('CrownArea' = pre.2019.ca,
                      'MeanHeight' = pre.2019.hmean,
                      'PrePostMonsoon' = 'PreMonsoon',
                      'Year' = '2019'),
           data.frame('CrownArea' = post.2019.ca,
                      'MeanHeight' = post.2019.hmean,
                      'PrePostMonsoon' = 'PostMonsoon',
                      'Year' = '2019'),
           data.frame('CrownArea' = pre.2023.ca,
                      'MeanHeight' = pre.2023.hmean,
                      'PrePostMonsoon' = 'PreMonsoon',
                      'Year' = '2023'),
           data.frame('CrownArea' = post.2023.ca,
                      'MeanHeight' = post.2023.hmean,
                      'PrePostMonsoon' = 'PostMonsoon',
                      'Year' = '2023'))

# Remove NA values
df = na.omit(df)

# Factor pre/post monsoon and years
df$PrePostMonsoon = factor(df$PrePostMonsoon, 
                           levels = c('PreMonsoon','PostMonsoon'),
                           labels = c('Pre-Monsoon','Post-Monsoon'))

df$Year = factor(df$Year, levels = c('2019','2023'))

# Set facet text colors
facet.cols = strip_themed(background_x = elem_list_rect(fill = c('darkorange4','forestgreen')),
                          text_x = elem_list_text(color = 'white', face = 'bold'))

# Plot crown area/height scatterplot relationships
sp = ggplot(data = df,
            aes(x = CrownArea, y = MeanHeight, color = PrePostMonsoon)) +
  geom_point(size = 1, alpha = 0.25, show.legend = FALSE) +
  scale_color_manual(values = c('darkorange4','forestgreen'), guide = 'none') +
  stat_smooth(formula = y ~ x, method = 'lm', color = 'red', se = TRUE, na.rm = TRUE) +
  stat_cor(aes(x = CrownArea, y = MeanHeight), 
           method = 'pearson', 
           size = 5.5, na.rm = TRUE) +
  ylab('Mean Height [ m ]') +
  xlab(expression(Crown~Area~'['~m^2~']')) +
  theme_bw() +
  facet_grid2(Year~PrePostMonsoon, strip = facet.cols, scales = 'free') +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 17, face = 'bold'),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 1),
        strip.background.y = element_rect(fill = 'white', color = 'transparent'),
        strip.background.x = element_rect(color = 'black'))

sp

ggsave(file = paste(figs.fp, 'PrePost_2019_2023_CrownArea_vs_HeightMean.png', sep = '/'),
       sp,
       width = 7, height = 6,
       bg = 'white')

# Plot box/whisker figures
df.2 = reshape2::melt(df,
                      id.vars = c('PrePostMonsoon','Year'),
                      variable.name = 'Metric',
                      value.name = 'Value')

df.2$Metric = factor(df.2$Metric, 
                     levels = c('CrownArea','MeanHeight'),
                     labels = c('Crown Area [ m\u00b2 ]','Mean Height [ m ]'))

get_box_stats = function(y) {
  return(data.frame(
    y = 1.5,
    label = paste(
      "Mean =", round(mean(y), 2), "\n",
      "Median =", round(median(y), 2), "\n",
      "Max =", round(max(y), 2), "\n",
      "Min =", round(min(y), 2), "\n"
    )
  ))
}

bw = ggplot(data = df.2,
            aes(x = PrePostMonsoon, y = Value, color = PrePostMonsoon)) +
  geom_boxplot(show.legend = FALSE) +
  stat_summary(fun.data = get_box_stats, 
               geom = "text", 
               hjust = 0.5, 
               vjust = c(-1.1,-1.1,
                         -1.1,-1.1,
                         -0.5,-0.5,
                         -1,-1),
               size = 4.5) +
  scale_color_manual(values = c('darkorange4','forestgreen'), guide = 'none') +
  theme_bw() +
  xlab('') +
  ylab('') +
  facet_nested_wrap(~Metric + Year,
                    nrow = 2,
                    scales = 'free',
                    nest_line = element_line()) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 17, face = 'bold'),
        panel.border = element_rect(color = 'black', fill = NA, linewidth = 1),
        strip.background.x = element_rect(fill = 'white', color = 'transparent'))

ggsave(file = paste(figs.fp, 'PrePost_2019_2023_CrownArea_&_HeightMean_BWP.png', sep = '/'),
       bw,
       width = 10, height = 8,
       bg = 'white')

# ------------------------------------------- Compute biomass
agb.fun = function(hmean, ca) {
  
  # Convert height mean (m) and canopy area (m2) to units of cm and cm2, respectively
  hmean.cm = hmean * 100
  ca.cm = ca * 10000
  
  # Set coefficients
  b0 = -304.77
  b1 = 0.061
  b2 = 4137.34
  
  # Compute AGB 
  agb = b0 + (b1 * ca) + (b2 * hmean)
  
  return(agb)
}

pre.2019.agb = agb.fun(pre.2019.hmean, pre.2019.ca)
post.2019.agb = agb.fun(post.2019.hmean, post.2019.ca)
pre.2023.agb = agb.fun(pre.2023.hmean, pre.2023.ca)
post.2023.agb = agb.fun(post.2023.hmean, post.2023.ca)

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

