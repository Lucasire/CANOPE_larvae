setwd("~/Documents/Files/IRBI/Projects/INFOBIOS/CANOPEE/barcode/data")
getwd()

# Libraries

library("ggplot2")
library("dplyr")
library("hrbrthemes")
library("ggpubr")
library("tidyr")
library("sunburstR")
library("tidyverse")
library("ggforce")
library("tibble")
library("openxlsx")

#### CODE TO SWAP ZOOM FIGURE ####

###

library(ggplot)
library(ggforce)
library(grid)

# define facet_zoom2 function to use FacetZoom2 instead of FacetZoom
# (everything else is the same as facet_zoom)
facet_zoom2 <- function(x, y, xy, zoom.data, xlim = NULL, ylim = NULL, 
                        split = FALSE, horizontal = TRUE, zoom.size = 2, 
                        show.area = TRUE, shrink = TRUE) {
  x <- if (missing(x)) if (missing(xy)) NULL else lazyeval::lazy(xy) else lazyeval::lazy(x)
  y <- if (missing(y)) if (missing(xy)) NULL else lazyeval::lazy(xy) else lazyeval::lazy(y)
  zoom.data <- if (missing(zoom.data)) NULL else lazyeval::lazy(zoom.data)
  if (is.null(x) && is.null(y) && is.null(xlim) && is.null(ylim)) {
    stop("Either x- or y-zoom must be given", call. = FALSE)
  }
  if (!is.null(xlim)) x <- NULL
  if (!is.null(ylim)) y <- NULL
  ggproto(NULL, FacetZoom2,
          shrink = shrink,
          params = list(
            x = x, y = y, xlim = xlim, ylim = ylim, split = split, zoom.data = zoom.data,
            zoom.size = zoom.size, show.area = show.area,
            horizontal = horizontal
          )
  )
}

# define FacetZoom as a ggproto object that inherits from FacetZoom,
# with a modified draw_panels function. the compute_layout function references
# the version currently on GH, which is slightly different from the CRAN
# package version.
FacetZoom2 <- ggproto(
  "FacetZoom2",
  ggforce::FacetZoom,
  
  compute_layout = function(data, params) {
    layout <- rbind( # has both x & y dimension
      data.frame(name = 'orig', SCALE_X = 1L, SCALE_Y = 1L),
      data.frame(name = 'x', SCALE_X = 2L, SCALE_Y = 1L),
      data.frame(name = 'y', SCALE_X = 1L, SCALE_Y = 2L),
      data.frame(name = 'full', SCALE_X = 2L, SCALE_Y = 2L),
      data.frame(name = 'orig_true', SCALE_X = 1L, SCALE_Y = 1L),
      data.frame(name = 'zoom_true', SCALE_X = 1L, SCALE_Y = 1L)
    )
    if (is.null(params$y) && is.null(params$ylim)) { # no y dimension
      layout <- layout[c(1,2, 5:6),]
    } else if (is.null(params$x) && is.null(params$xlim)) { # no x dimension
      layout <- layout[c(1,3, 5:6),]
    }
    layout$PANEL <- seq_len(nrow(layout))
    layout
  },
  
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord,
                         data, theme, params) {
    
    if (is.null(params$x) && is.null(params$xlim)) {
      params$horizontal <- TRUE
    } else if (is.null(params$y) && is.null(params$ylim)) {
      params$horizontal <- FALSE
    }
    if (is.null(theme[['zoom']])) {
      theme$zoom <- theme$strip.background
    }
    if (is.null(theme$zoom.x)) {
      theme$zoom.x <- theme$zoom
    }
    if (is.null(theme$zoom.y)) {
      theme$zoom.y <- theme$zoom
    }
    axes <- render_axes(ranges, ranges, coord, theme, FALSE)
    panelGrobs <- ggforce:::create_panels(panels, axes$x, axes$y)
    panelGrobs <- panelGrobs[seq_len(length(panelGrobs) - 2)]
    if ('full' %in% layout$name && !params$split) {
      panelGrobs <- panelGrobs[c(1, 4)]
    }
    
    # changed coordinates in indicator / lines to zoom from 
    # the opposite horizontal direction
    if ('y' %in% layout$name) {
      if (!inherits(theme$zoom.y, 'element_blank')) {
        zoom_prop <- scales::rescale(
          y_scales[[2]]$dimension(ggforce:::expansion(y_scales[[2]])),
          from = y_scales[[1]]$dimension(ggforce:::expansion(y_scales[[1]])))
        indicator <- polygonGrob(
          x = c(0, 0, 1, 1), # was x = c(1, 1, 0, 0), 
          y = c(zoom_prop, 1, 0), 
          gp = gpar(col = NA, fill = alpha(theme$zoom.y$fill, 0.5)))
        lines <- segmentsGrob(
          x0 = c(1, 1), x1 = c(0, 0), # was x0 = c(0, 0), x1 = c(1, 1)
          y0 = c(0, 1), y1 = zoom_prop,
          gp = gpar(col = theme$zoom.y$colour,
                    lty = theme$zoom.y$linetype,
                    lwd = theme$zoom.y$size,
                    lineend = 'round'))
        indicator_h <- grobTree(indicator, lines)
      } else {
        indicator_h <- zeroGrob()
      }
    }
    
    if ('x' %in% layout$name) {
      if (!inherits(theme$zoom.x, 'element_blank')) {
        zoom_prop <- scales::rescale(x_scales[[2]]$dimension(ggforce:::expansion(x_scales[[2]])),
                                     from = x_scales[[1]]$dimension(ggforce:::expansion(x_scales[[1]])))
        indicator <- polygonGrob(c(zoom_prop, 1, 0), c(1, 1, 0, 0), 
                                 gp = gpar(col = NA, fill = alpha(theme$zoom.x$fill, 0.5)))
        lines <- segmentsGrob(x0 = c(0, 1), y0 = c(0, 0), x1 = zoom_prop, y1 = c(1, 1), 
                              gp = gpar(col = theme$zoom.x$colour,
                                        lty = theme$zoom.x$linetype,
                                        lwd = theme$zoom.x$size,
                                        lineend = 'round'))
        indicator_v <- grobTree(indicator, lines)
      } else {
        indicator_v <- zeroGrob()
      }
    }
    
    if ('full' %in% layout$name && params$split) {
      space.x <- theme$panel.spacing.x
      if (is.null(space.x)) space.x <- theme$panel.spacing
      space.x <- unit(5 * as.numeric(convertUnit(space.x, 'cm')), 'cm')
      space.y <- theme$panel.spacing.y
      if (is.null(space.y)) space.y <- theme$panel.spacing
      space.y <- unit(5 * as.numeric(convertUnit(space.y, 'cm')), 'cm')
      
      # change horizontal order of panels from [zoom, original] to [original, zoom]
      # final <- gtable::gtable_add_cols(panelGrobs[[3]], space.x)
      # final <- cbind(final, panelGrobs[[1]], size = 'first')
      # final_tmp <- gtable::gtable_add_cols(panelGrobs[[4]], space.x)
      # final_tmp <- cbind(final_tmp, panelGrobs[[2]], size = 'first')
      final <- gtable::gtable_add_cols(panelGrobs[[1]], space.x)
      final <- cbind(final, panelGrobs[[3]], size = 'first')
      final_tmp <- gtable::gtable_add_cols(panelGrobs[[2]], space.x)
      final_tmp <- cbind(final_tmp, panelGrobs[[4]], size = 'first')
      
      final <- gtable::gtable_add_rows(final, space.y)
      final <- rbind(final, final_tmp, size = 'first')
      final <- gtable::gtable_add_grob(final, list(indicator_h, indicator_h),
                                       c(2, 6), 3, c(2, 6), 5,
                                       z = -Inf, name = "zoom-indicator")
      final <- gtable::gtable_add_grob(final, list(indicator_v, indicator_v), 
                                       3, c(2, 6), 5, 
                                       z = -Inf, name = "zoom-indicator")
      heights <- unit.c(
        unit(max_height(list(axes$x[[1]]$top, axes$x[[3]]$top)), 'cm'),
        unit(1, 'null'),
        unit(max_height(list(axes$x[[1]]$bottom, axes$x[[3]]$bottom)), 'cm'),
        space.y,
        unit(max_height(list(axes$x[[2]]$top, axes$x[[4]]$top)), 'cm'),
        unit(params$zoom.size, 'null'),
        unit(max_height(list(axes$x[[2]]$bottom, axes$x[[4]]$bottom)), 'cm')
      )
      
      # swop panel width specifications according to the new horizontal order
      widths <- unit.c(
        # unit(max_width(list(axes$y[[3]]$left, axes$y[[4]]$left)), 'cm'),
        # unit(params$zoom.size, 'null'),
        # unit(max_height(list(axes$y[[3]]$right, axes$y[[4]]$right)), 'cm'),
        # space.x,
        # unit(max_width(list(axes$y[[1]]$left, axes$y[[2]]$left)), 'cm'),
        # unit(1, 'null'),
        # unit(max_height(list(axes$y[[1]]$right, axes$y[[2]]$right)), 'cm')        
        unit(max_width(list(axes$y[[1]]$left, axes$y[[2]]$left)), 'cm'),
        unit(1, 'null'),
        unit(max_height(list(axes$y[[1]]$right, axes$y[[2]]$right)), 'cm'),
        space.x,
        unit(max_width(list(axes$y[[3]]$left, axes$y[[4]]$left)), 'cm'),
        unit(params$zoom.size, 'null'),
        unit(max_height(list(axes$y[[3]]$right, axes$y[[4]]$right)), 'cm')
        
      )
      final$heights <- heights
      final$widths <- widths
    } else {
      if (params$horizontal) {
        space <- theme$panel.spacing.x
        if (is.null(space)) space <- theme$panel.spacing
        space <- unit(5 * as.numeric(convertUnit(space, 'cm')), 'cm')
        heights <- unit.c(
          unit(max_height(list(axes$x[[1]]$top, axes$x[[2]]$top)), 'cm'),
          unit(1, 'null'),
          unit(max_height(list(axes$x[[1]]$bottom, axes$x[[2]]$bottom)), 'cm')
        )
        
        # change horizontal order of panels from [zoom, original] to [original, zoom]
        # first <- gtable::gtable_add_cols(panelGrobs[[2]], space)
        # first <- cbind(final, panelGrobs[[1]], size = 'first')
        final <- gtable::gtable_add_cols(panelGrobs[[1]], space) 
        final <- cbind(final, panelGrobs[[2]], size = "first") 
        
        final$heights <- heights
        
        # swop panel width specifications according to the new horizontal order
        # unit(c(params$zoom.size, 1), 'null')
        final$widths[panel_cols(final)$l] <- unit(c(1, params$zoom.size), 'null') 
        
        final <- gtable::gtable_add_grob(final, indicator_h, 2, 3, 2, 5, 
                                         z = -Inf, name = "zoom-indicator")
      } else {
        space <- theme$panel.spacing.y
        if (is.null(space)) space <- theme$panel.spacing
        space <- unit(5 * as.numeric(convertUnit(space, 'cm')), 'cm')
        widths <- unit.c(
          unit(max_width(list(axes$y[[1]]$left, axes$y[[2]]$left)), 'cm'),
          unit(1, 'null'),
          unit(max_height(list(axes$y[[1]]$right, axes$y[[2]]$right)), 'cm')
        )
        final <- gtable::gtable_add_rows(panelGrobs[[1]], space)
        final <- rbind(final, panelGrobs[[2]], size = 'first')
        final$widths <- widths
        final$heights[panel_rows(final)$t] <- unit(c(1, params$zoom.size), 'null')
        final <- gtable::gtable_add_grob(final, indicator_v, 3, 2, 5, 
                                         z = -Inf, name = "zoom-indicator")
      }
    }
    final
  }
)


barcode_gap <- readxl::read_xlsx("CANOPEE_data_final.xlsx", sheet = "barcode_gap")

figgap <- ggplot(barcode_gap, aes(x=Threshold, y=Frequency)) +    # ggplot2 facet_wrap plot
  geom_bar(stat = "identity") +
  facet_wrap(~ Zoom, scales = "free_y")

figgap2 <- ggplot(barcode_gap, aes(x=Threshold, y=Frequency, fill = Group)) +             # ggplot2 facet_zoom plot
  geom_bar(stat = "identity", position=position_dodge()) +
  xlab("Genetic divergence (%)") + 
  ylab("Frequency (%)") +
  scale_fill_manual(values = c("#104A2D", "#9ED6BA")) +
  geom_col(position=position_dodge()) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size=12, face="bold"), 
    axis.title.y = element_text(size=12, face="bold"), 
    legend.title = element_text(size=12, face = "bold")) +
  facet_zoom2(ylim = c(0, 15), show.area = TRUE, horizontal = FALSE)

figgap2

###
###
library(gridExtra)

df2 <- data.frame(Class=rep(c("Annelida", "Arthropoda"), each=1),
                  Data=rep(c("Specimens"),1),
                  len=c(22, 962))

head(df2)

A1 <- ggplot(data=df2, aes(x=Data, y=len, fill=Class)) +
  labs(title = "A", x = "Dataset", y = "") +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_manual(values = c("#BFBFBF", "#256271")) +
  theme_minimal() +
  theme(legend.position = "top", 
        legend.box = "horizontal",
        legend.key.size = unit(.5, 'cm'),
        legend.title = element_text(size=8),
        legend.text = element_text(size=7))

A1

#

df <- data.frame(a = c(39, 28, 9, 143, 22, 721, 
                       223, 392, 61, 34, 11),
                 b = c("x", "x", "x", "x", "x", "x", 
                       "y", "y", "y", "y", "y"),
                 Group = c("Arachnida", "Chilopoda", "Collembola", "Diplopoda", "Malacostraca", "Insecta", 
                       "Coleoptera", "Diptera", "Hymenoptera", "Lepidoptera", "Others"))

df$Group <- ordered(df$Group,
                    levels = c("Arachnida", "Chilopoda", "Collembola", "Diplopoda", "Malacostraca", "Unknown Class", "Insecta", 
                               "Coleoptera", "Diptera", "Hymenoptera", "Lepidoptera", "Others"))

B1 <- ggplot(df, aes(x = b, y = a, fill = Group)) +
  scale_fill_manual(values = c("#E5E5E5","#CCCCCC","#B3B3B3","#999999","#7F7F7F", "#67A1AF",
                               "#F9EEC1", "#E2D5A0", "#D6C88E", "#BFB071", "#A0904E"))+
  geom_bar(stat = "identity")+
  labs(title = "B", x = NULL, y = NULL) +
  coord_polar(theta="y") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        panel.grid = element_blank(),
        legend.key.size = unit(.5, 'cm'),
        legend.title = element_text(size=8),
        legend.text = element_text(size=7))
B1

#

  df3<- data.frame(
    name=c("Species","Genus","Family","Order","Class") ,  
    value=c(761,861,960,962,962)
  )
  
df3$name <- ordered(df3$name,
                            levels = c("Species","Genus","Family","Order","Class"))

C1 <- ggplot(data=df3, aes(x=name, y=value, fill = name)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#EEC591","#BFBFBF", "#A6A6A6","#8C8C8C", "#737373")) +
    labs(title = "C", x = "Identification level", y = "Number of individuals") +
    coord_flip()+
    theme_minimal() +
  theme(legend.position = "none",
        legend.key.size = unit(.5, 'cm'),
        legend.title = element_text(size=8),
        legend.text = element_text(size=7))
C1


# BINs
BINs <- readxl::read_xlsx("CANOPEE_BOLD_20240723.xlsx", sheet = "BINs")
# Compute the cumulative percentages (top of each rectangle)
BINs$ymax = cumsum(BINs$Value)
# Compute the bottom of each rectangle
BINs$ymin = c(0, head(BINs$ymax, n=-1))
# Compute label position
BINs$labelPosition <- (BINs$ymax + BINs$ymin) / 2

D1 <- ggplot(BINs, 
             aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = BIN)) +
  geom_rect() +
  coord_polar("y", start = 0) +
  xlim(c(2, 4)) +
  scale_fill_manual(values = c("#A07A43", "#BFBFBF")) +
  # guides(fill = "none") +
  labs(title = "D", x = NULL, y = NULL) +
  geom_text(x = 3.5, aes(y = labelPosition, label = Number), size = 7, fontface = 3) +
  annotate("text", x = 2, y = 1, label = "173", size = 11, fontface = 2) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        panel.grid = element_blank(),
        legend.key.size = unit(.5, 'cm'),
        legend.title = element_text(size=8),
        legend.text = element_text(size=7))
D1

Fam_distri <- readxl::read_xlsx("CANOPEE_BOLD_20240723.xlsx", sheet = "Ins_fam_distri_2")
Fam_distri$Category <- ordered(Fam_distri$Category,
                            levels = c("Species", "BINs"))

E1 <-ggplot(data = Fam_distri, aes(reorder(Family, -Richness, sum), y = Richness, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("#EEC591", "#A07A43")) +
  labs(title = "E", x = "Insect families") +
  theme_minimal() +
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
        legend.key.size = unit(.5, 'cm'),
        legend.title = element_text(size=8),
        legend.text = element_text(size=7))
E1


lay1 <- rbind(c(1,1,3,3),
              c(2,2,4,4),
              c(5,5,5,5))

grid.arrange(A1, C1, B1, D1, E1, layout_matrix = lay1)

###
###
###
library(gridExtra)

df2larvae <- data.frame(Stage=rep(c("Larvae", "Adult"), each=1), #Counting only Insecta, juveniles/nymphs being in Larvae category
                  Data=rep(c("Specimens"),1),
                  len=c(431, 290))

head(df2larvae)

A2 <- ggplot(data=df2larvae, aes(x=Data, y=len, fill=Stage)) +
  labs(title = "A", x = "Dataset", y = "Number of individuals") +
  geom_bar(stat="identity", position = position_dodge()) +
  coord_flip() +
  scale_fill_manual(values = c("#7EA8DA", "#E07A7F")) +
  theme_minimal() +
  theme(legend.position = "top", legend.box = "horizontal")
A2

df2larvae <- df2larvae %>% 
  arrange(desc(Stage)) %>%
  mutate(prop = len / sum(df2larvae$len) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

A2bis <- ggplot(df2larvae, aes(x="Data", y=len, fill=Stage)) +
  labs(title = "A") +
  geom_bar(stat="identity", width=1, color="white") +
  scale_fill_manual(values = c("#7EA8DA", "#E07A7F")) +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "bottom") +
  annotate("text", x = 1, y = 200, label = "59.8%", size = 8, fontface = 2) +
  annotate("text", x = 1, y = 10, label = "40.2%", size = 8, fontface = 2) 
A2bis
#

sp_abund <- readxl::read_xlsx("CANOPEE_BOLD_20240723.xlsx", sheet = "Sp_abund")

B2 <- ggplot(data=sp_abund, aes(x=Species, y=Number, fill = Stage)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_fill_manual(values = c("#7EA8DA", "#E07A7F")) +
  labs(title = "B", x = "Most represented species", y = "Number of individuals") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle=45, vjust=1, hjust=1, size = 6))
B2

#

df3larvae <- data.frame(
  name=c("Species","Genus","Family","Order") ,  
  value=c(285,337,430,431)
)

df3larvae$name <- ordered(df3larvae$name,
                    levels = c("Species","Genus","Family","Order"))

C2 <- ggplot(data=df3larvae, aes(x=name, y=value, fill = name)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#E07A7F","#BFBFBF", "#A6A6A6","#8C8C8C")) +
  labs(title = "C", x = "Larval identification level", y = "Number of individuals") +
  coord_flip()+
  theme_minimal() +
  theme(legend.position = "none")
C2

#

df3adult <- data.frame(
  name=c("Species","Genus","Family","Order") ,  
  value=c(241,286,290,290)
)

df3adult$name <- ordered(df3adult$name,
                          levels = c("Species","Genus","Family","Order"))

D2 <- ggplot(data=df3adult, aes(x=name, y=value, fill = name)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#7EA8DA","#BFBFBF", "#A6A6A6","#8C8C8C")) +
  labs(title = "D", x = "Adult identification level", y = "Number of individuals") +
  coord_flip()+
  theme_minimal() +
  theme(legend.position = "none")
D2

# 

library(ggvenn)
library(ggplot2)
# List of items
xLarvaeSP <- list(A = 1:55, B = 43:108)
names(xLarvaeSP) <- c("Larvae", "Adult")

E2 <- ggvenn(xLarvaeSP,
             fill_color = c("#CD2626", "#1874CD"),
             stroke_size = 0.5,
             set_name_size = 4,
) +
  labs(title = "E", subtitle = "Species")

E2

xLarvaeBIN <- list(A = 1:74, B = 58:142)
names(xLarvaeBIN) <- c("Larvae", "Adult")

F2 <- ggvenn(xLarvaeBIN,
             fill_color = c("#CD2626", "#1874CD"),
             stroke_size = 0.5,
             set_name_size = 4,
) +
  labs(title = "F", subtitle = "BINs")

F2

#ggarrange(A2, B2, C2, D2, ncol = 2, nrow = 2)

#lay <- rbind(c(1,3),
#             c(1,3),
#             c(2,4),
#             c(2,5))

lay2 <- rbind(c(1,1,2,2),
             c(3,4,5,6))

grid.arrange(A2bis, B2, C2, D2, E2, F2, layout_matrix = lay2)

#########
#  Variations  #
#########
library(mvabund)
library(ordinal)
library(ecoCopula)
library(corrplot)
library(iNEXT)
library(vegan)
library(ggrepel)
library(car)
library(fitdistrplus)


Richness <- readxl::read_xlsx("CANOPEE_BOLD_20240723.xlsx", sheet = "Div_BIN")
Richness$Stage <- ordered(Richness$Stage,
                            levels = c("Adult", "Larvae", "Both", "All invertebrates"))
### parametric test

descdist(Richness$Richness, discrete = TRUE)

fit.pois.Richness <- fitdist(Richness$Richness, "pois")
plot(fit.pois.Richness)

fit.nbinom.Richness <- fitdist(Richness$Richness, "nbinom")
plot(fit.nbinom.Richness)

ggqqplot(Richness$Richness)

shapiro.test(Richness$Richness)

leveneTest(Richness ~ Stage, data = Richness)

res.aov <- aov(Richness ~ Stage, data = Richness)
res.aov
summary(res.aov)
TukeyHSD(res.aov)
aov_residuals <- residuals(object = res.aov )

# non-parametric analysis
kruskal.test(Richness ~ Stage, data = Richness)
pairwise.wilcox.test(Richness$Richness, Richness$Stage,
                     P.adjust.method = "holm")

###

A3 <- ggplot(Richness, aes(x = Stage, y = Richness, fill = Stage)) +
  geom_boxplot()

#cmpr <- list(c("Both","Larvae"), c("Both","Adult"), c("Adult", "Larvae"), c("Both", "All invertebrates"))

A3sup <- A3 + 
  geom_jitter(shape=16, position=position_jitterdodge(0.5)) +
  scale_fill_manual(values = c("#7EA8DA", "#E07A7F", "#8d87b6","#8C8C8C")) +
  theme_minimal() +
  theme(legend.position = "none") + 
  labs(title = "A", y = "Species richness", x = "Studied taxa") +
  scale_x_discrete(label = c("Adult insects only", "Larval insects only", "All insects", "All invertebrates")) +
  annotate(geom="text", x=3, y=35.5, label="*",
           color="black") +
  geom_segment(aes(x=2,xend=4,y=35,yend=35))
  #stat_compare_means(comparisons = cmpr, tip.length=0.01,
                 # label = "p.signif", 
                 #symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                      #           symbols = c("****", "***", "**", "*", "ns")))

A3sup


### correlations between sites

Tot <- Richness[,6:178]
Tot <- pmin(as.matrix(Tot),1)

corrplot(cor(t(Tot)), diag = FALSE, tl.cex = 0.3)

### Mvabund object
Tot_mvabund = mvabund(Tot)
Mod_Tot <- manyglm(Tot_mvabund ~ Richness$Cavity + Richness$Stage + Richness$Sector, family="binomial")
plot(Mod_Tot)

Mod_Tot2 <- manyglm(Tot_mvabund ~ Richness$Cavity + Richness$Stage + Richness$Sector, family=binomial("cloglog"))
plot(Mod_Tot2)

#### Run 10 times to get a range of p-values
anova = anova(Mod_Tot2)
anova

Mod_Tot2.p.Cavity <- c(1, 0.999, 1, 1, , , , , , )
Mod_Tot2.p.Stage <- c(0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001)
Mod_Tot2.p.Sector <- c(0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001)

Mod_Tot2.p.adjust.Cavity <- p.adjust(Mod_Tot2.p.Cavity, method = "holm", n = length(Mod_Tot2.p.Cavity))
Mod_Tot2.p.adjust.Sector <- p.adjust(Mod_Tot2.p.Sector, method = "holm", n = length(Mod_Tot2.p.Sector))
Mod_Tot2.p.adjust.Stage <- p.adjust(Mod_Tot2.p.Stage, method = "holm", n = length(Mod_Tot2.p.Stage))

Mod_Tot2.p.adjust.Cavity
Mod_Tot2.p.adjust.Sector
Mod_Tot2.p.adjust.Stage

### Ordination
Ord_Tot <- cord(Mod_Tot2)

plot(Ord_Tot, biplot = TRUE)

Tot_dataset <- data.frame(Ord_Tot$scores, Richness)

alpha <- 2.5
B3 <- ggplot() +
  geom_segment(aes(x = 0, y = 0, xend = Factor1 * alpha * 0.0000000001, yend = Factor2 * alpha * 0.0000000001), data = Tot_dataset) +
  geom_point(aes(x = Factor1, y = Factor2, shape = Sector, fill = Stage), data = Tot_dataset, size = 3) +
  scale_fill_manual(values=c("#7EA8DA", "#E07A7F", "#d7bde2","#666666")) +
 # scale_color_manual(values=c("#BFBFBF", "#601A77", "#737373")) +
  labs(title = "B", y = "", x = "") +
  scale_shape_manual(values=c(21, 22, 23)) +
  geom_text_repel(aes(x = Factor1, y = Factor2, label = Cavity), data = Tot_dataset, size = 5) + 
  theme_classic() +
  theme(legend.position="top", legend.box = "vertical", legend.box.margin = margin(0, 0, 0, 0, "cm"),legend.title = element_text(size = 17), legend.text = element_text(size = 15)) +
  guides(fill=guide_legend(override.aes=list(shape=21), order = 1), shape = guide_legend(order = 2))
B3

ggarrange(A3sup, B3, ncol = 1, nrow = 2)