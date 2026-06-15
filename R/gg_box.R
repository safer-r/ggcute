# Error: class order not good when a class is removed due to NA
# Error: line 136 in check 20201126 with add argument
# Solve this: sometimes error messages can be more than the max display (8170). Thus, check every ERROR MESSAGE, and trunck the message if to big. In addition, add at the begining of the warning message that it is too long and see the $warn output for complete message. Add also this into fun_scatter
# add dot.shape ? See with available aesthetic layers
# rasterise: https://cran.r-project.org/web/packages/ggrastr/vignettes/Raster_geoms.html
# add horizontal argument and deal any conflict with vertical argument. Start with horizontal = NULL as default. If ! is.null() -> convert vertical if required
# time for excecution : microbenchmark package. See also in RStudio time per line of code. See also https://stackoverflow.com/questions/7561362/what-can-cause-a-program-to-run-much-faster-the-second-time

#' @title gg_box
#' @description
#' Plot ggplot2 boxplots + dots + means. For ggplot2 specifications, see: https://ggplot2.tidyverse.org/articles/ggplot2-specs.html.
#' @param data1 data frame containing one column of quantitative values (see the y argument below) and one or two columns of categories (see the categ argument below). Duplicated column names are not allowed.
#' @param y character string of the data1 column name for y-axis (column containing numeric values). Numeric values will be split according to the classes of the column names indicated in the categ argument to generate the boxes and will also be used to plot the dots.
#' @param categ vector of character strings of the data1 column name for categories (column of characters or factors). Must be either one or two column names. If a single column name (further referred to as categ1), then one box per class of categ1. If two column names (further referred to as categ1 and categ2), then one box per class of categ2, which form a group of boxes in each class of categ1. WARNING: no empty classes allowed. To have a single box, create a factor column with a single class and specify the name of this column in the categ argument (here, no categ2 in categ argument). For a single set of grouped boxes, create a factor column with a single class and specify this column in categ argument as first element (i.e., as categ1), in addition to the already used category (as categ2 in this situation).
#' @param categ.class.order list indicating the order of the classes of categ1 and categ2 represented on the boxplot (the first compartment for categ1 and and the second for categ2). If categ.class.order == NULL, classes are represented according to the alphabetical order. Some compartments can be NULL and others not. See the categ argument for categ1 and categ2 description.
#' @param categ.color vector of color character string for box frames (see the categ argument for categ1 and categ2 description). If categ.color == NULL, default colors of ggplot2, whatever categ1 and categ2. If categ.color is non-null and only categ1 in categ argument, categ.color can be either: (1) a single color string. All the boxes will have this color, whatever the number of classes of categ1; (2) a vector of string colors, one for each class of categ1. Each color will be associated according to categ.class.order of categ1; (3) a vector or factor of string colors, like if it was one of the column of data1 data frame. WARNING: a single color per class of categ1 and a single class of categ1 per color must be respected; Color functions, like grey(), hsv(), etc., are also accepted; Positive integers are also accepted instead of character strings, as long as above rules about length are respected. Integers will be processed by saferGG::gg_palette() using the maximal integer value among all the integers in categ.color (see saferGG::gg_palette()); If categ.color is non-null and categ1 and categ2 are specified, all the rules described above will apply to categ2 instead of categ1 (colors will be determined for boxes inside a group of boxes)
#' @param box.legend.name character string of the legend title. If box.legend.name is NULL, then box.legend.name <- categ1 if only categ1 is present, and box.legend.name <- categ2 if categ1 and categ2 are present in the categ argument. Write "" if no legend required. See the categ argument for categ1 and categ2 description.
#' @param box.fill logical. Fill the box? If TRUE, the categ.color argument will be used to generate filled boxplots (the box frames being black) as well as filled outlier dots (the dot border being controlled by the dot.border.color argument). If all the dots are plotted (argument dot.color other than NULL), they will be over the boxes. If FALSE, the categ.color argument will be used to color the box frames and the outlier dot borders. If all the dots are plotted, they will be beneath the boxes.
#' @param box.width single numeric value (from 0 to 1) of width of either boxes or group of boxes. When categ argument has a single categ1 element (i.e., separate boxes. See the categ argument for categ1 and categ2 description), then each class of categ1 is represented by a single box. In that case, box.width argument defines each box width, from 0 (no box width) to 1 (max box width), but also the space between boxes (the code uses 1 - box.width for the box spaces). Of note, xmin and xmax of the gg_box() output report the box boundaries (around x-axis unit 1, 2, 3, etc., for each box); When categ argument has a two categ1 and categ2 elements (i.e., grouped boxes), box.width argument defines the width allocated for each set of grouped boxes, from 0 (no group width) to 1 (max group width), but also the space between grouped boxes (the code uses 1 - box.width for the spaces). Of note, xmin and xmax of the gg_box() output report the box boundaries (around x-axis unit 1, 2, 3, etc., for each set of grouped box)
#' @param box.space single numeric value (from 0 to 1) indicating the box separation inside grouped boxes, when categ argument has a two categ1 and categ2 elements. 0 means no space and 1 means boxes shrunk to a vertical line. Ignored if categ argument has a single categ1 element.
#' @param box.line.size single numeric value of line width of boxes and whiskers in mm.
#' @param box.notch logical. Notched boxplot? It TRUE, display notched boxplot, notches corresponding approximately to the 95% confidence interval of the median (the notch interval is exactly 1.58 x Inter Quartile Range (IQR) / sqrt(n), with n the number of values that made the box). If notch intervals between two boxes do not overlap, it can be interpreted as significant median differences.
#' @param box.alpha single numeric value (from 0 to 1) of box transparency (full transparent to full opaque, respectively). To remove boxplots, use box.alpha = 0.
#' @param box.mean logical. Add mean value? If TRUE, a diamond-shaped dot, with the horizontal diagonal corresponding to the mean value, is displayed over each boxplot.
#' @param box.whisker.kind range of the whiskers. Either "no" (no whiskers), or "std" (length of each whisker equal to 1.5 x Inter Quartile Range (IQR)), or "max" (length of the whiskers up or down to the most distant dot).
#' @param box.whisker.width single numeric value (from 0 to 1) of the whisker width, with 0 meaning no whiskers and 1 meaning a width equal to the box width.
#' @param dot.color 
#' @param dot.categ optional single character string of a column name (further referred to as categ3) of the data1 argument. This column of data1 will be used to generate a legend for dots, in addition to the legend for boxes. See the dot.color argument for details about the way the legend is built using the two dot.categ and dot.color arguments. If NULL, no legend created and the colors of dots will depend on dot.color and categ arguments (as explained in the dot.color argument).
#' @param dot.categ.class.order optional vector of character strings indicating the order of the classes of categ3 (see the dot.categ argument). If dot.categ is non-NULL and dot.categ.class.order is NULL, classes are displayed in the legend according to the alphabetical order. Ignored if dot.categ is NULL.
#' @param dot.legend.name optional character string of the legend title for categ3 (see the dot.categ argument). If dot.legend.name == NULL, dot.categ value is used (name of the column in data1). Write "" if no legend required. Ignored if dot.categ is NULL.
#' @param dot.tidy logical. Nice dot spreading? If TRUE, use the geom_dotplot() function for a nice representation. WARNING: change the true quantitative coordinates of dots (i.e., y-axis values for vertical display) because of binning. Thus, the gain in aestheticism is associated with a loss in precision that can be very important. If FALSE, dots are randomly spread on the qualitative axis, using the dot.jitter argument (see below) keeping the true quantitative coordinates.
#' @param dot.tidy.bin.nb positive integer indicating the number of bins (i.e., nb of separations) of the y.lim range. Each dot will then be put in one of the bin, with a diameter of the width of the bin. In other words, increase the number of bins to have smaller dots. Not considered if dot.tidy is FALSE.
#' @param dot.jitter numeric value (from 0 to 1) of random dot horizontal dispersion (for vertical display), with 0 meaning no dispersion and 1 meaning dispersion in the corresponding box width interval. Not considered if dot.tidy is TRUE.
#' @param dot_seed integer value that set the random seed. Using the same number will generate the same dot jittering. Write NULL to have different jittering each time the same instruction is run. Ignored if dot.tidy is TRUE.
#' @param dot.size numeric value of dot diameter in mm. Not considered if dot.tidy is TRUE.
#' @param dot.alpha numeric value (from 0 to 1) of dot transparency (full transparent to full opaque, respectively).
#' @param dot.border.size numeric value of border dot width in mm. Write zero for no dot border. If dot.tidy is TRUE, value 0 remove the border and other values leave the border without size control (geom_doplot() feature).
#' @param dot.border.color single character color string defining the color of the dot border (same color for all the dots, whatever their categories). If dot.border.color == NULL, the border color will be the same as the dot color. A single integer is also accepted instead of a character string, that will be processed by saferGG::gg_palette().
#' @param x.lab a character string or expression for x-axis legend. If NULL, character string of categ1 (see the categ argument for categ1 and categ2 description).
#' @param x.angle integer value of the text angle for the x-axis numbers, using the same rules as in ggplot2. Positive values for counterclockwise rotation: 0 for horizontal, 90 for vertical, 180 for upside down etc. Negative values for clockwise rotation: 0 for horizontal, -90 for vertical, -180 for upside down etc.
#' @param y.lab a character string or expression for y-axis legend. If NULL, character string of the y argument.
#' @param y.lim 2 numeric values indicating the range of the y-axis. Order matters (for inverted axis). If NULL, the range of the x column name of data1 will be used.
#' @param y.log either "no", "log2" (values in the y argument column of the data1 data frame will be log2 transformed and y-axis will be log2 scaled) or "log10" (values in the y argument column of the data1 data frame will be log10 transformed and y-axis will be log10 scaled). WARNING: not possible to have horizontal boxes with a log axis, due to a bug in ggplot2 (see https://github.com/tidyverse/ggplot2/issues/881).
#' @param y.tick.nb approximate number of desired values labeling the y-axis (i.e., main ticks, see the n argument of the the cute::saferGraph::scale2() function). If NULL and if y.log is "no", then the number of labeling values is set by ggplot2. If NULL and if y.log is "log2" or "log10", then the number of labeling values corresponds to all the exposant integers in the y.lim range (e.g., 10^1, 10^2 and 10^3, meaning 3 main ticks for y.lim = c(9, 1200)). WARNING: if non-NULL and if y.log is "log2" or "log10", labeling can be difficult to read (e.g., ..., 10^2, 10^2.5, 10^3, ...).
#' @param y.second.tick.nb number of desired secondary ticks between main ticks. Ignored if y.log is other than "no" (log scale plotted). Use argument return = TRUE and see $plot$y.second.tick.values to have the values associated to secondary ticks. IF NULL, no secondary ticks.
#' @param y.include.zero logical. Does y.lim range include 0? Ignored if y.log is "log2" or "log10".
#' @param y.top.extra.margin single proportion (between 0 and 1) indicating if extra margins must be added to y.lim. If different from 0, add the range of the axis multiplied by y.top.extra.margin (e.g., abs(y.lim[2] - y.lim[1]) * y.top.extra.margin) to the top of y-axis.
#' @param y.bottom.extra.margin idem as y.top.extra.margin but to the bottom of y-axis.
#' @param y.second.tick.positions coordinates of secondary ticks (only if y.second.tick.nb argument is non-NULL or if y.log argument is different from "no").
#' @param y.second.tick.values values of secondary ticks. NULL except if y.second.tick.nb argument is non-NULL or if y.log argument is different from "no").
#' @param stat.pos add the median number above the corresponding box. Either NULL (no number shown), "top" (at the top of the plot region) or "above" (above each box).
#' @param stat.mean logical. Display mean numbers instead of median numbers? Ignored if stat.pos is NULL.
#' @param stat.size numeric value of the stat font size in mm. Ignored if stat.pos is NULL.
#' @param stat.dist numeric value of the stat distance in percentage of the y-axis range (stat.dist = 5 means move the number displayed at 5% of the y-axis range). Ignored if stat.pos is NULL or "top".
#' @param stat.angle integer value of the angle of stat, using the same rules as in ggplot2. Positive values for counterclockwise rotation: 0 for horizontal, 90 for vertical, 180 for upside down etc. Negative values for clockwise rotation: 0 for horizontal, -90 for vertical, -180 for upside down etc.
#' @param vertical logical. Vertical boxes? WARNING: will be automatically set to TRUE if y.log argument is other than "no". Indeed, not possible to have horizontal boxes with a log axis, due to a bug in ggplot2 (see https://github.com/tidyverse/ggplot2/issues/881).
#' @param text.size numeric value of the font size of the (1) axis numbers, (2) axis labels and (3) texts in the graphic legend (in mm).
#' @param title character string of the graph title.
#' @param title.text.size numeric value of the title font size in mm.
#' @param legend.show logical. Show legend? Not considered if categ argument is NULL, because this already generate no legend, excepted if legend.width argument is non-NULL. In that specific case (categ is NULL, legend.show is TRUE and legend.width is non-NULL), an empty legend space is created. This can be useful when desiring graphs of exactly the same width, whatever they have legends or not.
#' @param legend.width single proportion (between 0 and 1) indicating the relative width of the legend sector (on the right of the plot) relative to the width of the plot. Value 1 means that the window device width is split in 2, half for the plot and half for the legend. Value 0 means no room for the legend, which will overlay the plot region. Write NULL to inactivate the legend sector. In such case, ggplot2 will manage the room required for the legend display, meaning that the width of the plotting region can vary between graphs, depending on the text in the legend.
#' @param article logical. If TRUE, use an article theme (article like). If FALSE, use a classic related ggplot theme. Use the add argument (e.g., add = "+ggplot2::theme_classic()" for the exact classic ggplot theme.
#' @param grid logical. Draw lines in the background to better read the box values? Not considered if article == FALSE (grid systematically present).
#' @param add character string allowing to add more ggplot2 features (dots, lines, themes, facet, etc.). Ignored if NULL. WARNING: (1) the string must start with "+", (2) the string must finish with ")" and (3) each function must be preceded by "ggplot2::". Example: "+ ggplot2::coord_flip() + ggplot2::theme_bw()". If the character string contains the "ggplot2::theme" string, then the article argument of gg_box() (see above) is ignored with a warning. In addition, some arguments can be overwritten, like x.angle (check all the arguments). Handle the add argument with caution since added functions can create conflicts with the preexisting internal ggplot2 functions. The call of objects inside the quotes of add can lead to an error if the name of these objects are some of the gg_box() arguments. Indeed, the function will use the internal argument instead of the global environment object. Example article <- "a" in the working environment and add = '+ ggplot2::ggtitle(article)'. The risk here is to have TRUE as title. To solve this, use add = '+ ggplot2::ggtitle(get("article", envir = .GlobalEnv))'    
#' @param return logical. Return the graph parameters?.
#' @param return.ggplot logical. Return the ggplot object in the output list? Ignored if return argument is FALSE. WARNING: always assign the gg_box() function (e.g., a <- gg_box()) if return.ggplot argument is TRUE, otherwise, double plotting is performed. See $ggplot in the RETURN section below for more details.
#' @param return.gtable logical. Return the ggplot object as gtable of grobs in the output list? Ignored if plot argument is FALSE. Indeed, the graph must be plotted to get the grobs dispositions. See $gtable in the RETURN section below for more details.
#' @param plot logical. Plot the graphic? If FALSE and return argument is TRUE, graphical parameters and associated warnings are provided without plotting.
#' @param warn.print logical. Print warnings at the end of the execution? ? If FALSE, warning messages are never printed, but can still be recovered in the returned list. Some of the warning messages (those delivered by the internal ggplot2 functions) are not apparent when using the argument plot = FALSE.
#' @param safer_check Single logical value. Perform the "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) correct \code{lib_path} argument value 2) required functions and related packages effectively present in local R libraries and 3) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Warning: must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{c(lib_path, .libPaths())}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns A boxplot if plot argument is TRUE.
#' @returns A list of the graph info if return argument is TRUE: $data: the initial data with graphic information added; $stat: the graphic statistics (mostly equivalent to ggplot_build()$data[[2]]); $removed.row.nb: which rows have been removed due to NA/Inf detection in y and categ columns (NULL if no row removed); $removed.rows: removed rows (NULL if no row removed); $plot: the graphic box and dot coordinates; $dots: dot coordinates; $main.box: coordinates of boxes; $median: median coordinates; $sup.whisker: coordinates of top whiskers (y for base and y.end for extremities); $inf.whisker: coordinates of bottom whiskers (y for base and y.end for extremities); $sup.whisker.edge: coordinates of top whisker edges (x and xend); $inf.whisker.edge: coordinates of bottom whisker edges(x and xend); $mean: diamond mean coordinates (only if box.mean argument is TRUE); $stat.pos: coordinates of stat numbers (only if stat.pos argument is not NULL); $panel: the variable names used for the panels (NULL if no panels). WARNING: NA can be present according to ggplot2 upgrade to v3.3.0; $axes: the x-axis and y-axis info; $warn: the warning messages. Use cat() for proper display. NULL if no warning. WARNING: warning messages delivered by the internal ggplot2 functions are not apparent when using the argument plot = FALSE; $ggplot: ggplot object that can be used for reprint (use print(...$ggplot) or update (use ...$ggplot + ggplot2::...). NULL if return.ggplot argument is FALSE. Of note, a non-NULL $ggplot in the output list is sometimes annoying as the manipulation of this list prints the plot; $gtable: gtable object that can be used for reprint (use gridExtra::grid.arrange(...$ggplot) or with additionnal grobs (see the grob decomposition in the examples). NULL if return.ggplot argument is FALSE. Contrary to $ggplot, a non-NULL $gtable in the output list is not annoying as the manipulation of this list does not print the plot; 
#' @examples
#' set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(20, 100, 10), rnorm(20, 200, 50), rnorm(20, 500, 60), rnorm(20, 100, 50)), Categ1 = rep(c("CAT", "DOG"), times = 40), Categ2 = rep(c("A", "B", "C", "D"), each = 20), Color1 = rep(c("coral", "lightblue"), times = 40), Color2 = rep(c("#9F2108", "#306100", "#007479", "#8500C0"), each = 20), stringsAsFactors = TRUE) ; set.seed(NULL) ; gg_box(data1 = obs1, y = "Time", categ = "Categ1")
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_dotplot
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 scale_discrete_manual
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 waiver
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom grDevices colors
#' @importFrom gridExtra grid.arrange
#' @importFrom scales math_format
#' @importFrom scales rescale_none
#' @importFrom scales trans_format
#' @importFrom saferDev arg_check
#' @importFrom saferTool comp_2d
#' @importFrom saferTool name_change
#' @importFrom saferTool round2
#' @importFrom saferGraph scale2
#' @importFrom saferGraph inter_ticks
#' @details
#' - Rows containing NA in data1[, c(freq, categ)] will be removed before processing, with a warning (see below).
#' -Hinges are not computed like in the classical boxplot() function of R. See https://ggplot2.tidyverse.org/reference/geom_boxplot.html.
#' -To have a single box, please create a factor column with a single class and specify the name of this column in the categ argument. For a single set of grouped boxes, create a factor column with a single class and specify this column in categ argument as first element (i.e., as categ1, knowing that categ2 must also be specified in this situation). See categ argument below.
#" -The dot.alpha argument can alter the display of the color boxes when using pdf output"
#' - Size arguments (hole.text.size, border.size, title.text.size and annotation.size) are in mm. See Hadley comment in https://stackoverflow.com/questions/17311917/ggplot2-the-unit-of-size. See also http://sape.inf.usi.ch/quick-reference/ggplot2/size). Unit object are not accepted, but conversion can be used (e.g., grid::convertUnit(grid::unit(0.2, "inches"), "mm", valueOnly = TRUE)).
#' -Display seems to be done twice on Windows devices (like a blink). However, no double plots on pdf devices. Thus, the blink remains mysterious.
#' -To remove boxes and have only dots, use box.alpha = 0
#' @export
gg_box <- function(
    data1, 
    y, 
    categ, 
    categ.class.order = NULL, 
    categ.color = NULL, 
    box.legend.name = NULL, 
    box.fill = FALSE, 
    box.width = 0.5, 
    box.space = 0.1, 
    box.line.size = 0.75, 
    box.notch = FALSE, 
    box.alpha = 1, 
    box.mean = TRUE, 
    box.whisker.kind = "std", 
    box.whisker.width = 0, 
    dot.color = grDevices::grey(0.25), 
    dot.categ = NULL, 
    dot.categ.class.order = NULL, 
    dot.legend.name = NULL, 
    dot.tidy = FALSE, 
    dot.tidy.bin.nb = 50, 
    dot.jitter = 0.5, 
    dot_seed = 2, 
    dot.size = 3, 
    dot.alpha = 0.5, 
    dot.border.size = 0.5, 
    dot.border.color = NULL, 
    x.lab = NULL, 
    x.angle = 0, 
    y.lab = NULL, 
    y.lim = NULL, 
    y.log = "no", 
    y.tick.nb = NULL, 
    y.second.tick.nb = 1, 
    y.include.zero = FALSE, 
    y.top.extra.margin = 0.05, 
    y.bottom.extra.margin = 0.05, 
    stat.pos = "top", 
    stat.mean = FALSE, 
    stat.size = 4, 
    stat.dist = 5, 
    stat.angle = 0, 
    vertical = TRUE, 
    text.size = 12, 
    title = "", 
    title.text.size = 8, 
    legend.show = TRUE, 
    legend.width = 0.5, 
    article = TRUE, 
    grid = FALSE, 
    add = NULL, 
    return = FALSE, 
    return.ggplot = FALSE,
    return.gtable = TRUE,
    plot = TRUE, 
    warn.print = FALSE, 
    safer_check = TRUE, 
    lib_path = NULL, 
    error_text = ""
){
    # DEBUGGING
    # set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Categ1 = rep(c("G", "H"), each = 10), stringsAsFactors = TRUE) ; set.seed(NULL) ; obs1$Time[1:10] <- NA ; data1 = obs1 ; y = "Time" ; categ = c("Categ1") ; categ.class.order = NULL ; categ.color = NULL ; box.legend.name = NULL ; box.fill = FALSE ; box.width = 0.5 ; box.space = 0.1 ; box.line.size = 0.75 ; box.notch = FALSE ; box.alpha = 1 ; box.mean = TRUE ; box.whisker.kind = "std" ; box.whisker.width = 0 ; dot.color = grey(0.25) ; dot.categ = NULL ; dot.categ.class.order = NULL ; dot.legend.name = NULL ; dot.tidy = FALSE ; dot.tidy.bin.nb = 50 ; dot.jitter = 0.5 ; dot_seed = 2 ; dot.size = 3 ; dot.alpha = 0.5 ; dot.border.size = 0.5 ; dot.border.color = NULL ; x.lab = NULL ; x.angle = 0 ; y.lab = NULL ; y.lim = NULL ; y.log = "no" ; y.tick.nb = NULL ; y.second.tick.nb = 1 ; y.include.zero = FALSE ; y.top.extra.margin = 0.05 ; y.bottom.extra.margin = 0.05 ; stat.pos = "top" ; stat.mean = FALSE ; stat.size = 4 ; stat.dist = 5 ; stat.angle = 0 ; vertical = TRUE ; text.size = 12 ; title = "" ; title.text.size = 8 ; legend.show = TRUE ; legend.width = 0.5 ; article = TRUE ; grid = FALSE ; add = NULL ; return = FALSE ; return.ggplot = FALSE ; return.gtable = TRUE ; plot = TRUE ; warn.print = FALSE ; safer_check = FALSE ; lib_path = NULL ; error_text = ""




    #### package name
    package_name <- "saferGG" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message
    #### end internal error report link

    #### function name
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L))) # warning: I have written n = 0 to avoid error when a safer function is inside another functions. In addition, arguments values retrieved are not evaluated base::match.call, but this is solved with get() below
    function_name <- base::paste0(tempo_settings[[1]], "()", collapse = NULL, recycle0 = FALSE) 
    # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function_name[1] == "::()" | function_name[1] == ":::()"){
        function_name <- function_name[3]
    }
    #### end function name

    #### arguments settings
    arg_user_setting <- tempo_settings[-1] # list of the argument settings (excluding default values not provided by the user). Always a list, even if 1 argument. So ok for lapply() usage (management of NA section)
    arg_user_setting_names <- base::names(x = arg_user_setting)
    # evaluation of values if they are expression, call, etc.
    if(base::length(x = arg_user_setting) != 0){
        arg_user_setting_eval <- base::lapply(
            X = arg_user_setting_names, 
            FUN = function(x){
                base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = TRUE) # n = 2 because of lapply(), inherit = TRUE to be sure to correctly evaluate
            }
        )
        base::names(x = arg_user_setting_eval) <- arg_user_setting_names
    }else{
        arg_user_setting_eval <- NULL
    }
    # end evaluation of values if they are expression, call, etc.
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 2)), envir = base::parent.frame(n = 1))) # names of all the arguments
    #### end arguments settings

    #### error_text initiation

    ######## basic error text start
    error_text <- base::paste0(base::unlist(x = error_text, recursive = TRUE, use.names = TRUE), collapse = "", recycle0 = FALSE) # convert everything to string. if error_text is a string, changes nothing. If NULL or empty (even list) -> "" so no need to check for management of NULL or empty value
    package_function_name <- base::paste0(
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)), 
        function_name,
        collapse = NULL, 
        recycle0 = FALSE
    )
    error_text_start <- base::paste0(
        "ERROR IN ", # must not be changed, because this "ERROR IN " string is used for text replacement
        package_function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    ######## end basic error text start

    ######## internal error text
    intern_error_text_start <- base::paste0(
        package_function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    intern_error_text_end <- base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, ".", collapse = NULL, recycle0 = FALSE))
    ######## end internal error text

    ######## error text when embedding
    # use this in the error_text of safer functions if present in your main code 
    embed_error_text  <- base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    embed_error_text  <- base::sub(pattern = "\n*$", replacement = "", x = embed_error_text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove all the trailing \n, because added later
    ######## end error text when embedding

    #### end error_text initiation

    #### argument primary checking

    ######## arg ... forbidden
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    if("..." %in% arg_names) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start, 
            "ARGUMENT ... IS NOT ALLOWED IN SAFER-R FUNCTIONS.\n\nPLEASE, REWRITE YOUR FUNCTION CORRECTLY.", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # nocov end
    ######## end arg ... forbidden

    ######## mandatory arg of safer-r functions
    mandat_args <- base::c("lib_path", "safer_check", "error_text")
    tempo_log <- ! mandat_args %in% arg_names
    if(base::any(x = tempo_log, na.rm = TRUE)) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S ARE", no = " IS"), 
            " MANDATORY IN SAFER-R FUNCTIONS:\n", 
            base::paste0(mandat_args[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end mandatory arg of safer-r functions

    ######## arg with no default values
    # optional section: remove the code if none of your arguments has no default value
    no_def_args <- base::c(
        "data1", 
        "y", 
        "categ"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(no_def_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", 
            base::paste0(no_def_args[tempo], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end arg with no default values

    ######## management of NULL arguments
    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "data1", 
        "y", 
        "categ", 
        # "categ.class.order", 
        # "categ.color", 
        # "box.legend.name", 
        "box.fill", 
        "box.width", 
        "box.space", 
        "box.line.size", 
        "box.notch", 
        "box.alpha", 
        "box.mean", 
        "box.whisker.kind", 
        "box.whisker.width", 
        # "dot.color", 
        # "dot.categ", 
        # "dot.categ.class.order", 
        # "dot.legend.name", 
        "dot.tidy", 
        "dot.tidy.bin.nb", 
        "dot.jitter", 
        # "dot_seed", 
        "dot.size", 
        "dot.alpha", 
        "dot.border.size", 
        # "dot.border.color", 
        # "x.lab", 
        "x.angle", 
        # "y.lab", 
        # "y.lim", 
        "y.log", 
        # "y.tick.nb", 
        # "y.second.tick.nb", 
        "y.include.zero", 
        "y.top.extra.margin", 
        "y.bottom.extra.margin", 
        # "stat.pos", 
        "stat.mean", 
        "stat.size", 
        "stat.dist", 
        "stat.angle", 
        "vertical", 
        "text.size", 
        "title", 
        "title.text.size", 
        "legend.show", 
        # "legend.width", 
        "article", 
        "grid", 
        # "add", 
        "return", 
        "return.ggplot", 
        "return.gtable", 
        "plot", 
        "warn.print", 
        "safer_check" 
        # "lib_path", # inactivated because can be NULL
        # "error_text" # inactivated because NULL converted to "" above
    )
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
    if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA with base::is.null()
        tempo_cat <- base::paste0(
            error_text_start, 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
            " CANNOT BE NULL:\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # # before NA checking because is.na(logical()) is logical(0) (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "data1", 
        "y", 
        "categ", 
        "categ.class.order", 
        "categ.color", 
        "box.legend.name", 
        "box.fill", 
        "box.width", 
        "box.space", 
        "box.line.size", 
        "box.notch", 
        "box.alpha", 
        "box.mean", 
        "box.whisker.kind", 
        "box.whisker.width", 
        "dot.color", 
        "dot.categ", 
        "dot.categ.class.order", 
        "dot.legend.name", 
        "dot.tidy", 
        "dot.tidy.bin.nb", 
        "dot.jitter", 
        "dot_seed", 
        "dot.size", 
        "dot.alpha", 
        "dot.border.size", 
        "dot.border.color", 
        "x.lab", 
        "x.angle", 
        "y.lab", 
        "y.lim", 
        "y.log", 
        "y.tick.nb", 
        "y.second.tick.nb", 
        "y.include.zero", 
        "y.top.extra.margin", 
        "y.bottom.extra.margin", 
        "stat.pos", 
        "stat.mean", 
        "stat.size", 
        "stat.dist", 
        "stat.angle", 
        "vertical", 
        "text.size", 
        "title", 
        "title.text.size", 
        "legend.show", 
        "legend.width", 
        "article", 
        "grid", 
        "add", 
        "return", 
        "return.ggplot", 
        "return.gtable", 
        "plot", 
        "warn.print", 
        "safer_check", 
        "lib_path"
        # "error_text" # inactivated because empty value converted to "" above
    )
    tempo_arg_user_setting_eval <- arg_user_setting_eval[base::names(x = arg_user_setting_eval) %in% tempo_arg]
    if(base::length(x = tempo_arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = tempo_arg_user_setting_eval, 
                FUN = function(x){
                    base::length(x = x) == 0 & ! base::is.null(x = x)
                }, 
                simplify = TRUE, 
                USE.NAMES = TRUE
            ), 
            classes = "warning"
        ) # no argument provided by the user can be empty non NULL object. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE AN EMPTY NON NULL OBJECT:\n", 
                base::paste0(base::names(x = tempo_arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # Mandataory section : argument of safer-r functions cannot have NA as only value, to prevent all(, na.rm = TRUE) or any(, na.rm = TRUE) to return a logical value
    if(base::length(x = arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = base::lapply(
                    X = arg_user_setting_eval, 
                    FUN = function(x){
                        base::is.na(x = x) # if x is empty, return empty, but ok with below
                    }
                ), 
                FUN = function(x){
                    base::all(x = x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK
                }, 
                simplify = TRUE, 
                USE.NAMES = TRUE
            ), 
            classes = "warning"
        ) # no argument provided by the user can be just made of NA. is.na(NULL) returns logical(0), the reason why base::length(x = x) > 0 is used # warning: all(x = x, na.rm = TRUE) but normally no NA because base::is.na() used here. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE MADE OF NA ONLY:\n", 
                base::paste0(base::names(x = arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    if( ! (base::all(base::typeof(x = safer_check) == "logical", na.rm = TRUE) & base::length(x = safer_check) == 1)){ # no need to test NA because NA only already managed above and base::length(x = safer_check) == 1)
        if(base::all(base::mode(x = safer_check) == "function", na.rm = TRUE)){
            safer_check <- base::deparse1(expr = safer_check, collapse = "", width.cutoff = 500L)
        }
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).\nHERE IT IS:\n", 
            base::ifelse(test = base::length(x = safer_check) == 0 | base::all(base::suppressWarnings(expr = safer_check == base::quote(expr = ), classes = "warning"), na.rm = TRUE) | base::all(safer_check == "", na.rm = TRUE), yes = "<NULL, \"\", EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(safer_check, collapse = "\n", recycle0 = FALSE)),
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end safer_check argument checking

    ######## check of lib_path
    # must be before any :: or ::: non basic package calling
    if(safer_check == TRUE){ # this line must be inactivated if you want to use lib_path in the main code (other than in safer functions present in the main code) 
        if( ! base::is.null(x = lib_path)){ #  is.null(NA) returns FALSE so OK.
            if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = TRUE)){ # na.rm = TRUE but no NA returned with typeof (typeof(NA) == "character" returns FALSE)
                if(base::all(base::mode(x = lib_path) == "function", na.rm = TRUE)){
                    lib_path <- base::deparse1(expr = lib_path, collapse = "", width.cutoff = 500L)
                }
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\nHERE IT IS:\n", 
                    base::ifelse(test = base::length(x = lib_path) == 0 | base::all(base::suppressWarnings(expr = lib_path == base::quote(expr = ), classes = "warning"), na.rm = TRUE), yes = "<NULL, EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(lib_path, collapse = "\n", recycle0 = FALSE)),
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA. dir.exists(paths = NA) returns an error, so ok. dir.exists(paths = "") returns FALSE so ok
                tempo_log <- ! base::dir.exists(paths = lib_path)
                tempo_cat_b <- lib_path[tempo_log] # here lib_path is character string
                tempo_cat_b[tempo_cat_b == ""] <- "\"\""
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "THE DIRECTORY PATH",
                    base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S", no = ""), 
                    " INDICATED IN THE lib_path ARGUMENT DO", 
                    base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "", no = "ES"), 
                    " NOT EXIST:\n", 
                    base::paste0(tempo_cat_b, collapse = "\n", recycle0 = FALSE), 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                ini_lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
                base::on.exit(expr = base::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
                base::.libPaths(new = base::sub(x = base::c(ini_lib_path, lib_path), pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
                lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            }
        }else{
            lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base::.libPaths(new = lib_path) # or base::.libPaths(new = base::c(base:::.libPaths(), lib_path))
        }
    }  # this line must be inactivated if you want to use lib_path in the main code (other than in safer functions present in the main code) 
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check <- utils::getFromNamespace(x = ".pack_and_function_check", ns = "saferDev", pos = , envir = )
        .pack_and_function_check(
            fun = base::c(
                # functions required in this code
                "ggplot2::aes",
                "ggplot2::aes_string",
                "ggplot2::annotate",
                "ggplot2::coord_cartesian",
                "ggplot2::coord_flip",
                "ggplot2::element_blank",
                "ggplot2::element_line",
                "ggplot2::element_rect",
                "ggplot2::element_text",
                "ggplot2::geom_boxplot",
                "ggplot2::geom_dotplot",
                "ggplot2::geom_path",
                "ggplot2::geom_point",
                "ggplot2::geom_polygon",
                "ggplot2::geom_segment",
                "ggplot2::geom_text",
                "ggplot2::ggplot",
                "ggplot2::ggplot_build",
                "ggplot2::ggtitle",
                "ggplot2::guides",
                "ggplot2::guide_legend",
                "ggplot2::position_dodge",
                "ggplot2::scale_discrete_manual",
                "ggplot2::scale_y_continuous",
                "ggplot2::theme",
                "ggplot2::theme_classic",
                "ggplot2::theme_void",
                "ggplot2::waiver",
                "ggplot2::xlab",
                "ggplot2::ylab",
                "grDevices::colors",
                "gridExtra::grid.arrange",
                "scales::math_format",
                "scales::rescale_none",
                "scales::trans_format",
                "saferDev::arg_check", 
                "saferTool::comp_2d",  
                "saferTool::name_change", 
                "saferTool::round2", 
                "saferGraph::scale2",
                "saferGraph::inter_ticks", 
                "saferGG::gg_palette", 
                # end functions required in this code
                # internal functions required in this code
                "saferDev:::.base_op_check"
                # end internal functions required in this code
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev", pos = , envir = )
    # add here in the internal functions that are used in your main code (copy-paste the line above and replace .base_op_check by the name of the internal function
    # not mandatory if your function is not designed for submission to the CRAN

    ######## end escaping CRAN submission NOTE for internal functions

    ######## critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
            error_text = embed_error_text
        )
    }
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    arg_check_error_text <- base::paste0("ERROR ", embed_error_text, "\n\n", collapse = NULL, recycle0 = FALSE) # must be used instead of error_text = embed_error_text when several arg_check are performed on the same argument (tempo1, tempo2, see below)
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))
    # copy - paste ththe line below as much, one for each of the arguments of your own function
    tempo <- saferDev::arg_check(data = data1, class = "data.frame", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = y, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = categ, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(categ.class.order)){
        tempo <- saferDev::arg_check(data = categ.class.order, class = "list", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "categ.class.order")
    }
    if( ! base::is.null(box.legend.name)){
        tempo <- saferDev::arg_check(data = box.legend.name, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "box.legend.name")
    }
    if( ! base::is.null(categ.color)){
        tempo1 <- saferDev::arg_check(data = categ.color, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        tempo2 <- saferDev::arg_check(data = categ.color, class = "factor", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        checked_arg_names <- base::c(checked_arg_names, tempo2$object.name)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo.check.color <- saferDev::arg_check(data = categ.color, class = "integer", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)$problem
            if(tempo.check.color == TRUE){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "categ.color ARGUMENT MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }else if(base::any(categ.color == 0L, na.rm = TRUE)){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "categ.color ARGUMENT MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }
        }
    }else{
        checked_arg_names <- base::c(checked_arg_names, "categ.color")
    }
    tempo <- saferDev::arg_check(data = box.fill, class = "vector", typeof = NULL, mode = "logical", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box.width, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box.space, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box.line.size, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box.notch, class = "vector", typeof = NULL, mode = "logical", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box.alpha, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box.mean, class = "vector", typeof = NULL, mode = "logical", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box.whisker.kind, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = c("no", "std", "max"), all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = "") ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = box.whisker.width, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(dot.color)){
        tempo1 <- saferDev::arg_check(data = dot.color, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        tempo2 <- saferDev::arg_check(data = dot.color, class = "factor", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        checked_arg_names <- base::c(checked_arg_names, tempo2$object.name)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo.check.color <- saferDev::arg_check(data = dot.color, class = "integer", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)$problem
            if(tempo.check.color == TRUE){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "dot.color MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }else if(base::any(dot.color == 0L, na.rm = TRUE)){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "dot.color MUST BE A FACTOR OR CHARACTER VECTOR OR POSITVE INTEGER VECTOR",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }
        }
    }else{
        checked_arg_names <- base::c(checked_arg_names, "dot.color")
    }
    if( ! base::is.null(dot.categ)){
        tempo <- saferDev::arg_check(data = dot.categ, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "dot.categ")
    }
    if( ! base::is.null(dot.categ.class.order)){
        tempo <- saferDev::arg_check(data = dot.categ.class.order, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "dot.categ.class.order")
    }
    if( ! base::is.null(dot.legend.name)){
        tempo <- saferDev::arg_check(data = dot.legend.name, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "dot.legend.name")
    }
    tempo <- saferDev::arg_check(data = dot.tidy, class = "vector", typeof = NULL, mode = "logical", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = dot.tidy.bin.nb, class = "vector", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(tempo$problem == FALSE){
        if(dot.tidy.bin.nb == 0L){ # length and NA checked above
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                "dot.tidy.bin.nb ARGUMENT MUST BE A NON-NULL AND POSITVE INTEGER VALUE",
                collapse = NULL, 
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
        }
    }
    tempo <- saferDev::arg_check(data = dot.jitter, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(dot_seed)){
        tempo <- saferDev::arg_check(data = dot_seed, class = "vector", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "dot_seed")
    }
    tempo <- saferDev::arg_check(data = dot.size, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = dot.alpha, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = dot.border.size, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(dot.border.color)){
        tempo1 <- saferDev::arg_check(data = dot.border.color, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        tempo2 <- saferDev::arg_check(data = dot.border.color, class = "vector", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        checked_arg_names <- base::c(checked_arg_names, tempo2$object.name)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                "dot.border.color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR STRING STARTING BY #, OR (2) A COLOR NAME GIVEN BY grDevices::colors(), OR (3) AN INTEGER VALUE",
                collapse = NULL, 
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
        }else if(tempo1$problem == FALSE & tempo2$problem == TRUE){
            if( ! base::all(dot.border.color %in% grDevices::colors() | base::grepl(pattern = "^#", dot.border.color), na.rm = TRUE)){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "dot.border.color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR STRING STARTING BY #, OR (2) A COLOR NAME GIVEN BY grDevices::colors(), OR (3) AN INTEGER VALUE",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }
        }
    }else{
        checked_arg_names <- base::c(checked_arg_names, "dot.border.color")
    }
    if( ! base::is.null(x.lab)){
        tempo1 <- saferDev::arg_check(data = x.lab, class = "expression", typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        tempo2 <- saferDev::arg_check(data = x.lab, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        checked_arg_names <- base::c(checked_arg_names, tempo2$object.name)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                "x.lab ARGUMENT MUST BE A SINGLE CHARACTER STRING OR EXPRESSION",
                collapse = NULL, 
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
        }
    }else{
        checked_arg_names <- base::c(checked_arg_names, "x.lab")
    }
    tempo <- saferDev::arg_check(data = x.angle, class = "vector", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(y.lab)){
        tempo1 <- saferDev::arg_check(data = y.lab, class = "expression", typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        tempo2 <- saferDev::arg_check(data = y.lab, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        checked_arg_names <- base::c(checked_arg_names, tempo2$object.name)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                "y.lab ARGUMENT MUST BE A SINGLE CHARACTER STRING OR EXPRESSION",
                collapse = NULL, 
                recycle0 = FALSE
            )
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
        }
    }else{
        checked_arg_names <- base::c(checked_arg_names, "y.lab")
    }
    if( ! base::is.null(y.lim)){
        tempo <- saferDev::arg_check(data = y.lim, class = "vector", typeof = NULL, mode = "numeric", length = 2, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "y.lim")
    }
    tempo <- saferDev::arg_check(data = y.log, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c("no", "log2", "log10"), all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = "") ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(y.tick.nb)){
        tempo <- saferDev::arg_check(data = y.tick.nb, class = "vector", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        if(tempo$problem == FALSE){
            if(y.tick.nb < 0){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "y.tick.nb ARGUMENT MUST BE A NON NULL POSITIVE INTEGER",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }
        }
    }else{
        checked_arg_names <- base::c(checked_arg_names, "y.tick.nb")
    }
    if( ! base::is.null(y.second.tick.nb)){
        tempo <- saferDev::arg_check(data = y.second.tick.nb, class = "vector", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        if(tempo$problem == FALSE){
            if(y.second.tick.nb <= 0){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "y.second.tick.nb ARGUMENT MUST BE A NON NULL POSITIVE INTEGER",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                text_check <- base::c(text_check, tempo_cat)
                argum_check <- base::c(argum_check, TRUE)
            }
        }
    }else{
        checked_arg_names <- base::c(checked_arg_names, "y.second.tick.nb")
    }
    tempo <- saferDev::arg_check(data = y.include.zero, class = "vector", typeof = NULL, mode = "logical", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = y.top.extra.margin, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = y.bottom.extra.margin, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(stat.pos)){
        tempo <- saferDev::arg_check(data = stat.pos, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = c("top", "above"), all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = "") ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "stat.pos")
    }
    tempo <- saferDev::arg_check(data = stat.mean, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = stat.size, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = stat.dist, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = stat.angle, class = "vector", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = vertical, class = "vector", typeof = NULL, mode = "logical", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = text.size, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = title, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = title.text.size, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = legend.show, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(legend.width)){
        tempo <- saferDev::arg_check(data = legend.width, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = TRUE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "legend.width")
    }
    tempo <- saferDev::arg_check(data = article, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = grid, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(add)){
        tempo <- saferDev::arg_check(data = add, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }else{
        checked_arg_names <- base::c(checked_arg_names, "add")
    }
    tempo <- saferDev::arg_check(data = return, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = return.ggplot, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = return.gtable, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = plot, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = warn.print, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    if( ! base::is.null(x = argum_check)){
        if(base::any(argum_check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste0(text_check[argum_check], collapse = "\n\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # check with r_debugging_tools
    # source("https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    # optional section: remove the code if you do not want to check if arguments of mode character of your own function cannot contain ""
    tempo_arg <- base::c(
        "y", 
        "categ", 
        "box.legend.name", 
        "dot.categ", 
        "dot.categ.class.order", 
        "dot.legend.name", 
        # "title", 
        "add"
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())  #  need to test is.null() here
    if(base::any(tempo_log, na.rm = TRUE)){
        # This check is here in case the developer has not correctly fill tempo_arg
        tempo_cat <- base::paste0(
            "INTERNAL ERROR IN THE BACKBONE PART OF ", 
            intern_error_text_start, 
            "IN THE SECTION \"management of \"\" in arguments of mode character\"\n", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS ARE", no = "THIS ARGUMENT IS"), 
            " NOT CLASS \"character\":\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply()).  # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test is.null()
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), 
                base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),
                "\nCANNOT CONTAIN EMPTY STRING \"\".", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }
    }
    # Warning: backbone modified here
    tempo_arg <- base::c(
        "categ.color", 
        "dot.color", 
        "dot.border.color"
        # "x.lab", 
        # "y.lab", 
    )
    # Warning: backbone modified here
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())  #  need to test is.null() here
    if(base::any(tempo_log, na.rm = TRUE)){
        tempo_arg <- tempo_arg[tempo_log]
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply()).  # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test is.null()
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), 
                base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),
                "\nCANNOT CONTAIN EMPTY STRING \"\".", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }
    }
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## code that protects set.seed() in the global environment
    # optional section: remove the code if your own function has no seed argument
    # Warning: seeding is always at the .GlobalEnv level, whenever the seeding is applied inside a function, another envir, etc.
    # Warning: code changed compared to backbone seed -> dot_seed
    if (base::exists(x = ".Random.seed", where = -1, envir = .GlobalEnv, frame = , mode = "any", inherits = TRUE)) {
        # if .Random.seed does not exists, it means that no random operation has been performed yet in any R environment
        tempo.random.seed <- .Random.seed
        base::on.exit(expr = base::assign(x = ".Random.seed", value = tempo.random.seed, pos = -1, envir = .GlobalEnv, inherits = FALSE, immediate = TRUE), add = TRUE, after = TRUE)
    }else{
        base::on.exit(expr = base::set.seed(seed = NULL, kind = NULL, normal.kind = NULL, sample.kind = NULL), add = TRUE, after = TRUE) # inactivate seeding -> return to complete randomness
    }
    base::set.seed(seed = dot_seed, kind = NULL, normal.kind = NULL, sample.kind = NULL) # seed value is the seed argument of the function
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ini_warning_length <- base::options()$warning.length # required to have the max characters of output messages
    base::options(warning.length = 8170)
    warn <- NULL
    warn_count <- 0
    ######## end warning initiation

    ######## graphic device checking
    # optional section: remove the code if no graphics used in your functions
    # check the number of graphic devices on exit
    dev_list <- grDevices::dev.list() 
    # This check is here in case the developer has not correctly fill tempo_arg
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    base::on.exit(
        expr = if(base::length(x = dev_list) != base::length(x = grDevices::dev.list())){
            tempo_cat <- base::paste0(
                "INTERNAL ERROR IN THE BACKBONE PART OF ", 
                intern_error_text_start, 
                "SOME GRAPHIC DEVICES WERE OPENED BY ", 
                function_name, 
                " BUT NOT CLOSED BEFORE END OF EXECUTION.\n\nIF IT IS EXPECTED, JUST REMOVE THE CODE DISPLAYING THIS MESSAGE INSIDE ", 
                function_name, 
                ".\n\nOTHERWISE, THE PROBLEM COMES FROM OPENED GRAPHIC DEVICES BEFORE RUNNING ", 
                function_name, 
                " (n = ", 
                base::length(x = dev_list), 
                ") AND AFTER (n = ", 
                base::length(x = grDevices::dev.list()), 
                ").", 
                intern_error_text_end, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)

        }, 
        add = TRUE, 
        after = TRUE
    )
    # nocov end
    # end check the number of graphic devices on exit
    # restore the graphic parameters on exit
    if(base::length(x = grDevices::dev.list()) > 0){
        par_ini <- base::suppressWarnings(expr = graphics::par(no.readonly = TRUE), classes = "warning") # to recover the present graphical parameters
        base::on.exit(expr = base::suppressWarnings(expr = graphics::par(par_ini, no.readonly = TRUE), classes = "warning"), add = TRUE, after = TRUE)
    }
    # end restore the graphic parameters on exit
    ######## end graphic device checking

    ######## other checkings
    # reserved word checking to avoid bugs (names of dataframe columns used in this function)
    reserved_words <- base::c("categ.check", "categ.color", "dot.color", "dot.categ", "dot.max", "dot.min", "group", "PANEL", "group.check", "MEAN", "tempo_categ1", "tempo_categ2", "text.max.pos", "text.min.pos", "x", "x.y", "y", "y.check", "y_from.dot.max", "ymax", "tidy_group", "binwidth")
    if(base::any(base::names(data1) %in% reserved_words, na.rm = TRUE)){
        if(base::any(base::duplicated(base::names(data1)), na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                "DUPLICATED COLUMN NAMES OF data1 ARGUMENT NOT ALLOWED:\n", base::paste(base::names(data1)[base::duplicated(base::names(data1))], collapse = " "),
                "\nCANNOT CONTAIN EMPTY STRING \"\".", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if( ! base::is.null(dot.categ)){
            if(dot.categ %in% categ){
                reserved_words <- base::c(reserved_words, base::paste0(dot.categ, "_DOT")) # paste0(dot.categ, "_DOT") is added to the reserved words because in such situation, a new column will be added to data1 that is named paste0(dot.categ, "_DOT")
            }
        }
        tempo.output <- saferTool::name_change(base::names(data1), reserved_words)
        for(i2 in 1:base::length(tempo.output$ini)){ # a loop to be sure to take the good ones
            base::names(data1)[base::names(data1) == tempo.output$ini[i2]] <- tempo.output$post[i2]
            if(base::any(y == tempo.output$ini[i2])){ # any() without na.rm -> ok because y cannot be NA (tested above)
                y[y == tempo.output$ini[i2]] <- tempo.output$post[i2]
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") IN y ARGUMENT (COLUMN NAMES OF data1 ARGUMENT),\n", tempo.output$ini[i2], " HAS BEEN REPLACED BY ", tempo.output$post[i2], "\nBECAUSE RISK OF BUG AS SOME NAMES IN y ARGUMENT ARE RESERVED WORD USED BY THE ", function_name, " FUNCTION")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }
            # WARNING: names of y argument potentially replaced
            if(base::any(categ == tempo.output$ini[i2])){ # any() without na.rm -> ok because categ cannot be NA (tested above)
                categ[categ == tempo.output$ini[i2]] <- tempo.output$post[i2]
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") IN categ ARGUMENT (COLUMN NAMES OF data1 ARGUMENT),\n", tempo.output$ini[i2], " HAS BEEN REPLACED BY ", tempo.output$post[i2], "\nBECAUSE RISK OF BUG AS SOME NAMES IN categ ARGUMENT ARE RESERVED WORD USED BY THE ", function_name, " FUNCTION")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }
            # WARNING: names of categ argument potentially replaced
            if( ! base::is.null(dot.categ)){
                if(base::any(dot.categ == tempo.output$ini[i2])){ # any() without na.rm -> ok because dot.categ cannot be NA (tested above)
                    dot.categ[dot.categ == tempo.output$ini[i2]] <- tempo.output$post[i2]
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") IN dot.categ ARGUMENT (COLUMN NAMES OF data1 ARGUMENT),\n", tempo.output$ini[i2], " HAS BEEN REPLACED BY ", tempo.output$post[i2], "\nBECAUSE RISK OF BUG AS SOME NAMES IN dot.categ ARGUMENT ARE RESERVED WORD USED BY THE ", function_name, " FUNCTION")
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                }
            }
            # WARNING: names of dot.categ argument potentially replaced
        }
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") REGARDING COLUMN NAMES REPLACEMENT, THE NAMES\n", base::paste(tempo.output$ini, collapse = " "), "\nHAVE BEEN REPLACED BY\n", base::paste(tempo.output$post, collapse = " "))
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        if( ! (base::is.null(add) | base::is.null(tempo.output$ini))){
            if(base::grepl(x = add, pattern = base::paste(tempo.output$ini, collapse = "|"))){
                tempo_cat <- base::paste0("ERROR IN ", function_name, "\nDETECTION OF COLUMN NAMES OF data1 IN THE add ARGUMENT STRING, THAT CORRESPOND TO RESERVED STRINGS FOR ", function_name, "\nCOLUMN NAMES HAVE TO BE CHANGED\nTHE PROBLEMATIC COLUMN NAMES ARE SOME OF THESE NAMES:\n", base::paste(tempo.output$ini, collapse = " "), "\nIN THE DATA FRAME OF data1 AND IN THE STRING OF add ARGUMENT, TRY TO REPLACE NAMES BY:\n", base::paste(tempo.output$post, collapse = " "), "\n\nFOR INFORMATION, THE RESERVED WORDS ARE:\n", base::paste(reserved_words, collapse = "\n"))
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
        }
    }
    if( ! (base::is.null(add))){
        if(base::any(base::sapply(X = arg.names, FUN = grepl, x = add), na.rm = TRUE)){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") NAMES OF ", function_name, " ARGUMENTS DETECTED IN THE add STRING:\n", base::paste(arg.names[base::sapply(X = arg.names, FUN = grepl, x = add)], collapse = "\n"), "\nRISK OF WRONG OBJECT USAGE INSIDE ", function_name)
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
    }
    # end reserved word checking
    if(base::any(base::duplicated(base::names(data1)), na.rm = TRUE)){
        tempo_cat <- base::paste0("ERROR IN ", function_name, "\nDUPLICATED COLUMN NAMES OF data1 ARGUMENT NOT ALLOWED:\n", base::paste(base::names(data1)[base::duplicated(base::names(data1))], collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! (y %in% base::names(data1))){
        tempo_cat <- base::paste0("ERROR IN ", function_name, "\ny ARGUMENT MUST BE A COLUMN NAME OF data1")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else{

        tempo <- saferDev::arg_check(data = data1[, y], class = "vector", typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = "y COLUMN OF data1", data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo$problem == TRUE){
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\ny ARGUMENT MUST BE NUMERIC COLUMN IN data1")
            base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if(base::length(categ) > 2){
        tempo_cat <- base::paste0("ERROR IN ", function_name, "\ncateg ARGUMENT CANNOT HAVE MORE THAN 2 COLUMN NAMES OF data1")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else if( ! base::all(categ %in% base::names(data1))){ # all() without na.rm -> ok because categ cannot be NA (tested above)
        tempo_cat <- base::paste0("ERROR IN ", function_name, "\ncateg ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS:\n", base::paste(categ, collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(base::length(dot.categ) > 1){
        tempo_cat <- base::paste0("ERROR IN ", function_name, "\ndot.categ ARGUMENT CANNOT HAVE MORE THAN 1 COLUMN NAMES OF data1")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else if( ! base::all(dot.categ %in% base::names(data1))){ # all() without na.rm -> ok because dot.categ cannot be NA (tested above)
        tempo_cat <- base::paste0("ERROR IN ", function_name, "\ndot.categ ARGUMENT MUST BE COLUMN NAMES OF data1. HERE IT IS:\n", base::paste(dot.categ, collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # verif of add
    if( ! base::is.null(add)){
        if( ! base::grepl(pattern = "^\\s*\\+", add)){ # check that the add string start by +
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\nadd ARGUMENT MUST START WITH \"+\": ", base::paste(base::unique(add), collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! base::grepl(pattern = "(ggplot2|lemon)\\s*::", add)){ #
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\nFOR EASIER FUNCTION DETECTION, add ARGUMENT MUST CONTAIN \"ggplot2::\" OR \"lemon::\" IN FRONT OF EACH GGPLOT2 FUNCTION: ", base::paste(base::unique(add), collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! base::grepl(pattern = ")\\s*$", add)){ # check that the add string finished by )
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\nadd ARGUMENT MUST FINISH BY \")\": ", base::paste(base::unique(add), collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end verif of add
    # management of add containing facet
    facet.categ <- NULL
    if( ! base::is.null(add)){
        facet.check <- TRUE
        tempo <- base::unlist(base::strsplit(x = add, split = "\\s*\\+\\s*(ggplot2|lemon)\\s*::\\s*")) #
        tempo <- base::sub(x = tempo, pattern = "^facet_wrap", replacement = "ggplot2::facet_wrap")
        tempo <- base::sub(x = tempo, pattern = "^facet_grid", replacement = "ggplot2::facet_grid")
        tempo <- base::sub(x = tempo, pattern = "^facet_rep", replacement = "lemon::facet_rep")
        if(base::any(base::grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap"), na.rm = TRUE)){
            tempo1 <- base::suppressWarnings(base::eval(base::parse(text = tempo[base::grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap")])))
            facet.categ <- base::names(tempo1$params$facets)
            tempo.text <- "facet_wrap OR facet_rep_wrap"
            facet.check <- FALSE
        }else if(base::grepl(x = add, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid")){
            tempo1 <- base::suppressWarnings(base::eval(base::parse(text = tempo[base::grepl(x = tempo, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid")])))
            facet.categ <- base::c(base::names(tempo1$params$rows), base::names(tempo1$params$cols))
            tempo.text <- "facet_grid OR facet_rep_grid"
            facet.check <- FALSE
        }
        if(facet.check == FALSE & ! base::all(facet.categ %in% base::names(data1))){ # WARNING: all(facet.categ %in% names(data1)) is TRUE when facet.categ is NULL # all() without na.rm -> ok because facet.categ cannot be NA (tested above)
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\nDETECTION OF \"", tempo.text, "\" STRING IN THE add ARGUMENT BUT PROBLEM OF VARIABLE DETECTION (COLUMN NAMES OF data1)\nTHE DETECTED VARIABLES ARE:\n", base::paste(facet.categ, collapse = " "), "\nTHE data1 COLUMN NAMES ARE:\n", base::paste(base::names(data1), collapse = " "), "\nPLEASE REWRITE THE add STRING AND RERUN")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of add containing facet
    # conversion of categ columns in data1 into factors
    for(i1 in 1:base::length(categ)){
        tempo1 <- saferDev::arg_check(data = data1[, categ[i1]], class = "vector", typeof = NULL, mode = "character", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = base::paste0("categ NUMBER ", i1, " OF data1"), data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        tempo2 <- saferDev::arg_check(data = data1[, categ[i1]], class = "factor", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = base::paste0("categ NUMBER ", i1, " OF data1"), data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo_cat <- base::paste0(
                arg_check_error_text, 
                base::paste0("categ NUMBER ", i1, " OF data1"), 
                " MUST BE A FACTOR OR CHARACTER VECTOR.",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if(tempo1$problem == FALSE){ # character vector
            if(box.alpha != 0){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") IN categ NUMBER ", i1, " IN data1, THE CHARACTER COLUMN HAS BEEN CONVERTED TO FACTOR, WITH LEVELS ACCORDING TO THE ALPHABETICAL ORDER")
                warn <-base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }
        }
        data1[, categ[i1]] <- base::factor(data1[, categ[i1]]) # if already a factor, change nothing, if characters, levels according to alphabetical order
    }
    # OK: all the categ columns of data1 are factors from here
    # end conversion of categ columns in data1 into factors
    
    # management of log scale and Inf removal
    if(base::any(( ! base::is.finite(data1[, y])) & ( ! base::is.na(data1[, y])))){ # is.finite also detects NA: ( ! is.finite(data1[, y])) & ( ! is.na(data1[, y])) detects only Inf  # normally no NA with is.finite0() and is.na()
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") PRESENCE OF -Inf OR Inf VALUES IN THE ", y, " COLUMN OF THE data1 ARGUMENT AND CORRESPONDING ROWS REMOVED (SEE $removed.row.nb AND $removed.rows)")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
    }
    data1.ini <- data1 # strictly identical to data1 except that in data1 y is log converted if and only if y.log != "no"
    if(y.log != "no"){
        tempo1 <- ! base::is.finite(data1[, y]) # where are initial NA and Inf
        data1[, y] <- base::suppressWarnings(base::get(y.log)(data1[, y]))# no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
        if(base::any( ! (tempo1 | base::is.finite(data1[, y])))){ # normally no NA with is.finite
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") LOG CONVERSION INTRODUCED -Inf OR Inf OR NaN VALUES IN THE ", y, " COLUMN OF THE data1 ARGUMENT AND CORRESPONDING ROWS REMOVED (SEE $removed.row.nb AND $removed.rows)")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
    }
    # Inf removal
    if(base::any(( ! base::is.finite(data1[, y])) & ( ! base::is.na(data1[, y])))){ # is.finite also detects NA: ( ! is.finite(data1[, y])) & ( ! is.na(data1[, y])) detects only Inf # normally no NA with is.finite
        removed.row.nb <- base::which(( ! base::is.finite(data1[, y])) & ( ! base::is.na(data1[, y])))
        removed.rows <- data1.ini[removed.row.nb, ] # here data1.ini used to have the y = O rows that will be removed because of Inf creation after log transformation
        data1 <- data1[-removed.row.nb, ] #
        data1.ini <- data1.ini[-removed.row.nb, ] #
    }else{
        removed.row.nb <- NULL
        removed.rows <- base::data.frame(stringsAsFactors = FALSE)
    }
    # From here, data1 and data.ini have no more Inf
    # end Inf removal
    if(y.log != "no" & ! base::is.null(y.lim)){
        if(base::any(y.lim <= 0)){ # any() without na.rm -> ok because y.lim cannot be NA (tested above)
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\ny.lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y.log ARGUMENT SET TO ", y.log, ":\n", base::paste(y.lim, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if(base::any( ! base::is.finite(if(y.log == "log10"){base::log10(y.lim)}else{base::log2(y.lim)}))){ # normally no NA with is.finite
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\ny.lim ARGUMENT RETURNS INF/NA WITH THE y.log ARGUMENT SET TO ", y.log, "\nAS SCALE COMPUTATION IS ", base::ifelse(y.log == "log10", "log10", "log2"), ":\n", base::paste(if(y.log == "log10"){base::log10(y.lim)}else{base::log2(y.lim)}, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if(y.log != "no" & y.include.zero == TRUE){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") y.log ARGUMENT SET TO ", y.log, " AND y.include.zero ARGUMENT SET TO TRUE -> y.include.zero ARGUMENT RESET TO FALSE BECAUSE 0 VALUE CANNOT BE REPRESENTED IN LOG SCALE")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        y.include.zero <- FALSE
    }
    if(y.log != "no" & vertical == FALSE){
        vertical <- TRUE
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") BECAUSE OF A BUG IN ggplot2, CANNOT FLIP BOXES HORIZONTALLY WITH A Y.LOG SCALE -> vertical ARGUMENT RESET TO TRUE")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
    }
    # end management of log scale and Inf removal
    # na detection and removal (done now to be sure of the correct length of categ)
    column.check <- base::unique(base::c(y, categ, if( ! base::is.null(dot.color) & ! base::is.null(dot.categ)){dot.categ}, if( ! base::is.null(facet.categ)){facet.categ})) # dot.categ because can be a 3rd column of data1, categ.color and dot.color will be tested later
    if(base::any(base::is.na(data1[, column.check]))){ # data1 used here instead of data1.ini in case of new NaN created by log conversion (neg values) # normally no NA with is.na
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") NA DETECTED IN COLUMNS OF data1 AND CORRESPONDING ROWS REMOVED (SEE $removed.row.nb AND $removed.rows)")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        for(i2 in 1:base::length(column.check)){
            if(base::any(base::is.na(data1[, column.check[i2]]))){ # normally no NA with is.na
                tempo_warn <- base::paste0("NA REMOVAL DUE TO COLUMN ", column.check[i2], " OF data1")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n", tempo_warn)))
            }
        }
        tempo <- base::unique(base::unlist(base::lapply(base::lapply(base::c(data1[column.check]), FUN = is.na), FUN = which)))
        removed.row.nb <- base::c(removed.row.nb, tempo) # removed.row.nb created to remove Inf
        removed.rows <- base::rbind(removed.rows, data1.ini[tempo, ], stringsAsFactors = FALSE) # here data1.ini used to have the non NA rows that will be removed because of NAN creation after log transformation (neg values for instance)
        column.check <- column.check[ ! column.check == y] # remove y to keep quali columns
        if(base::length(tempo) != 0){
            data1 <- data1[-tempo, ] # WARNING tempo here and not removed.row.nb because the latter contain more numbers thant the former
            data1.ini <- data1.ini[-tempo, ] # WARNING tempo here and not removed.row.nb because the latter contain more numbers than the former
            for(i3 in 1:base::length(column.check)){
                if(base::any( ! base::unique(removed.rows[, column.check[i3]]) %in% base::unique(data1[, column.check[i3]]), na.rm = TRUE)){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") IN COLUMN ", column.check[i3], " OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED AFTER NA/Inf REMOVAL (IF COLUMN USED IN THE PLOT, THIS CLASS WILL NOT BE DISPLAYED):\n", base::paste(base::unique(removed.rows[, column.check[i3]])[ ! base::unique(removed.rows[, column.check[i3]]) %in% base::unique(data1[, column.check[i3]])], collapse = " "))
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                }
            }
        }
        count.categ <- 0
        for(i2 in 1:base::length(column.check)){
            if(column.check[i2] %in% categ){
                count.categ <- count.categ + 1
            }
            if(column.check[i2] == categ[count.categ]){
                categ.class.order[count.categ] <- base::list(base::levels(data1[, column.check[i2]])[base::levels(data1[, column.check[i2]]) %in% base::unique(data1[, column.check[i2]])]) # remove the absent color in the character vector
                data1[, column.check[i2]] <- base::factor(base::as.character(data1[, column.check[i2]]), levels = base::unique(categ.class.order[[count.categ]]))
            }
            if( ! base::is.null(dot.color) & ! base::is.null(dot.categ)){ # reminder : dot.categ cannot be a column name of categ anymore (because in that case dot.categ name is changed into "..._DOT"
                if(column.check[i2] == dot.categ){
                    dot.categ.class.order <- base::levels(data1[, column.check[i2]])[base::levels(data1[, column.check[i2]]) %in% base::unique(data1[, column.check[i2]])] # remove the absent color in the character vector
                    data1[, column.check[i2]] <- base::factor(base::as.character(data1[, column.check[i2]]), levels = base::unique(dot.categ.class.order))
                }
            }
            if(column.check[i2] %in% facet.categ){ # works if facet.categ == NULL this method should keep the order of levels when removing some levels
                tempo.levels <- base::levels(data1[, column.check[i2]])[base::levels(data1[, column.check[i2]]) %in% base::unique(base::as.character(data1[, column.check[i2]]))]
                data1[, column.check[i2]] <- base::factor(base::as.character(data1[, column.check[i2]]), levels = tempo.levels)
            }
        }
    }
    # end na detection and removal (done now to be sure of the correct length of categ)
    # From here, data1 and data.ini have no more NA or NaN in y, categ, dot.categ (if dot.color != NULL) and facet.categ
    
    if( ! base::is.null(categ.class.order)){
        if(base::length(categ.class.order) != base::length(categ)){
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\ncateg.class.order ARGUMENT MUST BE A LIST OF LENGTH EQUAL TO LENGTH OF categ\nHERE IT IS LENGTH: ", base::length(categ.class.order), " VERSUS ", base::length(categ))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            for(i3 in 1:base::length(categ.class.order)){
                if(base::is.null(categ.class.order[[i3]])){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") THE categ.class.order COMPARTMENT ", i3, " IS NULL. ALPHABETICAL ORDER WILL BE APPLIED")
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                    data1[, categ[i3]] <- base::factor(base::as.character(data1[, categ[i3]])) # if already a factor, change nothing, if characters, levels according to alphabetical order
                    categ.class.order[[i3]] <- base::levels(data1[, categ[i3]]) # character vector that will be used later
                }else{
                    tempo <- saferDev::arg_check(data = categ.class.order[[i3]], class = "vector", typeof = NULL, mode = "character", length = base::length(base::levels(data1[, categ[i3]])), prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = base::paste0("COMPARTMENT ", i3 , " OF categ.class.order ARGUMENT"), data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)# length(data1[, categ[i1]) -> if data1[, categ[i1] was initially character vector, then conversion as factor after the NA removal, thus class number ok. If data1[, categ[i1] was initially factor, no modification after the NA removal, thus class number ok
                    if(tempo$problem == TRUE){
                        base::stop(base::paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
                    }
                }
                if(base::any(base::duplicated(categ.class.order[[i3]]), na.rm = TRUE)){
                    tempo_cat <- base::paste0("ERROR IN ", function_name, "\nCOMPARTMENT ", i3, " OF categ.class.order ARGUMENT CANNOT HAVE DUPLICATED CLASSES: ", base::paste(categ.class.order[[i3]], collapse = " "))
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
                }else if( ! (base::all(categ.class.order[[i3]] %in% base::unique(data1[, categ[i3]]), na.rm = TRUE) & base::all(base::unique(data1[, categ[i3]]) %in% categ.class.order[[i3]], na.rm = TRUE))){
                    tempo_cat <- base::paste0("ERROR IN ", function_name, "\nCOMPARTMENT ", i3, " OF categ.class.order ARGUMENT MUST BE CLASSES OF ELEMENT ", i3, " OF categ ARGUMENT\nHERE IT IS:\n", base::paste(categ.class.order[[i3]], collapse = " "), "\nFOR COMPARTMENT ", i3, " OF categ.class.order AND IT IS:\n", base::paste(base::unique(data1[, categ[i3]]), collapse = " "), "\nFOR COLUMN ", categ[i3], " OF data1")
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
                }else{
                    data1[, categ[i3]] <- base::factor(data1[, categ[i3]], levels = categ.class.order[[i3]]) # reorder the factor
                    
                }
                base::names(categ.class.order)[i3] <- categ[i3]
            }
        }
    }else{
        categ.class.order <- base::vector("list", length = base::length(categ))
        tempo_categ.class.order <- NULL
        for(i2 in 1:base::length(categ.class.order)){
            categ.class.order[[i2]] <- base::levels(data1[, categ[i2]])
            base::names(categ.class.order)[i2] <- categ[i2]
            tempo_categ.class.order <- c(tempo_categ.class.order, base::ifelse(i2 != 1, "\n", ""), categ.class.order[[i2]])
        }
        if(box.alpha != 0){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") THE categ.class.order SETTING IS NULL. ALPHABETICAL ORDER WILL BE APPLIED FOR BOX ORDERING:\n", base::paste(tempo_categ.class.order, collapse = " "))
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
    }
    # categ.class.order not NULL anymore (list)
    if(base::is.null(box.legend.name) & box.alpha != 0){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") THE box.legend.name SETTING IS NULL. NAMES OF categ WILL BE USED: ", base::paste(categ, collapse = " "))
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        box.legend.name <- categ[base::length(categ)] # if only categ1, then legend name of categ1, if length(categ) == 2L, then legend name of categ2
    }
    # box.legend.name not NULL anymore (character string)
    # management of categ.color
    if( ! base::is.null(categ.color)){
        # check the nature of color
        # integer colors into gg_palette
        tempo.check.color <- saferDev::arg_check(data = categ.color, class = "vector", typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = "y COLUMN OF data1", data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)$problem
        if(tempo.check.color == FALSE){
            # convert integers into colors
            categ.color <- saferGG::gg_palette(base::max(categ.color, na.rm = TRUE))[categ.color]
        }
        # end integer colors into gg_palette
        if( ! (base::all(categ.color %in% grDevices::colors() | base::grepl(pattern = "^#", categ.color)))){ # check that all strings of low.color start by #, # all() without na.rm -> ok because categ.color cannot be NA (tested above)
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\ncateg.color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY grDevices::colors() OR A COLUMN NAME OF THE data1 PARAMETER: ", base::paste(base::unique(categ.color), collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(base::any(base::is.na(categ.color)) & box.alpha != 0){ # normally no NA with is.na
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") categ.color ARGUMENT CONTAINS NA")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
        # end check the nature of color
        # check the length of color
        categ.len <- base::length(categ) # if only categ1, then colors for classes of categ1, if length(categ) == 2L, then colors for classes of categ2
        if(base::length(data1[, categ[categ.len]]) == base::length(base::levels(data1[, categ[categ.len]])) & base::length(categ.color) == base::length(data1[, categ[categ.len]])){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") THE NUMBER OF CLASSES OF THE COLUMN ", categ[categ.len], " THE NUMBER OF ROWS OF THIS COLUMN AND THE NUMBER OF COLORS OF THE categ.color ARGUMENT ARE ALL EQUAL. BOX COLORS WILL BE ATTRIBUTED ACCORDING THE LEVELS OF ", categ[categ.len], ", NOT ACCORDING TO THE ROWS OF ", categ[categ.len])
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
        if(base::length(categ.color) == base::length(base::levels(data1[, categ[categ.len]]))){ # here length(categ.color) is equal to the different number of categ
            # data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
            data1 <- base::data.frame(data1, categ.color = data1[, categ[categ.len]], stringsAsFactors = TRUE)  # no need stringsAsFactors here for stat.nolog as factors remain factors
            data1$categ.color <- base::factor(data1$categ.color, labels = categ.color) # replace the characters of data1[, categ[categ.len]] put in the categ.color column by the categ.color (can be write like this because categ.color is length of levels of data1[, categ[categ.len]])
            if(box.alpha != 0){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") IN ", categ[categ.len], " OF categ ARGUMENT, THE FOLLOWING COLORS:\n", base::paste(categ.color, collapse = " "), "\nHAVE BEEN ATTRIBUTED TO THESE CLASSES:\n", base::paste(base::levels(base::factor(data1[, categ[categ.len]])), collapse = " "))
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }
        }else if(base::length(categ.color) == base::length(data1[, categ[categ.len]])){# here length(categ.color) is equal to nrow(data1) -> Modif to have length(categ.color) equal to the different number of categ (length(categ.color) == length(levels(data1[, categ[categ.len]])))
            data1 <- base::data.frame(data1, categ.color = categ.color, stringsAsFactors = TRUE)
            tempo.check <- base::unique(data1[ , base::c(categ[categ.len], "categ.color")])
            if( ! (base::nrow(tempo.check) == base::length(base::unique(categ.color)) & base::nrow(tempo.check) == base::length(base::unique(data1[ , categ[categ.len]])))){
                tempo_cat <- base::paste0("ERROR IN ", function_name, "\ncateg.color ARGUMENT HAS THE LENGTH OF data1 ROW NUMBER\nBUT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF categ ", categ[categ.len], ":\n", base::paste(base::unique(base::mapply(FUN = "paste", data1[ ,categ[categ.len]], data1[ ,"categ.color"])), collapse = "\n"))
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                # data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
                categ.color <- base::unique(data1$categ.color[base::order(data1[, categ[categ.len]])]) # Modif to have length(categ.color) equal to the different number of categ (length(categ.color) == length(levels(data1[, categ[categ.len]])))
                if(box.alpha != 0){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") categ.color ARGUMENT HAS THE LENGTH OF data1 ROW NUMBER\nCOLORS HAVE BEEN RESPECTIVELY ASSOCIATED TO EACH CLASS OF categ ", categ[categ.len], " AS:\n", base::paste(base::levels(base::factor(data1[, categ[categ.len]])), collapse = " "), "\n", base::paste(categ.color, collapse = " "))
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                }
            }
        }else if(base::length(categ.color) == 1L){
            # data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
            data1 <- base::data.frame(data1, categ.color = categ.color, stringsAsFactors = TRUE)
            categ.color <- base::rep(categ.color, base::length(base::levels(data1[, categ[categ.len]])))
            if(box.alpha != 0){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") categ.color ARGUMENT HAS LENGTH 1, MEANING THAT ALL THE DIFFERENT CLASSES OF ", categ[categ.len], "\n", base::paste(base::levels(base::factor(data1[, categ[categ.len]])), collapse = " "), "\nWILL HAVE THE SAME COLOR\n", base::paste(categ.color, collapse = " "))
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }
        }else{
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\ncateg.color ARGUMENT MUST BE (1) LENGTH 1, OR (2) THE LENGTH OF data1 NROWS AFTER NA/Inf REMOVAL, OR (3) THE LENGTH OF THE CLASSES IN THE categ ", categ[categ.len], " COLUMN. HERE IT IS COLOR LENGTH ", base::length(categ.color), " VERSUS CATEG LENGTH ", base::length(data1[, categ[categ.len]]), " AND CATEG CLASS LENGTH ", base::length(base::unique(data1[, categ[categ.len]])), "\nPRESENCE OF NA/Inf COULD BE THE PROBLEM")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }else{
        categ.len <- base::length(categ) # if only categ1, then colors for classes of categ1, if length(categ) == 2L, then colors for classes of categ2
        # data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
        categ.color <- saferGG::gg_palette(base::length(base::levels(data1[, categ[categ.len]])))
        data1 <- base::data.frame(data1, categ.color = data1[, categ[categ.len]], stringsAsFactors = TRUE)
        data1$categ.color <- base::factor(data1$categ.color, labels = categ.color)  # replace the characters of data1[, categ[categ.len]] put in the categ.color column by the categ.color (can be write like this because categ.color is length of levels of data1[, categ[categ.len]])
        if(box.alpha != 0){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") NULL categ.color ARGUMENT -> COLORS RESPECTIVELY ATTRIBUTED TO EACH CLASS OF ", categ[categ.len], " IN data1:\n", base::paste(categ.color, collapse = " "), "\n", base::paste(base::levels(data1[, categ[categ.len]]), collapse = " "))
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
    }
    # categ.color not NULL anymore
    categ.color <- base::as.character(categ.color)
    # categ.color is a character string representing the diff classes
    data1$categ.color <- base::factor(data1$categ.color, levels = base::unique(categ.color)) # ok because if categ.color is a character string, the order make class 1, class 2, etc. unique() because no duplicates allowed
    # data1$categ.color is a factor with order of levels -> categ.color
    # end management of categ.color
    # management of dot.color
    if( ! base::is.null(dot.color)){
        # optional legend of dot colors
        if( ! base::is.null(dot.categ)){
            ini.dot.categ <- dot.categ
            if( ! dot.categ %in% base::names(data1)){ # no need to use all() because length(dot.categ) = 1
                tempo_cat <- base::paste0("ERROR IN ", function_name, "\ndot.categ ARGUMENT MUST BE A COLUMN NAME OF data1. HERE IT IS:\n", dot.categ)
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else if(dot.categ %in% categ){ # no need to use all() because length(dot.categ) = 1. Do not use dot.categ %in% categ[length(categ)] -> error
                # management of dot legend if dot.categ %in% categ (because legends with the same name are joined in ggplot2) 
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") THE COLUMN NAME OF data1 INDICATED IN THE dot.categ ARGUMENT (", dot.categ, ") HAS BEEN REPLACED BY ", base::paste0(dot.categ, "_DOT"), " TO AVOID MERGED LEGEND BY GGPLOT2")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                data1 <- base::data.frame(data1, dot.categ = data1[, dot.categ], stringsAsFactors = TRUE) # dot.categ is not a column name of data1 (checked above with reserved words)
                dot.categ <- base::paste0(dot.categ, "_DOT")
                base::names(data1)[base::names(data1) == "dot.categ"] <- dot.categ # paste0(dot.categ, "_DOT") is not a column name of data1 (checked above with reserved words)
                # tempo_cat <- paste0("ERROR IN ", function_name, "\ndot.categ ARGUMENT CANNOT BE A COLUMN NAME OF data1 ALREADY SPECIFIED IN THE categ ARGUMENT:\n", dot.categ, "\nINDEED, dot.categ ARGUMENT IS MADE TO HAVE MULTIPLE DOT COLORS NOT RELATED TO THE BOXPLOT CATEGORIES")
                # stop(paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            tempo1 <- saferDev::arg_check(data = data1[, dot.categ], class = "vector", typeof = NULL, mode = "character", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = base::paste0(dot.categ, " COLUMN OF data1"), data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            tempo2 <- saferDev::arg_check(data = data1[, categ[i1]], class = "factor", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = base::paste0(dot.categ, " COLUMN OF data1"), data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo1$problem == TRUE & tempo2$problem == TRUE){
                tempo_cat <- base::paste0(
                    arg_check_error_text, 
                    "dot.categ COLUMN MUST BE A FACTOR OR CHARACTER VECTOR.",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            data1[, dot.categ] <- base::factor(data1[, dot.categ]) # if already a factor, change nothing, if characters, levels according to alphabetical order
            # dot.categ column of data1 is factor from here
            if( ! base::is.null(dot.categ.class.order)){
                if(base::any(base::duplicated(dot.categ.class.order), na.rm = TRUE)){
                    tempo_cat <- base::paste0(
                        error_text_start, 
                        "dot.categ.class.order ARGUMENT CANNOT HAVE DUPLICATED CLASSES: ", base::paste(dot.categ.class.order, collapse = " ", recycle0 = FALSE),
                        collapse = NULL, 
                        recycle0 = FALSE
                    )
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
                }else if( ! (base::all(dot.categ.class.order %in% base::levels(data1[, dot.categ])) & base::all(base::levels(data1[, dot.categ]) %in% dot.categ.class.order, na.rm = TRUE))){
                    tempo_cat <- base::paste0(
                        error_text_start, 
                        "dot.categ.class.order ARGUMENT MUST BE CLASSES OF dot.categ ARGUMENT\nHERE IT IS:\n", 
                        base::paste(dot.categ.class.order, collapse = " "), 
                        "\nFOR dot.categ.class.order AND IT IS:\n", 
                        base::paste(base::levels(data1[, dot.categ]), collapse = " "), 
                        "\nFOR dot.categ COLUMN (", 
                        ini.dot.categ, 
                        ") OF data1",
                        collapse = NULL, 
                        recycle0 = FALSE
                    )
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
                }else{
                    data1[, dot.categ] <- base::factor(data1[, dot.categ], levels = dot.categ.class.order) # reorder the factor
                }
            }else{
                if(base::all(dot.color == "same") & base::length(dot.color)== 1L){ # all() without na.rm -> ok because dot.color cannot be NA (tested above)
                    dot.categ.class.order <-base::unlist(categ.class.order[base::length(categ)])
                    data1[, dot.categ] <- base::factor(data1[, dot.categ], levels = dot.categ.class.order) # reorder the factor
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") THE dot.categ.class.order SETTING IS NULL AND dot.color IS \"same\". ORDER OF categ.class.order WILL BE APPLIED FOR LEGEND DISPLAY: ", base::paste(dot.categ.class.order, collapse = " "))
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                }else{
                    dot.categ.class.order <- base::sort(base::levels(data1[, dot.categ]))
                    data1[, dot.categ] <- base::factor(data1[, dot.categ], levels = dot.categ.class.order) # reorder the factor
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") THE dot.categ.class.order SETTING IS NULL. ALPHABETICAL ORDER WILL BE APPLIED FOR LEGEND DISPLAY: ", base::paste(dot.categ.class.order, collapse = " "))
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                }
            }
            # dot.categ.class.order not NULL anymore (character string) if dot.categ is not NULL
            if(base::all(dot.color == "same") & base::length(dot.color)== 1L){ # all() without na.rm -> ok because dot.color cannot be NA (tested above)
                if( ! base::identical(ini.dot.categ, categ[base::length(categ)])){
                    tempo_cat <- base::paste0("ERROR IN ", function_name, "\nWHEN dot.color ARGUMENT IS \"same\", THE COLUMN NAME IN dot.categ ARGUMENT MUST BE IDENTICAL TO THE LAST COLUMN NAME IN categ ARGUMENT. HERE IT IS:\ndot.categ: ", base::paste(ini.dot.categ, collapse = " "), "\ncateg: ", base::paste(categ, collapse = " "))
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
                }else if( ! fun_comp_1d(base::unlist(categ.class.order[base::length(categ)]), dot.categ.class.order)$identical.content){
                    tempo_cat <- base::paste0("ERROR IN ", function_name, "\nWHEN dot.color ARGUMENT IS \"same\",\nLAST COMPARTMENT OF categ.class.order ARGUMENT AND dot.categ.class.order ARGUMENT CANNOT BE DIFFERENT:\nLAST COMPARTMENT OF categ.class.order: ", base::paste(base::unlist(categ.class.order[base::length(categ)]), collapse = " "), "\ndot.categ.class.order: ", base::paste(dot.categ.class.order, collapse = " "))
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
                }
            }
            for(i3 in 1:base::length(categ)){
                if(base::identical(categ[i3], ini.dot.categ) & ! base::identical(base::unlist(categ.class.order[i3]), dot.categ.class.order) & base::identical(base::sort(base::unlist(categ.class.order[i3])), base::sort(dot.categ.class.order))){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") THE dot.categ ARGUMENT SETTING IS PRESENT IN THE categ ARGUMENT SETTING, BUT ORDER OF THE CLASSES IS NOT THE SAME:\ncateg.class.order: ", paste(base::unlist(categ.class.order[i3]), collapse = " "), "\ndot.categ.class.order: ", base::paste(dot.categ.class.order, collapse = " "), "\nNOTE THAT ORDER OF categ.class.order IS THE ONE USED FOR THE AXIS REPRESENTATION")
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                }
            }
            if(base::is.null(dot.legend.name)){
                dot.legend.name <- if(ini.dot.categ %in% categ[base::length(categ)]){dot.categ}else{ini.dot.categ} #
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") THE dot.legend.name SETTING IS NULL -> ", dot.legend.name, " WILL BE USED AS LEGEND TITLE OF DOTS")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }
            # dot.legend.name not NULL anymore (character string)
        }else{
            if( ! base::is.null(dot.categ.class.order)){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") THE dot.categ.class.order ARGUMENT IS NOT NULL, BUT IS THE dot.categ ARGUMENT\n-> dot.categ.class.order NOT CONSIDERED AS NO LEGEND WILL BE DRAWN")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }
            # But dot.categ.class.order will be converted to NULL below (not now)
        }
        # end optional legend of dot colors
        # check the nature of color
        # integer colors into gg_palette
        tempo.check.color <- saferDev::arg_check(data = dot.color, class = "vector", typeof = "integer", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = base::paste0(dot.categ, " COLUMN OF data1"), data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)$problem
        if(tempo.check.color == FALSE){
            # convert integers into colors
            dot.color <- saferGG::gg_palette(base::max(dot.color, na.rm = TRUE))[dot.color]
        }
        # end integer colors into gg_palette
        if(base::all(dot.color == "same") & base::length(dot.color)== 1L){# all() without na.rm -> ok because dot.color cannot be NA (tested above)
            dot.color <- categ.color # same color of the dots as the corresponding box color
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") dot.color ARGUMENT HAS BEEN SET TO \"same\"\nTHUS, DOTS WILL HAVE THE SAME COLORS AS THE CORRESPONDING BOXPLOT")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }else if( ! (base::all(dot.color %in% grDevices::colors() | base::grepl(pattern = "^#", dot.color)))){ # check that all strings of low.color start by #, # all() without na.rm -> ok because dot.color cannot be NA (tested above)
            tempo_cat <- base::paste0(
                error_text_start, 
                "dot.color ARGUMENT MUST BE (1) A HEXADECIMAL COLOR VECTOR STARTING BY #, OR (2) COLOR NAMES GIVEN BY grDevices::colors(), OR (3) INTEGERS, OR THE STRING \"same\"\nHERE IT IS: ", base::paste(base::unique(dot.color), collapse = " ", recycle0 = FALSE),
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(base::any(base::is.na(dot.color))){ # normally no NA with is.finite
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") dot.color ARGUMENT CONTAINS NA")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
        # end check the nature of color
        # check the length of color
        if( ! base::is.null(dot.categ)){
            # optional legend of dot colors
            if(base::length(data1[, dot.categ]) == base::length(base::levels(data1[, dot.categ])) & base::length(dot.color) == base::length(data1[, dot.categ])){
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") THE NUMBER OF CLASSES OF THE COLUMN ", dot.categ, " THE NUMBER OF ROWS OF THIS COLUMN AND THE NUMBER OF COLORS OF THE dot.color ARGUMENT ARE ALL EQUAL. DOT COLORS WILL BE ATTRIBUTED ACCORDING THE LEVELS OF ", dot.categ, ", NOT ACCORDING TO THE ROWS OF ", dot.categ)
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }
            if(base::length(dot.color) > 1 & ! (base::length(dot.color) == base::length(base::unique(data1[, dot.categ])) | base::length(dot.color) == base::length(data1[, dot.categ]))){
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "WHEN LENGTH OF THE dot.color ARGUMENT IS MORE THAN 1, IT MUST BE EQUAL TO THE NUMBER OF 1) ROWS OR 2) LEVELS OF dot.categ COLUMN (", dot.categ, "):\ndot.color: ", base::paste(dot.color, collapse = " "), "\ndot.categ LEVELS: ", base::paste(base::levels(data1[, dot.categ]), collapse = " ", recycle0 = FALSE),
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else if(base::length(dot.color) > 1 & base::length(dot.color) == base::length(base::unique(data1[, dot.categ]))){
                data1 <-base::data.frame(data1, dot.color = data1[, dot.categ], stringsAsFactors = TRUE)
                data1$dot.color <- base::factor(data1$dot.color, labels = dot.color) # do not use labels = unique(dot.color). Otherwise, we can have green1 green2 when dot.color is c("green", "green")
            }else if(base::length(dot.color) > 1 & base::length(dot.color) == base::length(data1[, dot.categ])){
                data1 <- base::data.frame(data1, dot.color = dot.color, stringsAsFactors = TRUE)
            }else if(base::length(dot.color)== 1L){ # to deal with single color. Warning: & length(dot.categ.class.order) > 1 removed because otherwise, the data1 is not with dot.color column when length(dot.categ.class.order) == 1
                data1 <- base::data.frame(data1, dot.color = dot.color, stringsAsFactors = TRUE)
            }
            dot.color <- base::as.character(base::unique(data1$dot.color[base::order(data1[, dot.categ])])) # reorder the dot.color character vector
            if(base::length(dot.color)== 1L & base::length(dot.categ.class.order) > 1){ # to deal with single color
                dot.color <- base::rep(dot.color, base::length(dot.categ.class.order))
            }
            tempo.check <- base::unique(data1[ , base::c(dot.categ, "dot.color")])
            if(base::length(base::unique(data1[ , "dot.color"])) > 1 & ( ! (base::nrow(tempo.check) == base::length(base::unique(data1[ , "dot.color"])) & base::nrow(tempo.check) == base::length(base::unique(data1[ , dot.categ]))))){ # length(unique(data1[ , "dot.color"])) > 1 because if only one color, can be attributed to each class of dot.categ
                tempo_cat <- base::paste0("ERROR IN ", function_name, "\ndot.color ARGUMENT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF dot.categ (", dot.categ, ") COLUMN:\n", base::paste(base::unique(base::mapply(FUN = "paste", data1[ , dot.categ], data1[ ,"dot.color"])), collapse = "\n"))
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") IN dot.categ ARGUMENT (", ini.dot.categ, "), THE FOLLOWING COLORS OF DOTS:\n", base::paste(dot.color, collapse = " "), "\nHAVE BEEN ATTRIBUTED TO THESE CLASSES:\n", base::paste(base::levels(data1[, dot.categ]), collapse = " "))
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }
            # dot.color is a character string representing the diff classes of dot.categ
            # data1$dot.color is a factor with order of levels -> dot.categ
            # end optional legend of dot colors
        }else{
            categ.len <- base::length(categ) # if only categ1, then colors for classes of categ1, if length(categ) == 2L, then colors for classes of categ2
            if(base::length(dot.color) == base::length(base::levels(data1[, categ[categ.len]]))){ # here length(dot.color) is equal to the different number of categ
                # data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
                data1 <- base::data.frame(data1, dot.color = data1[, categ[categ.len]], stringsAsFactors = TRUE)
                data1$dot.color <- base::factor(data1$dot.color, labels = dot.color)
                if(box.alpha != 0){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") IN ", categ[categ.len], " OF categ ARGUMENT, THE FOLLOWING COLORS:\n", base::paste(dot.color, collapse = " "), "\nHAVE BEEN ATTRIBUTED TO THESE CLASSES:\n", base::paste(base::levels(base::factor(data1[, categ[categ.len]])), collapse = " "))
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                }
            }else if(base::length(dot.color) == base::length(data1[, categ[categ.len]])){# here length(dot.color) is equal to nrow(data1) -> Modif to have length(dot.color) equal to the different number of categ (length(dot.color) == length(levels(data1[, categ[categ.len]])))
                data1 <- base::data.frame(data1, dot.color = dot.color, stringsAsFactors = TRUE)
            }else if(base::length(dot.color)== 1L & ! base::all(dot.color == "same")){ # all() without na.rm -> ok because dot.color cannot be NA (tested above)
                # data1[, categ[categ.len]] <- factor(data1[, categ[categ.len]]) # not required because sure that is is a factor
                data1 <- base::data.frame(data1, dot.color = dot.color, stringsAsFactors = TRUE)
                dot.color <- base::rep(dot.color, base::length(base::levels(data1[, categ[categ.len]])))
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0("(", warn_count,") dot.color ARGUMENT HAS LENGTH 1, MEANING THAT ALL THE DIFFERENT CLASSES OF ", categ[categ.len], "\n", base::paste(base::levels(base::factor(data1[, categ[categ.len]])), collapse = " "), "\nWILL HAVE THE SAME COLOR\n", base::paste(dot.color, collapse = " "))
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            }else{
                tempo_cat <- base::paste0("ERROR IN ", function_name, "\ndot.color ARGUMENT MUST BE (1) LENGTH 1, OR (2) THE LENGTH OF data1 NROWS AFTER NA/Inf REMOVAL, OR (3) THE LENGTH OF THE CLASSES IN THE categ ", categ[categ.len], " COLUMN. HERE IT IS COLOR LENGTH ", base::length(dot.color), " VERSUS CATEG LENGTH ", base::length(data1[, categ[categ.len]]), " AND CATEG CLASS LENGTH ", base::length(base::unique(data1[, categ[categ.len]])), "\nPRESENCE OF NA/Inf COULD BE THE PROBLEM")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            # end check the length of color
            dot.color <- base::as.character(dot.color)
            # dot.color is a character string representing the diff classes
            data1$dot.color <- base::factor(data1$dot.color, levels = base::unique(dot.color)) # ok because if dot.color is a character string, the order make class 1, class 2, etc. If dot.color is a column of data1, then levels will be created, without incidence, except if dot.categ specified (see below). unique() because no duplicates allowed
            # data1$dot.color is a factor with order of levels -> dot.color
        }
        # end optional legend of dot colors
    }else if(base::is.null(dot.color) & ! (base::is.null(dot.categ) & base::is.null(dot.categ.class.order) & base::is.null(dot.legend.name))){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") dot.categ OR dot.categ.class.order OR dot.legend.name ARGUMENT HAS BEEN SPECIFIED BUT dot.color ARGUMENT IS NULL (NO DOT PLOTTED)")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
    }
    # dot.color either NULL (no dot plotted) or character string (potentially representing the diff classes of dot.categ)
    # data1$dot.color is either NA or a factor (with order of levels -> depending on dot.categ or categ[length(categ)], or other
    if(base::is.null(dot.categ)){
        dot.categ.class.order <- NULL # because not used anyway
    }
    # dot.categ.class.order either NULL if dot.categ is NULL (no legend displayed) or character string (potentially representing the diff classes of dot.categ)
    # end management of dot.color
    if(base::is.null(dot.color) & box.fill == FALSE & dot.alpha <= 0.025){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") THE FOLLOWING ARGUMENTS WERE SET AS:\ndot.color = NULL (NOT ALL DOTS BUT ONLY POTENTIAL OUTLIER DOTS DISPLAYED)\nbox.fill = FALSE (NO FILLING COLOR FOR BOTH BOXES AND POTENTIAL OUTLIER DOTS)\ndot.alpha = ", saferTool::round2(dot.alpha, 4), "\n-> POTENTIAL OUTLIER DOTS MIGHT NOT BE VISIBLE BECAUSE ALMOST TRANSPARENT")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
    }
    if(base::is.null(dot.color) & box.fill == FALSE & dot.border.size == 0){
        tempo_cat <- base::paste0("ERROR IN ", function_name, "\nTHE FOLLOWING ARGUMENTS WERE SET AS:\ndot.color = NULL (NOT ALL DOTS BUT ONLY POTENTIAL OUTLIER DOTS DISPLAYED)\nbox.fill = FALSE (NO FILLING COLOR FOR BOTH BOXES AND POTENTIAL OUTLIER DOTS)\ndot.border.size = 0 (NO BORDER FOR POTENTIAL OUTLIER DOTS)\n-> THESE SETTINGS ARE NOT ALLOWED BECAUSE THE POTENTIAL OUTLIER DOTS WILL NOT BE VISIBLE")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # integer dot.border.color into gg_palette
    if( ! base::is.null(dot.border.color)){
        tempo <- saferDev::arg_check(data = dot.border.color, class = "vector", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = FALSE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo$problem == FALSE){ # convert integers into colors
            dot.border.color <- saferGG::gg_palette(base::max(dot.border.color, na.rm = TRUE))[dot.border.color]
        }
    }
    # end integer dot.border.color into gg_palette
    # na detection and removal (done now to be sure of the correct length of categ)
    column.check <- base::c("categ.color", if( ! base::is.null(dot.color)){"dot.color"}) # 
    if(base::any(base::is.na(data1[, column.check]))){ # data1 used here instead of data1.ini in case of new NaN created by log conversion (neg values) # normally no NA with is.na
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") NA DETECTED IN COLUMNS ", base::paste(column.check, collapse = " "), " OF data1 AND CORRESPONDING ROWS REMOVED (SEE $removed.row.nb AND $removed.rows)")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        for(i2 in 1:base::length(column.check)){
            if(base::any(base::is.na(data1[, column.check[i2]]))){ # normally no NA with is.na
                tempo_warn <- base::paste0("NA REMOVAL DUE TO COLUMN ", column.check[i2], " OF data1")
                warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n", tempo_warn)))
            }
        }
        tempo <- base::unique(base::unlist(base::lapply(base::lapply(base::c(data1[column.check]), FUN = is.na), FUN = which)))
        removed.row.nb <- base::c(removed.row.nb, tempo)
        removed.rows <- base::rbind(removed.rows, data1[tempo, ], stringsAsFactors = FALSE) # here data1 used because categorical columns tested
        if(base::length(tempo) != 0){
            data1 <- data1[-tempo, ] # WARNING tempo here and not removed.row.nb because the latter contain more numbers thant the former
            data1.ini <- data1.ini[-tempo, ] # WARNING tempo here and not removed.row.nb because the latter contain more numbers thant the former
            for(i3 in 1:base::length(column.check)){
                if(base::any( ! base::unique(removed.rows[, column.check[i3]]) %in% base::unique(data1[, column.check[i3]]), na.rm = TRUE)){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") IN COLUMN ", column.check[i3], " OF data1, THE FOLLOWING CLASSES HAVE DISAPPEARED AFTER NA/Inf REMOVAL (IF COLUMN USED IN THE PLOT, THIS CLASS WILL NOT BE DISPLAYED):\n", base::paste(unique(removed.rows[, column.check[i3]])[ ! base::unique(removed.rows[, column.check[i3]]) %in% base::unique(data1[, column.check[i3]])], collapse = " "))
                    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
                }
            }
        }
        for(i2 in 1:base::length(column.check)){
            if(column.check[i2] == "categ.color"){
                categ.color <- base::levels(data1[, column.check[i2]])[base::levels(data1[, column.check[i2]]) %in% base::unique(data1[, column.check[i2]])] # remove the absent color in the character vector
                if(base::length(categ.color)== 1L & base::length(base::unlist(categ.class.order[base::length(categ)])) > 1){ # to deal with single color
                    categ.color <- base::rep(categ.color, base::length(base::unlist(categ.class.order[base::length(categ)])))
                }
                data1[, column.check[i2]] <- base::factor(base::as.character(data1[, column.check[i2]]), levels = base::unique(categ.color))
            }
            if(column.check[i2] == "dot.color"){
                dot.color <- base::levels(data1[, column.check[i2]])[base::levels(data1[, column.check[i2]]) %in% base::unique(data1[, column.check[i2]])] # remove the absent color in the character vector
                if(base::length(dot.color)== 1L & base::length(dot.categ.class.order) > 1){ # to deal with single color. If dot.categ.class.order == NULL (which is systematically the case if dot.categ == NULL), no rep(dot.color, length(dot.categ.class.order)
                    dot.color <- base::rep(dot.color, base::length(dot.categ.class.order))
                }
                data1[, column.check[i2]] <- base::factor(base::as.character(data1[, column.check[i2]]), levels = base::unique(dot.color))
            }
        }
    }
    # end na detection and removal (done now to be sure of the correct length of categ)
    # From here, data1 and data.ini have no more NA or NaN

    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code

    # y coordinates recovery (create ini.box.coord, dot.coord and modify data1)
    if(base::length(categ)== 1L){
        # width commputations
        box.width2 <- box.width
        box.space <- 0 # to inactivate the shrink that add space between grouped boxes, because no grouped boxes here
        # end width commputations
        # data1 check categ order for dots coordinates recovery
        data1 <- base::data.frame(data1, categ.check = data1[, categ[1]], stringsAsFactors = TRUE)
        data1$categ.check <- base::as.integer(data1$categ.check) # to check that data1[, categ[1]] and dot.coord$group are similar, during merging
        # end data1 check categ order for dots coordinates recovery
        # per box dots coordinates recovery
        tempo.gg.name <- "gg.indiv.plot."
        tempo.gg.count <- 0
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), base::eval(base::parse(text = base::paste0("ggplot2::ggplot()", if(base::is.null(add)){""}else{add})))) # add added here to have the facets
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, color = categ[1]), stroke = dot.border.size, size = dot.size, alpha = dot.alpha, shape = 21))
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "color", name = box.legend.name, values = if(base::is.null(categ.color)){base::rep(NA, base::length(base::unique(data1[, categ[1]])))}else if(base::length(categ.color)== 1L){base::rep(categ.color, base::length(base::unique(data1[, categ[1]])))}else{categ.color})) # categ.color used for dot colors because at that stage, we do not care about colors
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, fill = categ[1]), coef = if(box.whisker.kind == "no"){0}else if(box.whisker.kind == "std"){1.5}else if(box.whisker.kind == "max"){Inf})) # fill because this is what is used with geom_box # to easily have the equivalent of the grouped boxes
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "fill", name = box.legend.name, values = if(base::length(categ.color)== 1L){base::rep(categ.color, base::length(base::unique(data1[, categ[1]])))}else{categ.color}))
        # end per box dots coordinates recovery
    }else if(base::length(categ) == 2L){
        # width commputations
        box.width2 <- box.width / base::length(base::unique(data1[, categ[base::length(categ)]])) # real width of each box in x-axis unit, among the set of grouped box. Not relevant if no grouped boxes length(categ)== 1L
        # end width commputations
        # data1 check categ order for dots coordinates recovery
        tempo.factor <- base::paste0(data1[base::order(data1[, categ[2]], data1[, categ[1]]), categ[2]], "_", data1[base::order(data1[, categ[2]], data1[, categ[1]]), categ[1]])
        data1 <- base::data.frame(data1[base::order(data1[, categ[2]], data1[, categ[1]]), ], categ.check = base::factor(tempo.factor, levels = base::unique(tempo.factor)), stringsAsFactors = TRUE)
        data1$categ.check <- base::as.integer(data1$categ.check)
        # end data1 check categ order for dots coordinates recovery
        # per box dots coordinates recovery
        tempo.gg.name <- "gg.indiv.plot."
        tempo.gg.count <- 0
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), base::eval(base::parse(text = base::paste0("ggplot2::ggplot()", if(base::is.null(add)){""}else{add})))) # add added here to have the facets
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, color = categ[2]), stroke = dot.border.size, size = dot.size, alpha = dot.alpha, shape = 21))
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "color", name = box.legend.name, values = if(base::is.null(categ.color)){base::rep(NA, base::length(base::unique(data1[, categ[2]])))}else if(base::length(categ.color)== 1L){base::rep(categ.color, base::length(base::unique(data1[, categ[2]])))}else{categ.color})) # categ.color used for dot colors because at that stage, we do not care about colors
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, fill = categ[2]), coef = if(box.whisker.kind == "no"){0}else if(box.whisker.kind == "std"){1.5}else if(box.whisker.kind == "max"){Inf})) # fill because this is what is used with geom_box # to easily have the equivalent of the grouped boxes
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "fill", name = box.legend.name, values = if(base::length(categ.color)== 1L){base::rep(categ.color, base::length(base::unique(data1[, categ[2]])))}else{categ.color}))
        # end per box dots coordinates recovery
    }else{
        tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nCODE INCONSISTENCY 1")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! base::is.null(stat.pos)){
        stat.just <- fun_gg_just(
            angle = stat.angle, 
            pos = base::ifelse(
                vertical == TRUE, 
                base::ifelse(stat.pos == "top", "bottom", "top"), # "bottom" because we want justification for text that are below the ref point which is the top of the graph. The opposite for "above"
                base::ifelse(stat.pos == "top", "left", "right") # "left" because we want justification for text that are on the left of the ref point which is the right border of the graph. The opposite for "above"
            ), 
            kind = "text"
        )
    }
    # has in fact no interest because ggplot2 does not create room for geom_text()
    tempo.data.max <- data1[base::which.max(data1[, y]), ]
    tempo.data.max <- base::data.frame(tempo.data.max, label = base::formatC(tempo.data.max[, y], digits = 2, drop0trailing = TRUE, format = "f"), stringsAsFactors = TRUE)
    # end has in fact no interest because ggplot2 does not create room for geom_text()
    tempo.graph.info.ini <- ggplot2::ggplot_build(base::eval(base::parse(text = base::paste(base::paste(base::paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), if( ! base::is.null(stat.pos)){' + ggplot2::geom_text(data = tempo.data.max, mapping = ggplot2::aes_string(x = 1, y = y, label = "label"), size = stat.size, color = "black", angle = stat.angle, hjust = stat.just$hjust, vjust = stat.just$vjust)'})))) # added here to have room for annotation
    dot.coord <- tempo.graph.info.ini$data[[1]]
    dot.coord$x <- base::as.numeric(dot.coord$x) # because weird class
    dot.coord$PANEL <- base::as.numeric(dot.coord$PANEL) # because numbers as levels. But may be a problem is facet are reordered ?
    tempo.mean <- base::aggregate(x = dot.coord$y, by = base::list(dot.coord$group, dot.coord$PANEL), FUN = mean, na.rm = TRUE)
    base::names(tempo.mean)[base::names(tempo.mean) == "x"] <- "MEAN"
    base::names(tempo.mean)[base::names(tempo.mean) == "Group.1"] <- "BOX"
    base::names(tempo.mean)[base::names(tempo.mean) == "Group.2"] <- "PANEL"
    dot.coord <- base::data.frame(
        dot.coord[base::order(dot.coord$group, dot.coord$y), ], # dot.coord$PANEL deals below
        y.check = base::as.double(data1[base::order(data1$categ.check, data1[, y]), y]), 
        categ.check = data1[base::order(data1$categ.check, data1[, y]), "categ.check"], 
        dot.color = if(base::is.null(dot.color)){NA}else{data1[base::order(data1$categ.check, data1[, y]), "dot.color"]}, 
        data1[base::order(data1$categ.check, data1[, y]), ][categ], # avoid the renaming below
        stringsAsFactors = TRUE
    ) # y.check to be sure that the order is the same between the y of data1 and the y of dot.coord
    # names(dot.coord)[names(dot.coord) == "tempo_categ1"] <- categ[1]
    if( ! base::is.null(dot.categ)){
        dot.coord <- base::data.frame(dot.coord, data1[base::order(data1$categ.check, data1[, y]), ][dot.categ], stringsAsFactors = TRUE) # avoid the renaming
    }
    if( ! base::is.null(facet.categ)){
        dot.coord <- base::data.frame(dot.coord, data1[base::order(data1$categ.check, data1[, y]), ][facet.categ], stringsAsFactors = TRUE) # for facet panels
        tempo.test <- NULL
        for(i2 in 1:base::length(facet.categ)){
            tempo.test <- base::paste0(tempo.test, ".", base::formatC(base::as.numeric(dot.coord[, facet.categ[i2]]), width = base::nchar(base::max(base::as.numeric(dot.coord[, facet.categ[i2]]), na.rm = TRUE)), flag = "0")) # convert factor into numeric with leading zero for proper ranking # merge the formatC() to create a new factor. The convertion to integer should recreate the correct group number. Here as.numeric is used and not as.integer in case of numeric in facet.categ (because comes from add and not checked by saferDev::arg_check, contrary to categ)
        }
        tempo.test <- base::as.integer(base::factor(tempo.test))
        if( ! base::identical(base::as.integer(dot.coord$PANEL), tempo.test)){
            tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nas.integer(dot.coord$PANEL) AND tempo.test MUST BE IDENTICAL. CODE HAS TO BE MODIFIED")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if(dot.tidy == TRUE){
        if( ! base::is.null(dot.categ)){
            dot.coord <- base::data.frame(dot.coord, tidy_group = data1[base::order(data1$categ.check, data1[, y]), ][, dot.categ], stringsAsFactors = TRUE) # avoid the renaming
            # tidy_group_coord is to be able to fuse table when creating the table for dot coordinates
            if(dot.categ %in% categ){
                dot.coord <- base::data.frame(dot.coord, tidy_group_coord = dot.coord$group, stringsAsFactors = TRUE)
            }else{
                dot.coord <- base::data.frame(dot.coord, tidy_group_coord = base::as.integer(base::factor(base::paste0(
                    base::formatC(base::as.integer(dot.coord[, categ[1]]), width = base::nchar(base::max(base::as.integer(dot.coord[, categ[1]]), na.rm = TRUE)), flag = "0"), # convert factor into numeric with leading zero for proper ranking
                    ".", 
                    if(base::length(categ) == 2L){base::formatC(base::as.integer(dot.coord[, categ[2]]), width = base::nchar(base::max(base::as.integer(dot.coord[, categ[2]]), na.rm = TRUE)), flag = "0")}, # convert factor into numeric with leading zero for proper ranking
                    if(base::length(categ) == 2L){"."}, 
                    base::formatC(base::as.integer(dot.coord[, dot.categ]), width = base::nchar(base::max(base::as.integer(dot.coord[, dot.categ]), na.rm = TRUE)), flag = "0") # convert factor into numeric with leading zero for proper ranking
                )), stringsAsFactors = TRUE) # merge the 2 or 3 formatC() to create a new factor. The convertion to integer should recreate the correct group number
                ) # for tidy dot plots
            }
        }else{
            dot.coord <- base::data.frame(dot.coord, tidy_group = if(base::length(categ)== 1L){
                dot.coord[, categ]}else{base::as.integer(base::factor(base::paste0(
                    base::formatC(base::as.integer(dot.coord[, categ[1]]), width = base::nchar(base::max(base::as.integer(dot.coord[, categ[1]]), na.rm = TRUE)), flag = "0"), # convert factor into numeric with leading zero for proper ranking
                    ".", 
                    base::formatC(base::as.integer(dot.coord[, categ[2]]), width = base::nchar(base::max(base::as.integer(dot.coord[, categ[2]]), na.rm = TRUE)), flag = "0")# convert factor into numeric with leading zero for proper ranking
                )), stringsAsFactors = TRUE) # merge the 2 formatC() to create a new factor. The convertion to integer should recreate the correct group number
                }) # for tidy dot plots
            # tidy_group_coord is to be able to fuse table when creating the table for dot coordinates
            dot.coord <- base::data.frame(dot.coord, tidy_group_coord = dot.coord$group, stringsAsFactors = TRUE)
        }
    }
    if( ! (base::identical(dot.coord$y, dot.coord$y.check) & base::identical(dot.coord$group, dot.coord$categ.check))){
        tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\n(dot.coord$y AND dot.coord$y.check) AS WELL AS (dot.coord$group AND dot.coord$categ.check) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else{
        if( ! base::identical(tempo.mean[base::order(tempo.mean$BOX, tempo.mean$PANEL), ]$BOX, base::unique(dot.coord[base::order(dot.coord$group, dot.coord$PANEL), base::c("group", "PANEL")])$group)){
            tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\n(tempo.mean$BOX, tempo.mean$PANEL) AND (dot.coord$group, dot.coord$PANEL) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            tempo <- base::unique(dot.coord[base::order(dot.coord$group, dot.coord$PANEL), base::c(categ, if( ! base::is.null(dot.color) & ! base::is.null(dot.categ)){if(dot.categ != ini.dot.categ){dot.categ}}, if( ! base::is.null(facet.categ)){facet.categ}), drop = FALSE])
            # names(tempo) <- paste0(names(tempo), ".mean")
            tempo.mean <- base::data.frame(tempo.mean[base::order(tempo.mean$BOX, tempo.mean$PANEL), ], tempo, stringsAsFactors = TRUE)
        }
    }
    # at that stage, categ color and dot color are correctly attributed in data1, box.coord and dot.coord
    # end y dot coordinates recovery (create ini.box.coord, dot.coord and modify data1)
    # ylim range
    if(base::is.null(y.lim)){
        y.lim <- tempo.graph.info.ini$layout$panel_params[[1]]$y.range # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
        if(base::any(( ! base::is.finite(y.lim)) | base::is.na(y.lim)) | base::length(y.lim) != 2){ # kept but normally no more Inf in data1 # normally no NA with is.finite, etc.
            tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\ntempo.graph.info.ini$layout$panel_params[[1]]$y.range[1] CONTAINS NA OR Inf OR HAS LENGTH 1")
           base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }else if(y.log != "no"){
        y.lim <- base::get(y.log)(y.lim) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
    }
    if(y.log != "no"){
        # normally this control is not necessary anymore
        if(base::any( ! base::is.finite(y.lim))){ # normally no NA with is.finite
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\ny.lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y.log ARGUMENT SET TO ", y.log, ":\n", base::paste(y.lim, collapse = " "), "\nPLEASE, CHECK DATA VALUES (PRESENCE OF ZERO OR INF VALUES)")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if(base::suppressWarnings(base::all(y.lim %in% c(Inf, -Inf)))){ # all() without na.rm -> ok because y.lim cannot be NA (tested above)
        # normally this control is not necessary anymore
        tempo_cat <- base::paste0("ERROR IN ", function_name, " y.lim CONTAINS Inf VALUES, MAYBE BECAUSE VALUES FROM data1 ARGUMENTS ARE NA OR Inf ONLY OR BECAUSE OF LOG SCALE REQUIREMENT")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(base::suppressWarnings(base::any(base::is.na(y.lim)))){ # normally no NA with is.na
        # normally this control is not necessary anymore
        tempo_cat <- base::paste0("ERROR IN ", function_name, " y.lim CONTAINS NA OR NaN VALUES, MAYBE BECAUSE VALUES FROM data1 ARGUMENTS ARE NA OR Inf ONLY OR BECAUSE OF LOG SCALE REQUIREMENT")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    y.lim.order <- base::order(y.lim) # to deal with inverse axis
    y.lim <- base::sort(y.lim)
    y.lim[1] <- y.lim[1] - base::abs(y.lim[2] - y.lim[1]) * base::ifelse(base::diff(y.lim.order) > 0, y.bottom.extra.margin, y.top.extra.margin) # diff(y.lim.order) > 0 medians not inversed axis
    y.lim[2] <- y.lim[2] + base::abs(y.lim[2] - y.lim[1]) * base::ifelse(base::diff(y.lim.order) > 0, y.top.extra.margin, y.bottom.extra.margin) # diff(y.lim.order) > 0 medians not inversed axis
    if(y.include.zero == TRUE){ # no need to check y.log != "no" because done before
        y.lim <- base::range(base::c(y.lim, 0), na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
    }
    y.lim <- y.lim[y.lim.order]
    if(base::any(base::is.na(y.lim))){ # normally no NA with is.na
        tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nCODE INCONSISTENCY 2")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end ylim range
    
    
    
    
    
    
    # drawing
    # constant part
    tempo.gg.name <- "gg.indiv.plot."
    tempo.gg.count <- 0
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), base::eval(base::parse(text = base::paste0("ggplot2::ggplot()", if(base::is.null(add)){""}else{add})))) # add is directly put here to deal with additional variable of data, like when using facet_grid. No problem if add is a theme, will be dealt below
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::xlab(if(base::is.null(x.lab)){categ[1]}else{x.lab}))
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ylab(if(base::is.null(y.lab)){y}else{y.lab}))
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ggtitle(title))
    # text angle management
    axis.just <- fun_gg_just(angle = x.angle, pos = base::ifelse(vertical == TRUE, "bottom", "left"), kind = "axis")
    # end text angle management
    add.check <- TRUE
    if( ! base::is.null(add)){ # if add is NULL, then = 0
        if(base::grepl(pattern = "ggplot2\\s*::\\s*theme", add) == TRUE){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") \"ggplot2::theme\" STRING DETECTED IN THE add ARGUMENT\n-> INTERNAL GGPLOT2 THEME FUNCTIONS theme() AND theme_classic() HAVE BEEN INACTIVATED, TO BE USED BY THE USER\n-> article ARGUMENT WILL BE IGNORED")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
            add.check <- FALSE
        }
    }
    if(add.check == TRUE & article == TRUE){
        # WARNING: not possible to add theme()several times. NO message but the last one overwrites the others
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::theme_classic(base_size = text.size))
        if(grid == TRUE){
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), m.gg <- ggplot2::theme(
                text = ggplot2::element_text(size = text.size), 
                plot.title = ggplot2::element_text(size = title.text.size), # stronger than text
                line = ggplot2::element_line(size = 0.5), 
                legend.key = ggplot2::element_rect(color = "white", size = 1.5), # size of the frame of the legend
                axis.line.y.left = ggplot2::element_line(colour = "black"), # draw lines for the y axis
                axis.line.x.bottom = ggplot2::element_line(colour = "black"), # draw lines for the x axis
                panel.grid.major.x = if(vertical == TRUE){NULL}else{ggplot2::element_line(colour = "grey85", size = 0.75)},
                panel.grid.major.y = if(vertical == TRUE){ggplot2::element_line(colour = "grey85", size = 0.75)}else{NULL},
                panel.grid.minor.y = if(vertical == TRUE){ggplot2::element_line(colour = "grey90", size = 0.25)}else{NULL},
                axis.text.x = if(vertical == TRUE){ggplot2::element_text(angle = axis.just$angle, hjust = axis.just$hjust, vjust = axis.just$vjust)}else{NULL},
                axis.text.y = if(vertical == TRUE){NULL}else{ggplot2::element_text(angle = axis.just$angle, hjust = axis.just$hjust, vjust = axis.just$vjust)},
                strip.background = ggplot2::element_rect(fill = NA, colour = NA) # for facet background
            ))
        }else{
            assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), m.gg <- ggplot2::theme(
                text = ggplot2::element_text(size = text.size), 
                plot.title = ggplot2::element_text(size = title.text.size), # stronger than text
                line = ggplot2::element_line(size = 0.5), 
                legend.key = ggplot2::element_rect(color = "white", size = 1.5), # size of the frame of the legend
                axis.line.y.left = ggplot2::element_line(colour = "black"), 
                axis.line.x.bottom = ggplot2::element_line(colour = "black"),
                axis.text.x = if(vertical == TRUE){ggplot2::element_text(angle = axis.just$angle, hjust = axis.just$hjust, vjust = axis.just$vjust)}else{NULL},
                axis.text.y = if(vertical == TRUE){NULL}else{ggplot2::element_text(angle = axis.just$angle, hjust = axis.just$hjust, vjust = axis.just$vjust)},
                strip.background = ggplot2::element_rect(fill = NA, colour = NA)
            ))
        }
    }else if(add.check == TRUE & article == FALSE){
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), m.gg <- ggplot2::theme(
            text = ggplot2::element_text(size = text.size), 
            plot.title = ggplot2::element_text(size = title.text.size), # stronger than text
            line = ggplot2::element_line(size = 0.5), 
            legend.key = ggplot2::element_rect(color = "white", size = 1.5), # size of the frame of the legend
            panel.background = ggplot2::element_rect(fill = "grey95"), 
            axis.line.y.left = ggplot2::element_line(colour = "black"), 
            axis.line.x.bottom = ggplot2::element_line(colour = "black"), 
            panel.grid.major.x = ggplot2::element_line(colour = "grey85", size = 0.75), 
            panel.grid.major.y = ggplot2::element_line(colour = "grey85", size = 0.75), 
            panel.grid.minor.x = ggplot2::element_blank(), 
            panel.grid.minor.y = ggplot2::element_line(colour = "grey90", size = 0.25), 
            strip.background = ggplot2::element_rect(fill = NA, colour = NA),
            axis.text.x = if(vertical == TRUE){ggplot2::element_text(angle = axis.just$angle, hjust = axis.just$hjust, vjust = axis.just$vjust)}else{NULL},
            axis.text.y = if(vertical == TRUE){NULL}else{ggplot2::element_text(angle = axis.just$angle, hjust = axis.just$hjust, vjust = axis.just$vjust)}
        ))
    }
    # Contrary to fun_gg_bar(), cannot plot the boxplot right now, because I need the dots plotted first
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, group = categ[base::length(categ)]), position = ggplot2::position_dodge(width = NULL), color = NA, width = box.width, fill = NA)) # this is to set the graph (i.e., a blanck boxplot to be able to use x coordinates to plot dots before boxes)
    # end constant part
    
    
    
    
    # graphic info recovery (including means)
    tempo.graph.info <- ggplot2::ggplot_build(base::eval(base::parse(text = base::paste0(base::paste(base::paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), ' + ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, fill = categ[base::length(categ)]), position = ggplot2::position_dodge(width = NULL), width = box.width, notch = box.notch, coef = if(box.whisker.kind == "no"){0}else if(box.whisker.kind == "std"){1.5}else if(box.whisker.kind == "max"){Inf}) + ggplot2::scale_discrete_manual(aesthetics = "fill", name = box.legend.name, values = if(length(categ.color)== 1L){base::rep(categ.color, base::length(base::unique(data1[, categ[base::length(categ)]])))}else{categ.color})')))) # will be recovered later again, when ylim will be considered
    tempo.yx.ratio <- (tempo.graph.info$layout$panel_params[[1]]$y.range[2] - tempo.graph.info$layout$panel_params[[1]]$y.range[1]) / (tempo.graph.info$layout$panel_params[[1]]$x.range[2] - tempo.graph.info$layout$panel_params[[1]]$x.range[1])
    box.coord <- tempo.graph.info$data[[2]] # to have the summary statistics of the plot. Contrary to ini.box.plot, now integrates ylim Here because can be required for stat.pos when just box are plotted
    box.coord$x <- base::as.numeric(box.coord$x) # because x is of special class that block comparison of values using identical
    box.coord$PANEL <- base::as.numeric(box.coord$PANEL) # because numbers as levels. But may be a problem is facet are reordered ?
    box.coord <- box.coord[base::order(box.coord$group, box.coord$PANEL), ]
    if( ! (base::identical(tempo.mean$BOX, box.coord$group) & base::identical(tempo.mean$PANEL, box.coord$PANEL))){
        tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nidentical(tempo.mean$BOX, box.coord$group) & identical(tempo.mean$PANEL, box.coord$PANEL) DO NOT HAVE THE SAME VALUE ORDER")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else{
        # tempo <- base::c(categ, if( ! base::is.null(dot.color) & ! base::is.null(dot.categ)){if(dot.categ != ini.dot.categ){dot.categ}}, if( ! base::is.null(facet.categ)){facet.categ})
        if(base::any(base::names(tempo.mean) %in% base::names(box.coord), na.rm = TRUE)){
            base::names(tempo.mean)[base::names(tempo.mean) %in% base::names(box.coord)] <- base::paste0(base::names(tempo.mean)[base::names(tempo.mean) %in% base::names(box.coord)], ".mean")
        }
        box.coord <- base::data.frame(box.coord, tempo.mean, stringsAsFactors = TRUE)
    }
    # end graphic info recovery (including means)
    
    
    
    # stat output (will also serve for boxplot and mean display)
    # x not added now (to do not have them in stat.nolog)
    stat <- base::data.frame(
        MIN = box.coord$ymin_final, 
        QUART1 = box.coord$lower, 
        MEDIAN = box.coord$middle, 
        MEAN = box.coord$MEAN, 
        QUART3 = box.coord$upper, 
        MAX = box.coord$ymax_final, 
        WHISK_INF = box.coord$ymin, 
        BOX_INF = box.coord$lower, 
        NOTCH_INF = box.coord$notchlower, 
        NOTCH_SUP = box.coord$notchupper, 
        BOX_SUP = box.coord$upper, 
        WHISK_SUP = box.coord$ymax, 
        OUTLIERS = box.coord["outliers"], 
        tempo.mean[base::colnames(tempo.mean) != "MEAN"], 
        COLOR = box.coord$fill, 
        stringsAsFactors = TRUE
    ) # box.coord["outliers"] written like this because it is a list. X coordinates not put now because several features to set
    base::names(stat)[base::names(stat) == "outliers"] <- "OUTLIERS"
    stat.nolog <- stat # stat.nolog ini will serve for outputs
    if(y.log != "no"){
        stat.nolog[base::c("MIN", "QUART1", "MEDIAN", "MEAN", "QUART3", "MAX", "WHISK_INF", "BOX_INF", "NOTCH_INF", "NOTCH_SUP", "BOX_SUP", "WHISK_SUP")] <- base::ifelse(y.log == "log2", 2, 10)^(stat.nolog[base::c("MIN", "QUART1", "MEDIAN", "MEAN", "QUART3", "MAX", "WHISK_INF", "BOX_INF", "NOTCH_INF", "NOTCH_SUP", "BOX_SUP", "WHISK_SUP")])
        stat.nolog$OUTLIERS <- base::lapply(stat.nolog$OUTLIERS, FUN = function(X){base::ifelse(y.log == "log2", 2, 10)^X})
    }
    # end stat output (will also serve for boxplot and mean display)
    
    
    
    
    
    
    # x coordinates management (for random plotting and for stat display)
    # width commputations
    width.ini <- base::c(box.coord$xmax - box.coord$xmin)[1] # all the box widths are equal here. Only the first one taken
    width.correct <- width.ini * box.space / 2
    if( ! (base::identical(stat$BOX, box.coord$group) & base::identical(stat$PANEL, box.coord$PANEL))){
        tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nidentical(stat$BOX, box.coord$group) & identical(stat$PANEL, box.coord$PANEL) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else{
        stat <- base::data.frame(
            stat, 
            X = box.coord$x, 
            X_BOX_INF = box.coord$xmin + width.correct, 
            X_BOX_SUP = box.coord$xmax - width.correct, 
            X_NOTCH_INF = box.coord$x - (box.coord$x - (box.coord$xmin + width.correct)) / 2, 
            X_NOTCH_SUP = box.coord$x + (box.coord$x - (box.coord$xmin + width.correct)) / 2, 
            X_WHISK_INF = box.coord$x - (box.coord$x - (box.coord$xmin + width.correct)) * box.whisker.width, 
            X_WHISK_SUP = box.coord$x + (box.coord$x - (box.coord$xmin + width.correct)) * box.whisker.width, 
            # tempo.mean[colnames(tempo.mean) != "MEAN"], # already added above
            stringsAsFactors = TRUE
        )
        stat$COLOR <- base::factor(stat$COLOR, levels = base::unique(categ.color))
        if( ! base::all(stat$NOTCH_SUP < stat$BOX_SUP & stat$NOTCH_INF > stat$BOX_INF, na.rm = TRUE) & box.notch == TRUE){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") SOME NOTCHES ARE BEYOND BOX HINGES. TRY ARGUMENT box.notch = FALSE")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
    }
    dot.jitter <- base::c((box.coord$xmax - width.correct) - (box.coord$xmin + width.correct))[1] * dot.jitter # real dot.jitter. (box.coord$xmin + width.correct) - (box.coord$xmax - width.correct))[1] is the width of the box. Is equivalent to (box.coord$x - (box.coord$xmin + width.correct))[1] * 2
    # end width commputations
    if( ! base::is.null(dot.color)){
        # random dots
        if(dot.tidy == FALSE){
            dot.coord.rd1 <- base::merge(dot.coord, box.coord[base::c("fill", "PANEL", "group", "x")], by = base::c("PANEL", "group"), sort = FALSE) # rd for random. Send the coord of the boxes into the coord data.frame of the dots (in the column x.y). WARNING: by = c("PANEL", "group") without fill column because PANEL & group columns are enough as only one value of x column per group number in box.coord. Thus, no need to consider fill column
            if(base::nrow(dot.coord.rd1) != base::nrow(dot.coord)){
                tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nTHE merge() FUNCTION DID NOT RETURN A CORRECT dot.coord.rd1 DATA FRAME. CODE HAS TO BE MODIFIED")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            sampled.dot.jitter <- if(base::nrow(dot.coord.rd1)== 1L){base::runif(n = base::nrow(dot.coord.rd1), min = - dot.jitter / 2, max = dot.jitter / 2)}else{base::sample(x = base::runif(n = base::nrow(dot.coord.rd1), min = - dot.jitter / 2, max = dot.jitter / 2), size = base::nrow(dot.coord.rd1), replace = FALSE)}
            dot.coord.rd2 <- base::data.frame(dot.coord.rd1, dot.x = dot.coord.rd1$x.y + sampled.dot.jitter, stringsAsFactors = TRUE) # set the dot.jitter thanks to runif and dot.jitter range. Then, send the coord of the boxes into the coord data.frame of the dots (in the column x.y)
            if(base::length(categ)== 1L){
                tempo.data1 <- base::unique(base::data.frame(data1[categ[1]], group = base::as.integer(data1[, categ[1]]), stringsAsFactors = TRUE)) # categ[1] is factor
                base::names(tempo.data1)[base::names(tempo.data1) == categ[1]] <- base::paste0(categ[1], ".check")
                verif <- base::paste0(categ[1], ".check")
            }else if(base::length(categ) == 2L){
                tempo.data1 <- base::unique(
                    base::data.frame(
                        data1[base::c(categ[1], categ[2])], 
                        group = base::as.integer(base::factor(base::paste0(
                            base::formatC(base::as.integer(data1[, categ[2]]), width = base::nchar(base::max(base::as.integer(data1[, categ[2]]), na.rm = TRUE)), flag = "0"), # convert factor into numeric with leading zero for proper ranking
                            ".", 
                            base::formatC(base::as.integer(data1[, categ[1]]), width = base::nchar(base::max(base::as.integer(data1[, categ[1]]), na.rm = TRUE)), flag = "0")# convert factor into numeric with leading zero for proper ranking
                        )), stringsAsFactors = TRUE) # merge the 2 formatC() to create a new factor. The convertion to integer should recreate the correct group number
                    )
                ) # categ[2] first if categ[2] is used to make the categories in ggplot and categ[1] is used to make the x-axis
                base::names(tempo.data1)[base::names(tempo.data1) == categ[1]] <- base::paste0(categ[1], ".check")
                base::names(tempo.data1)[base::names(tempo.data1) == categ[2]] <- base::paste0(categ[2], ".check")
                verif <- base::c(base::paste0(categ[1], ".check"), base::paste0(categ[2], ".check"))
            }else{
                tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nCODE INCONSISTENCY 3")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            dot.coord.rd3 <- base::merge(dot.coord.rd2, tempo.data1, by = base::intersect("group", "group"), sort = FALSE) # send the factors of data1 into coord. WARNING: I have replaced by = "group" by intersect("group", "group") because of an error due to wrong group group merging in dot.coord.rd3
            if(base::nrow(dot.coord.rd3) != base::nrow(dot.coord) | ( ! saferTool::comp_2d(dot.coord.rd3[categ], dot.coord.rd3[verif])$identical.content)){
                tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nTHE merge() FUNCTION DID NOT RETURN A CORRECT dot.coord.rd3 DATA FRAME. CODE HAS TO BE MODIFIED")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            # end random dots
        }
        # tidy dots
        # coordinates are recovered during plotting (see dot.coord.tidy1 below)
        # end tidy dots
    }
    # end x coordinates management (for random plotting and for stat display)
    
    
    
    
    
    # boxplot display before dot display if box.fill = TRUE
    coord.names <- NULL
    # creation of the data frame for (main box + legend) and data frame for means
    if(box.notch == FALSE){
        for(i3 in 1:base::length(categ)){
            if(i3== 1L){
                tempo.polygon <- base::data.frame(GROUPX = base::c(base::t(stat[, base::rep(categ[i3], 5)])), stringsAsFactors = TRUE)
            }else{
                tempo.polygon <- base::cbind(tempo.polygon, base::c(base::t(stat[, base::rep(categ[i3], 5)])), stringsAsFactors = TRUE)
            }
        }
        base::names(tempo.polygon) <- categ
        tempo.polygon <- base::data.frame(X = base::c(base::t(stat[, base::c("X_BOX_INF", "X_BOX_SUP", "X_BOX_SUP", "X_BOX_INF", "X_BOX_INF")])), Y = base::c(base::t(stat[, base::c("BOX_INF", "BOX_INF", "BOX_SUP", "BOX_SUP", "BOX_INF")])), COLOR = base::c(base::t(stat[, base::c("COLOR", "COLOR", "COLOR", "COLOR", "COLOR")])), BOX = base::as.character(base::c(base::t(stat[, base::c("BOX", "BOX", "BOX", "BOX", "BOX")]))), tempo.polygon, stringsAsFactors = TRUE)
        if( ! base::is.null(facet.categ)){
            for(i4 in 1:base::length(facet.categ)){
                tempo.polygon <- base::data.frame(tempo.polygon, base::c(base::t(stat[, base::c(facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4])])), stringsAsFactors = TRUE)
                base::names(tempo.polygon)[base::length(base::names(tempo.polygon))] <- facet.categ[i4]
            }
        }
    }else{
        for(i3 in 1:base::length(categ)){
            if(i3== 1L){
                tempo.polygon <- base::data.frame(GROUPX = base::c(base::t(stat[, base::rep(categ[i3], 11)])), stringsAsFactors = TRUE)
            }else{
                tempo.polygon <- base::cbind(tempo.polygon, base::c(base::t(stat[, base::rep(categ[i3], 11)])), stringsAsFactors = TRUE)
            }
        }
        base::names(tempo.polygon) <- categ
        tempo.polygon <- base::data.frame(X = base::c(base::t(stat[, base::c("X_BOX_INF", "X_BOX_SUP", "X_BOX_SUP", "X_NOTCH_SUP", "X_BOX_SUP", "X_BOX_SUP", "X_BOX_INF", "X_BOX_INF", "X_NOTCH_INF", "X_BOX_INF", "X_BOX_INF")])), Y = base::c(base::t(stat[, base::c("BOX_INF", "BOX_INF", "NOTCH_INF", "MEDIAN", "NOTCH_SUP", "BOX_SUP", "BOX_SUP", "NOTCH_SUP", "MEDIAN", "NOTCH_INF", "BOX_INF")])), COLOR = base::c(base::t(stat[, base::c("COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR", "COLOR")])), BOX = base::as.character(c(t(stat[, base::c("BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX", "BOX")]))), tempo.polygon, stringsAsFactors = TRUE)
        if( ! base::is.null(facet.categ)){
            for(i4 in 1:base::length(facet.categ)){
                tempo.polygon <- base::data.frame(tempo.polygon, base::c(base::t(stat[, base::c(facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4], facet.categ[i4])])), stringsAsFactors = TRUE)
                base::names(tempo.polygon)[base::length(base::names(tempo.polygon))] <- facet.categ[i4]
            }
        }
    }
    tempo.polygon$COLOR <- base::factor(tempo.polygon$COLOR, levels = base::unique(categ.color))
    if( ! base::is.null(categ.class.order)){
        for(i3 in 1:base::length(categ)){
            tempo.polygon[, categ[i3]] <- base::factor(tempo.polygon[, categ[i3]], levels = categ.class.order[[i3]])
        }
    }
    # modified name of dot.categ column (e.g., "Categ1_DOT") must be included for boxplot using ridy dots
    if( ! base::is.null(dot.color) & ! base::is.null(dot.categ)){
        if(dot.categ != ini.dot.categ){
            tempo.polygon <- base::data.frame(tempo.polygon, GROUPX = tempo.polygon[, ini.dot.categ], stringsAsFactors = TRUE)
            base::names(tempo.polygon)[base::names(tempo.polygon) == "GROUPX"] <- dot.categ
            
        }
    }
    tempo.diamon.mean <- base::data.frame(X = base::c(base::t(stat[, base::c("X", "X_NOTCH_INF", "X", "X_NOTCH_SUP", "X")])), Y = base::c(base::t(base::cbind(stat["MEAN"] - (stat[, "X"] - stat[, "X_NOTCH_INF"]) * tempo.yx.ratio, stat["MEAN"], stat["MEAN"] + (stat[, "X"] - stat[, "X_NOTCH_INF"]) * tempo.yx.ratio, stat["MEAN"], stat["MEAN"] - (stat[, "X"] - stat[, "X_NOTCH_INF"]) * tempo.yx.ratio, stringsAsFactors = TRUE))), COLOR = base::c(base::t(stat[, base::c("COLOR", "COLOR", "COLOR", "COLOR", "COLOR")])), GROUP = base::c(base::t(stat[, base::c("BOX", "BOX", "BOX", "BOX", "BOX")])), stringsAsFactors = TRUE) # stringsAsFactors = TRUE for cbind() because stat["MEAN"] is a data frame. Otherwise, stringsAsFactors is not an argument for cbind() on vectors
    if( ! base::is.null(facet.categ)){
        for(i3 in 1:base::length(facet.categ)){
            tempo.diamon.mean <- base::data.frame(tempo.diamon.mean, base::c(base::t(stat[, base::c(facet.categ[i3], facet.categ[i3], facet.categ[i3], facet.categ[i3], facet.categ[i3])])), stringsAsFactors = TRUE)
            base::names(tempo.diamon.mean)[base::length(base::names(tempo.diamon.mean))] <- facet.categ[i3]
        }
    }
    tempo.diamon.mean$COLOR <- base::factor(tempo.diamon.mean$COLOR, levels = base::unique(categ.color))
    # end creation of the data frame for (main box + legend) and data frame for means
    if(box.fill == TRUE){
        # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, color = categ[length(categ)], fill = categ[length(categ)]), position = ggplot2::position_dodge(width = NULL), width = box.width, size = box.line.size, notch = box.notch, coef = if(box.whisker.kind == "no"){0}else if(box.whisker.kind == "std"){1.5}else if(box.whisker.kind == "max"){Inf}, alpha = box.alpha, outlier.shape = if( ! base::is.null(dot.color)){NA}else{21}, outlier.color = if( ! base::is.null(dot.color)){NA}else{dot.border.color}, outlier.fill = if( ! base::is.null(dot.color)){NA}else{NULL}, outlier.size = if( ! base::is.null(dot.color)){NA}else{dot.size}, outlier.stroke = if( ! base::is.null(dot.color)){NA}else{dot.border.size}, outlier.alpha = if( ! base::is.null(dot.color)){NA}else{dot.alpha})) # the color, size, etc. of the outliers are dealt here. outlier.color = NA to do not plot outliers when dots are already plotted. Finally, boxplot redrawn (see below)
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_polygon(
            data = tempo.polygon, 
            mapping = ggplot2::aes_string(x = "X", y = "Y", group = "BOX", fill = categ[base::length(categ)], color = categ[base::length(categ)]), 
            size = box.line.size, 
            alpha = box.alpha # works only for fill, not for color
        ))
        coord.names <- base::c(coord.names, "main.box")
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X, xend = X, y = BOX_SUP, yend = WHISK_SUP, group = categ[base::length(categ)]), color = "black", size = box.line.size, alpha = box.alpha)) # 
        coord.names <- base::c(coord.names, "sup.whisker")
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X, xend = X, y = BOX_INF, yend = WHISK_INF, group = categ[base::length(categ)]), color = "black", size = box.line.size, alpha = box.alpha)) # 
        coord.names <- base::c(coord.names, "inf.whisker")
        if(box.whisker.width > 0){
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X_WHISK_INF, xend = X_WHISK_SUP, y = WHISK_SUP, yend = WHISK_SUP, group = categ[base::length(categ)]), color = "black", size = box.line.size, alpha = box.alpha, lineend = "round")) # 
            coord.names <- base::c(coord.names, "sup.whisker.edge")
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X_WHISK_INF, xend = X_WHISK_SUP, y = WHISK_INF, yend = WHISK_INF, group = categ[base::length(categ)]), color = "black", size = box.line.size, alpha = box.alpha, lineend = "round")) # 
            coord.names <- base::c(coord.names, "inf.whisker.edge")
        }
        if(box.mean == TRUE){
            # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(data = stat, mapping = ggplot2::aes_string(x = "X", y = "MEAN", group = categ[length(categ)]), shape = 23, stroke = box.line.size * 2, fill = stat$COLOR, size = box.mean.size, color = "black", alpha = box.alpha)) # group used in aesthetic to do not have it in the legend
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_polygon(
                data = tempo.diamon.mean, 
                mapping = ggplot2::aes(x = X, y = Y, group = GROUP), 
                fill = tempo.diamon.mean[, "COLOR"], 
                color = base::hsv(0, 0, 0, alpha = box.alpha), # outline of the polygon in black but with alpha
                size = box.line.size, 
                alpha = box.alpha
            ))
            coord.names <- base::c(coord.names, "mean")
        }
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = if(box.notch == FALSE){X_BOX_INF}else{X_NOTCH_INF}, xend = if(box.notch == FALSE){X_BOX_SUP}else{X_NOTCH_SUP}, y = MEDIAN, yend = MEDIAN, group = categ[base::length(categ)]), color = "black", size = box.line.size * 2, alpha = box.alpha)) # 
        coord.names <- base::c(coord.names, "median")
    }
    # end boxplot display before dot display if box.fill = TRUE
    
    
    
    
    
    
    # dot display
    if( ! base::is.null(dot.color)){
        if(dot.tidy == FALSE){
            if(base::is.null(dot.categ)){
                if(dot.border.size == 0){
                    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(
                        data = dot.coord.rd3, 
                        mapping = ggplot2::aes_string(x = "dot.x", y = "y", group = categ[base::length(categ)]), 
                        size = dot.size, 
                        shape = 19, 
                        color = dot.coord.rd3$dot.color, 
                        alpha = dot.alpha
                    )) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
                }else{
                    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(
                        data = dot.coord.rd3, 
                        mapping = ggplot2::aes_string(x = "dot.x", y = "y", group = categ[base::length(categ)]), 
                        shape = 21, 
                        stroke = dot.border.size, 
                        color = if(base::is.null(dot.border.color)){dot.coord.rd3$dot.color}else{base::rep(dot.border.color, base::nrow(dot.coord.rd3))}, 
                        size = dot.size, 
                        fill = dot.coord.rd3$dot.color, 
                        alpha = dot.alpha
                    )) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
                }
            }else{
                if(dot.border.size == 0){
                    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(
                        data = dot.coord.rd3, 
                        mapping = ggplot2::aes_string(x = "dot.x", y = "y", alpha = dot.categ), 
                        size = dot.size, 
                        shape = 19, 
                        color = dot.coord.rd3$dot.color
                    )) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
                }else{
                    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(
                        data = dot.coord.rd3, 
                        mapping = ggplot2::aes_string(x = "dot.x", y = "y", alpha = dot.categ), 
                        size = dot.size, 
                        shape = 21, 
                        stroke = dot.border.size, 
                        color = if(base::is.null(dot.border.color)){dot.coord.rd3$dot.color}else{base::rep(dot.border.color, base::nrow(dot.coord.rd3))}, 
                        fill = dot.coord.rd3$dot.color
                    )) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
                }
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "alpha", name = dot.legend.name, values = base::rep(dot.alpha, base::length(dot.categ.class.order)), guide = ggplot2::guide_legend(override.aes = base::list(fill = dot.color, color = if(base::is.null(dot.border.color)){dot.color}else{dot.border.color}, stroke = dot.border.size, alpha = dot.alpha)))) # values are the values of color (which is the border color in geom_box. WARNING: values = categ.color takes the numbers to make the colors if categ.color is a factor
            }
            coord.names <- base::c(coord.names, "dots")
        }else if(dot.tidy == TRUE){
            # here plot using group -> no scale
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_dotplot(
                data = dot.coord, 
                mapping = ggplot2::aes_string(x = categ[1], y = "y", group = "group"), # not dot.categ here because the classes of dot.categ create new separations
                position = ggplot2::position_dodge(width = box.width), 
                binpositions = "all", 
                binaxis = "y", 
                stackdir = "center", 
                alpha = dot.alpha, 
                fill = dot.coord$dot.color, 
                stroke = dot.border.size, 
                color = if(base::is.null(dot.border.color)){dot.coord$dot.color}else{base::rep(dot.border.color, base::nrow(dot.coord))}, 
                show.legend = FALSE, # WARNING: do not use show.legend = TRUE because it uses the arguments outside aes() as aesthetics (here color and fill). Thus I must find a way using ggplot2::scale_discrete_manual()
                binwidth = (y.lim[2] - y.lim[1]) / dot.tidy.bin.nb
            )) # geom_dotplot ggplot2 v3.3.0: I had to remove rev() in fill and color # very weird behavior of geom_dotplot ggplot2 v3.2.1, (1) because with aes group = (to avoid legend), the dot plotting is not good in term of coordinates, and (2) because data1 seems reorderer according to x = categ[1] before plotting. Thus, I have to use fill = dot.coord[rev(order(dot.coord[, categ[1]], decreasing = TRUE)), "dot.color"] to have the good corresponding colors # show.legend option do not remove the legend, only the aesthetic of the legend (dot, line, etc.)
            coord.names <- base::c(coord.names, "dots")
            if( ! base::is.null(dot.categ)){
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_dotplot(
                    data = dot.coord, 
                    mapping = ggplot2::aes_string(x = categ[1], y = "y", alpha = dot.categ), # not dot.categ here because the classes of dot.categ create new separations
                    position = ggplot2::position_dodge(width = box.width), 
                    binpositions = "all", 
                    binaxis = "y", 
                    stackdir = "center", 
                    fill = NA, 
                    stroke = NA, 
                    color = NA, 
                    # WARNING: do not use show.legend = TRUE because it uses the arguments outside aes() as aesthetics (here color and fill). Thus I must find a way using ggplot2::scale_discrete_manual()
                    binwidth = (y.lim[2] - y.lim[1]) / dot.tidy.bin.nb
                )) # geom_dotplot ggplot2 v3.3.0: I had to remove rev() in fill and color # very weird behavior of geom_dotplot ggplot2 v3.2.1, (1) because with aes group = (to avoid legend), the dot plotting is not good in term of coordinates, and (2) because data1 seems reorderer according to x = categ[1] before plotting. Thus, I have to use fill = dot.coord[rev(order(dot.coord[, categ[1]], decreasing = TRUE)), "dot.color"] to have the good corresponding colors # show.legend option do not remove the legend, only the aesthetic of the legend (dot, line, etc.)
                # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "linetype", name = dot.legend.name, values = rep(1, length(categ.color)))) # values = rep("black", length(categ.color)) are the values of color (which is the border color of dots), and this modify the border color on the plot. WARNING: values = categ.color takes the numbers to make the colors if categ.color is a factor
                coord.names <- base::c(coord.names, "bad_remove")
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "alpha", name = dot.legend.name, values = base::rep(dot.alpha, base::length(dot.categ.class.order)), labels = dot.categ.class.order, guide = ggplot2::guide_legend(title = if(ini.dot.categ == categ[base::length(categ)]){dot.categ}else{ini.dot.categ}, override.aes = base::list(fill = base::levels(dot.coord$dot.color), color = if(base::is.null(dot.border.color)){base::levels(dot.coord$dot.color)}else{dot.border.color}, stroke = dot.border.size, alpha = dot.alpha)))) # values are the values of color (which is the border color in geom_box. WARNING: values = categ.color takes the numbers to make the colors if categ.color is a factor
            }
            # coordinates of tidy dots
            tempo.coord <- ggplot2::ggplot_build(base::eval(base::parse(text = base::paste(base::paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "))))$data # to have the tidy dot coordinates
            if(base::length(base::which(base::sapply(X = tempo.coord, FUN = function(X){base::any(base::names(X) == "binwidth", na.rm = TRUE)}))) != 1){ # detect the compartment of tempo.coord which is the binned data frame
                # if(length(which(sapply(tempo.coord, FUN = nrow) == nrow(data1))) > if(base::is.null(dot.categ)){1}else{2}){ # this does not work if only one dot per class, thus replaced by above # if(base::is.null(dot.categ)){1}else{2} because 1 dotplot if dot.categ is NULL and 2 dotplots if not, with the second being a blank dotplot with wrong coordinates. Thus take the first in that situation
                tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nEITHER MORE THAN 1 OR NO COMPARTMENT HAVING A DATA FRAME WITH binwidth AS COLUMN NAME IN THE tempo.coord LIST (FOR TIDY DOT COORDINATES). CODE HAS TO BE MODIFIED")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                # dot.coord.tidy1 <- tempo.coord[[which(sapply(tempo.coord, FUN = nrow) == nrow(data1))[1]]] # this does not work if only one dot per class, thus replaced by above # the second being a blank dotplot with wrong coordinates. Thus take the first whatever situation
                dot.coord.tidy1 <- tempo.coord[[base::which(base::sapply(X = tempo.coord, FUN = function(X){base::any(base::names(X) == "binwidth", na.rm = TRUE)}))]] # detect the compartment of tempo.coord which is the binned data frame
                dot.coord.tidy1$x <- base::as.numeric(dot.coord.tidy1$x) # because weird class
                dot.coord.tidy1$PANEL <- base::as.numeric(dot.coord.tidy1$PANEL) # because numbers as levels. But may be a problem is facet are reordered ?
            }
            # tempo.box.coord <- merge(box.coord, unique(dot.coord[, c("PANEL", "group", categ)]), by = c("PANEL", "group"), sort = FALSE) # not required anymore because box.coord already contains categ do not add dot.categ and tidy_group_coord here because the coordinates are for stats. Add the categ in box.coord. WARNING: by = c("PANEL", "group") without fill column because PANEL & group columns are enough as only one value of x column per group number in box.coord. Thus, no need to consider fill column
            # below inactivated because not true when dealing with dot.categ different from categ
            # if(nrow(tempo.box.coord) != nrow(box.coord)){
            # tempo_cat <- paste0("INTERNAL CODE ERROR IN ", function_name, "\nTHE merge() FUNCTION DID NOT RETURN A CORRECT tempo.box.coord DATA FRAME. CODE HAS TO BE MODIFIED")
            # stop(paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            # }
            dot.coord.tidy2 <- base::merge(dot.coord.tidy1, box.coord[base::c("fill", "PANEL", "group", "x", categ)], by = base::c("PANEL", "group"), sort = FALSE) # send the coord of the boxes into the coord data.frame of the dots (in the column x.y).WARNING: by = c("PANEL", "group") without fill column because PANEL & group columns are enough as only one value of x column per group number in tempo.box.coord. Thus, no need to consider fill colum # DANGER: from here the fill.y and x.y (from tempo.box.coord) are not good in dot.coord.tidy2. It is ok because Categ1 Categ2 from tempo.box.coord are ok with the group column from dot.coord.tidy1. This is due to the fact that dot.coord.tidy resulting from geom_dotplot does not make the same groups as the other functions
            if(base::nrow(dot.coord.tidy2) != base::nrow(dot.coord)){
                tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nTHE merge() FUNCTION DID NOT RETURN A CORRECT dot.coord.tidy2 DATA FRAME. CODE HAS TO BE MODIFIED")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            # From here, check for dot.coord.tidy3 which wil be important for stat over the plot. WARNING: dot.categ has nothing to do here for stat coordinates. Thus, not in tempo.data1
            if(base::length(categ)== 1L){
                tempo.data1 <- base::unique(base::data.frame(data1[categ[1]], group = base::as.integer(data1[, categ[1]]), stringsAsFactors = TRUE)) # categ[1] is factor
                base::names(tempo.data1)[base::names(tempo.data1) == categ[1]] <- base::paste0(categ[1], ".check")
                verif <- base::paste0(categ[1], ".check")
            }else if(base::length(categ) == 2L){
                tempo.data1 <- base::unique(
                    base::data.frame(
                        data1[base::c(categ[1], categ[2])], 
                        group = base::as.integer(base::factor(base::paste0(
                            base::formatC(base::as.integer(data1[, categ[2]]), width = base::nchar(base::max(base::as.integer(data1[, categ[2]]), na.rm = TRUE)), flag = "0"), # convert factor into numeric with leading zero for proper ranking
                            ".", 
                            base::formatC(base::as.integer(data1[, categ[1]]), width = base::nchar(base::max(base::as.integer(data1[, categ[1]]), na.rm = TRUE)), flag = "0")# convert factor into numeric with leading zero for proper ranking
                        )), stringsAsFactors = TRUE) # merge the 2 formatC() to create a new factor. The convertion to integer should recreate the correct group number
                    )
                ) # categ[2] first if categ[2] is used to make the categories in ggplot and categ[1] is used to make the x-axis
                base::names(tempo.data1)[base::names(tempo.data1) == categ[1]] <- base::paste0(categ[1], ".check")
                base::names(tempo.data1)[base::names(tempo.data1) == categ[2]] <- base::paste0(categ[2], ".check")
                verif <- base::c(base::paste0(categ[1], ".check"), base::paste0(categ[2], ".check"))
            }else{
                tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nCODE INCONSISTENCY 4")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            dot.coord.tidy3 <- base::merge(dot.coord.tidy2, tempo.data1, by = base::intersect("group", "group"), sort = FALSE) # send the factors of data1 into coord. WARNING: I have tested intersect("group", "group") instead of by = "group". May be come back to by = "group" in case of error. But I did this because of an error in dot.coord.rd3 above
            if(base::nrow(dot.coord.tidy3) != base::nrow(dot.coord) | ( ! saferTool::comp_2d(dot.coord.tidy3[categ], dot.coord.tidy3[verif])$identical.content)){
                tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nTHE merge() FUNCTION DID NOT RETURN A CORRECT dot.coord.tidy3 DATA FRAME. CODE HAS TO BE MODIFIED")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            # end coordinates of tidy dots
        }
    }
    # end dot display
    
    
    
    # boxplot display (if box.fill = FALSE, otherwise, already plotted above)
    if(box.fill == TRUE){
        # overcome "work only for the filling of boxes, not for the frame. See https://github.com/tidyverse/ggplot2/issues/252"
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "fill", name = box.legend.name, values = if(base::length(categ.color)== 1L){base::rep(categ.color, base::length(base::unique(data1[, categ[base::length(categ)]])))}else{categ.color}, guide = ggplot2::guide_legend(order = 1))) #, guide = ggplot2::guide_legend(override.aes = list(fill = levels(tempo.polygon$COLOR), color = "black")))) # values are the values of color (which is the border color in geom_box. WARNING: values = categ.color takes the numbers to make the colors if categ.color is a factor
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "color", name = box.legend.name, values = base::rep(base::hsv(0, 0, 0, alpha = box.alpha), base::length(base::unique(data1[, categ[base::length(categ)]]))), guide = ggplot2::guide_legend(order = 1))) # , guide = ggplot2::guide_legend(override.aes = list(color = "black", alpha = box.alpha)))) # values are the values of color (which is the border color in geom_box. WARNING: values = categ.color takes the numbers to make the colors if categ.color is a factor # outline of the polygon in black but with alpha
    }else{
        # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_boxplot(data = data1, mapping = ggplot2::aes_string(x = categ[1], y = y, color = categ[length(categ)], fill = categ[length(categ)]), position = ggplot2::position_dodge(width = NULL), width = box.width, size = box.line.size, notch = box.notch, alpha = box.alpha, coef = if(box.whisker.kind == "no"){0}else if(box.whisker.kind == "std"){1.5}else if(box.whisker.kind == "max"){Inf}, outlier.shape = if( ! base::is.null(dot.color)){NA}else{21}, outlier.color = if( ! is.null(dot.color)){NA}else{if(dot.border.size == 0){NA}else{dot.border.color}}, outlier.fill = if( ! base::is.null(dot.color)){NA}else{NULL}, outlier.size = if( ! base::is.null(dot.color)){NA}else{dot.size}, outlier.stroke = if( ! base::is.null(dot.color)){NA}else{dot.border.size}, outlier.alpha = if( ! base::is.null(dot.color)){NA}else{dot.alpha})) # the color, size, etc. of the outliers are dealt here. outlier.color = NA to do not plot outliers when dots are already plotted
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_path(
            data = tempo.polygon, 
            mapping = ggplot2::aes_string(x = "X", y = "Y", group = "BOX", color = categ[base::length(categ)]), 
            size = box.line.size, 
            alpha = box.alpha, 
            lineend = "round", 
            linejoin = "round"
        ))
        coord.names <- base::c(coord.names, "main.box")
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = if(box.notch == FALSE){X_BOX_INF}else{X_NOTCH_INF}, xend = if(box.notch == FALSE){X_BOX_SUP}else{X_NOTCH_SUP}, y = MEDIAN, yend = MEDIAN, group = categ[base::length(categ)]), color = stat$COLOR, size = box.line.size * 2, alpha = box.alpha)) # 
        coord.names <- base::c(coord.names, "median")
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X, xend = X, y = BOX_SUP, yend = WHISK_SUP, group = categ[base::length(categ)]), color = stat$COLOR, size = box.line.size, alpha = box.alpha)) # 
        coord.names <- base::c(coord.names, "sup.whisker")
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X, xend = X, y = BOX_INF, yend = WHISK_INF, group = categ[base::length(categ)]), color = stat$COLOR, size = box.line.size, alpha = box.alpha)) # 
        coord.names <- base::c(coord.names, "inf.whisker")
        if(box.whisker.width > 0){
           base:: assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X_WHISK_INF, xend = X_WHISK_SUP, y = WHISK_SUP, yend = WHISK_SUP, group = categ[base::length(categ)]), color = stat$COLOR, size = box.line.size, alpha = box.alpha, lineend = "round")) # 
            coord.names <- base::c(coord.names, "sup.whisker.edge")
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_segment(data = stat, mapping = ggplot2::aes(x = X_WHISK_INF, xend = X_WHISK_SUP, y = WHISK_INF, yend = WHISK_INF, group = categ[base::length(categ)]), color = stat$COLOR, size = box.line.size, alpha = box.alpha, lineend = "round")) # 
            coord.names <- c(coord.names, "inf.whisker.edge")
        }
        if(box.mean == TRUE){
            # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(data = stat, mapping = ggplot2::aes_string(x = "X", y = "MEAN", group = categ[length(categ)]), shape = 23, stroke = box.line.size * 2, color = stat$COLOR, size = box.mean.size, fill = NA, alpha = box.alpha)) # group used in aesthetic to do not have it in the legend. Here ggplot2::scale_discrete_manual() cannot be used because of the group easthetic
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_path(
                data = tempo.diamon.mean, 
                mapping = ggplot2::aes(x = X, y = Y, group = GROUP), 
                color = tempo.diamon.mean[, "COLOR"], 
                size = box.line.size, 
                alpha = box.alpha, 
                lineend = "round", 
                linejoin = "round"
            ))
            coord.names <- base::c(coord.names, "mean")
        }
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "fill", name = box.legend.name, values = base::rep(NA, base::length(base::unique(data1[, categ[base::length(categ)]]))))) #, guide = ggplot2::guide_legend(override.aes = list(color = categ.color)))) # values are the values of color (which is the border color in geom_box. WARNING: values = categ.color takes the numbers to make the colors if categ.color is a factor
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "color", name = box.legend.name, values = if(base::length(categ.color)== 1L){base::rep(categ.color, base::length(base::unique(data1[, categ[base::length(categ)]])))}else{categ.color}, guide = ggplot2::guide_legend(override.aes = base::list(alpha = if(plot == TRUE & ((base::length(base::dev.list()) > 0 & base::names(base::dev.cur()) == "windows") | (base::length(base::dev.list()) == 0L & base::Sys.info()["sysname"] == "Windows"))){1}else{box.alpha})))) # , guide = ggplot2::guide_legend(override.aes = list(color = as.character(categ.color))))) # values are the values of color (which is the border color in geom_box. WARNING: values = categ.color takes the numbers to make the colors if categ.color is a factor
        if(plot == TRUE & ((base::length(base::dev.list()) > 0 & base::names(base::dev.cur()) == "windows") | (base::length(base::dev.list()) == 0L & base::Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
            # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE LINES IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
    }
    if(box.alpha == 0){ # remove box legend because no boxes drawn
        # add this after the scale_xxx_manual() for boxplots
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::guides(fill = "none", color = "none")) # inactivate the legend
    }
    # end boxplot display (if box.fill = FALSE, otherwise, already plotted above)
    
    
    
    
    # stat display
    # layer after dots but ok, behind dots on the plot
    if( ! base::is.null(stat.pos)){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") NUMBERS DISPLAYED ARE ", base::ifelse(stat.mean == FALSE, "MEDIANS", "MEANS"))
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        if(stat.pos == "top"){
            tempo.stat <- base::data.frame(stat, Y = y.lim[2], stringsAsFactors = TRUE) # I had to create a data frame for geom_tex() so that facet is taken into account, (ggplot2::annotate() does not deal with facet because no data and mapping arguments). Of note, facet.categ is in tempo.stat, via tempo.mean, via dot.coord
            if(stat.mean == FALSE){tempo.stat$MEDIAN <- base::formatC(stat.nolog$MEDIAN, digits = 2, drop0trailing = TRUE, format = "f")}else{tempo.stat$MEAN <- base::formatC(stat.nolog$MEAN, digits = 2, drop0trailing = TRUE, format = "f")}
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_text(
                data = tempo.stat, 
                mapping = ggplot2::aes_string(x = "X", y = "Y", label = base::ifelse(stat.mean == FALSE, "MEDIAN", "MEAN")),
                size = stat.size, 
                color = "black", 
                angle = stat.angle, 
                hjust = stat.just$hjust, 
                vjust = stat.just$vjust
            )) # stat$X used here because identical to stat.nolog but has the X. WARNING: no need of order() for labels because box.coord$x set the order. For justification, see https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot
            coord.names <- base::c(coord.names, "stat.pos")
        }else if(stat.pos == "above"){
            # stat coordinates
            if( ! base::is.null(dot.color)){ # for text just above max dot
                if(dot.tidy == FALSE){
                    tempo.stat.ini <- dot.coord.rd3
                }else if(dot.tidy == TRUE){
                    tempo.stat.ini <- dot.coord.tidy3
                    tempo.stat.ini$x.y <- tempo.stat.ini$x.x # this is just to be able to use tempo.stat.ini$x.y for untidy or tidy dots (remember that dot.coord.tidy3$x.y is not good, see above)
                }
                stat.coord1 <- base::aggregate(x = tempo.stat.ini["y"], by = {x.env <- if(base::length(categ)== 1L){base::list(tempo.stat.ini$group, tempo.stat.ini$PANEL, tempo.stat.ini$x.y, tempo.stat.ini[, categ[1]])}else if(base::length(categ) == 2L){base::list(tempo.stat.ini$group, tempo.stat.ini$PANEL, tempo.stat.ini$x.y, tempo.stat.ini[, categ[1]], tempo.stat.ini[, categ[2]])} ;base::names(x.env) <- if(base::length(categ)== 1L){base::c("group", "PANEL", "x.y", categ[1])}else if(base::length(categ) == 2L){base::c("group", "PANEL", "x.y", categ[1], categ[2])} ; x.env}, FUN = min, na.rm = TRUE)
                base::names(stat.coord1)[base::names(stat.coord1) == "y"] <- "dot.min"
                stat.coord2 <- base::aggregate(x = tempo.stat.ini["y"], by = {x.env <- if(base::length(categ)== 1L){base::list(tempo.stat.ini$group, tempo.stat.ini$PANEL, tempo.stat.ini$x.y, tempo.stat.ini[, categ[1]])}else if(base::length(categ) == 2L){base::list(tempo.stat.ini$group, tempo.stat.ini$PANEL, tempo.stat.ini$x.y, tempo.stat.ini[, categ[1]], tempo.stat.ini[, categ[2]])} ; base::names(x.env) <- if(base::length(categ)== 1L){base::c("group", "PANEL", "x.y", categ[1])}else if(base::length(categ) == 2L){base::c("group", "PANEL", "x.y", categ[1], categ[2])} ; x.env}, FUN = max, na.rm = TRUE)
                base::names(stat.coord2) <- base::paste0(base::names(stat.coord2), "_from.dot.max")
                base::names(stat.coord2)[base::names(stat.coord2) == "y_from.dot.max"] <- "dot.max"
                stat.coord3 <- base::cbind(box.coord[base::order(box.coord$group, box.coord$PANEL), ], stat.coord1[base::order(stat.coord1$group, stat.coord1$x.y), ], stat.coord2[base::order(stat.coord2$group, stat.coord2$x.y), ], stringsAsFactors = TRUE) # 
                if( ! base::all(base::identical(base::round(stat.coord3$x, 9), base::round(base::as.numeric(stat.coord3$x.y), 9)), na.rm = TRUE)){ # as.numeric() because stat.coord3$x is class "mapped_discrete" "numeric"
                    tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nFUSION OF box.coord, stat.coord1 AND stat.coord2 ACCORDING TO box.coord$x, stat.coord1$x.y AND stat.coord2$x.y IS NOT CORRECT. CODE HAS TO BE MODIFIED")
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
                }
                # text.coord <- stat.coord3[, c("x", "group", "dot.min", "dot.max")]
                # names(text.coord)[names(text.coord) == "dot.min"] <- "text.min.pos"
                #names(text.coord)[names(text.coord) == "dot.max"] <- "text.max.pos"
                box.coord <- box.coord[base::order(box.coord$x, box.coord$group, box.coord$PANEL), ]
                # text.coord <- text.coord[order(text.coord$x), ] # to be sure to have the two objects in the same order for x. WARNING: cannot add identical(as.integer(text.coord$group), as.integer(box.coord$group)) because with error, the correspondence between x and group is not the same
                stat.coord3 <- stat.coord3[base::order(stat.coord3$x, stat.coord3$group, stat.coord3$PANEL), ] # to be sure to have the two objects in the same order for x. WARNING: cannot add identical(as.integer(text.coord$group), as.integer(box.coord$group)) because with error, the correspondence between x and group is not the same
                if( ! (base::identical(box.coord$x, stat.coord3$x) & base::identical(box.coord$group, stat.coord3$group) & base::identical(box.coord$PANEL, stat.coord3$PANEL))){
                    tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\ntext.coord AND box.coord DO NOT HAVE THE SAME x, group AND PANEL COLUMN CONTENT")
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
                }
            }else{
                stat.coord3 <- box.coord
            }
            stat.coord3 <- base::data.frame(
                stat.coord3, 
                Y = stat.coord3[, base::ifelse(
                    base::is.null(dot.color), 
                    base::ifelse(base::diff(y.lim) > 0, "ymax", "ymin"), 
                    base::ifelse(base::diff(y.lim) > 0, "ymax_final", "ymin_final")
                )], 
                stringsAsFactors = TRUE
            ) # ymax is top whisker, ymax_final is top dot
            # stat.coord3 <- data.frame(stat.coord3, Y = vector("numeric", length = nrow(stat.coord3)), stringsAsFactors = TRUE)
            # check.Y <- as.logical(stat.coord3$Y) # convert everything in Y into FALSE (because Y is full of zero)
            # end stat coordinates
            # stat display
            # performed twice: first for y values >=0, then y values < 0, because only a single value allowed for hjust anf vjust
            if(stat.mean == FALSE){
                tempo.center.ref <- "middle"
            }else{
                tempo.center.ref <- "MEAN"
            }
            # if(base::is.null(dot.color)){
            # tempo.low.ref <- "ymin"
            # tempo.high.ref <- "ymax"
            # }else{
            # tempo.low.ref <- "ymin_final"
            # tempo.high.ref <- "ymax_final"
            # }
            # tempo.log.high <- if(diff(y.lim) > 0){stat.coord3[, tempo.center.ref] >= 0}else{stat.coord3[, tempo.center.ref] < 0}
            # tempo.log.low <- if(diff(y.lim) > 0){stat.coord3[, tempo.center.ref] < 0}else{stat.coord3[, tempo.center.ref] >= 0}
            # stat.coord3$Y[tempo.log.high] <- stat.coord3[tempo.log.high, tempo.high.ref]
            # stat.coord3$Y[tempo.log.low] <- stat.coord3[tempo.log.low, tempo.low.ref]
            # add distance
            stat.coord3$Y <- stat.coord3$Y + base::diff(y.lim) * stat.dist / 100
            # end add distance
            # correct median or mean text format
            if(y.log != "no"){
                stat.coord3[, tempo.center.ref] <- base::ifelse(y.log == "log2", 2, 10)^(stat.coord3[, tempo.center.ref])
            }
            stat.coord3[, tempo.center.ref] <- base::formatC(stat.coord3[, tempo.center.ref], digits = 2, drop0trailing = TRUE, format = "f")
            # end correct median or mean text format
            # if(any(tempo.log.high) == TRUE){
            # tempo.stat <- stat.coord3[tempo.log.high,]
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_text(
                data = stat.coord3, 
                mapping = ggplot2::aes_string(x = "x", y = "Y", label = tempo.center.ref),
                size = stat.size, 
                color = "black", 
                angle = stat.angle, 
                hjust = stat.just$hjust, 
                vjust = stat.just$vjust
            )) # WARNING: no need of order() for labels because box.coord$x set the order
            coord.names <- base::c(coord.names, "stat.pos")
            # }
            # if(any(tempo.log.low) == TRUE){
            # tempo.stat <- stat.coord3[tempo.log.low,]
            # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_text(
            # data = tempo.stat, 
            # mapping = ggplot2::aes_string(x = "x", y = "Y", label = tempo.center.ref),
            # size = stat.size, 
            # color = "black", 
            # hjust = base::ifelse(vertical == TRUE, 0.5, 0.5 + stat.dist), 
            # vjust = base::ifelse(vertical == TRUE, 0.5 + stat.dist, 0.5)
            # )) # WARNING: no need of order() for labels because box.coord$x set the order
            # coord.names <- c(coord.names, "stat.pos.negative")
            # }
            # end stat display
        }else{
            tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nCODE INCONSISTENCY 5")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end stat display
    # legend management
    if(legend.show == FALSE){ # must be here because must be before bef.final.plot <- 
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::guides(fill = "none", color = "none", alpha = "none")) # inactivate the initial legend
    }
    # end legend management
    
    
    
    # y scale management (cannot be before dot plot management)
    # the rescaling aspect is complicated and not intuitive. See:
    # explaination: https://github.com/tidyverse/ggplot2/issues/3948
    # the oob argument of scale_y_continuous() https://ggplot2.tidyverse.org/reference/scale_continuous.html
    # see also https://github.com/rstudio/cheatsheets/blob/master/data-visualization-2.1.pdf
    # secondary ticks
    bef.final.plot <- ggplot2::ggplot_build(base::eval(base::parse(text = base::paste(base::paste(base::paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), ' + if(vertical == TRUE){ggplot2::scale_y_continuous(expand = c(0, 0), limits = sort(y.lim), oob = scales::rescale_none)}else{ggplot2::coord_flip(ylim = y.lim)}')))) # here I do not need the x-axis and y-axis orientation, I just need the number of main ticks and the legend. I DI NOT UNDERSTAND THE COMMENT HERE BECAUSE WE NEED COORD_FLiP
    tempo.coord <- bef.final.plot$layout$panel_params[[1]]
    # y.second.tick.positions: coordinates of secondary ticks (only if y.second.tick.nb argument is non NULL or if y.log argument is different from "no")
    if(y.log != "no"){ # integer main ticks for log2 and log10
        tempo.scale <- (base::as.integer(base::min(y.lim, na.rm = TRUE)) - 1):(base::as.integer(base::max(y.lim, na.rm = TRUE)) + 1)
    }else{
        tempo <- if(base::is.null(base::attributes(tempo.coord$y$breaks))){tempo.coord$y$breaks}else{base::unlist(base::ttributes(tempo.coord$y$breaks))}
        if(base::all(base::is.na(tempo))){# all() without na.rm -> ok because is.na() cannot be NA
            tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nONLY NA IN tempo.coord$y$breaks")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        tempo.scale <- saferGraph::scale2(lim = y.lim, n = base::ifelse(base::is.null(y.tick.nb), base::length(tempo[ ! base::is.na(tempo)]), y.tick.nb)) # in ggplot 3.3.0, tempo.coord$y.major_source replaced by tempo.coord$y$breaks. If fact: n = base::ifelse(base::is.null(y.tick.nb), length(tempo[ ! is.na(tempo)]), y.tick.nb)) replaced by n = base::ifelse(base::is.null(y.tick.nb), 4, y.tick.nb))
    }
    y.second.tick.values <- NULL
    y.second.tick.pos <- NULL
    if(y.log != "no"){
        tempo <- saferGraph::inter_ticks(lim = y.lim, log = y.log)
        y.second.tick.values <- tempo$values
        y.second.tick.pos <- tempo$coordinates
        # if(vertical == TRUE){ # do not remove in case the bug is fixed
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(geom = "segment", y = y.second.tick.pos, yend = y.second.tick.pos, x = tempo.coord$x.range[1], xend = tempo.coord$x.range[1] + base::diff(tempo.coord$x.range) / 80))
        # }else{ # not working because of the ggplot2 bug
        # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(geom = "segment", x = y.second.tick.pos, xend = y.second.tick.pos, y = tempo.coord$y.range[1], yend = tempo.coord$y.range[1] + diff(tempo.coord$y.range) / 80))
        # }
        coord.names <- base::c(coord.names, "y.second.tick.positions")
    }else if(( ! base::is.null(y.second.tick.nb)) & y.log == "no"){
        # if(y.second.tick.nb > 0){ #inactivated because already checked before
        if(base::length(tempo.scale) < 2){
            tempo_cat1 <- base::c("y.tick.nb", "y.second.tick.nb")
            tempo_cat2 <- base::sapply(base::list(y.tick.nb, y.second.tick.nb), FUN = base::paste0, collapse = " ")
            tempo.sep <- base::sapply(base::mapply(" ", base::max(base::nchar(tempo_cat1)) - base::nchar(tempo_cat1) + 3, FUN = rep, SIMPLIFY = FALSE), FUN = base::paste0, collapse = "")
            tempo_cat <- base::paste0("ERROR IN ", function_name, "\nTHE NUMBER OF GENERATED TICKS FOR THE Y-AXIS IS NOT CORRECT: ", base::length(tempo.scale), "\nUSING THESE ARGUMENT SETTINGS (NO DISPLAY MEANS NULL VALUE):\n", base::paste0(tempo_cat1, tempo.sep, tempo_cat2, collapse = "\n"), "\nPLEASE, TEST OTHER VALUES")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            tempo <- saferGraph::inter_ticks(lim = y.lim, log = y.log, breaks = tempo.scale, n = y.second.tick.nb)
        }
        y.second.tick.values <- tempo$values
        y.second.tick.pos <- tempo$coordinates
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(
            geom = "segment", 
            y = y.second.tick.pos, 
            yend = y.second.tick.pos, 
            x = if(vertical == TRUE){tempo.coord$x.range[1]}else{tempo.coord$y.range[1]}, 
            xend = if(vertical == TRUE){tempo.coord$x.range[1] + base::diff(tempo.coord$x.range) / 80}else{tempo.coord$y.range[1] + base::diff(tempo.coord$y.range) / 80}
        ))
        coord.names <- base::c(coord.names, "y.second.tick.positions")
    }
    # end y.second.tick.positions
    # for the ggplot2 bug with y.log, this does not work: eval(parse(text = base::ifelse(vertical == FALSE & y.log == "log10", "ggplot2::scale_x_continuous", "ggplot2::scale_y_continuous")))
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_y_continuous(
        breaks = tempo.scale, 
        minor_breaks = y.second.tick.pos, 
        labels = if(y.log == "log10"){scales::trans_format("identity", scales::math_format(10^.x))}else if(y.log == "log2"){scales::trans_format("identity", scales::math_format(2^.x))}else if(y.log == "no"){ggplot2::waiver()}else{tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nCODE INCONSISTENCY 6") ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)}, # == in stop() to be able to add several messages between ==
        expand = base::c(0, 0), # remove space after after axis limits
        limits = base::sort(y.lim), # NA indicate that limits must correspond to data limits but ylim() already used
        oob = scales::rescale_none, 
        trans = base::ifelse(base::diff(y.lim) < 0, "reverse", "identity") # equivalent to ggplot2::scale_y_reverse() but create the problem of y-axis label disappearance with y.lim decreasing. Thus, do not use. Use ylim() below and after this
    ))
    if(vertical == TRUE){
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::coord_cartesian(ylim = y.lim)) # problem of ggplot2::ylim() is that it redraws new breaks # coord_cartesian(ylim = y.lim)) not used because bug -> y-axis label disappearance with y.lim decreasing I DO NOT UNDERSTAND THIS MESSAGE WHILE I USE COORD_CARTESIAN # clip = "off" to have secondary ticks outside plot region does not work
    }else{
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::coord_flip(ylim = y.lim)) # clip = "off" to have secondary ticks outside plot region does not work # create the problem of y-axis label disappearance with y.lim decreasing. IDEM ABOVE
        
    }
    # end y scale management (cannot be before dot plot management)
    
    
    # legend management
    if( ! base::is.null(legend.width)){
        legend.final <- fun_gg_get_legend(ggplot_built = bef.final.plot, fun.name = function_name, lib_path = lib_path) # get legend
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::guides(fill = "none", color = "none", alpha = "none")) # inactivate the initial legend
        if(base::is.null(legend.final) & plot == TRUE){ # even if any(unlist(legend.disp)) is TRUE
            legend.final <- ggplot2::ggplot()+ggplot2::theme_void() # empty graph instead of legend
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") LEGEND REQUESTED (NON NULL categ ARGUMENT OR legend.show ARGUMENT SET TO TRUE)\nBUT IT SEEMS THAT THE PLOT HAS NO LEGEND -> EMPTY LEGEND SPACE CREATED BECAUSE OF THE NON NULL legend.width ARGUMENT\n")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
        }
    }
    # end legend management
    
    
    # drawing
    fin.plot <- base::suppressMessages(base::suppressWarnings(base::eval(base::parse(text = base::paste(base::paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + ")))))
    grob.save <- NULL
    if(plot == TRUE){
        # following lines inactivated because of problem in warn.recov and message.recov
        # assign("env_fun_get_message", new.env())
        # assign("tempo.gg.name", tempo.gg.name, envir = env_fun_get_message)
        # assign("tempo.gg.count", tempo.gg.count, envir = env_fun_get_message)
        # assign("add", add, envir = env_fun_get_message)
        # two next line: for the moment, I cannot prevent the warning printing
        # warn.recov <- fun_get_message(paste(paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), if(base::is.null(add)){NULL}else{add}), kind = "warning", header = FALSE, print.no = FALSE, env = env_fun_get_message) # for recovering warnings printed by ggplot() functions
        # message.recov <- fun_get_message('print(eval(parse(text = paste(paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), if(base::is.null(add)){NULL}else{add}))))', kind = "message", header = FALSE, print.no = FALSE, env = env_fun_get_message) # for recovering messages printed by ggplot() functions
        # if( ! (return == TRUE & return.ggplot == TRUE)){ # because return() plots when return.ggplot is TRUE # finally not used -> see return.ggplot description
        if(base::is.null(legend.width)){
            grob.save <- base::suppressMessages(base::suppressWarnings(gridExtra::grid.arrange(fin.plot)))
        }else{
            grob.save <-base::suppressMessages(base::suppressWarnings(gridExtra::grid.arrange(fin.plot, legend.final, ncol=2, widths=base::c(1, legend.width))))
        }
        # }
        # suppressMessages(suppressWarnings(print(eval(parse(text = paste(paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), if(base::is.null(add)){NULL}else{add}))))))
    }else{
        # following lines inactivated because of problem in warn.recov and message.recov
        # message.recov <- NULL
        # warn.recov <- NULL
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") PLOT NOT SHOWN AS REQUESTED")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo_warn, base::paste0(warn, "\n\n", tempo_warn)))
    }
    # end drawing
    
    
    
    # output
    # following lines inactivated because of problem in warn.recov and message.recov
    # if( ! (base::is.null(warn) & base::is.null(warn.recov) & base::is.null(message.recov))){
    # warn <- paste0(warn, "\n\n", if(length(warn.recov) > 0 | length(message.recov) > 0){paste0(paste0("MESSAGES FROM ggplot2 FUNCTIONS: ", base::ifelse( ! base::is.null(warn.recov), unique(message.recov), ""), base::ifelse( ! base::is.null(message.recov), unique(message.recov), ""), collapse = "\n\n"), "\n\n")})
    # }else if( ! (base::is.null(warn) & base::is.null(warn.recov)) & base::is.null(message.recov)){
    # warn <- paste0(warn, "\n\n", if(length(warn.recov) > 0){paste0(paste0("MESSAGES FROM ggplot2 FUNCTIONS: ", unique(warn.recov), collapse = "\n\n"), "\n\n")})
    # }else if( ! (base::is.null(warn) & base::is.null(message.recov)) & base::is.null(warn.recov)){
    # warn <- paste0(warn, "\n\n", if(length(message.recov) > 0){paste0(paste0("MESSAGES FROM ggplot2 FUNCTIONS: ", unique(message.recov), collapse = "\n\n"), "\n\n")})
    # }
    if(warn.print == TRUE & ! base::is.null(warn)){
        base::on.exit(base::warning(base::paste0("FROM ", function_name, ":\n\n", warn), call. = FALSE))
    }
    base::on.exit(exp = base::options(warning.length = ini.warning.length), add = TRUE)
    if(return == TRUE){
        tempo.output <- ggplot2::ggplot_build(fin.plot)
        tempo.output$data <- tempo.output$data[-1] # remove the first data because corresponds to the initial empty boxplot
        if(base::length(tempo.output$data) != base::length(coord.names)){
            tempo_cat <- base::paste0("INTERNAL CODE ERROR IN ", function_name, "\nlength(tempo.output$data) AND length(coord.names) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            base::names(tempo.output$data) <- coord.names
            tempo.output$data <- tempo.output$data[coord.names != "bad_remove"]
        }
        tempo <- tempo.output$layout$panel_params[[1]]
        output <- base::list(
            data = data1.ini, 
            stat = stat.nolog, 
            removed.row.nb = removed.row.nb, 
            removed.rows = removed.rows, 
            plot = base::c(tempo.output$data, y.second.tick.values = base::list(y.second.tick.values)), 
            panel = facet.categ, 
            axes = base::list(
                x.range = tempo$x.range, 
                x.labels = if(base::is.null(base::attributes(tempo$x$breaks))){tempo$x$breaks}else{tempo$x$scale$get_labels()}, # is.null(attributes(tempo$x$breaks)) test if it is number (TRUE) or character (FALSE)
                x.positions = if(base::is.null(base::attributes(tempo$x$breaks))){tempo$x$breaks}else{base::unlist(base::attributes(tempo$x$breaks))}, 
                y.range = tempo$y.range, 
                y.labels = if(base::is.null(base::attributes(tempo$y$breaks))){tempo$y$breaks}else{tempo$y$scale$get_labels()}, 
                y.positions = if(base::is.null(base::attributes(tempo$y$breaks))){tempo$y$breaks}else{base::unlist(base::attributes(tempo$y$breaks))}
            ), 
            warn = base::paste0("\n", warn, "\n\n"), 
            ggplot = if(return.ggplot == TRUE){fin.plot}else{NULL}, # fin.plot plots the graph if return == TRUE
            gtable = if(return.gtable == TRUE){grob.save}else{NULL} 
        )
        base::return(output) # this plots the graph if return.ggplot is TRUE and if no assignment
    }
    # end output
    # end main code
}





