#' @title gg_scatter
#' @description
#' Plot ggplot2 scatterplot with the possibility to overlay dots from up to 3 different data frames (-> three different legends) and lines from up to 3 different data frames (-> three different legends) -> up to 6 overlays totally.
#' 
#' For ggplot2 specifications, see: https://ggplot2.tidyverse.org/articles/ggplot2-specs.html.
#' @param data1 a dataframe compatible with ggplot2, or a list of data frames. Order matters for the order of the legend and for the layer staking (starting from below to top)
#' @param x single character string of the data1 column name for x-axis coordinates. If data1 is a list, then x must be a list of single character strings, of same size as data1, with compartment 1 related to compartment 1 of data1, etc. Write NULL for each "geom_hline" in geom argument
#' @param y single character string of the data1 column name for y-axis coordinates. If data1 is a list, then y must be a list of single character strings, of same size as data1, with compartment 1 related to compartment 1 of data1, etc. Write NULL for each "geom_vline" in geom argument
#' @param categ either NULL or a single character string or a list of single character strings, indicating the data1 column names to use for categories which creates legend display
#' If categ == NULL, no categories -> no legend displayed
#' If data1 is a data frame, categ must be a single character string of the data1 column name for categories
#' If data1 is a list, then categ must be a list of single character strings, of same size as data1, with compartment 1 related to compartment 1 of data1, etc. Some of the list compartments can be NULL (no legend display for these compartments), and other not
#' @param categ.class.order either (1) NULL or (2) a vector of character strings or (3) a list of these vectors, setting the order of the classes of categ in the legend display
#' If categ.class.order is NULL, classes are represented according to the alphabetical order
#' If data1 is a data frame, categ.class.order must be a vector of character strings specifying the different classes in the categ column name of data1
#' If data1 is a list, then categ.class.order must be a list of vector of character strings, of same size as data1, with compartment 1 related to compartment 1 of data1, etc. Some of the list compartments can be NULL (alphabetical order for these compartments), and other not
#' @param color either (1) NULL, or (2) a vector of character strings or integers, or (3) a list of vectors of character strings or integers
#' If color is NULL, default colors of ggplot2
#' If data1 is a data frame, color argument can be either:
#' (1) a single color string. All the dots of the corresponding data1 will have this color, whatever the categ value (NULL or not)
#' (2) if categ is non-null, a vector of string colors, one for each class of categ. Each color will be associated according to the categ.class.order argument if specified, or to the alphabetical order of categ classes otherwise
#' (3) if categ is non-null, a vector or factor of string colors, like if it was one of the column of data1 data frame. WARNING: a single color per class of categ and a single class of categ per color must be respected
#' Positive integers are also accepted instead of character strings, as long as above rules about length are respected. Integers will be processed by gg_palette() using the max integer value among all the integers in color (see gg_palette())
#' If data1 is a list, then color argument must be either: 
#' (1) a list of character strings or integers, of same size as data1, with compartment 1 related to compartment 1 of data1, etc.
#' (2) a single character string or a single integer
#' With a list (first possibility), the rules described for when data1 is a data frame apply to each compartment of the list. Some of the compartments can be NULL. In that case, a different grey color will be used for each NULL compartment. With a single value (second possibility), the same color will be used for all the dots and lines, whatever the data1 list
#' @param geom single character string of the kind of plot, or a list of single character strings
#' Either:
#' "geom_point" (scatterplot)
#' "geom_line" (coordinates plotted then line connection, from the lowest to highest x coordinates first and from the lowest to highest y coordinates thenafter)
#' "geom_path" (coordinates plotted then line connection respecting the row order in data1)
#' "geom_step" coordinates plotted then line connection respecting the row order in data1 but drawn in steps). See the geom.step.dir argument
#' "geom_hline" (horizontal line, no x value provided)
#' "geom_vline" (vertical line, no y value provided)
#' "geom_stick" (dots as vertical bars)
#' If data1 is a list, then geom must be either:
#' (1) a list of single character strings, of same size as data1, with compartment 1 related to compartment 1 of data1, etc.
#' (2) a single character string. In that case the same kind of plot will apply for the different compartments of the data1 list
#' WARNING concerning "geom_hline" or "geom_vline":
#' (1) x or y argument must be NULL, respectively
#' (2) x.lim or y.lim argument must NOT be NULL, respectively, if only these kind of lines are drawn (if other geom present, then x.lim = NULL and y.lim = NULL will generate x.lim and y.lim defined by these other geom, which is not possible with "geom_hline" or "geom_vline" alone)
#' (3) the function will draw n lines for n values in the x argument column name of the data1 data frame. If several colors required, the categ argument must be specified and the corresponding categ column name must exist in the data1 data frame with a different class name for each row
#' @param geom.step.dir single character string indicating the direction when using "geom_step" of the geom argument, or a list of single character strings
#' Either:
#' "vh" (vertical then horizontal)
#' "hv" (horizontal then vertical)
#' "mid" (step half-way between adjacent x-values)
#' See https://ggplot2.tidyverse.org/reference/geom_path.html
#' If data1 is a list, then geom.step.dir must be either:
#' (1) a list of single character strings, of same size as data1, with compartment 1 related to compartment 1 of data1, etc. The value in compartments related to other geom values than "geom_step" will be ignored
#' (2) a single character string, which will be used for all the "geom_step" values of the geom argument, whatever the data1 list
#' @param geom.stick.base either (1) NULL or (2) a single numeric value or (3) a list of single numeric values, setting the base of the sticks when using "geom_stick" of the geom argument
#' If geom.stick.base is NULL, the bottom limit of the y-axis is taken as the base
#' If data1 is a list, then geom.stick.base must be either (1) a list of single numeric values, of same size as data1, with compartment 1 related to compartment 1 of data1, etc., or (2) a single numeric value. With a list (former possibility), the values in compartments related to other geom values than "geom_stick" will be ignored. With a single value (latter possibility), the same base will be used for all the sticks, whatever the data1 list
#' Warning: the y-axis limits are not modified by the value of geom.stick.base, meaning that this value can be outside of the range of y.lim. Add the value of geom.stick.base also in the y.lim argument if required
#' Warning: if geom.stick.base is NULL, the bottom limit of the y-axis is taken as the base. Thus, be careful with inverted y-axis
#' @param alpha single numeric value (from 0 to 1) of transparency. If data1 is a list, then alpha must be either (1) a list of single numeric values, of same size as data1, with compartment 1 related to compartment 1 of data1, etc., or (2) a single numeric value. In that case the same transparency will apply for the different compartments of the data1 list
#' @param dot.size single numeric value of dot shape radius? in mm. If data1 is a list, then dot.size must be either (1) a list of single numeric values, of same size as data1, with compartment 1 related to compartment 1 of data1, etc., or (2) a single numeric value. With a list (former possibility), the value in compartments related to lines will be ignored. With a single value (latter possibility), the same dot.size will be used for all the dots, whatever the data1 list
#' @param dot.shape value indicating the shape of the dots (see https://ggplot2.tidyverse.org/articles/ggplot2-specs.html) If data1 is a list, then dot.shape must be either (1) a list of single shape values, of same size as data1, with compartment 1 related to compartment 1 of data1, etc., or (2) a single shape value. With a list (former possibility), the value in compartments related to lines will be ignored. With a single value (latter possibility), the same dot.shape will be used for all the dots, whatever the data1 list
#' @param dot.border.size single numeric value of border dot width in mm. Write zero for no dot border. If data1 is a list, then dot.border.size must be either (1) a list of single numeric values, of same size as data1, with compartment 1 related to compartment 1 of data1, etc., or (2) a single numeric value. With a list (former possibility), the value in compartments related to lines will be ignored. With a single value (latter possibility), the same dot.border.size will be used for all the dots, whatever the data1 list
#' @param dot.border.color single character color string defining the color of the dot border (same border color for all the dots, whatever their categories). If dot.border.color == NULL, the border color will be the same as the dot color. A single integer is also accepted instead of a character string, that will be processed by gg_palette()
#' @param line.size single numeric value of line width in mm. If data1 is a list, then line.size must be either (1) a list of single numeric values, of same size as data1, with compartment 1 related to compartment 1 of data1, etc., or (2) a single numeric value. With a list (former possibility), the value in compartments related to dots will be ignored. With a single value (latter possibility), the same line.size will be used for all the lines, whatever the data1 list
#' @param line.type value indicating the kind of lines (see https://ggplot2.tidyverse.org/articles/ggplot2-specs.html) If data1 is a list, then line.type must be either (1) a list of single line kind values, of same size as data1, with compartment 1 related to compartment 1 of data1, etc., or (2) a single line kind value. With a list (former possibility), the value in compartments related to dots will be ignored. With a single value (latter possibility), the same line.type will be used for all the lines, whatever the data1 list
#' @param x.lim 2 numeric values setting the x-axis range. Order of the 2 values matters (for inverted axis). If NULL, the range of the x column name of data1 will be used
#' @param x.lab a character string or expression for x-axis label. If NULL, will use the first value of x (x column name of the first data frame in data1). Warning message if the elements in x are different between data frames in data1
#' @param x.log either "no", "log2" (values in the x column name of the data1 data frame will be log2 transformed and x-axis will be log2 scaled) or "log10" (values in the x column name of the data1 data frame will be log10 transformed and x-axis will be log10 scaled)
#' @param x.tick.nb approximate number of desired values labeling the x-axis (i.e., main ticks, see the n argument of the the saferGraph::scale2() function). If NULL and if x.log is "no", then the number of labeling values is set by ggplot2. If NULL and if x.log is "log2" or "log10", then the number of labeling values corresponds to all the exposant integers in the x.lim range (e.g., 10^1, 10^2 and 10^3, meaning 3 main ticks for x.lim = c(9, 1200)). WARNING: if non-NULL and if x.log is "log2" or "log10", labeling can be difficult to read (e.g., ..., 10^2, 10^2.5, 10^3, ...)
#' @param x.second.tick.nb number of desired secondary ticks between main ticks. Ignored if x.log is other than "no" (log scale plotted). Use argument return = TRUE and see $plot$x.second.tick.values to have the values associated to secondary ticks. IF NULL, no secondary ticks
#' @param x.include.zero logical. Does x.lim range include 0? Ignored if x.log is "log2" or "log10"
#' @param x.left.extra.margin single proportion (between 0 and 1) indicating if extra margins must be added to x.lim. If different from 0, add the range of the axis multiplied by x.left.extra.margin (e.g., abs(x.lim[2] - x.lim[1]) * x.left.extra.margin) to the left of x-axis
#' @param x.right.extra.margin idem as x.left.extra.margin but to the right of x-axis
#' @param x.text.angle integer value of the text angle for the x-axis labeling values, using the same rules as in ggplot2. Use positive value for clockwise rotation: 0 for horizontal, 90 for vertical, 180 for upside down etc. Use negative values for counterclockwise rotation: 0 for horizontal, -90 for vertical, -180 for upside down etc.
#' @param y.lim 2 numeric values setting the y-axis range. Order of the 2 values matters (for inverted axis). If NULL, the range of the y column name of data1 will be used
#' @param y.lab a character string or expression for y-axis label. If NULL, will use the first value of y (y column name of the first data frame in data1). Warning message if the elements in y are different between data frames in data1
#' @param y.log either "no", "log2" (values in the y column name of the data1 data frame will be log2 transformed and y-axis will be log2 scaled) or "log10" (values in the y column name of the data1 data frame will be log10 transformed and y-axis will be log10 scaled)
#' @param y.tick.nb approximate number of desired values labeling the y-axis (i.e., main ticks, see the n argument of the the saferGraph::scale2() function). If NULL and if y.log is "no", then the number of labeling values is set by ggplot2. If NULL and if y.log is "log2" or "log10", then the number of labeling values corresponds to all the exposant integers in the y.lim range (e.g., 10^1, 10^2 and 10^3, meaning 3 main ticks for y.lim = c(9, 1200)). WARNING: if non-NULL and if y.log is "log2" or "log10", labeling can be difficult to read (e.g., ..., 10^2, 10^2.5, 10^3, ...)
#' @param y.second.tick.nb number of desired secondary ticks between main ticks. Ignored if y.log is other than "no" (log scale plotted). Use argument return = TRUE and see $plot$y.second.tick.values to have the values associated to secondary ticks. IF NULL, no secondary ticks
#' @param y.include.zero logical. Does y.lim range include 0? Ignored if y.log is "log2" or "log10"
#' @param y.top.extra.margin single proportion (between 0 and 1) indicating if extra margins must be added to y.lim. If different from 0, add the range of the axis multiplied by y.top.extra.margin (e.g., abs(y.lim[2] - y.lim[1]) * y.top.extra.margin) to the top of y-axis
#' @param y.bottom.extra.margin idem as y.top.extra.margin but to the bottom of y-axis
#' @param y.text.angle integer value of the text angle for the y-axis labeling values, using the same rules as in ggplot2. Use positive value for clockwise rotation: 0 for horizontal, 90 for vertical, 180 for upside down etc. Use negative values for counterclockwise rotation: 0 for horizontal, -90 for vertical, -180 for upside down etc.
#' @param raster logical. Dots in raster mode? If FALSE, dots from each "geom_point" from geom argument are plotted in vectorial mode (bigger pdf and long to display if lots of dots). If TRUE, dots from each "geom_point" from geom argument are plotted in matricial mode (smaller pdf and easy display if lots of dots, but it takes time to generate the layer). If TRUE, the raster.ratio argument is used to avoid an ellipsoid representation of the dots. If TRUE, solve the transparency problem with some GUI. Overriden by the non-NULL raster.threshold argument
#' @param raster.ratio single numeric value indicating the height / width ratio of the graphic device used (for instance provided by the $dim compartment in the output of the fun_open() function). The default value is 1 because by default R opens a square graphic device. But this argument has to be set when using other device dimensions. Ignored if raster == FALSE
#' @param raster.threshold positive integer value indicating the limit of the dot number above which "geom_point" layers from the geom argument switch from vectorial mode to matricial mode (see the raster argument). If any layer is matricial, then the raster.ratio argument is used to avoid an ellipsoid representation of the dots. If non-NULL, it overrides the raster argument
#' @param text.size numeric value of the font size of the (1) axis numbers and axis legends and (2) texts in the graphic legend (in mm)
#' @param title character string of the graph title
#' @param title.text.size numeric value of the title font size in mm
#' @param legend.show logical. Show legend? Not considered if categ argument is NULL, because this already generate no legend, excepted if legend.width argument is non-NULL. In that specific case (categ is NULL, legend.show is TRUE and legend.width is non-NULL), an empty legend space is created. This can be useful when desiring graphs of exactly the same width, whatever they have legends or not
#' @param legend.width single proportion (between 0 and 1) indicating the relative width of the legend sector (on the right of the plot) relative to the width of the plot. Value 1 means that the window device width is split in 2, half for the plot and half for the legend. Value 0 means no room for the legend, which will overlay the plot region. Write NULL to inactivate the legend sector. In such case, ggplot2 will manage the room required for the legend display, meaning that the width of the plotting region can vary between graphs, depending on the text in the legend
#' @param legend.name character string of the legend title. If legend.name is NULL and categ argument is not NULL, then legend.name <- categ. If data1 is a list, then legend.name must be a list of character strings, of same size as data1, with compartment 1 related to compartment 1 of data1, etc. Some of the list compartments can be NULL, and other not
#' @param article logical. If TRUE, use an article theme (article like). If FALSE, use a classic related ggplot theme. Use the add argument (e.g., add = "+ggplot2::theme_classic()" for the exact classic ggplot theme
#' @param grid logical. Draw lines in the background to better read the box values? Not considered if article == FALSE (grid systematically present)
#' @param add character string allowing to add more ggplot2 features (dots, lines, themes, facet, etc.). Ignored if NULL
#' WARNING: (1) the string must start with "+", (2) the string must finish with ")" and (3) each function must be preceded by "ggplot2::". Example: "+ ggplot2::coord_flip() + ggplot2::theme_bw()"
#' If the character string contains the "ggplot2::theme" string, then the article argument of fun_gg_scatter() (see above) is ignored with a warning. In addition, some arguments can be overwritten, like x.angle (check all the arguments)
#' Handle the add argument with caution since added functions can create conflicts with the preexisting internal ggplot2 functions
#' WARNING: the call of objects inside the quotes of add can lead to an error if the name of these objects are some of the fun_gg_scatter() arguments. Indeed, the function will use the internal argument instead of the global environment object. Example article <- "a" in the working environment and add = '+ ggplot2::ggtitle(article)'. The risk here is to have TRUE as title. To solve this, use add = '+ ggplot2::ggtitle(get("article", envir = .GlobalEnv))'
#' @param return logical. Return the graph parameters?
#' @param return.ggplot logical. Return the ggplot object in the output list? Ignored if return argument is FALSE. WARNING: always assign the fun_gg_scatter() function (e.g., a <- fun_gg_scatter()) if return.ggplot argument is TRUE, otherwise, double plotting is performed. See $ggplot in the RETURN section below for more details
#' @param return.gtable logical. Return the ggplot object as gtable of grobs in the output list? Ignored if plot argument is FALSE. Indeed, the graph must be plotted to get the grobs dispositions. See $gtable in the RETURN section below for more details
#' @param plot logical. Plot the graphic? If FALSE and return argument is TRUE, graphical parameters and associated warnings are provided without plotting
#' @param warn.print logical. Print warnings at the end of the execution? ? If FALSE, warning messages are never printed, but can still be recovered in the returned list. Some of the warning messages (those delivered by the internal ggplot2 functions) are not apparent when using the argument plot = FALSE
#' @param safer_check Single logical value. Perform some "safer" checks? If TRUE, checkings are performed before main code running (see https://github.com/safer-r): 1) correct lib_path argument value 2) required functions and related packages effectively present in local R lybraries and 3) R classical operators (like "<-") not overwritten by another package because of the R scope. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R package are not installed in the default directories because of lack of admin rights.  More precisely, lib_path is passed through the new argument of .libPaths() so that the new library paths are unique(c(new, .Library.site, .Library)). Warning: .libPaths() is restored to the initial paths, after function execution. Ignored if NULL (default) or if the safer_check argument is FALSE: only the pathways specified by the current .libPaths() are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>.". If NULL, converted into "".
#' @returns  An error message if at least one of the checked packages is missing in lib_path, or if at least one of the checked functions is missing in the required package, nothing otherwise.
#' @returns
#' a scatter plot if plot argument is TRUE
#' a list of the graph info if return argument is TRUE:
#' $data: the initial data with graphic information added. WARNING: if the x.log or y.log argument is not "no", x or y argument column of the data1 data frame are log2 or log10 converted in $data, respectively. Use 2^values or 10^$values to recover the initial values
#' $removed.row.nb: a list of the removed rows numbers in data frames (because of NA). NULL if no row removed
#' $removed.rows: a list of the removed rows in data frames (because of NA). NULL if no row removed
#' $plot: the graphic box and dot coordinates
#' $dots: dot coordinates
#' y.second.tick.positions: coordinates of secondary ticks (only if y.second.tick.nb argument is non-null or if y.log argument is different from "no")
#' y.second.tick.values: values of secondary ticks. NULL except if y.second.tick.nb argument is non-null or if y.log argument is different from "no")
#' $panel: the variable names used for the panels (NULL if no panels). WARNING: NA can be present according to ggplot2 upgrade to v3.3.0
#' $axes: the x-axis and y-axis info
#' $warn: the warning messages. Use cat() for proper display. NULL if no warning. WARNING: warning messages delivered by the internal ggplot2 functions are not apparent when using the argument plot = FALSE
#' $ggplot: ggplot object that can be used for reprint (use print($ggplot) or update (use $ggplot + ggplot2::...). NULL if return.ggplot argument is FALSE. Of note, a non-null $ggplot in the output list is sometimes annoying as the manipulation of this list prints the plot
#' $gtable: gtable object that can be used for reprint (use gridExtra::grid.arrange(...$ggplot) or with additionnal grobs (see the grob decomposition in the examples). NULL if return.ggplot argument is FALSE. Contrary to $ggplot, a non-NULL $gtable in the output list is not annoying as the manipulation of this list does not print the plot
#' @details 
#' WARNINGS
#' 
#' Rows containing NA in data1[, c(x, y, categ)] will be removed before processing, with a warning (see below).
#' 
#' Size arguments (dot.size, dot.border.size, line.size, text.size and title.text.size) are in mm. See Hadley comment in https://stackoverflow.com/questions/17311917/ggplot2-the-unit-of-size. See also http://sape.inf.usi.ch/quick-reference/ggplot2/size). Unit object are not accepted, but conversion can be used (e.g., grid::convertUnit(grid::unit(0.2, "inches"), "mm", valueOnly = TRUE)).
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @examples
#' set.seed(1) ; obs1 <- data.frame(Km = c(2, 1, 6, 5, 4, 7), Time = c(2, 1, 6, 5, 4, 7)^2, Car = c("TUUT", "TUUT", "TUUT", "WIIM", "WIIM", "WIIM"), Color1 = rep(c("coral", "lightblue"), each = 3), stringsAsFactors = TRUE) ; gg_scatter(data1 = obs1, x = "Km", y = "Time")
#' # @importFrom # TODO
#' @export
gg_scatter <- function(
    data1, 
    x, 
    y, 
    categ = NULL, 
    categ.class.order = NULL, 
    color = NULL, 
    geom = "geom_point", 
    geom.step.dir = "hv", 
    geom.stick.base = NULL, 
    alpha = 0.5, 
    dot.size = 2, 
    dot.shape = 21, 
    dot.border.size = 0.5, 
    dot.border.color = NULL, 
    line.size = 0.5, 
    line.type = "solid", 
    x.lim = NULL, 
    x.lab = NULL, 
    x.log = "no", 
    x.tick.nb = NULL, 
    x.second.tick.nb = NULL, 
    x.include.zero = FALSE, 
    x.left.extra.margin = 0.05, 
    x.right.extra.margin = 0.05, 
    x.text.angle = 0, 
    y.lim = NULL, 
    y.lab = NULL, 
    y.log = "no", 
    y.tick.nb = NULL, 
    y.second.tick.nb = NULL, 
    y.include.zero = FALSE, 
    y.top.extra.margin = 0.05, 
    y.bottom.extra.margin = 0.05, 
    y.text.angle = 0, 
    raster = FALSE, 
    raster.ratio = 1, 
    raster.threshold = NULL, 
    text.size = 12, 
    title = "", 
    title.text.size = 12, 
    legend.show = TRUE, 
    legend.width = 0.5, 
    legend.name = NULL, 
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
    # set.seed(1) ; obs1 <- data.frame(km = rnorm(1000, 10, 3), time = rnorm(1000, 10, 3), group1 = rep(c("A1", "A2"), 500), stringsAsFactors = TRUE) ; obs2 <-data.frame(km = rnorm(1000, 15, 3), time = rnorm(1000, 15, 3), group2 = rep(c("G1", "G2"), 500), stringsAsFactors = TRUE) ; set.seed(NULL) ; obs1$km[2:3] <- NA ; data1 = list(L1 = obs1, L2 = obs2) ; x = list(L1 = "km", L2 = "km") ; y = list(L1 = "time", L2 = "time") ; categ = list(L1 = "group1", L2 = "group2") ; categ = NULL ; categ.class.order = NULL ; color = NULL ; geom = "geom_point" ; geom.step.dir = "hv" ; geom.stick.base = NULL ; alpha = 0.5 ; dot.size = 2 ; dot.shape = 21 ; dot.border.size = 0.5 ; dot.border.color = NULL ; line.size = 0.5 ; line.type = "solid" ; x.lim = NULL ; x.lab = NULL ; x.log = "no" ; x.tick.nb = NULL ; x.second.tick.nb = NULL ; x.include.zero = FALSE ; x.left.extra.margin = 0.05 ; x.right.extra.margin = 0.05 ; x.text.angle = 0 ; y.lim = NULL ; y.lab = NULL ; y.log = "no" ; y.tick.nb = NULL ; y.second.tick.nb = NULL ; y.include.zero = FALSE ; y.top.extra.margin = 0.05 ; y.bottom.extra.margin = 0.05 ; y.text.angle = 0 ; raster = FALSE ; raster.ratio = 1 ; raster.threshold = NULL ; text.size = 12 ; title = "" ; title.text.size = 12 ; legend.show = TRUE ; legend.width = 0.5 ; legend.name = NULL ; article = TRUE ; grid = FALSE ; add = NULL ; return = FALSE ; return.ggplot = FALSE ; return.gtable = TRUE ; plot = TRUE ; warn.print = FALSE ; lib.path = NULL
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
    # evaluation of values if they are espression, call, etc.
    if(base::length(x = arg_user_setting) != 0){
        arg_user_setting_eval <- base::lapply(
            X = arg_user_setting_names, 
            FUN = function(x){
                base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = TRUE) # n = 2 because of lapply(), inherit = TRUE to be sure to correctly evaluate
            }
        )
        base::names(x = arg_user_setting_eval) <- arg_user_setting_names
    }
    # end evaluation of values if they are espression, call, etc.
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
    # use this in the error_text of safer functions if present below 
    embed_error_text  <- base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    embed_error_text  <- base::sub(pattern = "\n*$", replacement = "", x = embed_error_text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove all the trailing \n, because added later
    ######## end error text when embedding

    #### end error_text initiation

    #### argument primary checking

    ######## arg with no default values
    mandat_args <- base::c(
        "data1", 
        "x", 
        "y"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", 
            base::paste0(mandat_args[tempo], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end arg with no default values

    ######## management of NULL arguments
    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "data1", 
        "x", 
        "y", 
        # "categ", # inactivated because can be NULL
        # "categ.class.order", # inactivated because can be NULL
        # "color", # inactivated because can be NULL
        "geom", 
        "geom.step.dir", 
        # "geom.stick.base", # inactivated because can be NULL
        "alpha", 
        "dot.size", 
        "dot.shape", 
        "dot.border.size", 
        # "dot.border.color", # inactivated because can be NULL
        "line.size", 
        "line.type", 
        # "x.lim", # inactivated because can be NULL
        # "x.lab", # inactivated because can be NULL
        "x.log", 
        # "x.tick.nb", # inactivated because can be NULL
        # "x.second.tick.nb", # inactivated because can be NULL
        "x.include.zero", 
        "x.left.extra.margin", 
        "x.right.extra.margin", 
        "x.text.angle", 
        # "y.lim", # inactivated because can be NULL
        # "y.lab", # inactivated because can be NULL
        "y.log", 
        # "y.tick.nb", # inactivated because can be NULL
        # "y.second.tick.nb", # inactivated because can be NULL
        "y.include.zero", 
        "y.top.extra.margin", 
        "y.bottom.extra.margin", 
        "y.text.angle", 
        "raster", 
        "raster.ratio", 
        # "raster.threshold", # inactivated because can be NULL
        "text.size", 
        "title", 
        "title.text.size", 
        "legend.show", 
        "legend.width", 
        # "legend.name", # inactivated because can be NULL
        "article", 
        "grid", 
        # "add", # inactivated because can be NULL
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
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # # before NA checking because is.na(logical()) is logical(0) (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "data1", 
        "x", 
        "y", 
        "categ", 
        "categ.class.order", 
        "color", 
        "geom", 
        "geom.step.dir", 
        "geom.stick.base", 
        "alpha", 
        "dot.size", 
        "dot.shape", 
        "dot.border.size", 
        "dot.border.color", 
        "line.size", 
        "line.type", 
        "x.lim", 
        "x.lab", 
        "x.log", 
        "x.tick.nb", 
        "x.second.tick.nb", 
        "x.include.zero", 
        "x.left.extra.margin", 
        "x.right.extra.margin", 
        "x.text.angle", 
        "y.lim", 
        "y.lab", 
        "y.log", 
        "y.tick.nb", 
        "y.second.tick.nb", 
        "y.include.zero", 
        "y.top.extra.margin", 
        "y.bottom.extra.margin", 
        "y.text.angle", 
        "raster", 
        "raster.ratio", 
        "raster.threshold", 
        "text.size", 
        "title", 
        "title.text.size", 
        "legend.show", 
        "legend.width", 
        "legend.name", 
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
    tempo_arg_user_setting_eval <- arg_user_setting_eval[base::names(arg_user_setting_eval) %in% tempo_arg]
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
        ) # no argument provided by the user can be empty non NULL object. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even is 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE AN EMPTY NON NULL OBJECT:\n", 
                base::paste0(tempo_arg_user_setting_eval[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
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
        ) # no argument provided by the user can be just made of NA. is.na(NULL) returns logical(0), the reason why base::length(x = x) > 0 is used # warning: all(x = x, na.rm = TRUE) but normally no NA because base::is.na() used here. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even is 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE MADE OF NA ONLY:\n", 
                base::paste0(arg_user_setting_names[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    if( ! (base::all(base::typeof(x = safer_check) == "logical", na.rm = TRUE) & base::length(x = safer_check) == 1)){ # no need to test NA because NA only already managed above and base::length(x = safer_check) == 1)
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).\nHERE IT IS:\n", 
            base::ifelse(test = base::length(x = safer_check) == 0 | base::all(safer_check == base::quote(expr = ), na.rm = TRUE) | base::all(safer_check == "", na.rm = TRUE), yes = "<NULL, \"\", EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(safer_check, collapse = "\n", recycle0 = FALSE)),
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end safer_check argument checking

    ######## check of lib_path
    # must be before any :: or ::: non basic package calling
    if(safer_check == TRUE){
        if( ! base::is.null(x = lib_path)){ #  is.null(NA) returns FALSE so OK.
            if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = TRUE)){ # na.rm = TRUE but no NA returned with typeof (typeof(NA) == "character" returns FALSE)
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\nHERE IT IS:\n", 
                    base::ifelse(test = base::length(x = lib_path) == 0 | base::all(lib_path == base::quote(expr = ), na.rm = TRUE), yes = "<NULL, EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(lib_path, collapse = "\n", recycle0 = FALSE)),
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
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
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
            }else{
                ini_lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
                base::on.exit(expr = base:::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
                base:::.libPaths(new = base::sub(x = lib_path, pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base:::.libPaths(new = ) add path to default path. BEWARE: base:::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
                lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            }
        }else{
            lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base:::.libPaths(new = lib_path) # or base:::.libPaths(new = base::c(base:::.libPaths(), lib_path))
        }
    }
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        saferDev:::.pack_and_function_check(
            fun = base::c(
                "saferDev::arg_check", # write each function preceeded by their package name
                "saferDev:::.base_op_check"
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    if(safer_check == TRUE){
        saferDev:::.base_op_check(
            error_text = embed_error_text
        )
    }
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()

    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    tempo_arg <- base::c(
        "x", 
        "y", 
        "categ", 
        "categ.class.order", 
        "color", 
        "geom", 
        "geom.step.dir", 
        "dot.border.color", 
        "x.lab", 
        "x.log",  
        "y.lab", 
        "y.log", 
        # "title",  # inactivated because can be ""
        "legend.name", 
        "add"
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
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
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
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
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## reserved words
    reserved_words <- c("fake_x", "fake_y", "fake_categ")
    ######## end reserved words

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ini_warning_length <- base::options()$warning.length # required to have the max characters of output messages
    base::options(warning.length = 8170)
    warn <- NULL
    warn_count <- 0
    ######## end warning initiation

    ######## graphic device checking
    # check the number of graphic devices on exit
    # end check the number of graphic devices on exit
    # restore the graphic parameters on exit
    if(base::length(x = grDevices::dev.list()) > 0){
        par_ini <- base::suppressWarnings(expr = graphics::par(no.readonly = TRUE), classes = "warning") # to recover the present graphical parameters
        base::on.exit(expr = base::suppressWarnings(expr = graphics::par(par_ini, no.readonly = TRUE), classes = "warning"), add = TRUE, after = TRUE)
    }
    # end restore the graphic parameters on exit
    ######## end graphic device checking

    ######## other checkings
    # check list lengths (and names of data1 compartments if present)
    list.color <- NULL
    list.geom <- NULL
    list.geom.step.dir <- NULL
    list.geom.stick.base <- NULL
    list.alpha <- NULL
    list.dot.size <- NULL
    list.dot.shape <- NULL
    list.dot.border.size <- NULL
    list.dot.border.color <- NULL
    list.line.size <- NULL
    list.line.type <- NULL
    if(all(class(data1) == "list")){
        if(length(data1) > 6){
            tempo.cat <- paste0(error_text_start, "data1 ARGUMENT MUST BE A LIST OF 6 DATA FRAMES MAXIMUM (6 OVERLAYS MAX).")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if(is.null(names(data1))){
            names(data1) <- paste0("L", 1:length(data1))
            warn_count <- warn_count + 1
            tempo.warn <- paste0("(", warn_count,") NULL NAME COMPARTMENT OF data1 LIST -> NAMES RESPECTIVELY ATTRIBUTED TO EACH COMPARTMENT:\n", paste(names(data1), collapse = " "))
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
        if( ! is.null(x)){
            if( ! (all(class(x) == "list") & length(data1) == length(x))){
                tempo.cat <- paste0(error_text_start, "x ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST.")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }else{
            x <- vector("list", length(data1))
        }
        if( ! is.null(y)){
            if( ! (all(class(y) == "list") & length(data1) == length(y))){
                tempo.cat <- paste0(error_text_start, "y ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }else{
            y <- vector("list", length(data1))
        }
        if( ! is.null(categ)){
            if( ! (all(class(categ) == "list") & length(data1) == length(categ))){
                tempo.cat <- paste0(error_text_start, "categ ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
        if( ! is.null(categ.class.order)){
            if( ! (all(class(categ.class.order) == "list") & length(data1) == length(categ.class.order))){
                tempo.cat <- paste0(error_text_start, "categ.class.order ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
        if( ! is.null(color)){
            if( ! ((all(class(color) == "list") & length(data1) == length(color)) | ((all(mode(color) == "character") | all(mode(color) == "numeric")) & length(color)== 1L))){ # list of same length as data1 or single value
                tempo.cat <- paste0(error_text_start, "color ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE CHARACTER STRING OR INTEGER")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if((all(mode(color) == "character") | all(mode(color) == "numeric")) & length(color)== 1L){ # convert the single value into a list of single value
                list.color <- vector(mode = "list", length = length(data1))
                list.color[] <- color
            }
        }
        if( ! ((all(class(geom) == "list") & length(data1) == length(geom)) | (all(mode(geom) == "character") & length(geom)== 1L))){ # list of same length as data1 or single value
            tempo.cat <- paste0(error_text_start, "geom ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE CHARACTER VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(all(mode(geom) == "character") & length(geom)== 1L){ # convert the single value into a list of single value
            list.geom <- vector(mode = "list", length = length(data1))
            list.geom[] <- geom
        }
        if( ! ((all(class(geom.step.dir) == "list") & length(data1) == length(geom.step.dir)) | (all(mode(geom.step.dir) == "character") & length(geom.step.dir)== 1L))){ # list of same length as data1 or single value
            tempo.cat <- paste0(error_text_start, "geom.step.dir ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE CHARACTER VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(all(mode(geom.step.dir) == "character") & length(geom.step.dir)== 1L){ # convert the single value into a list of single value
            list.geom.step.dir <- vector(mode = "list", length = length(data1))
            list.geom.step.dir[] <- geom.step.dir
        }
        if( ! is.null(geom.stick.base)){
            if( ! ((all(class(geom.stick.base) == "list") & length(data1) == length(geom.stick.base)) | (all(mode(geom.stick.base) == "numeric") & length(geom.stick.base)== 1L))){ # list of same length as data1 or single value
                tempo.cat <- paste0(error_text_start, "geom.stick.base ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE NUMERIC VALUE")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if(all(mode(geom.stick.base) == "numeric") & length(geom.stick.base)== 1L){ # convert the single value into a list of single value
                list.geom.stick.base <- vector(mode = "list", length = length(data1))
                list.geom.stick.base[] <- geom.stick.base
            }
        }
        if( ! ((all(class(alpha) == "list") & length(data1) == length(alpha)) | (all(mode(alpha) == "numeric") & length(alpha)== 1L))){ # list of same length as data1 or single value
            tempo.cat <- paste0(error_text_start, "alpha ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE NUMERIC VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(all(mode(alpha) == "numeric") & length(alpha)== 1L){ # convert the single value into a list of single value
            list.alpha <- vector(mode = "list", length = length(data1))
            list.alpha[] <- alpha
        }
        if( ! ((all(class(dot.size) == "list") & length(data1) == length(dot.size)) | (all(mode(dot.size) == "numeric") & length(dot.size)== 1L))){ # list of same length as data1 or single value
            tempo.cat <- paste0(error_text_start, "dot.size ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE NUMERIC VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(all(mode(dot.size) == "numeric") & length(dot.size)== 1L){ # convert the single value into a list of single value
            list.dot.size <- vector(mode = "list", length = length(data1))
            list.dot.size[] <- dot.size
        }
        if( ! ((all(class(dot.shape) == "list") & length(data1) == length(dot.shape)) | (all(mode(dot.shape) != "list") & length(dot.shape)== 1L))){ # list of same length as data1 or single value
            tempo.cat <- paste0(error_text_start, "dot.shape ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE SHAPE VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(all(mode(dot.shape) != "list") & length(dot.shape)== 1L){ # convert the single value into a list of single value
            list.dot.shape <- vector(mode = "list", length = length(data1))
            list.dot.shape[] <- dot.shape
        }
        if( ! ((all(class(dot.border.size) == "list") & length(data1) == length(dot.border.size)) | (all(mode(dot.border.size) == "numeric") & length(dot.border.size)== 1L))){ # list of same length as data1 or single value
            tempo.cat <- paste0(error_text_start, "dot.border.size ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE NUMERIC VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(all(mode(dot.border.size) == "numeric") & length(dot.border.size)== 1L){ # convert the single value into a list of single value
            list.dot.border.size <- vector(mode = "list", length = length(data1))
            list.dot.border.size[] <- dot.border.size
        }
        if( ! is.null(dot.border.color)){
            if( ! ((all(class(dot.border.color) == "list") & length(data1) == length(dot.border.color)) | ((all(mode(dot.border.color) == "character") | all(mode(dot.border.color) == "numeric")) & length(dot.border.color)== 1L))){ # list of same length as data1 or single value
                tempo.cat <- paste0(error_text_start, "dot.border.color ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE CHARACTER STRING OR INTEGER")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if((all(mode(dot.border.color) == "character") | all(mode(dot.border.color) == "numeric")) & length(dot.border.color)== 1L){ # convert the single value into a list of single value
                list.dot.border.color <- vector(mode = "list", length = length(data1))
                list.dot.border.color[] <- dot.border.color
            }
        }
        if( ! ((all(class(line.size) == "list") & length(data1) == length(line.size)) | (all(mode(line.size) == "numeric") & length(line.size)== 1L))){ # list of same length as data1 or single value
            tempo.cat <- paste0(error_text_start, "line.size ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE NUMERIC VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(all(mode(line.size) == "numeric") & length(line.size)== 1L){ # convert the single value into a list of single value
            list.line.size <- vector(mode = "list", length = length(data1))
            list.line.size[] <- line.size
        }
        if( ! ((all(class(line.type) == "list") & length(data1) == length(line.type)) | (all(mode(line.type) != "list") & length(line.type)== 1L))){ # list of same length as data1 or single value
            tempo.cat <- paste0(error_text_start, "line.type ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST, OR A SINGLE LINE KIND VALUE")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(all(mode(line.type) != "list") & length(line.type)== 1L){ # convert the single value into a list of single value
            list.line.type <- vector(mode = "list", length = length(data1))
            list.line.type[] <- line.type
        }
        if( ! is.null(legend.name)){
            if( ! (all(class(legend.name) == "list") & length(data1) == length(legend.name))){
                tempo.cat <- paste0(error_text_start, "legend.name ARGUMENT MUST BE A LIST OF SAME LENGTH AS data1 IF data1 IS A LIST")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
    }
    # end check list lengths (and names of data1 compartments if present)
    # conversion into lists
    if(all(is.data.frame(data1))){
        data1 <- list(L1 = data1)
        if(all(class(x) == "list")){
            tempo.cat <- paste0(error_text_start, "x ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            x <- list(L1 = x)
        }
        if(all(class(y) == "list")){
            tempo.cat <- paste0(error_text_start, "y ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            y <- list(L1 = y)
        }
        if( ! is.null(categ)){
            if(all(class(categ) == "list")){
                tempo.cat <- paste0(error_text_start, "categ ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                categ <- list(L1 = categ)
            }
        }
        if( ! is.null(categ.class.order)){
            if(all(class(categ.class.order) == "list")){
                tempo.cat <- paste0(error_text_start, "categ.class.order ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                categ.class.order <- list(L1 = categ.class.order)
            }
        }
        if( ! is.null(color)){
            if(all(class(color) == "list")){
                tempo.cat <- paste0(error_text_start, "color ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                color <- list(L1 = color)
            }
        }
        if(all(class(geom) == "list")){
            tempo.cat <- paste0(error_text_start, "geom ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            geom <- list(L1 = geom)
        }
        if(all(class(geom.step.dir) == "list")){
            tempo.cat <- paste0(error_text_start, "geom.step.dir ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            geom.step.dir <- list(L1 = geom.step.dir)
        }
        if( ! is.null(geom.stick.base)){
            if(all(class(geom.stick.base) == "list")){
                tempo.cat <- paste0(error_text_start, "geom.stick.base ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                geom.stick.base <- list(L1 = geom.stick.base)
            }
        }
        if(all(class(alpha) == "list")){
            tempo.cat <- paste0(error_text_start, "alpha ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            alpha <- list(L1 = alpha)
        }
        if(all(class(dot.size) == "list")){
            tempo.cat <- paste0(error_text_start, "dot.size ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            dot.size <- list(L1 = dot.size)
        }
        if(all(class(dot.shape) == "list")){
            tempo.cat <- paste0(error_text_start, "dot.shape ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            dot.shape <- list(L1 = dot.shape)
        }
        if(all(class(dot.border.size) == "list")){
            tempo.cat <- paste0(error_text_start, "dot.border.size ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            dot.border.size <- list(L1 = dot.border.size)
        }
        if( ! is.null(dot.border.color)){
            if(all(class(dot.border.color) == "list")){
                tempo.cat <- paste0(error_text_start, "dot.border.color ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                dot.border.color <- list(L1 = dot.border.color)
            }
        }
        if(all(class(line.size) == "list")){
            tempo.cat <- paste0(error_text_start, "line.size ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            line.size <- list(L1 = line.size)
        }
        if(all(class(line.type) == "list")){
            tempo.cat <- paste0(error_text_start, "line.type ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            line.type <- list(L1 = line.type)
        }
        if( ! is.null(legend.name)){
            if(all(class(legend.name) == "list")){
                tempo.cat <- paste0(error_text_start, "legend.name ARGUMENT CANNOT BE A LIST IF data1 IS A DATA FRAME")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                legend.name <- list(L1 = legend.name)
            }
        }
    }else if( ! all(sapply(data1, FUN = "class") == "data.frame")){ # if not a data frame, data1 can only be a list, as tested above
        tempo.cat <- paste0(error_text_start, "data1 ARGUMENT MUST BE A DATA FRAME OR A LIST OF DATA FRAMES")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # single value converted into list now reattributed to the argument name
    if( ! is.null(color)){
        if( ! is.null(list.color)){
            color <- list.color
        }
    }
    if( ! is.null(list.geom)){
        geom <- list.geom
    }
    if( ! is.null(list.geom.step.dir)){
        geom.step.dir <- list.geom.step.dir
    }
    if( ! is.null(geom.stick.base)){
        if( ! is.null(list.geom.stick.base)){
            geom.stick.base <- list.geom.stick.base
        }
    }
    if( ! is.null(list.alpha)){
        alpha <- list.alpha
    }
    if( ! is.null(list.dot.size)){
        dot.size <- list.dot.size
    }
    if( ! is.null(list.dot.shape)){
        dot.shape <- list.dot.shape
    }
    if( ! is.null(list.dot.border.size)){
        dot.border.size <- list.dot.border.size
    }
    if( ! is.null(dot.border.color)){
        if( ! is.null(list.dot.border.color)){
            dot.border.color <- list.dot.border.color
        }
    }
    if( ! is.null(list.line.size)){
        line.size <- list.line.size
    }
    if( ! is.null(list.line.type)){
        line.type <- list.line.type
    }
    # end single value converted into list now reattributed to the argument name
    # data, x, y, geom, alpha, dot.size, shape, dot.border.size, line.size, line.type, legend.name are list now
    # if non-null, categ, categ.class.order, legend.name, color, dot.border.color are list now
    # end conversion into lists
    # verif of add
    if( ! is.null(add)){
        if( ! grepl(pattern = "^\\s*\\+", add)){ # check that the add string start by +
            tempo.cat <- paste0(error_text_start, "add ARGUMENT MUST START WITH \"+\": ", paste(unique(add), collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            
        }else if( ! grepl(pattern = "(ggplot2|lemon)\\s*::", add)){ #
            tempo.cat <- paste0(error_text_start, "FOR EASIER FUNCTION DETECTION, add ARGUMENT MUST CONTAIN \"ggplot2::\" OR \"lemon::\" IN FRONT OF EACH GGPLOT2 FUNCTION: ", paste(unique(add), collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if( ! grepl(pattern = ")\\s*$", add)){ # check that the add string finished by )
            tempo.cat <- paste0(error_text_start, "add ARGUMENT MUST FINISH BY \")\": ", paste(unique(add), collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # end verif of add
    # management of add containing facet
    facet.categ <- NULL
    if( ! is.null(add)){
        facet.check <- TRUE
        tempo <- unlist(strsplit(x = add, split = "\\s*\\+\\s*(ggplot2|lemon)\\s*::\\s*")) #
        tempo <- sub(x = tempo, pattern = "^facet_wrap", replacement = "ggplot2::facet_wrap")
        tempo <- sub(x = tempo, pattern = "^facet_grid", replacement = "ggplot2::facet_grid")
        tempo <- sub(x = tempo, pattern = "^facet_rep", replacement = "lemon::facet_rep")
        if(length(data1) > 1 & (any(grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap")) | grepl(x = add, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid"))){
            tempo.cat <- paste0(error_text_start, "facet PANELS CANNOT BE USED IF MORE THAN ONE DATA FRAME IN THE data1 ARGUMENT\nPLEASE REWRITE THE add STRING AND RERUN")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            if(any(grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap"))){
                tempo1 <- suppressWarnings(eval(parse(text = tempo[grepl(x = tempo, pattern = "ggplot2::facet_wrap|lemon::facet_rep_wrap")])))
                facet.categ <- list(names(tempo1$params$facets)) # list of length 1
                tempo.text <- "facet_wrap OR facet_rep_wrap"
                facet.check <- FALSE
            }else if(grepl(x = add, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid")){
                tempo1 <- suppressWarnings(eval(parse(text = tempo[grepl(x = tempo, pattern = "ggplot2::facet_grid|lemon::facet_rep_grid")])))
                facet.categ <- list(c(names(tempo1$params$rows), names(tempo1$params$cols))) # list of length 1
                tempo.text <- "facet_grid OR facet_rep_grid"
                facet.check <- FALSE
            }
            if(facet.check == FALSE & ! all(facet.categ %in% names(data1[[1]]))){ # WARNING: all(facet.categ %in% names(data1)) is TRUE when facet.categ is NULL
                tempo.cat <- paste0(error_text_start, "DETECTION OF \"", tempo.text, "\" STRING IN THE add ARGUMENT BUT PROBLEM OF VARIABLE DETECTION (COLUMN NAMES OF data1)\nTHE DETECTED VARIABLES ARE:\n", paste(facet.categ, collapse = " "), "\nTHE data1 COLUMN NAMES ARE:\n", paste(names(data1[[1]]), collapse = " "), "\nPLEASE REWRITE THE add STRING AND RERUN")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
    }
    # if facet.categ is not NULL, it is a list of length 1 now
    # end management of add containing facet
    # legend name filling
    if(is.null(legend.name) & ! is.null(categ)){
        legend.name <- categ
    }else if(is.null(legend.name) & is.null(categ)){
        legend.name <- vector("list", length(data1)) # null list
    }
    # legend.name not NULL anymore (list)
    # end legend name filling
    # ini categ for legend display
    fin.lg.disp <- vector("list", 6) # will be used at the end to display or not legends
    fin.lg.disp[] <- FALSE
    legend.disp <- vector("list", length(data1))
    if(is.null(categ) | legend.show == FALSE){
        legend.disp[] <- FALSE
    }else{
        for(i2 in 1:length(data1)){
            if(is.null(categ[[i2]])){
                legend.disp[[i2]] <- FALSE
            }else{
                legend.disp[[i2]] <- TRUE
            }
        }
    }
    # end ini categ for legend display
    # integer colors into gg_palette
    tempo.check.color <- NULL
    for(i1 in 1:length(data1)){
        if(any(is.na(color[[i1]]))){
            tempo.cat <- paste0(error_text_start, "", ifelse(length(color)== 1L, "color", paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), ": color ARGUMENT CANNOT CONTAIN NA")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        tempo.check.color <- c(tempo.check.color, saferDev::arg_check(data = color[[i1]], data_name = ifelse(length(color)== 1L, "color", paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), class = "integer", double_as_integer_allowed = TRUE, na_contain = TRUE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)$problem)
    }
    tempo.check.color <- ! tempo.check.color # invert TRUE and FALSE because if integer, then problem = FALSE
    if(any(tempo.check.color == TRUE)){ # convert integers into colors
        tempo.integer <- unlist(color[tempo.check.color])
        tempo.color <- gg_palette(max(tempo.integer, na.rm = TRUE))
        for(i1 in 1:length(data1)){
            if(tempo.check.color[i1] == TRUE){
                color[[i1]] <-tempo.color[color[[i1]]]
            }
        }
    }
    # end integer colors into gg_palette
    # loop (checking inside list compartment)
    compart.null.color <- 0 # will be used to attribute a color when color is non-null but a compartment of color is NULL
    data1.ini <- data1 # to report NA removal
    removed.row.nb <- vector("list", length = length(data1)) # to report NA removal. Contains NULL
    removed.rows <- vector("list", length = length(data1)) # to report NA removal. Contains NULL
    for(i1 in 1:length(data1)){
        tempo <- saferDev::arg_check(data = data1[[i1]], data_name = ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), class = "data.frame", na_contain = TRUE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo$problem == TRUE){
            stop(paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        # reserved word checking
        if(any(names(data1[[i1]]) %in% reserved_words)){ # I do not use fun_name_change() because cannot control y before creating "fake_y". But ok because reserved are not that common
            tempo.cat <- paste0(error_text_start, "COLUMN NAMES OF ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " ARGUMENT CANNOT BE ONE OF THESE WORDS\n", paste(reserved_words, collapse = " "), "\nTHESE ARE RESERVED FOR THE ", function_name, " FUNCTION")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if( ! (is.null(add))){
            if(any(sapply(X = reserved_words, FUN = grepl, x = add))){
                tempo.cat <- paste0(error_text_start, "DETECTION OF COLUMN NAMES OF data1 IN THE add ARGUMENT STRING, THAT CORRESPOND TO RESERVED STRINGS FOR ", function_name, "\nFOLLOWING COLUMN NAMES HAVE TO BE CHANGED:\n", paste(arg.names[sapply(X = reserved_words, FUN = grepl, x = add)], collapse = "\n"), "\nFOR INFORMATION, THE RESERVED WORDS ARE:\n", paste(reserved_words, collapse = "\n"))
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if(any(sapply(X = arg.names, FUN = grepl, x = add))){
                warn_count <- warn_count + 1
                tempo.warn <- paste0("(", warn_count,") NAMES OF ", function_name, " ARGUMENTS DETECTED IN THE add STRING:\n", paste(arg.names[sapply(X = arg.names, FUN = grepl, x = add)], collapse = "\n"), "\nRISK OF WRONG OBJECT USAGE INSIDE ", function_name)
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
        }
        # end reserved word checking
        # check of geom now because required for y argument
        tempo <- saferDev::arg_check(data = geom[[i1]], data_name = ifelse(length(geom)== 1L, "geom", paste0("geom NUMBER ", i1)), options = c("geom_point", "geom_line", "geom_path", "geom_step", "geom_hline", "geom_vline", "geom_stick"), length = 1, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo$problem == TRUE){
            stop(paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        if(geom[[i1]] == "geom_step" & is.null(geom.step.dir[[i1]])){
            tempo.cat <- paste0(error_text_start, "", ifelse(length(geom.step.dir)== 1L, "geom.step.dir", paste0("ELEMENT ", i1, " OF geom.step.dir ARGUMENT")), ": geom.step.dir ARGUMENT CANNOT BE NULL IF ", ifelse(length(geom)== 1L, "geom", paste0("ELEMENT ", i1, " OF geom")), " ARGUMENT IS \"geom_step\"\nHERE geom.step.dir ARGUMENT IS: ", paste(geom.step.dir[[i1]], collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(geom[[i1]] == "geom_step" & ! is.null(geom.step.dir[[i1]])){
            tempo <- saferDev::arg_check(data = geom.step.dir[[i1]], data_name = ifelse(length(geom.step.dir)== 1L, "geom.step.dir", paste0("geom.step.dir NUMBER ", i1)), options = c("vh", "hv", "mid"), length = 1, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo$problem == TRUE){
                stop(paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
        }
        if( ! (is.null(geom.stick.base))){
            if(geom[[i1]] == "geom_stick" & ! is.null(geom.stick.base[[i1]])){
                tempo <- saferDev::arg_check(data = geom.stick.base[[i1]], data_name = ifelse(length(geom.stick.base)== 1L, "geom.stick.base", paste0("geom.stick.base NUMBER ", i1)), mode = "numeric", length = 1, na_contain = FALSE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
                if(tempo$problem == TRUE){
                    stop(paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                }
            }
        }
        # end check of geom now because required for y argument
        if(is.null(x[[i1]])){
            if(all(geom[[i1]] != "geom_hline")){
                tempo.cat <- paste0(error_text_start, "", ifelse(length(x)== 1L, "x", paste0("ELEMENT ", i1, " OF x ARGUMENT")), " IN ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ": x ARGUMENT CANNOT BE NULL EXCEPT IF ", ifelse(length(geom)== 1L, "x", paste0("geom NUMBER ", i1)), " ARGUMENT IS \"geom_hline\"\nHERE geom ARGUMENT IS: ", paste(geom[[i1]], collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                x[[i1]] <- "fake_x"
                data1[[i1]] <- cbind(data1[[i1]], fake_x = NA, stringsAsFactors = TRUE)
                data1[[i1]][, "fake_x"] <- as.numeric(data1[[i1]][, "fake_x"])
                warn_count <- warn_count + 1
                tempo.warn <- paste0("(", warn_count,") NULL ", ifelse(length(x)== 1L, "x", paste0("ELEMENT ", i1, " OF x")), " ARGUMENT ASSOCIATED TO ", ifelse(length(geom)== 1L, "geom", paste0("geom NUMBER ", i1)), " ARGUMENT ", geom[[i1]], " -> FAKE COLUMN ADDED TO DATA FRAME ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ", NAMED \"fake_x\" FOR FINAL DRAWING")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
        }else{
            if(all(geom[[i1]] == "geom_hline")){
                tempo.cat <- paste0(error_text_start, "", ifelse(length(x)== 1L, "x", paste0("ELEMENT ", i1, " OF x ARGUMENT")), " IN ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ": x ARGUMENT MUST BE NULL IF ", ifelse(length(geom)== 1L, "geom", paste0("geom NUMBER ", i1)), " ARGUMENT IS \"geom_hline\"")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            tempo <- saferDev::arg_check(data = x[[i1]], data_name = ifelse(length(x)== 1L, "x", paste0("ELEMENT ", i1, " OF x ARGUMENT")), class = "vector", mode = "character", length = 1, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo$problem == TRUE){
                stop(paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
        }
        if(is.null(y[[i1]])){
            if(all(geom[[i1]] != "geom_vline")){
                tempo.cat <- paste0(error_text_start, "", ifelse(length(y)== 1L, "y", paste0("ELEMENT ", i1, " OF y ARGUMENT")), " IN ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ": y ARGUMENT CANNOT BE NULL EXCEPT IF ", ifelse(length(geom)== 1L, "y", paste0("geom NUMBER ", i1)), " ARGUMENT IS \"geom_vline\"\nHERE geom ARGUMENT IS: ", paste(geom[[i1]], collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                y[[i1]] <- "fake_y"
                data1[[i1]] <- cbind(data1[[i1]], fake_y = NA, stringsAsFactors = TRUE)
                data1[[i1]][, "fake_y"] <- as.numeric(data1[[i1]][, "fake_y"])
                warn_count <- warn_count + 1
                tempo.warn <- paste0("(", warn_count,") NULL ", ifelse(length(y)== 1L, "y", paste0("ELEMENT ", i1, " OF y")), " ARGUMENT ASSOCIATED TO ", ifelse(length(geom)== 1L, "geom", paste0("geom NUMBER ", i1)), " ARGUMENT ", geom[[i1]], " -> FAKE COLUMN ADDED TO DATA FRAME ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ", NAMED \"fake_y\" FOR FINAL DRAWING")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
        }else{
            if(all(geom[[i1]] == "geom_vline")){
                tempo.cat <- paste0(error_text_start, "", ifelse(length(y)== 1L, "y", paste0("ELEMENT ", i1, " OF y ARGUMENT")), " IN ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ": y ARGUMENT MUST BE NULL IF ", ifelse(length(geom)== 1L, "geom", paste0("geom NUMBER ", i1)), " ARGUMENT IS \"geom_vline\"")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            tempo <- saferDev::arg_check(data = y[[i1]], data_name = ifelse(length(y)== 1L, "y", paste0("ELEMENT ", i1, " OF y ARGUMENT")), class = "vector", mode = "character", length = 1, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo$problem == TRUE){
                stop(paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
        }
        # x[[i1]] and y[[i1]] not NULL anymore
        if( ! (x[[i1]] %in% names(data1[[i1]]))){
            tempo.cat <- paste0(error_text_start, "", ifelse(length(x)== 1L, "x", paste0("ELEMENT ", i1, " OF x")), " ARGUMENT MUST BE A COLUMN NAME OF ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT\nHERE IT IS: ", paste(x[[i1]], collapse = " "))))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if( ! (y[[i1]] %in% names(data1[[i1]]))){
            tempo.cat <- paste0(error_text_start, "", ifelse(length(y)== 1L, "y", paste0("ELEMENT ", i1, " OF y")), " ARGUMENT MUST BE A COLUMN NAME OF ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT\nHERE IT IS: ", paste(y[[i1]], collapse = " "))))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        tempo <- saferDev::arg_check(data = data1[[i1]][, x[[i1]]], data_name = ifelse(length(x)== 1L, "x ARGUMENT (AS COLUMN NAME OF data1 DATA FRAME)", paste0("ELEMENT ", i1, " OF x ARGUMENT", " (AS COLUMN NAME OF data1 DATA FRAME NUMBER ", i1, ")")), class = "vector", mode = "numeric", na_contain = TRUE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo$problem == TRUE){
            stop(paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        tempo <- saferDev::arg_check(data = data1[[i1]][, y[[i1]]], data_name = ifelse(length(y)== 1L, "y ARGUMENT (AS COLUMN NAME OF data1 DATA FRAME)", paste0("ELEMENT ", i1, " OF y ARGUMENT", " (AS COLUMN NAME OF data1 DATA FRAME NUMBER ", i1, ")")), class = "vector", mode = "numeric", na_contain = TRUE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo$problem == TRUE){
            stop(paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        if(x[[i1]] == "fake_x" & y[[i1]] == "fake_y"){ # because the code cannot accept to be both "fake_x" and "fake_y" at the same time
            tempo.cat <- paste0(error_text_start, "CODE INCONSISTENCY 2\nTHE CODE CANNOT ACCEPT x AND y TO BE \"fake_x\" AND \"fake_y\" IN THE SAME DATA FRAME ", i1, " ")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        
        if(( ! is.null(categ)) & ( ! is.null(categ[[i1]]))){ # is.null(categ[[i1]]) works even if categ is NULL # is.null(categ[[i1]]) works even if categ is NULL # if categ[[i1]] = NULL, fake_categ will be created later on
            tempo <- saferDev::arg_check(data = categ[[i1]], data_name = ifelse(length(categ)== 1L, "categ", paste0("ELEMENT ", i1, " OF categ ARGUMENT")),, class = "vector", mode = "character", length = 1, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo$problem == TRUE){
                stop(paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
            if( ! (categ[[i1]] %in% names(data1[[i1]]))){
                tempo.cat <- paste0(error_text_start, "", ifelse(length(categ)== 1L, "categ", paste0("ELEMENT ", i1, " OF categ")), " ARGUMENT MUST BE A COLUMN NAME OF ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT\nHERE IT IS: ", paste(categ[[i1]], collapse = " "))))
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            tempo1 <- saferDev::arg_check(data = data1[[i1]][, categ[[i1]]], data_name = ifelse(length(categ)== 1L, "categ OF data1 ARGUMENT", paste0("ELEMENT ", i1, " OF categ ARGUMENT IN DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), class = "vector", mode = "character", na_contain = TRUE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            tempo2 <- saferDev::arg_check(data = data1[[i1]][, categ[[i1]]], data_name = ifelse(length(categ)== 1L, "categ OF data1 ARGUMENT", paste0("ELEMENT ", i1, " OF categ ARGUMENT IN DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), class = "factor", na_contain = TRUE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo1$problem == TRUE & tempo2$problem == TRUE){
                tempo.cat <- paste0(error_text_start, "", ifelse(length(categ)== 1L, "categ OF data1 ARGUMENT", paste0("ELEMENT ", i1, " OF categ ARGUMENT IN DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " MUST BE A FACTOR OR CHARACTER VECTOR")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if(tempo1$problem == FALSE){
                data1[[i1]][, categ[[i1]]] <- factor(data1[[i1]][, categ[[i1]]]) # if already a factor, change nothing, if characters, levels according to alphabetical order
                warn_count <- warn_count + 1
                tempo.warn <- paste0("(", warn_count,") IN ", ifelse(length(categ)== 1L, "categ", paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IN ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ", THE CHARACTER COLUMN HAS BEEN CONVERTED TO FACTOR")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                
            }
            if(geom[[i1]] == "geom_vline" | geom[[i1]] == "geom_hline"){
                if(length(unique(data1[[i1]][, categ[[i1]]])) != nrow(data1[[i1]])){
                    tempo.cat <- paste0(error_text_start, "", ifelse(length(geom)== 1L, "geom OF data1 ARGUMENT", paste0("geom NUMBER ", i1, " OF DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " ARGUMENT IS ", geom[[i1]], ", MEANING THAT ", ifelse(length(categ)== 1L, "categ OF data1 ARGUMENT", paste0("ELEMENT ", i1, " OF categ ARGUMENT IN DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " MUST HAVE A DIFFERENT CLASS PER LINE OF data1 (ONE x VALUE PER CLASS)")
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }
            }
        }else if(( ! is.null(categ)) & is.null(categ[[i1]])){ # is.null(categ[[i1]]) works even if categ is NULL # if categ[[i1]] = NULL, fake_categ will be created. WARNING: is.null(categ[[i1]]) means no legend display (see above), because categ has not been precised. This also means a single color for data1[[i1]]
            if(length(color[[i1]]) > 1){ # 0 means is.null(color[[i1]]) or is.null(color) and 1 is ok -> single color for data1[[i1]]
                warn_count <- warn_count + 1
                tempo.warn <- paste0("(", warn_count,") NULL ", ifelse(length(categ)== 1L, "categ", paste0("ELEMENT ", i1, " OF categ")), " ARGUMENT BUT CORRESPONDING COLORS IN ", ifelse(length(color)== 1L, "color", paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " HAS LENGTH OVER 1\n", paste(color[[i1]], collapse = " "), "\nWHICH IS NOT COMPATIBLE WITH NULL CATEG -> COLOR RESET TO A SINGLE COLOR")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                color[i1] <- list(NULL) # will provide a single color below # Warning color[[i1]] <- NULL removes the compartment
            }
            categ[[i1]] <- "fake_categ"
            data1[[i1]] <- cbind(data1[[i1]], fake_categ = "", stringsAsFactors = TRUE)
            # inactivated because give a different color to different "Line_" categ while a single color for all the data1[[i1]] required. Thus, put back after the color management
            # if(geom[[i1]] == "geom_hline" | geom[[i1]] == "geom_vline"){
            # data1[[i1]][, "fake_categ"] <- paste0("Line_", 1:nrow(data1[[i1]]))
            # }else{
            data1[[i1]][, "fake_categ"] <- data1[[i1]][, "fake_categ"] # as.numeric("") create a vector of NA but class numeric
            # }
            warn_count <- warn_count + 1
            tempo.warn <- paste0("(", warn_count,") NULL ", ifelse(length(categ)== 1L, "categ", paste0("ELEMENT ", i1, " OF categ")), " ARGUMENT -> FOR DATA FRAME ", ifelse(length(data1)== 1L, "data1 ARGUMENT:", paste0("NUMBER ", i1, " OF data1 ARGUMENT:")), "\n- FAKE \"fake_categ\" COLUMN ADDED FILLED WITH \"\"(OR WITH \"Line_...\" FOR LINES)\n- SINGLE COLOR USED FOR PLOTTING\n- NO LEGEND DISPLAYED")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
        # OK: if categ is not NULL, all the non-null categ columns of data1 are factors from here
        
        # management of log scale and Inf removal
        if(x[[i1]] != "fake_x"){
            if(any(( ! is.finite(data1[[i1]][, x[[i1]]])) & ( ! is.na(data1[[i1]][, x[[i1]]])))){ # is.finite also detects NA: ( ! is.finite(data1[, y])) & ( ! is.na(data1[, y])) detects only Inf
                warn_count <- warn_count + 1
                tempo.warn <- paste0("(", warn_count,") PRESENCE OF -Inf OR Inf VALUES IN ", ifelse(length(categ)== 1L, "x", paste0("ELEMENT ", i1, " OF x ARGUMENT")), " IN ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " AND CORRESPONDING ROWS REMOVED (SEE $removed.row.nb AND $removed.rows)")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
        }
        if(y[[i1]] != "fake_y"){
            if(any(( ! is.finite(data1[[i1]][, y[[i1]]])) & ( ! is.na(data1[[i1]][, y[[i1]]])))){ # is.finite also detects NA: ( ! is.finite(data1[, y])) & ( ! is.na(data1[, y])) detects only Inf
                warn_count <- warn_count + 1
                tempo.warn <- paste0("(", warn_count,") PRESENCE OF -Inf OR Inf VALUES IN ", ifelse(length(categ)== 1L, "y", paste0("ELEMENT ", i1, " OF y ARGUMENT")), " IN ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " AND CORRESPONDING ROWS REMOVED (SEE $removed.row.nb AND $removed.rows)")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
        }
        # log conversion
        if(x.log != "no"){
            tempo1 <- ! is.finite(data1[[i1]][, x[[i1]]]) # where are initial NA and Inf
            data1[[i1]][, x[[i1]]] <- suppressWarnings(get(x.log)(data1[[i1]][, x[[i1]]]))# no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
            if(any( ! (tempo1 | is.finite(data1[[i1]][, x[[i1]]])))){
                warn_count <- warn_count + 1
                tempo.warn <- paste0("(", warn_count,") LOG CONVERSION INTRODUCED -Inf OR Inf OR NaN VALUES IN ", ifelse(length(categ)== 1L, "x", paste0("ELEMENT ", i1, " OF x ARGUMENT")), " IN ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " AND CORRESPONDING ROWS REMOVED (SEE $removed.row.nb AND $removed.rows)")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
        }
        if(y.log != "no"){
            tempo1 <- ! is.finite(data1[[i1]][, y[[i1]]]) # where are initial NA and Inf
            data1[[i1]][, y[[i1]]] <- suppressWarnings(get(y.log)(data1[[i1]][, y[[i1]]]))# no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
            if(any( ! (tempo1 | is.finite(data1[[i1]][, y[[i1]]])))){
                warn_count <- warn_count + 1
                tempo.warn <- paste0("(", warn_count,") LOG CONVERSION INTRODUCED -Inf OR Inf OR NaN VALUES IN ", ifelse(length(categ)== 1L, "y", paste0("ELEMENT ", i1, " OF y ARGUMENT")), " IN ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " AND CORRESPONDING ROWS REMOVED (SEE $removed.row.nb AND $removed.rows)")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
        }
        # Inf removal
        # removed.row.nb[[i1]] <- NULL # already NULL and Warning this removes the compartment
        removed.rows[[i1]] <- data.frame(stringsAsFactors = FALSE)
        if(any(( ! is.finite(data1[[i1]][, x[[i1]]])) & ( ! is.na(data1[[i1]][, x[[i1]]])))){ # is.finite also detects NA: ( ! is.finite(data1[[i1]][, x[[i1]]])) & ( ! is.na(data1[[i1]][, x[[i1]]])) detects only Inf
            removed.row.nb[[i1]] <- c(removed.row.nb[[i1]], which(( ! is.finite(data1[[i1]][, x[[i1]]])) & ( ! is.na(data1[[i1]][, x[[i1]]]))))
        }
        if(any(( ! is.finite(data1[[i1]][, y[[i1]]])) & ( ! is.na(data1[[i1]][, y[[i1]]])))){ # is.finite also detects NA: ( ! is.finite(data1[[i1]][, y[[i1]]])) & ( ! is.na(data1[[i1]][, y[[i1]]])) detects only Inf
            removed.row.nb[[i1]] <- c(removed.row.nb[[i1]], which(( ! is.finite(data1[[i1]][, y[[i1]]])) & ( ! is.na(data1[[i1]][, y[[i1]]]))))
        }
        if( ! is.null(removed.row.nb[[i1]])){
            removed.row.nb[[i1]] <- unique(removed.row.nb[[i1]]) # to remove the duplicated positions (NA in both x and y)
            removed.rows[[i1]] <- rbind(removed.rows[[i1]], data1.ini[[i1]][removed.row.nb[[i1]], ]) # here data1.ini used to have the y = O rows that will be removed because of Inf creation after log transformation
            data1[[i1]] <- data1[[i1]][-removed.row.nb[[i1]], ]
            data1.ini[[i1]] <- data1.ini[[i1]][-removed.row.nb[[i1]], ] #
        }
        # From here, data1 and data.ini have no more Inf
        # end Inf removal
        # x.lim and y.lim dealt later on, after the end f the loop
        # end management of log scale and Inf removal
        # na detection and removal
        column.check <- unique(unlist(c( # unlist because creates a list
            if(x[[i1]] == "fake_x"){NULL}else{x[[i1]]}, 
            if(y[[i1]] == "fake_y"){NULL}else{y[[i1]]}, 
            if( ! is.null(categ)){if(is.null(categ[[i1]])){NULL}else{categ[[i1]]}}, 
            if( ! is.null(facet.categ)){if(is.null(facet.categ[[i1]])){NULL}else{facet.categ[[i1]]}}
        ))) # dot.categ because can be a 3rd column of data1
        if(any(is.na(data1[[i1]][, column.check]))){
            warn_count <- warn_count + 1
            tempo.warn <- paste0("(", warn_count,") NA DETECTED IN COLUMNS ", paste(column.check, collapse = " "), " OF ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " AND CORRESPONDING ROWS REMOVED (SEE $removed.row.nb AND $removed.rows)")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            for(i3 in 1:length(column.check)){
                if(any(is.na(data1[[i1]][, column.check[i3]]))){
                    warn_count <- warn_count + 1
                    tempo.warn <- paste0("NA REMOVAL DUE TO COLUMN ", column.check[i3], " OF ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")))
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
            }
            tempo <- unique(unlist(lapply(lapply(c(data1[[i1]][column.check]), FUN = is.na), FUN = which)))
            removed.row.nb[[i1]] <- c(removed.row.nb[[i1]], tempo)
            removed.rows[[i1]] <- rbind(removed.rows[[i1]], data1.ini[[i1]][tempo, ]) #  # tempo used because removed.row.nb is not empty. Here data1.ini used to have the non NA rows that will be removed because of NAN creation after log transformation (neg values for instance)
            column.check <- column.check[ ! (column.check == x[[i1]] | column.check == y[[i1]])] # remove x and y to keep quali columns
            if(length(tempo) != 0){
                data1[[i1]] <- data1[[i1]][-tempo, ] # WARNING tempo here and not removed.row.nb because the latter contain more numbers thant the former
                data1.ini[[i1]] <- data1.ini[[i1]][-tempo, ] # WARNING tempo here and not removed.row.nb because the latter contain more numbers than the former
                for(i4 in 1:length(column.check)){
                    if(any( ! unique(removed.rows[[i1]][, column.check[i4]]) %in% unique(data1[[i1]][, column.check[i4]]))){
                        warn_count <- warn_count + 1
                        tempo.warn <- paste0("(", warn_count,") IN COLUMN ", column.check[i4], " OF ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ", THE FOLLOWING CLASSES HAVE DISAPPEARED AFTER NA REMOVAL\n(IF COLUMN USED IN THE PLOT, THIS CLASS WILL NOT BE DISPLAYED):\n", paste(unique(removed.rows[[i1]][, column.check[i4]])[ ! unique(removed.rows[[i1]][, column.check[i4]]) %in% unique(data1[[i1]][, column.check[i4]])], collapse = " "))
                        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                        tempo.levels <- levels(data1[[i1]][, column.check[i4]])[levels(data1[[i1]][, column.check[i4]]) %in% unique(as.character(data1[[i1]][, column.check[i4]]))]
                        data1[[i1]][, column.check[i4]] <- factor(as.character(data1[[i1]][, column.check[i4]]), levels = tempo.levels)
                        if(column.check[i4] %in% categ[[i1]] & ! is.null(categ.class.order)){
                            categ.class.order[[i1]] <- levels(data1[[i1]][, column.check[i4]])[levels(data1[[i1]][, column.check[i4]]) %in% unique(data1[[i1]][, column.check[i4]])] # remove the absent class in the categ.class.order vector
                            data1[[i1]][, column.check[i4]] <- factor(as.character(data1[[i1]][, column.check[i4]]), levels = unique(categ.class.order[[i1]]))
                        }
                    }
                }
            }
        }
        # end na detection and removal
        # From here, data1 and data.ini have no more NA or NaN in x, y, categ (if categ != NULL) and facet.categ (if categ != NULL)
        if( ! is.null(categ.class.order)){
            # the following check will be done several times but I prefer to keep it here, after the creation of categ
            if(is.null(categ[[i1]]) & ! is.null(categ.class.order[[i1]])){
                tempo.cat <- paste0(error_text_start, "COMPARTMENT ", i1, " OF categ ARGUMENT CANNOT BE NULL IF COMPARTMENT ", i1, " OF categ.class.order ARGUMENT IS NOT NULL: ", paste(categ.class.order, collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                if(is.null(categ.class.order[[i1]])){
                    warn_count <- warn_count + 1
                    tempo.warn <- paste0("(", warn_count,") THE categ.class.order COMPARTMENT ", i1, " IS NULL. ALPHABETICAL ORDER WILL BE APPLIED")
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    data1[[i1]][, categ[[i1]]] <- factor(as.character(data1[[i1]][, categ[[i1]]])) # if already a factor, change nothing, if characters, levels according to alphabetical order
                    categ.class.order[[i1]] <- levels(data1[[i1]][, categ[[i1]]]) # character vector that will be used later
                }else{
                    tempo <- saferDev::arg_check(data = categ.class.order[[i1]], data_name = paste0("COMPARTMENT ", i1 , " OF categ.class.order ARGUMENT"), class = "vector", mode = "character", length = length(levels(data1[[i1]][, categ[[i1]]])), data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) # length(data1[, categ[i1]) -> if data1[, categ[i1] was initially character vector, then conversion as factor after the NA removal, thus class number ok. If data1[, categ[i1] was initially factor, no modification after the NA removal, thus class number ok
                    if(tempo$problem == TRUE){
                        stop(paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                    }
                }
                if(any(duplicated(categ.class.order[[i1]]))){
                    tempo.cat <- paste0(error_text_start, "COMPARTMENT ", i1, " OF categ.class.order ARGUMENT CANNOT HAVE DUPLICATED CLASSES: ", paste(categ.class.order[[i1]], collapse = " "))
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }else if( ! (all(categ.class.order[[i1]] %in% unique(data1[[i1]][, categ[[i1]]])) & all(unique(data1[[i1]][, categ[[i1]]]) %in% categ.class.order[[i1]]))){
                    tempo.cat <- paste0(error_text_start, "COMPARTMENT ", i1, " OF categ.class.order ARGUMENT MUST BE CLASSES OF COMPARTMENT ", i1, " OF categ ARGUMENT\nHERE IT IS:\n", paste(categ.class.order[[i1]], collapse = " "), "\nFOR COMPARTMENT ", i1, " OF categ.class.order AND IT IS:\n", paste(unique(data1[[i1]][, categ[[i1]]]), collapse = " "), "\nFOR COLUMN ", categ[[i1]], " OF data1 NUMBER ", i1)
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }else{
                    data1[[i1]][, categ[[i1]]] <- factor(data1[[i1]][, categ[[i1]]], levels = categ.class.order[[i1]]) # reorder the factor
                }
                names(categ.class.order)[i1] <- categ[[i1]]
            }
        }
        # OK: if categ.class.order is not NULL, all the NULL categ.class.order columns of data1 are character from here
        
        if( ! is.null(legend.name[[i1]])){
            tempo <- saferDev::arg_check(data = legend.name[[i1]], data_name = ifelse(length(legend.name)== 1L, "legend.name", paste0("legend.name NUMBER ", i1)),, class = "vector", mode = "character", length = 1, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo$problem == TRUE){
                stop(paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
        }
        if( ! is.null(color)){ # if color is NULL, will be filled later on
            # check the nature of color
            if(is.null(color[[i1]])){
                compart.null.color <- compart.null.color + 1
                color[[i1]] <- grey(compart.null.color / 8) # cannot be more than 7 overlays. Thus 7 different greys. 8/8 is excluded because white dots
                warn_count <- warn_count + 1
                tempo.warn <- paste0("(", warn_count,") NULL COLOR IN ", ifelse(length(color)== 1L, "color", paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " ASSOCIATED TO ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ", SINGLE COLOR ", paste(color[[i1]], collapse = " "), " HAS BEEN ATTRIBUTED")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
            tempo1 <- saferDev::arg_check(data = color[[i1]], data_name = ifelse(length(color)== 1L, "color", paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), class = "vector", mode = "character", na_contain = TRUE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) # na_contain = TRUE in case of colum of data1
            tempo2 <- saferDev::arg_check(data = color[[i1]], data_name = ifelse(length(color)== 1L, "color", paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), class = "factor", na_contain = TRUE, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) # idem
            if(tempo1$problem == TRUE & tempo2$problem == TRUE){
                tempo.cat <- paste0(error_text_start, "", ifelse(length(color)== 1L, "color", paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " MUST BE A FACTOR OR CHARACTER VECTOR OR INTEGER VECTOR") # integer possible because dealt above
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if( ! (all(color[[i1]] %in% colors() | grepl(pattern = "^#", color[[i1]])))){ # check that all strings of low.color start by #
                tempo.cat <- paste0(error_text_start, "", ifelse(length(color)== 1L, "color", paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY colors() OR A COLUMN NAME OF THE data1 PARAMETER: ", paste(unique(color[[i1]]), collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            if(any(is.na(color[[i1]]))){
                warn_count <- warn_count + 1
                tempo.warn <- paste0("(", warn_count,") IN ", ifelse(length(color)== 1L, "color", paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), ", THE COLORS:\n", paste(unique(color[[i1]]), collapse = " "), "\nCONTAINS NA")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
            # end check the nature of color
            # check the length of color
            if(is.null(categ) & length(color[[i1]]) != 1){
                tempo.cat <- paste0(error_text_start, "", ifelse(length(color)== 1L, "color", paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " MUST BE A SINGLE COLOR IF categ IS NULL")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if( ! is.null(categ)){
                # No problem of NA management by ggplot2 because already removed
                if(categ[[i1]] == "fake_categ" & length(color[[i1]]) != 1){
                    tempo.cat <- paste0(error_text_start, "", ifelse(length(color)== 1L, "color", paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " MUST BE A SINGLE COLOR IF ", ifelse(length(categ)== 1L, "categ", paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IS NULL")
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }else if(length(color[[i1]]) == length(unique(data1[[i1]][, categ[[i1]]]))){ # here length(color) is equal to the different number of categ
                    data1[[i1]][, categ[[i1]]] <- factor(data1[[i1]][, categ[[i1]]]) # if already a factor, change nothing, if characters, levels according to alphabetical order
                    warn_count <- warn_count + 1
                    tempo.warn <- paste0("(", warn_count,") IN ", ifelse(length(categ)== 1L, "categ", paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IN ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ", THE FOLLOWING COLORS:\n", paste(color[[i1]], collapse = " "), "\nHAVE BEEN ATTRIBUTED TO THESE CLASSES:\n", paste(levels(factor(data1[[i1]][, categ[[i1]]])), collapse = " "))
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }else if(length(color[[i1]]) == length(data1[[i1]][, categ[[i1]]])){# here length(color) is equal to nrow(data1[[i1]]) -> Modif to have length(color) equal to the different number of categ (length(color) == length(levels(data1[[i1]][, categ[[i1]]])))
                    data1[[i1]] <- cbind(data1[[i1]], color = color[[i1]], stringsAsFactors = TRUE)
                    tempo.check <- unique(data1[[i1]][ , c(categ[[i1]], "color")])
                    if( ! (nrow(data1[[i1]]) == length(color[[i1]]) & nrow(tempo.check) == length(unique(data1[[i1]][ , categ[[i1]]])))){
                        tempo.cat <- paste0(error_text_start, "", ifelse(length(color)== 1L, "color", paste0("ELEMENT NUMBER ", i1, " OF color")), " ARGUMENT HAS THE LENGTH OF ", ifelse(length(categ)== 1L, "categ", paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IN ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), "\nBUT IS INCORRECTLY ASSOCIATED TO EACH CLASS OF THIS categ:\n", paste(unique(mapply(FUN = "paste", data1[[i1]][ ,categ[[i1]]], data1[[i1]][ ,"color"])), collapse = "\n"))
                        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    }else{
                        data1[[i1]][, categ[[i1]]] <- factor(data1[[i1]][, categ[[i1]]]) # if already a factor, change nothing, if characters, levels according to alphabetical order
                        color[[i1]] <- unique(color[[i1]][order(data1[[i1]][, categ[[i1]]])]) # Modif to have length(color) equal to the different number of categ (length(color) == length(levels(data1[[i1]][, categ[[i1]]])))
                        warn_count <- warn_count + 1
                        tempo.warn <- paste0("(", warn_count, ") FROM FUNCTION ", function_name, ": ", ifelse(length(color)== 1L, "color", paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " HAS THE LENGTH OF ", ifelse(length(categ)== 1L, "categ", paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IN ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " COLUMN VALUES\nCOLORS HAVE BEEN RESPECTIVELY ASSOCIATED TO EACH CLASS OF categ AS:\n", paste(levels(factor(data1[[i1]][, categ[[i1]]])), collapse = " "), "\n", paste(color[[i1]], collapse = " "))
                        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    }
                }else if(length(color[[i1]])== 1L){
                    data1[[i1]][, categ[[i1]]] <- factor(data1[[i1]][, categ[[i1]]]) # if already a factor, change nothing, if characters, levels according to alphabetical order
                    color[[i1]] <- rep(color[[i1]], length(levels(data1[[i1]][, categ[[i1]]])))
                    warn_count <- warn_count + 1
                    tempo.warn <- paste0("(", warn_count,") IN ", ifelse(length(categ)== 1L, "categ", paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IN ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), ", COLOR HAS LENGTH 1 MEANING THAT ALL THE DIFFERENT CLASSES OF ", ifelse(length(categ)== 1L, "categ", paste0("ELEMENT ", i1, " OF categ ARGUMENT")), "\n", paste(levels(factor(data1[[i1]][, categ[[i1]]])), collapse = " "), "\nWILL HAVE THE SAME COLOR\n", paste(color[[i1]], collapse = " "))
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }else{
                    tempo.cat <- paste0(error_text_start, "", ifelse(length(color)== 1L, "color", paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " MUST BE\n(1) LENGTH 1\nOR (2) THE LENGTH OF ", ifelse(length(categ)== 1L, "categ", paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IN ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), " COLUMN VALUES\nOR (3) THE LENGTH OF THE CLASSES IN THIS COLUMN\nHERE IT IS COLOR LENGTH ", length(color[[i1]]), " VERSUS CATEG LENGTH ", length(data1[[i1]][, categ[[i1]]]), " AND CATEG CLASS LENGTH ", length(unique(data1[[i1]][, categ[[i1]]])), "\nPRESENCE OF NA IN THE COLUMN x, y OR categ OF data1 COULD BE THE PROBLEME")
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }
            }
        }
        if((geom[[i1]] == "geom_hline" | geom[[i1]] == "geom_vline") & ! is.null(categ[[i1]])){ # add here after the color management, to deal with the different lines to plot inside any data[[i1]]
            if(categ[[i1]] == "fake_categ"){
                data1[[i1]][, "fake_categ"] <- factor(paste0("Line_", formatC(1:nrow(data1[[i2]]), width = nchar(nrow(data1[[i2]])), flag = "0")))
            }
        }
        tempo <- saferDev::arg_check(data = alpha[[i1]], data_name = ifelse(length(alpha)== 1L, "alpha", paste0("alpha NUMBER ", i1)), prop = TRUE, length = 1, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo$problem == TRUE){
            stop(paste0("\n\n================\n\n", tempo$text, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
    }
    # end loop (checking inside list compartment)
    if(length(data1) > 1){
        if(length(unique(unlist(x)[ ! x == "fake_x"])) > 1){
            warn_count <- warn_count + 1
            tempo.warn <- paste0("(", warn_count,") THE x ARGUMENT DOES NOT CONTAIN IDENTICAL COLUMN NAMES:\n", paste(unlist(x), collapse = " "), "\nX-AXIS OVERLAYING DIFFERENT VARIABLES?")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
    }
    if(length(data1) > 1){
        if(length(unique(unlist(y)[ ! y == "fake_y"])) > 1){
            warn_count <- warn_count + 1
            tempo.warn <- paste0("(", warn_count,") THE y ARGUMENT DOES NOT CONTAIN IDENTICAL COLUMN NAMES:\n", paste(unlist(y), collapse = " "), "\nY-AXIS OVERLAYING DIFFERENT VARIABLES?")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
    }
    if(sum(geom %in% "geom_point") > 3){
        tempo.cat <- paste0(error_text_start, "geom ARGUMENT CANNOT HAVE MORE THAN THREE \"geom_point\" ELEMENTS")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }else if(length(geom) - sum(geom %in% "geom_point") > 3){
        tempo.cat <- paste0(error_text_start, "geom ARGUMENT CANNOT HAVE MORE THAN THREE LINE ELEMENTS")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # x.lim management before transfo by x.log
    if(x.log != "no" & ! is.null(x.lim)){
        if(any(x.lim <= 0)){
            tempo.cat <- paste0(error_text_start, "x.lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE x.log ARGUMENT SET TO ", x.log, ":\n", paste(x.lim, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(any( ! is.finite(if(x.log == "log10"){log10(x.lim)}else{log2(x.lim)}))){
            tempo.cat <- paste0(error_text_start, "x.lim ARGUMENT RETURNS INF/NA WITH THE x.log ARGUMENT SET TO ", x.log, "\nAS SCALE COMPUTATION IS ", ifelse(x.log == "log10", "log10", "log2"), ":\n", paste(if(x.log == "log10"){log10(x.lim)}else{log2(x.lim)}, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if(x.log != "no" & x.include.zero == TRUE){
        warn_count <- warn_count + 1
        tempo.warn <- paste0("(", warn_count,") x.log ARGUMENT SET TO ", x.log, " AND x.include.zero ARGUMENT SET TO TRUE -> x.include.zero ARGUMENT RESET TO FALSE BECAUSE 0 VALUE CANNOT BE REPRESENTED IN LOG SCALE")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        x.include.zero <- FALSE
    }
    # end x.lim management before transfo by x.log
    # y.lim management before transfo by y.log
    if(y.log != "no" & ! is.null(y.lim)){
        if(any(y.lim <= 0)){
            tempo.cat <- paste0(error_text_start, "y.lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y.log ARGUMENT SET TO ", y.log, ":\n", paste(y.lim, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(any( ! is.finite(if(y.log == "log10"){log10(y.lim)}else{log2(y.lim)}))){
            tempo.cat <- paste0(error_text_start, "y.lim ARGUMENT RETURNS INF/NA WITH THE y.log ARGUMENT SET TO ", y.log, "\nAS SCALE COMPUTATION IS ", ifelse(y.log == "log10", "log10", "log2"), ":\n", paste(if(y.log == "log10"){log10(y.lim)}else{log2(y.lim)}, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if(y.log != "no" & y.include.zero == TRUE){
        warn_count <- warn_count + 1
        tempo.warn <- paste0("(", warn_count,") y.log ARGUMENT SET TO ", y.log, " AND y.include.zero ARGUMENT SET TO TRUE -> y.include.zero ARGUMENT RESET TO FALSE BECAUSE 0 VALUE CANNOT BE REPRESENTED IN LOG SCALE")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        y.include.zero <- FALSE
    }
    # end y.lim management before transfo by y.log
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # axes management
    if(is.null(x.lim)){
        if(any(unlist(mapply(FUN = "[[", data1, x, SIMPLIFY = FALSE)) %in% c(Inf, -Inf))){
            warn_count <- warn_count + 1
            tempo.warn <- paste0("(", warn_count,") THE x COLUMN IN data1 CONTAINS -Inf OR Inf VALUES THAT WILL NOT BE CONSIDERED IN THE PLOT RANGE")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
        x.lim <- suppressWarnings(range(unlist(mapply(FUN = "[[", data1, x, SIMPLIFY = FALSE)), na.rm = TRUE, finite = TRUE)) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only. y.lim added here. If NULL, ok if y argument has values
    }else if(x.log != "no"){
        x.lim <- get(x.log)(x.lim) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
    }
    if(x.log != "no"){
        if(any( ! is.finite(x.lim))){
            tempo.cat <- paste0(error_text_start, "x.lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE x.log ARGUMENT SET TO ", x.log, ":\n", paste(x.lim, collapse = " "), "\nPLEASE, CHECK DATA VALUES (PRESENCE OF ZERO OR INF VALUES)")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if(suppressWarnings(all(x.lim %in% c(Inf, -Inf)))){ # happen when x is only NULL
        if(all(unlist(geom) %in% c("geom_vline", "geom_stick"))){
            tempo.cat <- paste0(error_text_start, "NOT POSSIBLE TO DRAW geom_vline OR geom_stick KIND OF LINES ALONE IF x.lim ARGUMENT IS SET TO NULL, SINCE NO X-AXIS DEFINED (", ifelse(length(x)== 1L, "x", paste0("ELEMENT ", i1, " OF x")), " ARGUMENT MUST BE NULL FOR THESE KIND OF LINES)")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            tempo.cat <- paste0(error_text_start, "x.lim ARGUMENT MADE OF NA, -Inf OR Inf ONLY: ", paste(x.lim, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    x.lim.order <- order(x.lim) # to deal with inverse axis
    # print(x.lim.order)
    x.lim <- sort(x.lim)
    x.lim[1] <- x.lim[1] - abs(x.lim[2] - x.lim[1]) * ifelse(diff(x.lim.order) > 0, x.right.extra.margin, x.left.extra.margin) # diff(x.lim.order) > 0 means not inversed axis
    x.lim[2] <- x.lim[2] + abs(x.lim[2] - x.lim[1]) * ifelse(diff(x.lim.order) > 0, x.left.extra.margin, x.right.extra.margin) # diff(x.lim.order) > 0 means not inversed axis
    if(x.include.zero == TRUE){ # no need to check x.log != "no" because done before
        x.lim <- range(c(x.lim, 0), na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
    }
    x.lim <- x.lim[x.lim.order]
    if(any(is.na(x.lim))){
        tempo.cat <- paste0(error_text_start, "CODE INCONSISTENCY 3")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if(is.null(y.lim)){
        if(any(unlist(mapply(FUN = "[[", data1, y, SIMPLIFY = FALSE)) %in% c(Inf, -Inf))){
            warn_count <- warn_count + 1
            tempo.warn <- paste0("(", warn_count,") THE y COLUMN IN data1 CONTAINS -Inf OR Inf VALUES THAT WILL NOT BE CONSIDERED IN THE PLOT RANGE")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
        y.lim <- suppressWarnings(range(unlist(mapply(FUN = "[[", data1, y, SIMPLIFY = FALSE)), na.rm = TRUE, finite = TRUE)) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only. y.lim added here. If NULL, ok if y argument has values
    }else if(y.log != "no"){
        y.lim <- get(y.log)(y.lim) # no env = sys.nframe(), inherit = FALSE in get() because look for function in the classical scope
    }
    if(y.log != "no"){
        if(any( ! is.finite(y.lim))){
            tempo.cat <- paste0(error_text_start, "y.lim ARGUMENT CANNOT HAVE ZERO OR NEGATIVE VALUES WITH THE y.log ARGUMENT SET TO ", y.log, ":\n", paste(y.lim, collapse = " "), "\nPLEASE, CHECK DATA VALUES (PRESENCE OF ZERO OR INF VALUES)")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if(suppressWarnings(all(y.lim %in% c(Inf, -Inf)))){ # happen when y is only NULL
        if(all(unlist(geom) == "geom_vline")){
            tempo.cat <- paste0(error_text_start, "NOT POSSIBLE TO DRAW geom_vline KIND OF LINES ALONE IF y.lim ARGUMENT IS SET TO NULL, SINCE NO Y-AXIS DEFINED (", ifelse(length(y)== 1L, "y", paste0("ELEMENT ", i1, " OF y")), " ARGUMENT MUST BE NULL FOR THESE KIND OF LINES)")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            tempo.cat <- paste0(error_text_start, "y.lim ARGUMENT MADE OF NA, -Inf OR Inf ONLY: ", paste(y.lim, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    y.lim.order <- order(y.lim) # to deal with inverse axis
    y.lim <- sort(y.lim)
    y.lim[1] <- y.lim[1] - abs(y.lim[2] - y.lim[1]) * ifelse(diff(y.lim.order) > 0, y.bottom.extra.margin, y.top.extra.margin) # diff(y.lim.order) > 0 means not inversed axis
    y.lim[2] <- y.lim[2] + abs(y.lim[2] - y.lim[1]) * ifelse(diff(y.lim.order) > 0, y.top.extra.margin, y.bottom.extra.margin) # diff(y.lim.order) > 0 means not inversed axis
    if(y.include.zero == TRUE){ # no need to check y.log != "no" because done before
        y.lim <- range(c(y.lim, 0), na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
    }
    y.lim <- y.lim[y.lim.order]
    if(any(is.na(y.lim))){
        tempo.cat <- paste0(error_text_start, "CODE INCONSISTENCY 4")
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # end axes management
    
    
    
    
    # create a fake categ if NULL to deal with legend display
    if(is.null(categ)){
        categ <- vector("list", length(data1))
        categ[] <- "fake_categ"
        for(i2 in 1:length(data1)){
            data1[[i2]] <- cbind(data1[[i2]], fake_categ = "", stringsAsFactors = TRUE)
            if(geom[[i2]] == "geom_hline" | geom[[i2]] == "geom_vline"){
                data1[[i2]][, "fake_categ"] <- factor(paste0("Line_", 1:nrow(data1[[i2]])))
            }
        }
        warn_count <- warn_count + 1
        tempo.warn <- paste0("(", warn_count,") NULL categ ARGUMENT -> FAKE \"fake_categ\" COLUMN ADDED TO EACH DATA FRAME OF data1, AND FILLED WITH \"\"")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
    }
    # categ is not NULL anymore
    if(is.null(categ.class.order)){
        categ.class.order <- vector("list", length = length(data1))
        tempo.categ.class.order <- NULL
        for(i2 in 1:length(categ.class.order)){
            categ.class.order[[i2]] <- levels(data1[[i2]][, categ[[i2]]])
            names(categ.class.order)[i2] <- categ[[i2]]
            tempo.categ.class.order <- c(tempo.categ.class.order, ifelse(i2 != 1, "\n", ""), categ.class.order[[i2]])
        }
        if(any(unlist(legend.disp))){
            warn_count <- warn_count + 1
            tempo.warn <- paste0("(", warn_count,") THE categ.class.order SETTING IS NULL. ALPHABETICAL ORDER WILL BE APPLIED FOR CLASS ORDERING:\n", paste(tempo.categ.class.order, collapse = " "))
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
    }
    # end create a fake categ if NULL to deal with legend display
    # categ.class.order is not NULL anymore
    
    
    # vector of color with length as in levels(categ) of data1
    if(is.null(color)){
        color <- vector("list", length(data1))
        length.categ.list <- lapply(lapply(mapply(FUN = "[[", data1, categ, SIMPLIFY = FALSE), FUN = unique), FUN = function(x){length(x[ ! is.na(x)])})
        length.categ.list[sapply(categ, FUN = "==", "fake_categ")] <- 1 # when is.null(color), a single color for all the dots or lines of data[[i1]] that contain "fake_categ" category
        total.categ.length <- sum(unlist(length.categ.list), na.rm = TRUE)
        tempo.color <- gg_palette(total.categ.length)
        tempo.count <- 0
        for(i2 in 1:length(data1)){
            color[[i2]] <- tempo.color[(1:length.categ.list[[i2]]) + tempo.count]
            tempo.count <- tempo.count + length.categ.list[[i2]]
            warn_count <- warn_count + 1
            tempo.warn <- paste0("(", warn_count,") NULL color ARGUMENT -> COLORS RESPECTIVELY ATTRIBUTED TO EACH CLASS OF ", ifelse(length(categ)== 1L, "categ", paste0("ELEMENT ", i2, " OF categ ARGUMENT")), " (", categ[[i2]], ") IN ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i2, " OF data1 ARGUMENT")), ":\n", paste(color[[i2]], collapse = " "), "\n", paste(if(all(levels(data1[[i2]][, categ[[i2]]]) == "")){'\"\"'}else{levels(data1[[i2]][, categ[[i2]]])}, collapse = " "))
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
    }
    # end vector of color with length as in levels(categ) of data1
    # color is not NULL anymore
    
    
    
    
    
    # last check
    for(i1 in 1:length(data1)){
        if(categ[[i1]] != "fake_categ" & length(color[[i1]]) != length(unique(data1[[i1]][, categ[[i1]]]))){
            tempo.cat <- paste0(error_text_start, "LAST CHECK: ", ifelse(length(color)== 1L, "color", paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " MUST HAVE THE LENGTH OF LEVELS OF ", ifelse(length(categ)== 1L, "categ", paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IN ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), "\nHERE IT IS COLOR LENGTH ", length(color[[i1]]), " VERSUS CATEG LEVELS LENGTH ", length(unique(data1[[i1]][, categ[[i1]]])), "\nREMINDER: A SINGLE COLOR PER CLASS OF CATEG AND A SINGLE CLASS OF CATEG PER COLOR MUST BE RESPECTED")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(categ[[i1]] == "fake_categ" & length(color[[i1]]) != 1){
            tempo.cat <- paste0(error_text_start, "LAST CHECK: ", ifelse(length(color)== 1L, "color", paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " MUST HAVE LENGTH 1 WHEN ", ifelse(length(categ)== 1L, "categ", paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IS NULL\nHERE IT IS COLOR LENGTH ", length(color[[i1]]))
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # end last check
    
    
    
    
    
    # conversion of geom_hline and geom_vline
    for(i1 in 1:length(data1)){
        if(geom[[i1]] == "geom_hline" | geom[[i1]] == "geom_vline"){
            final.data.frame <- data.frame()
            for(i3 in 1:nrow(data1[[i1]])){
                tempo.data.frame <- rbind(data1[[i1]][i3, ], data1[[i1]][i3, ], stringsAsFactors = TRUE)
                if(geom[[i1]] == "geom_hline"){
                    tempo.data.frame[, x[[i1]]] <- x.lim
                }else if(geom[[i1]] == "geom_vline"){
                    tempo.data.frame[, y[[i1]]] <- y.lim
                }else{
                    tempo.cat <- paste0(error_text_start, "CODE INCONSISTENCY 5")
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }
                # 3 lines below inactivated because I put that above
                # if(is.null(categ[[i1]])){
                # data1[, "fake_categ"] <- paste0("Line_", i3)
                # }
                final.data.frame <- rbind(final.data.frame, tempo.data.frame, stringsAsFactors = TRUE)
            }
            data1[[i1]] <- final.data.frame
            geom[[i1]] <- "geom_line"
            if(length(color[[i1]])== 1L){
                color[[i1]] <- rep(color[[i1]], length(unique(data1[[i1]][ , categ[[i1]]])))
            }else if(length(color[[i1]]) != length(unique(data1[[i1]][ , categ[[i1]]]))){
                tempo.cat <- paste0(error_text_start, "geom_hline AND geom_vline CONVERSION TO FIT THE XLIM AND YLIM LIMITS OF THE DATA: ", ifelse(length(color)== 1L, "color", paste0("ELEMENT NUMBER ", i1, " OF color ARGUMENT")), " MUST HAVE THE LENGTH OF LEVELS OF ", ifelse(length(categ)== 1L, "categ", paste0("ELEMENT ", i1, " OF categ ARGUMENT")), " IN ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i1, " OF data1 ARGUMENT")), "\nHERE IT IS COLOR LENGTH ", length(color[[i1]]), " VERSUS CATEG LEVELS LENGTH ", length(unique(data1[[i1]][, categ[[i1]]])))
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
    }
    # end conversion of geom_hline and geom_vline
    
    
    
    
    # kind of geom_point (vectorial or raster)
    scatter.kind <- vector("list", length = length(data1)) # list of same length as data1, that will be used to use either ggplot2::geom_point() (vectorial dot layer) or fun_gg_point_rast() (raster dot layer)
    fix.ratio <- FALSE
    if(is.null(raster.threshold)){
        if(raster == TRUE){
            scatter.kind[] <- "fun_gg_point_rast" # not important to fill everything: will be only used when geom == "geom_point"
            fix.ratio <- TRUE
            warn_count <- warn_count + 1
            tempo.warn <- paste0("(", warn_count,") RASTER PLOT GENERATED -> ASPECT RATIO OF THE PLOT REGION SET BY THE raster.ratio ARGUMENT (", fun_round(raster.ratio, 2), ") TO AVOID A BUG OF ELLIPSOID DOT DRAWING")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }else{
            scatter.kind[] <- "ggplot2::geom_point"
        }
    }else{
        for(i2 in 1:length(data1)){
            if(geom[[i2]] == "geom_point"){
                if(nrow(data1[[i2]]) <= raster.threshold){
                    scatter.kind[[i2]] <- "ggplot2::geom_point"
                }else{
                    scatter.kind[[i2]] <- "fun_gg_point_rast"
                    fix.ratio <- TRUE
                    warn_count <- warn_count + 1
                    tempo.warn <- paste0("(", warn_count,") ", ifelse(length(data1)== 1L, "data1 ARGUMENT", paste0("DATA FRAME NUMBER ", i2, " OF data1 ARGUMENT")), " LAYER AS RASTER (NOT VECTORIAL)")
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                }
            }
        }
        if(any(unlist(scatter.kind) == "fun_gg_point_rast")){
            warn_count <- warn_count + 1
            tempo.warn <- paste0("(", warn_count,") RASTER PLOT GENERATED -> ASPECT RATIO OF THE PLOT REGION SET BY THE raster.ratio ARGUMENT (", fun_round(raster.ratio, 2), ") TO AVOID A BUG OF ELLIPSOID DOT DRAWING")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
    }
    # end kind of geom_point (vectorial or raster)
    
    
    
    
    # no need loop part
    coord.names <- NULL
    tempo.gg.name <- "gg.indiv.plot."
    tempo.gg.count <- 0
    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), eval(parse(text = paste0("ggplot2::ggplot()", if(is.null(add)){""}else{add})))) # add added here to have the facets
    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::xlab(if(is.null(x.lab)){x[[1]]}else{x.lab}))
    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ylab(if(is.null(y.lab)){y[[1]]}else{y.lab}))
    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ggtitle(title))
    # text angle management
    x.tempo.just <- gg_just(angle = x.text.angle, pos = "bottom", kind = "axis")
    y.tempo.just <- gg_just(angle = y.text.angle, pos = "left", kind = "axis")
    # end text angle management
    add.check <- TRUE
    if( ! is.null(add)){ # if add is NULL, then = 0
        if(grepl(pattern = "ggplot2::theme", add) == TRUE){
            warn_count <- warn_count + 1
            tempo.warn <- paste0("(", warn_count,") \"ggplot2::theme\" STRING DETECTED IN THE add ARGUMENT\n-> INTERNAL GGPLOT2 THEME FUNCTIONS theme() AND theme_classic() HAVE BEEN INACTIVATED, TO BE USED BY THE USER\n-> article ARGUMENT WILL BE IGNORED\nIT IS RECOMMENDED TO USE \"+ theme(aspect.ratio = raster.ratio)\" IF RASTER MODE IS ACTIVATED")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            add.check <- FALSE
        }
    }
    if(add.check == TRUE & article == TRUE){
        # WARNING: not possible to add several times theme(). NO message but the last one overwrites the others
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::theme_classic(base_size = text.size))
        if(grid == TRUE){
            assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), m.gg <- ggplot2::theme(
                text = ggplot2::element_text(size = text.size), 
                plot.title = ggplot2::element_text(size = title.text.size), # stronger than text
                legend.key = ggplot2::element_rect(color = "white", size = 1.5), # size of the frame of the legend
                line = ggplot2::element_line(size = 0.5), 
                axis.line.y.left = ggplot2::element_line(colour = "black"), # draw lines for the y axis
                axis.line.x.bottom = ggplot2::element_line(colour = "black"), # draw lines for the x axis
                panel.grid.major.x = ggplot2::element_line(colour = "grey85", size = 0.75), 
                panel.grid.minor.x = ggplot2::element_line(colour = "grey90", size = 0.25), 
                panel.grid.major.y = ggplot2::element_line(colour = "grey85", size = 0.75), 
                panel.grid.minor.y = ggplot2::element_line(colour = "grey90", size = 0.25), 
                axis.text.x = ggplot2::element_text(angle = x.tempo.just$angle, hjust = x.tempo.just$hjust, vjust = x.tempo.just$vjust),
                axis.text.y = ggplot2::element_text(angle = y.tempo.just$angle, hjust = y.tempo.just$hjust, vjust = y.tempo.just$vjust), 
                aspect.ratio = if(fix.ratio == TRUE){raster.ratio}else{NULL} # for raster
            ))
        }else{
            assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), m.gg <- ggplot2::theme(
                text = ggplot2::element_text(size = text.size), 
                plot.title = ggplot2::element_text(size = title.text.size), # stronger than text
                line = ggplot2::element_line(size = 0.5), 
                legend.key = ggplot2::element_rect(color = "white", size = 1.5), # size of the frame of the legend
                axis.line.y.left = ggplot2::element_line(colour = "black"), 
                axis.line.x.bottom = ggplot2::element_line(colour = "black"), 
                axis.text.x = ggplot2::element_text(angle = x.tempo.just$angle, hjust = x.tempo.just$hjust, vjust = x.tempo.just$vjust),
                axis.text.y = ggplot2::element_text(angle = y.tempo.just$angle, hjust = y.tempo.just$hjust, vjust = y.tempo.just$vjust), 
                aspect.ratio = if(fix.ratio == TRUE){raster.ratio}else{NULL} # for raster
            ))
        }
    }else if(add.check == TRUE & article == FALSE){
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), m.gg <- ggplot2::theme(
            text = ggplot2::element_text(size = text.size), 
            plot.title = ggplot2::element_text(size = title.text.size), # stronger than text
            line = ggplot2::element_line(size = 0.5), 
            legend.key = ggplot2::element_rect(color = "white", size = 1.5), # size of the frame of the legend
            panel.background = ggplot2::element_rect(fill = "grey95"), 
            axis.line.y.left = ggplot2::element_line(colour = "black"), 
            axis.line.x.bottom = ggplot2::element_line(colour = "black"), 
            panel.grid.major.x = ggplot2::element_line(colour = "grey85", size = 0.75), 
            panel.grid.minor.x = ggplot2::element_line(colour = "grey90", size = 0.25), 
            panel.grid.major.y = ggplot2::element_line(colour = "grey85", size = 0.75), 
            panel.grid.minor.y = ggplot2::element_line(colour = "grey90", size = 0.25), 
            strip.background = ggplot2::element_rect(fill = "white", colour = "black"), 
            axis.text.x = ggplot2::element_text(angle = x.tempo.just$angle, hjust = x.tempo.just$hjust, vjust = x.tempo.just$vjust),
            axis.text.y = ggplot2::element_text(angle = y.tempo.just$angle, hjust = y.tempo.just$hjust, vjust = y.tempo.just$vjust), 
            aspect.ratio = if(fix.ratio == TRUE){raster.ratio}else{NULL} # for raster
            # do not work -> legend.position = "none" # to remove the legend completely: https://www.datanovia.com/en/blog/how-to-remove-legend-from-a-ggplot/
        ))
    }
    # end no need loop part
    
    
    # loop part
    point.count <- 0
    line.count <- 0
    lg.order <- vector(mode = "list", length = 6) # order of the legend
    lg.order <- lapply(lg.order, as.numeric) # order of the legend
    lg.color <- vector(mode = "list", length = 6) # color of the legend
    lg.dot.shape <- vector(mode = "list", length = 6) # etc.
    lg.dot.size <- vector(mode = "list", length = 6) # etc.
    lg.dot.size <- lapply(lg.dot.size, as.numeric) # etc.
    lg.dot.border.size <- vector(mode = "list", length = 6) # etc.
    lg.dot.border.size <- lapply(lg.dot.border.size, as.numeric) # etc.
    lg.dot.border.color <- vector(mode = "list", length = 6) # etc.
    lg.line.size <- vector(mode = "list", length = 6) # etc.
    lg.line.size <- lapply(lg.line.size, as.numeric) # etc.
    lg.line.type <- vector(mode = "list", length = 6) # etc.
    lg.alpha <- vector(mode = "list", length = 6) # etc.
    lg.alpha <- lapply(lg.alpha, as.numeric) # etc.
    for(i1 in 1:length(data1)){
        if(geom[[i1]] == "geom_point"){
            point.count <- point.count + 1
            if(point.count== 1L){
                fin.lg.disp[[1]] <- legend.disp[[point.count + line.count]]
                lg.order[[1]] <- point.count + line.count
                lg.color[[1]] <- color[[i1]] # if color == NULL -> NULL
                lg.dot.shape[[1]] <- dot.shape[[i1]]
                lg.dot.size[[1]] <- dot.size[[i1]]
                lg.dot.border.size[[1]] <- dot.border.size[[i1]]
                lg.dot.border.color[[1]] <- dot.border.color[[i1]] # if dot.border.color == NULL -> NULL
                if(plot == TRUE & fin.lg.disp[[1]] == TRUE & dot.shape[[1]] %in% 0:14 & ((length(dev.list()) > 0 & names(dev.cur()) == "windows") | (length(dev.list())== 0L & Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo.warn <- paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE DOTS (DOT LAYER NUMBER ", point.count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)")
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    lg.alpha[[1]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg.alpha[[1]] <- alpha[[i1]]
                }
                class.categ <- levels(factor(data1[[i1]][, categ[[i1]]]))
                for(i5 in 1:length(color[[i1]])){ # or length(class.categ). It is the same because already checked that lengths are the same
                    tempo.data.frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class.categ[i5], ]
                    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), eval(parse(text = scatter.kind[[i1]]))(data = tempo.data.frame, mapping = ggplot2::aes_string(x = x[[i1]], y = y[[i1]], fill = categ[[i1]]), shape = dot.shape[[i1]], size = dot.size[[i1]], stroke = dot.border.size[[i1]], color = if(dot.shape[[i1]] %in% 21:24 & ! is.null(dot.border.color)){dot.border.color[[i1]]}else{color[[i1]][i5]}, alpha = alpha[[i1]], show.legend = if(i5== 1L){TRUE}else{FALSE})) # WARNING: a single color allowed for color argument outside aesthetic, but here a single color for border --> loop could be inactivated but kept for commodity # legend.show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord.names <- c(coord.names, paste0(geom[[i1]], ".", class.categ[i5]))
                }
                assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_fill_manual(name = if(is.null(legend.name)){NULL}else{legend.name[[i1]]}, values = as.character(color[[i1]]), breaks = class.categ)) # values are the values of fill, breaks reorder the classes according to class.categ in the legend, order argument of guide_legend determines the order of the different aesthetics in the legend (not order of classes). See guide_legend settings of scale_..._manual below
            }
            if(point.count== 2L){
                fin.lg.disp[[2]] <- legend.disp[[point.count + line.count]]
                lg.order[[2]] <- point.count + line.count
                lg.color[[2]] <- color[[i1]] # if color == NULL -> NULL
                lg.dot.shape[[2]] <- dot.shape[[i1]]
                lg.dot.size[[2]] <- dot.size[[i1]]
                lg.dot.border.size[[2]] <- dot.border.size[[i1]]
                lg.dot.border.color[[2]] <- dot.border.color[[i1]] # if dot.border.color == NULL -> NULL
                if(plot == TRUE & fin.lg.disp[[2]] == TRUE & dot.shape[[2]] %in% 0:14 & ((length(dev.list()) > 0 & names(dev.cur()) == "windows") | (length(dev.list())== 0L & Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo.warn <- paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE DOTS (DOT LAYER NUMBER ", point.count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)")
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    lg.alpha[[2]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg.alpha[[2]] <- alpha[[i1]]
                }
                class.categ <- levels(factor(data1[[i1]][, categ[[i1]]]))
                for(i5 in 1:length(color[[i1]])){ # or length(class.categ). It is the same because already checked that lengths are the same
                    tempo.data.frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class.categ[i5], ]
                    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), eval(parse(text = scatter.kind[[i1]]))(data = tempo.data.frame, mapping = ggplot2::aes_string(x = x[[i1]], y = y[[i1]], shape = categ[[i1]]), size = dot.size[[i1]], stroke = dot.border.size[[i1]], fill = color[[i1]][i5], color = if(dot.shape[[i1]] %in% 21:24 & ! is.null(dot.border.color)){dot.border.color[[i1]]}else{color[[i1]][i5]}, alpha = alpha[[i1]], show.legend = FALSE)) # WARNING: a single color allowed for fill argument outside aesthetic, hence the loop # legend.show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord.names <- c(coord.names, paste0(geom[[i1]], ".", class.categ[i5]))
                }
                assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_shape_manual(name = if(is.null(legend.name)){NULL}else{legend.name[[i1]]}, values = rep(dot.shape[[i1]], length(color[[i1]])), breaks = class.categ)) # values are the values of shape, breaks reorder the classes according to class.categ in the legend. See guide_legend settings of scale_..._manual below
                
            }
            if(point.count== 3L){
                fin.lg.disp[[3]] <- legend.disp[[point.count + line.count]]
                lg.order[[3]] <- point.count + line.count
                lg.color[[3]] <- color[[i1]] # if color == NULL -> NULL
                lg.dot.shape[[3]] <- dot.shape[[i1]]
                lg.dot.size[[3]] <- dot.size[[i1]]
                lg.dot.border.size[[3]] <- dot.border.size[[i1]]
                lg.dot.border.color[[3]] <- dot.border.color[[i1]] # if dot.border.color == NULL -> NULL
                if(plot == TRUE & fin.lg.disp[[3]] == TRUE & dot.shape[[3]] %in% 0:14 & ((length(dev.list()) > 0 & names(dev.cur()) == "windows") | (length(dev.list())== 0L & Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo.warn <- paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE DOTS (DOT LAYER NUMBER ", point.count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)")
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    lg.alpha[[3]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg.alpha[[3]] <- alpha[[i1]]
                }
                class.categ <- levels(factor(data1[[i1]][, categ[[i1]]]))
                for(i5 in 1:length(color[[i1]])){ # or length(class.categ). It is the same because already checked that lengths are the same
                    tempo.data.frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class.categ[i5], ]
                    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), eval(parse(text = scatter.kind[[i1]]))(data = tempo.data.frame, mapping = ggplot2::aes_string(x = x[[i1]], y = y[[i1]], stroke = categ[[i1]]), shape = dot.shape[[i1]], size = dot.size[[i1]], fill = color[[i1]][i5], stroke = dot.border.size[[i1]], color = if(dot.shape[[i1]] %in% 21:24 & ! is.null(dot.border.color)){dot.border.color[[i1]]}else{color[[i1]][i5]}, alpha = alpha[[i1]], show.legend = FALSE)) # WARNING: a single color allowed for color argument outside aesthetic, hence the loop # legend.show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord.names <- c(coord.names, paste0(geom[[i1]], ".", class.categ[i5]))
                }
                assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "stroke", name = if(is.null(legend.name)){NULL}else{legend.name[[i1]]}, values = rep(dot.border.size[[i1]], length(color[[i1]])), breaks = class.categ)) # values are the values of stroke, breaks reorder the classes according to class.categ in the legend. See guide_legend settings of scale_..._manual below
                
            }
        }else{
            line.count <- line.count + 1
            if(line.count== 1L){
                fin.lg.disp[[4]] <- legend.disp[[point.count + line.count]]
                lg.order[[4]] <- point.count + line.count
                lg.color[[4]] <- color[[i1]] # if color == NULL -> NULL
                lg.line.size[[4]] <- line.size[[i1]]
                lg.line.type[[4]] <- line.type[[i1]]
                if(plot == TRUE & fin.lg.disp[[4]] == TRUE & ((length(dev.list()) > 0 & names(dev.cur()) == "windows") | (length(dev.list())== 0L & Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo.warn <- paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE LINES (LINE LAYER NUMBER ", line.count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)")
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    lg.alpha[[4]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg.alpha[[4]] <- alpha[[i1]]
                }
                class.categ <- levels(factor(data1[[i1]][, categ[[i1]]]))
                for(i5 in 1:length(color[[i1]])){ # or length(class.categ). It is the same because already checked that lengths are the same
                    tempo.data.frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class.categ[i5], ]
                    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), eval(parse(text = paste0("ggplot2::", # no CR here te0("ggpl
                                                                                                                 ifelse(geom[[i1]] == 'geom_stick', 'geom_segment', geom[[i1]]), # geom_segment because geom_stick converted to geom_segment for plotting
                                                                                                                 "(data = tempo.data.frame, mapping = ggplot2::aes(x = ", 
                                                                                                                 x[[i1]], 
                                                                                                                 ifelse(geom[[i1]] == 'geom_stick', ", yend = ", ", y = "), 
                                                                                                                 y[[i1]], 
                                                                                                                 if(geom[[i1]] == 'geom_stick'){paste0(', xend = ', x[[i1]], ', y = ', ifelse(is.null(geom.stick.base), y.lim[1], geom.stick.base[[i1]]))}, 
                                                                                                                 ", linetype = ", 
                                                                                                                 categ[[i1]], 
                                                                                                                 "), color = \"", 
                                                                                                                 color[[i1]][i5], 
                                                                                                                 "\", size = ", 
                                                                                                                 line.size[[i1]], 
                                                                                                                 ifelse(geom[[i1]] == 'geom_path', ', lineend = \"round\"', ''), 
                                                                                                                 ifelse(geom[[i1]] == 'geom_step', paste0(', direction = \"', geom.step.dir[[i1]], '\"'), ''), 
                                                                                                                 ", alpha = ", 
                                                                                                                 alpha[[i1]], 
                                                                                                                 ", show.legend = ", 
                                                                                                                 ifelse(i5== 1L, TRUE, FALSE), 
                                                                                                                 ")"
                    )))) # WARNING: a single color allowed for color argument outside aesthetic, hence the loop # legend.show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord.names <- c(coord.names, paste0(geom[[i1]], ".", class.categ[i5]))
                }
                assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "linetype", name = if(is.null(legend.name)){NULL}else{legend.name[[i1]]}, values = rep(line.type[[i1]], length(color[[i1]])), breaks = class.categ)) # values are the values of linetype. 1 means solid. Regarding the alpha bug, I have tried different things without success: alpha in guide alone, in geom alone, in both, with different values, breaks reorder the classes according to class.categ in the legend
            }
            if(line.count== 2L){
                fin.lg.disp[[5]] <- legend.disp[[point.count + line.count]]
                lg.order[[5]] <- point.count + line.count
                lg.color[[5]] <- color[[i1]] # if color == NULL -> NULL
                lg.line.size[[5]] <- line.size[[i1]]
                lg.line.type[[5]] <- line.type[[i1]]
                if(plot == TRUE & fin.lg.disp[[5]] == TRUE & ((length(dev.list()) > 0 & names(dev.cur()) == "windows") | (length(dev.list())== 0L & Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo.warn <- paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE LINES (LINE LAYER NUMBER ", line.count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)")
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    lg.alpha[[5]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg.alpha[[5]] <- alpha[[i1]]
                }
                class.categ <- levels(factor(data1[[i1]][, categ[[i1]]]))
                for(i5 in 1:length(color[[i1]])){ # or length(class.categ). It is the same because already checked that lengths are the same
                    tempo.data.frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class.categ[i5], ]
                    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), eval(parse(text = paste0("ggplot2::", # no CR here te0("ggpl
                                                                                                                 ifelse(geom[[i1]] == 'geom_stick', 'geom_segment', geom[[i1]]), # geom_segment because geom_stick converted to geom_segment for plotting
                                                                                                                 "(data = tempo.data.frame, mapping = ggplot2::aes(x = ", 
                                                                                                                 x[[i1]], 
                                                                                                                 ifelse(geom[[i1]] == 'geom_stick', ", yend = ", ", y = "), 
                                                                                                                 y[[i1]], 
                                                                                                                 if(geom[[i1]] == 'geom_stick'){paste0(', xend = ', x[[i1]], ', y = ', ifelse(is.null(geom.stick.base), y.lim[1], geom.stick.base[[i1]]))}, 
                                                                                                                 ", alpha = ", 
                                                                                                                 categ[[i1]], 
                                                                                                                 "), color = \"", 
                                                                                                                 color[[i1]][i5], 
                                                                                                                 "\", size = ", 
                                                                                                                 line.size[[i1]], 
                                                                                                                 ", linetype = ", 
                                                                                                                 ifelse(is.numeric(line.type[[i1]]), "", "\""), 
                                                                                                                 line.type[[i1]], 
                                                                                                                 ifelse(is.numeric(line.type[[i1]]), "", "\""), 
                                                                                                                 ifelse(geom[[i1]] == 'geom_path', ', lineend = \"round\"', ''), 
                                                                                                                 ifelse(geom[[i1]] == 'geom_step', paste0(', direction = \"', geom.step.dir[[i1]], '\"'), ''), 
                                                                                                                 ", show.legend = FALSE)"
                    )))) # WARNING: a single color allowed for color argument outside aesthetic, hence the loop # legend.show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord.names <- c(coord.names, paste0(geom[[i1]], ".", class.categ[i5]))
                }
                assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "alpha", name = if(is.null(legend.name)){NULL}else{legend.name[[i1]]}, values = rep(alpha[[i1]], length(color[[i1]])), breaks = class.categ)) # values are the values of linetype. 1 means solid. Regarding the alpha bug, I have tried different things without success: alpha in guide alone, in geom alone, in both, with different values, breaks reorder the classes according to class.categ in the legend
            }
            if(line.count== 3L){
                fin.lg.disp[[6]] <- legend.disp[[point.count + line.count]]
                lg.order[[6]] <- point.count + line.count
                lg.color[[6]] <- color[[i1]] # if color == NULL -> NULL
                lg.line.size[[6]] <- line.size[[i1]]
                lg.line.type[[6]] <- line.type[[i1]]
                if(plot == TRUE & fin.lg.disp[[6]] == TRUE & ((length(dev.list()) > 0 & names(dev.cur()) == "windows") | (length(dev.list())== 0L & Sys.info()["sysname"] == "Windows"))){ # if any Graph device already open and this device is "windows", or if no Graph device opened yet and we are on windows system -> prevention of alpha legend bug on windows using value 1
                    warn_count <- warn_count + 1
                    tempo.warn <- paste0("(", warn_count,") GRAPHIC DEVICE USED ON A WINDOWS SYSTEM ->\nTRANSPARENCY OF THE LINES (LINE LAYER NUMBER ", line.count, ") IS INACTIVATED IN THE LEGEND TO PREVENT A WINDOWS DEPENDENT BUG (SEE https://github.com/tidyverse/ggplot2/issues/2452)\nTO OVERCOME THIS ON WINDOWS, USE ANOTHER DEVICE (pdf() FOR INSTANCE)")
                    warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
                    lg.alpha[[6]] <- 1 # to avoid a bug on windows: if alpha argument is different from 1 for lines (transparency), then lines are not correctly displayed in the legend when using the R GUI (bug https://github.com/tidyverse/ggplot2/issues/2452). No bug when using a pdf
                }else{
                    lg.alpha[[6]] <- alpha[[i1]]
                }
                class.categ <- levels(factor(data1[[i1]][, categ[[i1]]]))
                for(i5 in 1:length(color[[i1]])){ # or length(class.categ). It is the same because already checked that lengths are the same
                    tempo.data.frame <- data1[[i1]][data1[[i1]][, categ[[i1]]] == class.categ[i5], ]
                    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), eval(parse(text = paste0("ggplot2::", # no CR here te0("ggpl
                                                                                                                 ifelse(geom[[i1]] == 'geom_stick', 'geom_segment', geom[[i1]]), # geom_segment because geom_stick converted to geom_segment for plotting
                                                                                                                 "(data = tempo.data.frame, mapping = ggplot2::aes(x = ", 
                                                                                                                 x[[i1]], 
                                                                                                                 ifelse(geom[[i1]] == 'geom_stick', ", yend = ", ", y = "), 
                                                                                                                 y[[i1]], 
                                                                                                                 if(geom[[i1]] == 'geom_stick'){paste0(', xend = ', x[[i1]], ', y = ', ifelse(is.null(geom.stick.base), y.lim[1], geom.stick.base[[i1]]))}, 
                                                                                                                 ", size = ", 
                                                                                                                 categ[[i1]], 
                                                                                                                 "), color = \"", 
                                                                                                                 color[[i1]][i5], 
                                                                                                                 "\", linetype = ", 
                                                                                                                 ifelse(is.numeric(line.type[[i1]]), "", "\""), 
                                                                                                                 line.type[[i1]], 
                                                                                                                 ifelse(is.numeric(line.type[[i1]]), "", "\""), 
                                                                                                                 ifelse(geom[[i1]] == 'geom_path', ', lineend = \"round\"', ''), 
                                                                                                                 ifelse(geom[[i1]] == 'geom_step', paste0(', direction = \"', geom.step.dir[[i1]], '\"'), ''), 
                                                                                                                 ", alpha = ", 
                                                                                                                 alpha[[i1]], 
                                                                                                                 ", show.legend = FALSE)"
                    )))) # WARNING: a single color allowed for color argument outside aesthetic, hence the loop # legend.show option do not remove the legend, only the aesthetic of the legend (dot, line, etc.). Used here to avoid multiple layers of legend which corrupt transparency
                    coord.names <- c(coord.names, paste0(geom[[i1]], ".", class.categ[i5]))
                }
                assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "size", name = if(is.null(legend.name)){NULL}else{legend.name[[i1]]}, values = rep(line.size[[i1]], length(color[[i1]])), breaks = class.categ)) # values are the values of linetype. 1 means solid. Regarding the alpha bug, I have tried different things without success: alpha in guide alone, in geom alone, in both, breaks reorder the classes according to class.categ in the legend
            }
        }
    }
    # end loop part
    
    
    
    
    # legend display
    tempo.legend.final <- 'ggplot2::guides(
fill = if(fin.lg.disp[[1]] == TRUE){
ggplot2::guide_legend(
order = lg.order[[1]], 
override.aes = list(
fill = lg.color[[1]], 
colour = if(lg.dot.shape[[1]] %in% 21:24 & ! is.null(dot.border.color)){lg.dot.border.color[[1]]}else{lg.color[[1]]}, # lg.dot.shape[[1]] %in% 21:24 are the only one that can be filled
shape = lg.dot.shape[[1]], 
size = lg.dot.size[[1]], 
stroke = lg.dot.border.size[[1]], 
alpha = lg.alpha[[1]], 
linetype = 0
)
)
}else{
"none"
}, 
shape = if(fin.lg.disp[[2]] == TRUE){
ggplot2::guide_legend(
order = lg.order[[2]], 
override.aes = list(
fill = lg.color[[2]], 
colour = if(lg.dot.shape[[2]] %in% 21:24 & ! is.null(dot.border.color)){lg.dot.border.color[[2]]}else{lg.color[[2]]}, # lg.dot.shape[[2]] %in% 21:24 are the only one that can be filled
shape = lg.dot.shape[[2]], 
size = lg.dot.size[[2]], 
stroke = lg.dot.border.size[[2]], 
alpha = lg.alpha[[2]], 
linetype = 0
)
)
}else{
"none"
}, 
stroke = if(fin.lg.disp[[3]] == TRUE){
ggplot2::guide_legend(
order = lg.order[[3]], 
override.aes = list(
fill = lg.color[[3]], 
colour = if(lg.dot.shape[[3]] %in% 21:24 & ! is.null(dot.border.color)){lg.dot.border.color[[3]]}else{lg.color[[3]]}, # lg.dot.shape[[3]] %in% 21:24 are the only one that can be filled
shape = lg.dot.shape[[3]], 
size = lg.dot.size[[3]], 
stroke = lg.dot.border.size[[3]], 
alpha = lg.alpha[[3]], 
linetype = 0
)
)
}else{
"none"
}, 
linetype = if(fin.lg.disp[[4]] == TRUE){
ggplot2::guide_legend(
order = lg.order[[4]], 
override.aes = list(
color = lg.color[[4]], 
size = lg.line.size[[4]], 
linetype = lg.line.type[[4]], 
alpha = lg.alpha[[4]], 
shape = NA
)
)
}else{
"none"
}, 
alpha = if(fin.lg.disp[[5]] == TRUE){
ggplot2::guide_legend(
order = lg.order[[5]], 
override.aes = list(
color = lg.color[[5]], 
size = lg.line.size[[5]], 
linetype = lg.line.type[[5]], 
alpha = lg.alpha[[5]], 
shape = NA
)
)
}else{
"none"
}, 
size = if(fin.lg.disp[[6]] == TRUE){
ggplot2::guide_legend(
order = lg.order[[6]], 
override.aes = list(
color = lg.color[[6]], 
size = lg.line.size[[6]], 
linetype = lg.line.type[[6]], 
alpha = lg.alpha[[6]], 
shape = NA
)
)
}else{
"none"
}
)' # clip = "off" to have secondary ticks outside plot region does not work
    if( ! is.null(legend.width)){
        if(any(unlist(legend.disp))){ # means some TRUE
            tempo.graph.info <- suppressMessages(ggplot2::ggplot_build(eval(parse(text = paste0(paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), ' + ', tempo.legend.final))))) # will be recovered later again, when ylim will be considered
            legend.final <- fun_gg_get_legend(ggplot_built = tempo.graph.info, fun.name = function_name) # get legend
            fin.lg.disp[] <- FALSE # remove all the legends. Must be done even if fin.lg.disp is not appearing in the code thenafter. Otherwise twice the legend
            if(is.null(legend.final) & plot == TRUE){ # even if any(unlist(legend.disp)) is TRUE
                legend.final <- fun_gg_empty_graph() # empty graph instead of legend
                warn_count <- warn_count + 1
                tempo.warn <- paste0("(", warn_count,") LEGEND REQUESTED (NON-NULL categ ARGUMENT OR legend.show ARGUMENT SET TO TRUE)\nBUT IT SEEMS THAT THE PLOT HAS NO LEGEND -> EMPTY LEGEND SPACE CREATED BECAUSE OF THE NON-NULL legend.width ARGUMENT\n")
                warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            }
        }else if(plot == TRUE){ # means all FALSE
            legend.final <- ggplot2::ggplot()+ggplot2::theme_void() # empty graph instead of legend
            warn_count <- warn_count + 1
            tempo.warn <- paste0("(", warn_count,") LEGEND REQUESTED (NON-NULL categ ARGUMENT OR legend.show ARGUMENT SET TO TRUE)\nBUT IT SEEMS THAT THE PLOT HAS NO LEGEND -> EMPTY LEGEND SPACE CREATED BECAUSE OF THE NON-NULL legend.width ARGUMENT\n")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
    }
    if( ! any(unlist(legend.disp))){
        fin.lg.disp[] <- FALSE # remove all the legends. Must be done even if fin.lg.disp is not appearing in the code thenafter. Otherwise twice the legend
    }
    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), eval(parse(text = tempo.legend.final)))
    # end legend display





    # scale management
    tempo.coord <- suppressMessages(ggplot2::ggplot_build(eval(parse(text = paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + ", ' + ggplot2::scale_x_continuous(expand = c(0, 0), limits = sort(x.lim), oob = scales::rescale_none) + ggplot2::scale_y_continuous(expand = c(0, 0), limits = sort(y.lim), oob = scales::rescale_none)'))))$layout$panel_params[[1]]) # here I do not need the x-axis and y-axis orientation, I just need the number of main ticks
    # x.second.tick.positions # coordinates of secondary ticks (only if x.second.tick.nb argument is non-null or if x.log argument is different from "no")
    if(x.log != "no"){ # integer main ticks for log2 and log10
        tempo.scale <- (as.integer(min(x.lim, na.rm = TRUE)) - 1):(as.integer(max(x.lim, na.rm = TRUE)) + 1)
    }else{
        tempo <- if(is.null(attributes(tempo.coord$x$breaks))){tempo.coord$x$breaks}else{unlist(attributes(tempo.coord$x$breaks))}
        if(all(is.na(tempo))){
            tempo.cat <- paste0(
                "INTERNAL ERROR 1 IN ", 
                intern_error_text_start, 
                "ONLY NA IN tempo.coord$x$breaks.", 
                intern_error_text_end, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if(length(unique(x.lim)) <= 1){
            tempo.cat <- paste0(error_text_start, "IT SEEMS THAT X-AXIS VALUES HAVE A NULL RANGE: ", paste(x.lim, collapse = " "), "\nPLEASE, USE THE x.lim ARGUMENT WITH 2 DIFFERENT VALUES TO SOLVE THIS")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            tempo.scale <- saferGraph::scale2(lim = x.lim, n = ifelse(is.null(x.tick.nb), length(tempo[ ! is.na(tempo)]), x.tick.nb)) # in ggplot 3.3.0, tempo.coord$x.major_source replaced by tempo.coord$x$breaks. If fact: n = ifelse(is.null(x.tick.nb), length(tempo[ ! is.na(tempo)]), x.tick.nb)) replaced by n = ifelse(is.null(x.tick.nb), 4, x.tick.nb))
        }
    }
    x.second.tick.values <- NULL
    x.second.tick.pos <- NULL
    if(x.log != "no"){
        tempo <- fun_inter_ticks(lim = x.lim, log = x.log)
        x.second.tick.values <- tempo$values
        x.second.tick.pos <- tempo$coordinates
        # if(vertical == TRUE){ # do not remove in case the bug is fixed
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(
            geom = "segment", 
            x = x.second.tick.pos, 
            xend = x.second.tick.pos, 
            y = if(diff(y.lim) > 0){tempo.coord$y.range[1]}else{tempo.coord$y.range[2]}, 
            yend = if(diff(y.lim) > 0){tempo.coord$y.range[1] + abs(diff(tempo.coord$y.range)) / 80}else{tempo.coord$y.range[2] - abs(diff(tempo.coord$y.range)) / 80}
        ))
        # }else{ # not working because of the ggplot2 bug
        # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(geom = "segment", y = x.second.tick.pos, yend = x.second.tick.pos, x = tempo.coord$x.range[1], xend = tempo.coord$x.range[1] + diff(tempo.coord$x.range) / 80))
        # }
        coord.names <- c(coord.names, "x.second.tick.positions")
    }else if(( ! is.null(x.second.tick.nb)) & x.log == "no"){
        # if(x.second.tick.nb > 0){ #inactivated because already checked before
        if(length(tempo.scale) < 2){
            tempo.cat1 <- c("x.tick.nb", "x.second.tick.nb")
            tempo.cat2 <- sapply(list(x.tick.nb, x.second.tick.nb), FUN = paste0, collapse = " ")
            tempo.sep <- sapply(mapply(" ", max(nchar(tempo.cat1)) - nchar(tempo.cat1) + 3, FUN = rep, SIMPLIFY = FALSE), FUN = paste0, collapse = "")
            tempo.cat <- paste0(error_text_start, "THE NUMBER OF GENERATED TICKS FOR THE X-AXIS IS NOT CORRECT: ", length(tempo.scale), "\nUSING THESE ARGUMENT SETTINGS (NO DISPLAY MEANS NULL VALUE):\n", paste0(tempo.cat1, tempo.sep, tempo.cat2, collapse = "\n"), "\nPLEASE, TEST OTHER VALUES")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }else{
            tempo <- fun_inter_ticks(lim = x.lim, log = x.log, breaks = tempo.scale, n = x.second.tick.nb)
        }
        x.second.tick.values <- tempo$values
        x.second.tick.pos <- tempo$coordinates
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(
            geom = "segment", 
            x = x.second.tick.pos, 
            xend = x.second.tick.pos, 
            y = if(diff(y.lim) > 0){tempo.coord$y.range[1]}else{tempo.coord$y.range[2]}, 
            yend = if(diff(y.lim) > 0){tempo.coord$y.range[1] + abs(diff(tempo.coord$y.range)) / 80}else{tempo.coord$y.range[2] - abs(diff(tempo.coord$y.range)) / 80}
        ))
        coord.names <- c(coord.names, "x.second.tick.positions")
    }
    # for the ggplot2 bug with x.log, this does not work: eval(parse(text = ifelse(vertical == FALSE & x.log == "log10", "ggplot2::scale_x_continuous", "ggplot2::scale_x_continuous")))
    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_x_continuous(
        breaks = tempo.scale, 
        minor_breaks = x.second.tick.pos, 
        labels = if(x.log == "log10"){scales::trans_format("identity", scales::math_format(10^.x))}else if(x.log == "log2"){scales::trans_format("identity", scales::math_format(2^.x))}else if(x.log == "no"){ggplot2::waiver()}else{tempo.cat <- paste0("INTERNAL ERROR 2 IN ", intern_error_text_start, "CODE INCONSISTENCY 10.", 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)}, 
        expand = c(0, 0), # remove space after after axis limits
        limits = sort(x.lim), # NA indicate that limits must correspond to data limits but xlim() already used
        oob = scales::rescale_none, 
        trans = ifelse(diff(x.lim) < 0, "reverse", "identity") # equivalent to ggplot2::scale_x_reverse() but create the problem of x-axis label disappearance with x.lim decreasing. Thus, do not use. Use xlim() below and after this
    ))
    # end x.second.tick.positions
    # y.second.tick.positions # coordinates of secondary ticks (only if y.second.tick.nb argument is non-null or if y.log argument is different from "no")
    if(y.log != "no"){ # integer main ticks for log2 and log10
        tempo.scale <- (as.integer(min(y.lim, na.rm = TRUE)) - 1):(as.integer(max(y.lim, na.rm = TRUE)) + 1)
    }else{
        tempo <- if(is.null(attributes(tempo.coord$y$breaks))){tempo.coord$y$breaks}else{unlist(attributes(tempo.coord$y$breaks))}
        if(all(is.na(tempo))){
            tempo.cat <- paste0("INTERNAL ERROR 3 IN ", intern_error_text_start, "ONLY NA IN tempo.coord$y$breaks.", 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if(length(unique(y.lim)) <= 1){
            tempo.cat <- paste0(error_text_start, "IT SEEMS THAT Y-AXIS VALUES HAVE A NULL RANGE: ", paste(y.lim, collapse = " "), "\nPLEASE, USE THE y.lim ARGUMENT WITH 2 DIFFERENT VALUES TO SOLVE THIS")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            tempo.scale <- saferGraph::scale2(lim = y.lim, n = ifelse(is.null(y.tick.nb), length(tempo[ ! is.na(tempo)]), y.tick.nb)) # in ggplot 3.3.0, tempo.coord$y.major_source replaced by tempo.coord$y$breaks. If fact: n = ifelse(is.null(y.tick.nb), length(tempo[ ! is.na(tempo)]), y.tick.nb)) replaced by n = ifelse(is.null(y.tick.nb), 4, y.tick.nb))
        }
    }
    y.second.tick.values <- NULL
    y.second.tick.pos <- NULL
    if(y.log != "no"){
        tempo <- fun_inter_ticks(lim = y.lim, log = y.log)
        y.second.tick.values <- tempo$values
        y.second.tick.pos <- tempo$coordinates
        # if(vertical == TRUE){ # do not remove in case the bug is fixed
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(
            geom = "segment", 
            y = y.second.tick.pos, 
            yend = y.second.tick.pos, 
            x = if(diff(x.lim) > 0){tempo.coord$x.range[1]}else{tempo.coord$x.range[2]}, 
            xend = if(diff(x.lim) > 0){tempo.coord$x.range[1] + abs(diff(tempo.coord$x.range)) / 80}else{tempo.coord$x.range[2] - abs(diff(tempo.coord$x.range)) / 80}
        ))
        # }else{ # not working because of the ggplot2 bug
        # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(geom = "segment", x = y.second.tick.pos, xend = y.second.tick.pos, y = tempo.coord$y.range[1], yend = tempo.coord$y.range[1] + diff(tempo.coord$y.range) / 80))
        # }
        coord.names <- c(coord.names, "y.second.tick.positions")
    }else if(( ! is.null(y.second.tick.nb)) & y.log == "no"){
        # if(y.second.tick.nb > 0){ #inactivated because already checked before
        if(length(tempo.scale) < 2){
            tempo.cat1 <- c("y.tick.nb", "y.second.tick.nb")
            tempo.cat2 <- sapply(list(y.tick.nb, y.second.tick.nb), FUN = paste0, collapse = " ")
            tempo.sep <- sapply(mapply(" ", max(nchar(tempo.cat1)) - nchar(tempo.cat1) + 3, FUN = rep, SIMPLIFY = FALSE), FUN = paste0, collapse = "")
            tempo.cat <- paste0(error_text_start, "THE NUMBER OF GENERATED TICKS FOR THE Y-AXIS IS NOT CORRECT: ", length(tempo.scale), "\nUSING THESE ARGUMENT SETTINGS (NO DISPLAY MEANS NULL VALUE):\n", paste0(tempo.cat1, tempo.sep, tempo.cat2, collapse = "\n"), "\nPLEASE, TEST OTHER VALUES")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }else{
            tempo <- fun_inter_ticks(lim = y.lim, log = y.log, breaks = tempo.scale, n = y.second.tick.nb)
        }
        y.second.tick.values <- tempo$values
        y.second.tick.pos <- tempo$coordinates
        assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotate(
            geom = "segment", 
            y = y.second.tick.pos, 
            yend = y.second.tick.pos, 
            x = if(diff(x.lim) > 0){tempo.coord$x.range[1]}else{tempo.coord$x.range[2]}, 
            xend = if(diff(x.lim) > 0){tempo.coord$x.range[1] + abs(diff(tempo.coord$x.range)) / 80}else{tempo.coord$x.range[2] - abs(diff(tempo.coord$x.range)) / 80}
        ))
        coord.names <- c(coord.names, "y.second.tick.positions")
    }
    # for the ggplot2 bug with y.log, this does not work: eval(parse(text = ifelse(vertical == FALSE & y.log == "log10", "ggplot2::scale_x_continuous", "ggplot2::scale_y_continuous")))
    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_y_continuous(
        breaks = tempo.scale, 
        minor_breaks = y.second.tick.pos, 
        labels = if(y.log == "log10"){scales::trans_format("identity", scales::math_format(10^.x))}else if(y.log == "log2"){scales::trans_format("identity", scales::math_format(2^.x))}else if(y.log == "no"){ggplot2::waiver()}else{tempo.cat <- paste0("INTERNAL ERROR 4 IN ", intern_error_text_start, "CODE INCONSISTENCY 10.", 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)}, 
        expand = c(0, 0), # remove space after axis limits
        limits = sort(y.lim), # NA indicate that limits must correspond to data limits but ylim() already used
        oob = scales::rescale_none, 
        trans = ifelse(diff(y.lim) < 0, "reverse", "identity") # equivalent to ggplot2::scale_y_reverse() but create the problem of y-axis label disappearance with y.lim decreasing. Thus, do not use. Use ylim() below and after this
    ))
    # end y.second.tick.positions
    assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::coord_cartesian(xlim = x.lim, ylim = y.lim)) # clip = "off" to have secondary ticks outside plot region. The problem is that points out of bounds are also drawn outside the plot region. Thus, I cannot use it # at that stage, x.lim and y.lim not NULL anymore
    # end scale management




    # drawing
    fin.plot <- eval(parse(text = paste(paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + ")))
    grob.save <- NULL
    if(plot == TRUE){
        if( ! is.null(legend.width)){ # any(unlist(legend.disp)) == TRUE removed to have empty legend space # not & any(unlist(fin.lg.disp)) == TRUE here because converted to FALSE
            grob.save <- suppressMessages(suppressWarnings(gridExtra::grid.arrange(fin.plot, legend.final, ncol=2, widths=c(1, legend.width))))
        }else{
            grob.save <- suppressMessages(suppressWarnings(print(fin.plot)))
        }
    }else{
        warn_count <- warn_count + 1
        tempo.warn <- paste0("(", warn_count,") PLOT NOT SHOWN AS REQUESTED")
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
    }
    # end drawing
    #### end main code

    #### output
    if(return == TRUE){
        output <- suppressMessages(ggplot2::ggplot_build(fin.plot))
        # output$data <- output$data[-1] # yes for boxplot but not for scatter # remove the first data because corresponds to the initial empty boxplot
        if(length(output$data) != length(coord.names)){
            tempo.cat <- paste0(
                "INTERNAL ERROR 5 IN ", 
                intern_error_text_start, 
                "length(output$data) AND length(coord.names) MUST BE IDENTICAL. CODE HAS TO BE MODIFIED", 
                intern_error_text_end, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            names(output$data) <- coord.names
        }
        if(is.null(unlist(removed.row.nb))){
            removed.row.nb <- NULL
            removed.rows <- NULL
        }else{
            for(i3 in 1:length(data1)){
                if( ! is.null(removed.row.nb[[i3]])){
                    removed.row.nb[[i3]] <- sort(removed.row.nb[[i3]])
                    removed.rows[[i3]] <- data1.ini[[i3]][removed.row.nb[[i3]], ]
                }
            }
        }
        tempo <- output$layout$panel_params[[1]]
        output <- list(
            data = data1, 
            removed.row.nb = removed.row.nb, 
            removed.rows = removed.rows, 
            plot = c(output$data, x.second.tick.values = list(x.second.tick.values), y.second.tick.values = list(y.second.tick.values)), 
            panel = facet.categ, 
            axes = list(
                x.range = tempo$x.range, 
                x.labels = if(is.null(attributes(tempo$x$breaks))){tempo$x$breaks}else{tempo$x$scale$get_labels()}, # is.null(attributes(tempo$x$breaks)) test if it is number (TRUE) or character (FALSE)
                x.positions = if(is.null(attributes(tempo$x$breaks))){tempo$x$breaks}else{unlist(attributes(tempo$x$breaks))}, 
                y.range = tempo$y.range, 
                y.labels = if(is.null(attributes(tempo$y$breaks))){tempo$y$breaks}else{tempo$y$scale$get_labels()}, 
                y.positions = if(is.null(attributes(tempo$y$breaks))){tempo$y$breaks}else{unlist(attributes(tempo$y$breaks))}
            ), 
            warn = paste0("\n", warn, "\n\n"), 
            ggplot = if(return.ggplot == TRUE){fin.plot}else{NULL}, # fin.plot plots the graph if return == TRUE
            gtable = if(return.gtable == TRUE){grob.save}else{NULL} #
        )
        return(output) # this plots the graph if return.ggplot is TRUE and if no assignment
    }
    #### end output

    #### warning output
    if( ! base::is.null(x = warn)){
        base::on.exit(
            expr = base::warning(
                base::paste0(
                    base::sub(pattern = "^ERROR IN ", replacement = "FROM ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
                    warn, 
                    collapse = NULL, 
                    recycle0 = FALSE
                ), call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL
            ), add = TRUE, after = TRUE
        )
    }
    base::on.exit(expr = base::options(warning.length = ini_warning_length), add = TRUE, after = TRUE)
    #### end warning output
}



