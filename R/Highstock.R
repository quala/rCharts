# Custom setSpec method that also accepts list as an argument
# - for series, xAxis and yAxis
setListSpec <- function(obj, ..., replace)
{
  args <- list(...)
  
  if (length(args) == 1 && is.list(args[[1]]) && is.null(names(args))) {
    args <- args[[1]]
  } else {
    args <- list(args)
  }
  
  # Convert data values to a list (fixes issue 138)
  args <- lapply(args, function(x) {
    if (!is.null(x$data) && !is.list(x$data)){
      x$data <- as.list(x$data)
    }
    return(x)
  })
  
  if (replace) {
    obj <<- args
  } else {
    obj <<- c(obj, args)
  }
}

Highstock <- setRefClass("Highstock", contains = "rCharts", methods = list(
  initialize = function()
  {
    callSuper(); lib <<- 'highstock'; LIB <<- get_lib(lib)
    
    params <<- c(params, list(
      credits = list(href = NULL, text = NULL),
      exporting = list(enabled = T),
      title = list(text = NULL),
      yAxis = list(title = list(text = NULL))
    ))
  },
  
  getPayload = function(chartId){
    
    # Set params before rendering
    params$chart$renderTo <<- chartId
    
    list(chartParams = toJSON2(params, digits = 13), chartId = chartId)
  },
  
  # Wrapper methods
  chart = function(..., replace = T)
  {
    params$chart <<- setSpec(params$chart, ..., replace = replace)
  },
  
  colors = function(..., replace = T)
  {
    args <- unlist(list(...))
    
    if (replace) {
      params$colors <<- args
    } else {
      params$colors <<- c(params$colors, args)
    }
  },
  
  credits = function(..., replace = T){
    params$credits <<- setSpec(params$credits, ..., replace = replace)
  },
  
  exporting = function(..., replace = T){
    params$exporting <<- setSpec(params$exporting, ..., replace = replace)
  },
  
  global = function(..., replace = T){
    params$global <<- setSpec(params$global, ..., replace = replace)
  },
  
  labels = function(..., replace = T){
    params$labels <<- setSpec(params$labels, ..., replace = replace)
  },
  
  lang = function(..., replace = T){
    params$lang <<- setSpec(params$lang, ..., replace = replace)
  },
  
  legend = function(..., replace = T){
    params$legend <<- setSpec(params$legend, ..., replace = replace)
  },
  
  loading = function(..., replace = T){
    params$loading <<- setSpec(params$loading, ..., replace = replace)
  },
  
  navigation = function(..., replace = T){
    params$navigation <<- setSpec(params$navigation, ..., replace = replace)
  },
  
  # Pane is not in Highstock
  #pane = function(..., replace = T){
  #    params$pane <<- setSpec(params$pane, ..., replace = replace)
  #},
  
  plotOptions = function(..., replace = T){
    params$plotOptions <<- setSpec(params$plotOptions, ..., replace = replace)
  },
  
  series = function(..., replace = F) {
    params$series <<- setListSpec(params$series, ..., replace = replace)
  },
  
  subtitle = function(..., replace = T){
    params$subtitle <<- setSpec(params$subtitle, ..., replace = replace)
  },
  
  title = function(..., replace = T){
    params$title <<- setSpec(params$title, ..., replace = replace)
  },
  
  tooltip = function(..., replace = T){
    params$tooltip <<- setSpec(params$tooltip, ..., replace = replace)
  },
  
  xAxis = function(..., replace = T) {
    params$xAxis <<- setListSpec(params$xAxis, ..., replace = replace)
  },
  
  yAxis = function(..., replace = T) {
    params$yAxis <<- setListSpec(params$yAxis, ..., replace = replace)
  },
  
  # >>> Specific to Highstock
  navigator = function(..., replace = T) {
    params$navigator <<- setSpec(params$navigator, ..., replace = replace)
  },
  
  scrollbar = function(..., replace = T) {
    params$scrollbar <<- setSpec(params$scrollbar, ..., replace = replace)
  },
  
  rangeSelector = function(..., replace = T) {
    params$rangeSelector <<- setSpec(params$rangeSelector, ..., replace = replace)
  },
  # <<< Specific to Highstock
  
  # Custom add data method
  #!! Seems to mostly work but may need to be adpated to highstock
  data = function(x = NULL, y = NULL, ...)
  {
    if (is.data.frame(x))
    {
      for (i in colnames(x))
      {
        if (is.numeric(x[[i]]))
          series(name = i, data = x[[i]], ...)
        else
          warning (sprintf("Column '%s' wasn't added since it's not a numeric", i))
      }
    } else
    {
      if (is.null(y) || !is.numeric(y))
        series(data = x, ...)
      else
      {
        if (length(x) != length(y))
          stop ("Arguments x and y must be of the same length")
        
        xy <- lapply(1:length(x), function(i) list(x[i], y[i]))
        series(data = xy, ...)
      }
    }
  }
))

##
## OHLSV graph
## - Need to fix geometry issues
ohlcvPlot <- highstockOHLCV <- function(name='a stock', data=data, type='candlestick', radius = 3, title = name, subtitle = NULL)
{
  rChart <- Highstock$new()
  
  nrows <- nrow(data)
  data  <- na.omit(data) # remove remaining observations with NA's
  
  if (nrows != nrow(data))
    warning("Observations with NA has been removed")  
  
  rChart$yAxis(title='OHLC', height=180, linewidth=2)
  rChart$yAxis(title='Volume', top = 250, height=50, offset=0, linewidth=2, replace=F)
  
  rChart$series(
    data = toJSONArray2(data[c('date', 'Open', 'High', 'Low', 'Close')], json = F, names = F),
    type = type,
    name = name,
    marker = list(radius = radius))
  
  rChart$series(
    data = toJSONArray2(data[c('date', 'Volume')], json = F, names = F, replace = F),
    type = 'column',
    name = 'Volume',
    yAxis =1,
    marker = list(radius = radius))
  
  rChart$legend(enabled = F)
  
  ## title
  rChart$title(text = title, replace = T)
  
  ## subtitle
  rChart$subtitle(text = subtitle, replace = T)
  
  return(rChart$copy())
}

##
## OHLC graph
ohlcPlot <- highstockOHLC <- function(name='a stock', data=data, type='candlestick', radius = 3, title = name, subtitle = NULL)
{
  rChart <- Highstock$new()
  
  nrows <- nrow(data)
  data  <- na.omit(data) # remove remaining observations with NA's
  
  if (nrows != nrow(data))
    warning("Observations with NA has been removed")  
  
  rChart$series(
    data = toJSONArray2(data[c('date', 'Open', 'High', 'Low', 'Close')], json = F, names = F),
    type = type,
    name = name,
    marker = list(radius = radius))
  
  rChart$legend(enabled = F)
  
  ## title
  rChart$title(text = title, replace = T)
  
  ## subtitle
  rChart$subtitle(text = subtitle, replace = T)
  
  return(rChart$copy())
}

# Utils
is.categorical <- function(x) is.factor(x) || is.character(x)

#' Highstock Plot
#'
#' ...
#'
#' @param ... see getLayer(...)
#' @param radius circle size
#' @param title chart title
#' @param subtitle chart subttitle
#' @param group.na replace NA's with chosen value in the group column, or else these observations will be removed
sPlot <- highstockPlot <- function(..., radius = 3, title = NULL, subtitle = NULL, group.na = NULL, navigator = TRUE)
{
  rChart <- Highstock$new()
  
  # Get layers
  d <- getLayer(...)
  
  data <- data.frame( x = d$data[[d$x]], y = d$data[[d$y]])
  
  if (!is.null(d$group))
  {
    data$group <- as.character(d$data[[d$group]])
    
    if (!is.null(group.na))
      data$group[is.na(data$group)] <- group.na
  }
  
  if (!is.null(d$size))
    data$size <- d$data[[d$size]]
  
  nrows <- nrow(data)
  data <- na.omit(data) # remove remaining observations with NA's
  
  if (nrows != nrow(data))
    warning("Observations with NA has been removed")
  
  data <- data[order(data$x, data$y), ] # order data (due to line charts)
  
  if ("bubble" %in% d$type && is.null(data$size)) stop("'size' is missing")
  
  if (!is.null(d$group))
  {
    groups <- sort(unique(data$group))
    types <- rep(d$type, length(groups)) # repeat types to match length of groups
    
    plyr::ddply(data,
                .(group),
                function(x) {
                  g <- unique(x$group)
                  i <- which(groups == g)
                  
                  x$group <- NULL # fix
                  rChart$series(
                    data = toJSONArray2(x, json = F, names = F),
                    name = g,
                    type = types[[i]],
                    marker = list(radius = radius)
                  )
                  return(NULL)
                })
    
    rChart$legend(enabled = TRUE)
  } else
  {
    rChart$series(
      data = toJSONArray2(data, json = F, names = F),
      type = d$type[[1]],               # The type is lifted from the ...
      marker = list(radius = radius))
    
    rChart$legend(enabled = FALSE)
  }
  
  # Fix defaults
  
  ## xAxis
  if (is.categorical(data$x))
    rChart$xAxis(title = list(text = d$x), categories = unique(as.character(data$x)), replace = T)
  else
    rChart$xAxis(title = list(text = d$x), replace = T)
  
  ## yAxis
  if (is.categorical(data$y))
    rChart$yAxis(title = list(text = d$y), categories = unique(as.character(data$y)), replace = T)
  else
    rChart$yAxis(title = list(text = d$y), replace = T)
  
  ## title
  rChart$title(text = title, replace = T)
  
  ## subtitle
  rChart$subtitle(text = subtitle, replace = T)
  
  ## navigator and scrollbar panel
  rChart$navigator(enabled = navigator, replace = T)
  rChart$scrollbar(enabled = navigator, replace = T)
  
  return(rChart$copy())
}
