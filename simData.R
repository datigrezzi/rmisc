# simulated data for dashboard and alluvial plots
# Iyad Aldaqre
# 29.07.2019

library(sp)

make_aois <- function(limits = c(1920, 1080), n_hori = 4, n_vert = 3){
  aoi_names <- letters[1:(n_vert * n_hori)]
  x <- seq(floor((limits[1]%%n_hori)/2), limits[1], floor(limits[1]/n_hori))
  y <- seq(floor((limits[2]%%n_vert)/2), limits[2], floor(limits[2]/n_vert))
  aois <- array(dim = c(4, 2, length(aoi_names)))
  idx <- 1
  for(i in 2:length(x)){
    for(ii in 2:length(y)){
      aois[,1,idx] <- c(x[i-1], x[i]-1, x[i]-1, x[i-1]) # minus 1 to avoid overlap
      aois[,2,idx] <- c(y[ii-1], y[ii-1], y[ii]-1, y[ii]-1)
      idx <- idx + 1
    }
  }
  return(list(names=aoi_names, aois=aois))
}

divisors <- function(x){
  allnumbers <- seq(x)
  return(allnumbers[x%%allnumbers == 0])
}

make_actions <- function(ts, aoi_names, n = 3){
  actions <- data.frame("event" = sample(aoi_names, n), "recordingTimestamp" = sort(sample(ts, n, replace = F)))
  return(actions)
}

centroid <- function(x, y){
  return(c(mean(x), mean(y)))
}

draw_aois <- function(aois, add = T){
  for(i in 1:dim(aois$aois)[3]){
    x <- aois$aois[,1,i]
    y <- aois$aois[,2,i]
    thiscol <- col2rgb(i)
    thiscolor <- rgb(thiscol[1], thiscol[2], thiscol[3], max = 255, alpha = 128)
    polygon(x, y, col = thiscolor)
    cx <- centroid(x, y)
    text(cx[1], cx[2], aois$names[i], font = 2)
  }
}

point.in.aoi <- function(x, y, aois){
  newvar <- rep("", length(x))
  for(i in 1:dim(aois$aois)[3]){
    newvar[point.in.polygon(x, y, aois$aois[,1,i], aois$aois[,2,i])==1] <- aois$names[i]
  }
  return(newvar)
}

simdata <- function(i, aois, limits = c(1920, 1080), n = 200){
  parname <- paste("participant_", i, sep = "")
  recname <- paste("recording_", i, sep = "")
  sex <- sample(c("M", "F"), 1)
  age <- sample(c("20-29", "30-39", "40-49", "50-59"), 1)
  fav_aoi <- sample(aois$names, 1)
  # fixations
  ts <- sort(sample(0:300000, n, replace = F))
  duration <- diff(c(0, ts))
  fix_x <- sample(0:limits[1], n, replace = T)
  fix_y <- sample(0:limits[2], n, replace = T)
  # actions
  actions <- make_actions(ts, aois$names, 3)
  alldata <- merge(data.frame("participantName"=parname, "recordingName"=recname, "Sex" = sex, "Age" = age, "Favorite" = fav_aoi, "recordingTimestamp"=ts, "gazePointX"=fix_x, "gazePointY"=fix_y, "fixationDuration" = duration, "aois" = point.in.aoi(fix_x, fix_y, aois)), actions, all = T, by = "recordingTimestamp")
  
  return(alldata)
}

getfilter <- function(data, sexFilter, ageFilter, favFilter){
    myfilter <- rep(TRUE, nrow(data))
    if(sexFilter != "All"){
        myfilter <- myfilter & as.character(data$Sex) == sexFilter
    }
    if(ageFilter != "All"){
        myfilter <- myfilter & as.character(data$Age) == ageFilter
    }
    if(favFilter != "All"){
        myfilter <- myfilter & as.character(data$Favorite) == favFilter
    }
    return(myfilter)
}

aoilist <- function(aois){
    aoishapelist <- list()
    for(i in 1:dim(aois$aois)[3]){
        aoishapelist[[i]] <- list(type = "rect",
                            fillcolor = "blue", line = list(color = "black"), opacity = 0.3,
                            x0 = aois$aois[1,1,i], x1 = aois$aois[2,1,i], xref = "x",
                            y0 = aois$aois[1,2,i], y1 = aois$aois[3,2,i], yref = "y")
    }
    return(aoishapelist)
}

if(TRUE){
    # set random seed for replicability
    set.seed(seed = 111)
    # generate aois
    aois <- make_aois(n_hori = 5)
    # generate data for 50 participants
    all_data <- data.frame()
    for(i in 1:50){
      all_data <- rbind(all_data, simdata(i, aois))
    }
    # plot data
    plot(all_data$gazePointX, all_data$gazePointY)
    draw_aois(aois)
}
