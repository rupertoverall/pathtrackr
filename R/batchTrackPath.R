#' Track an animal's movement across a series of still frames
#'
#' This function is the core of the \code{pathtrackr} package. It takes a series
#' of jpegs (extracted from a video) as inputs and tracks an animal's movement
#' across frames. A list is returned containing the xy co-ordinates of the
#' animal in each frame, as well as summary statistics. The returned list can be
#' called for plotting and further functions in the \code{pathtrackr} package.
#' @param input.dir a character string specifying a directory containing only
#'   jpeg files extracted from a video
#' @param output.filename a character string specifying the path of an output
#'   track file (in CSV format). If this is NULL (default) then no file will be
#'   written.
#' @param fps an integer specifying the frame rate at which jpegs were extracted
#'   from a video; this value is used for distance and velocity calculations,
#'   this is required for accurate timestamp calculations
#' @param box an integer specifying the size of the tracking box relative to the
#'   initial animal selection box; a larger box size will help prevent the
#'   animal being lost (useful for fast moving animals) but will also increase
#'   sensitivity to background and lighting changes (default 1)
#' @param liveplot if TRUE, the image fram will be shown with a red point at the
#'   tracked position. This makes tracking very much slower but is usseful if
#'   tracking a new animal or using a new arena for the first time to help
#'   adjust detection parameters. Default is FALSE.
#' @details \code{trackPath} tracks an individual animal's movement across a
#'   series of still frames. The function utilises a focusing box to limit the
#'   search area for the animal relative to its previous position. This makes
#'   \code{trackPath} relatively robust to background lighting changes,
#'   extraneous background movement and jpeg noise. It can  handle a dark animal
#'   on a light background and vice versa, as well as heterogeneous backgrounds
#'   where the animal is at times darker and lighter than the background.
#' @return A list containing the position/co-ordinates of the animal (matrix
#'   with columns 'x' and 'y') and the timestamp (matrix with 1 column 't') for
#'   each frame, as well as some quality control metrics
#' @importFrom stats median na.omit
#' @importFrom utils tail write.csv flush.console
#' @importFrom raster raster extent select
#' @importFrom grDevices gray.colors
#' @importFrom graphics points
#' @importFrom pbapply pboptions pbapply pblapply
#' @importFrom abind abind
#' @importFrom EBImage bwlabel opening thresh rmObjects
#' @importFrom imager isoblur as.cimg
#' @importFrom plyr count
#' @export
batchTrackPath = function(input.dir, output.filename = NULL, fps, box = 1, liveplot = FALSE) {
  file.list = NULL
  if (length(dir(input.dir, "*.jpg")) > 0) {
    file.list = list.files(input.dir, full.names = TRUE)
  } else {
    stop("No files were found... check that the path to your directory is correct and that it contains only jpg files.")
  }

  # Set progress bar options
  pbapply::pboptions(type = "txt", char = ":")

  bg.crop = raster::extent(raster::raster(file.list[1], band = 2))

  # Animal tracking box is initially whole frame
  bg.ref = greyJPEG(file.list[1])
  bg.ref = bg.ref[(dim(bg.ref)[1] - bg.crop[3]):(dim(bg.ref)[1] - bg.crop[4]), bg.crop[1]:bg.crop[2]]
  bg.dim = dim(bg.ref)
  animal.crop = bg.crop

  ref.x1 = animal.crop[1]
  ref.x2 = animal.crop[2]
  ref.y1 = animal.crop[4]
  ref.y2 = animal.crop[3]
  dim.x = abs(ref.x1 - ref.x2)
  dim.y = abs(ref.y1 - ref.y2)
  # Animal search box is initially the whole field - this will be refined when a target is found.
  x1 = bg.crop[1]
  x2 = bg.crop[2]
  y1 = bg.crop[3]
  y2 = bg.crop[4]

  # Generate background reference frame
  message("Generating background reference frame...\n")
  if (length(file.list) >= 1000) {
    idx = sample(file.list, 1000)
    bg.sample = abind::abind(pbapply::pblapply(idx, greyJPEG), along = 3)
    bg.sample = bg.sample[(dim(bg.sample)[1] - bg.crop[3]):(dim(bg.sample)[1] - bg.crop[4]), bg.crop[1]:bg.crop[2],]
    bg.med = pbapply::pbapply(bg.sample, 1:2, stats::median)
  } else {
    bg.sample = abind::abind(pbapply::pblapply(file.list, greyJPEG), along = 3)
    bg.sample = bg.sample[(dim(bg.sample)[1] - bg.crop[3]):(dim(bg.sample)[1] - bg.crop[4]), bg.crop[1]:bg.crop[2],]
    bg.med = pbapply::pbapply(bg.sample, 1:2, stats::median)
  }

  message("\nTracking animal...\n")
  # Loop through frames fitting tracking box and extracting animal position etc.
  xpos = c()
  ypos = c()
  animal.size = c()
  breaks = c()
  break.count = 1
  animal.last = c()
  blur = 5
  min.animal = 0.25
  max.animal = 1.75

  # For first frame...
  # Find, segment and label blobs, then fit an ellipse
  frame = greyJPEG(file.list[1])
  frame = frame[(dim(frame)[1] - bg.crop[3]):(dim(frame)[1] - bg.crop[4]), bg.crop[1]:bg.crop[2]]
  frame = abs(frame - bg.med)
  tbox = reflect(frame[ref.y1:ref.y2,ref.x1:ref.x2])
  tbox.bin = as.matrix(EBImage::bwlabel(EBImage::opening(EBImage::thresh(imager::isoblur(imager::as.cimg(tbox), blur)))))
  
  # What is the largest and brightest bin?
  bins = table(tbox.bin)
  bins = bins[as.numeric(names(bins)) > 0] # 0 is background.
  bin.mean.intensity = by(as.numeric(tbox), as.factor(tbox.bin), mean)[names(bins)]
  bin.score = as.numeric(bins) * bin.mean.intensity
  top.bin = names(sort(bin.score, decreasing = T))[1]

  animal = ellPar(which(tbox.bin == top.bin, arr.ind = TRUE))
  animal.last = which(tbox.bin == top.bin)

  # Correct xy positions relative to entire frame and store
  xpos[1] = round(animal$centre[2] + ref.x1)
  ypos[1] = round(animal$centre[1] + ref.y2)

  # Store animal size
  animal.size[1] = animal$area

  # Calculate bounding box based on animal size. (Will be recalculated if focus is lost).
  x.bounds = range(animal$boundary[, 2])
  x.bounds = (c(-1, 1) * diff(x.bounds)) + mean(x.bounds)
  y.bounds = range(animal$boundary[, 1])
  y.bounds = (c(-1, 1) * diff(y.bounds)) + mean(y.bounds)
  dim.x = diff(x.bounds)
  dim.y = diff(y.bounds)

  # For the remaining frames...
  pbloop = pbapply::timerProgressBar(min = 0, max = length(file.list), style = 3, char = ":", width = 50)
  for (i in 2:length(file.list)) {

    # Calculate co-oordinates to redraw tracking box around last position
    if (!is.na(utils::tail(xpos, 1))) {x1 = xpos[i - 1] - dim.x * box}
    if (x1 < 0) {x1 = 0}
    if (x1 > bg.dim[2]) {x1 = bg.dim[2]}
    if (!is.na(utils::tail(xpos, 1))) {x2 = xpos[i - 1] + dim.x * box}
    if (x2 < 0) {x2 = 0}
    if (x2 > bg.dim[2]) {x2 = bg.dim[2]}
    if (!is.na(utils::tail(ypos, 1))) {y1 = ypos[i - 1] - dim.y * box}
    if (y1 < 0) {y1 = 0}
    if (y1 > bg.dim[1]) {y1 = bg.dim[1]}
    if (!is.na(utils::tail(ypos, 1))) {y2 = ypos[i - 1] + dim.y * box}
    if (y2 < 0) {y2 = 0}
    if (y2 > bg.dim[1]) {y2 = bg.dim[1]}

    # Find, segment and label blobs, then fit an ellipse
    frame = greyJPEG(file.list[i])
    frame = frame[(dim(frame)[1] - bg.crop[3]):(dim(frame)[1] - bg.crop[4]), bg.crop[1]:bg.crop[2]]
    frame = abs(frame - bg.med)
    tbox = reflect(frame[y2:y1,x1:x2])
    tbox.bin = as.matrix(EBImage::bwlabel(EBImage::opening(EBImage::thresh(imager::isoblur(imager::as.cimg(tbox), blur)))))

    # What is the largest and brightest bin?
    bins = table(tbox.bin)
    bins = bins[as.numeric(names(bins)) > 0] # 0 is background.
    bin.mean.intensity = by(as.numeric(tbox), as.factor(tbox.bin), mean)[names(bins)]
    bin.score = as.numeric(bins) * bin.mean.intensity
    top.bin = names(sort(bin.score, decreasing = T))[1]

    # Calculate proportion of overlapping pixels from between current & previous frame
    animal.new = which(tbox.bin == top.bin)
    animal.move = (length(stats::na.omit(match(animal.last, animal.new))))/(max(c(length(animal.last), length(animal.new))))
    animal.last = animal.new
  
    # Check if animal is of ~right size
    if (length(which(tbox.bin == top.bin)) > mean(animal.size, na.rm = TRUE)*min.animal & length(which(tbox.bin == 1)) < mean(animal.size, na.rm = TRUE)*max.animal) {

      # Check animal hasn't moved more than 10% of size
      if (animal.move < 0.9) {

        animal = ellPar(which(tbox.bin == top.bin, arr.ind = TRUE))

        # Correct xy positions relative to entire frame and store
        xpos[i] = round(animal$centre[2] + x1)
        ypos[i] = round(animal$centre[1] + y1)

        # Store animal size
        animal.size[i] = animal$area

      } else {
        # Update bounding box based on animal size as it seems we have lost the animal.
        x.bounds = range(animal$boundary[, 2])
        x.bounds = (c(-1, 1) * diff(x.bounds)) + mean(x.bounds)
        y.bounds = range(animal$boundary[, 1])
        y.bounds = (c(-1, 1) * diff(y.bounds)) + mean(y.bounds)
        dim.x = diff(x.bounds)
        dim.y = diff(y.bounds)

        # Store last known position of animal and size
        xpos[i] = xpos[i - 1]
        ypos[i] = ypos[i - 1]
        animal.size[i] = animal.size[i - 1]
      }

    } else {
        # Update bounding box based on animal size as it seems we have lost the animal.
        x.bounds = range(animal$boundary[, 2])
        x.bounds = (c(-1, 1) * diff(x.bounds)) + mean(x.bounds)
        y.bounds = range(animal$boundary[, 1])
        y.bounds = (c(-1, 1) * diff(y.bounds)) + mean(y.bounds)
        dim.x = diff(x.bounds)
        dim.y = diff(y.bounds)

      frame.break = greyJPEG(file.list[i])
      frame.break = frame.break[(dim(frame.break)[1] - bg.crop[3]):(dim(frame.break)[1] - bg.crop[4]), bg.crop[1]:bg.crop[2]]
      frame.break = reflect(abs(frame.break - bg.med))
      frame.break.bin = as.matrix(EBImage::bwlabel(EBImage::opening(EBImage::thresh(imager::isoblur(imager::as.cimg(frame.break), blur)))))
      blob.pixcount = as.matrix(plyr::count(frame.break.bin[frame.break.bin > 0]))

      if (nrow(blob.pixcount) > 1) {
        frame.break.bin = EBImage::rmObjects(frame.break.bin, blob.pixcount[blob.pixcount[,2] < mean(animal.size, na.rm = TRUE)*min.animal | blob.pixcount[,2] > mean(animal.size, na.rm = TRUE)*max.animal,1])
      }

      if (length(which(frame.break.bin == 1)) > mean(animal.size, na.rm = TRUE)*min.animal & length(which(frame.break.bin == 1)) < mean(animal.size, na.rm = TRUE)*max.animal) {

        animal = ellPar(which(frame.break.bin == 1, arr.ind = TRUE))

        # Correct xy positions relative to entire frame and store
        xpos[i] = round(animal$centre[2])
        ypos[i] = bg.dim[1] - round(animal$centre[1])

        # Store animal size
        animal.size[i] = animal$area

      } else {

        # Mark position and size as unknown
        xpos[i] = NA
        ypos[i] = NA
        animal.size[i] = NA

        # Store breaks
        breaks[break.count] = i
        break.count = break.count + 1
      }
    }

    if(liveplot){
      raster::plot(raster::raster(file.list[i], band = 2), col = grDevices::gray.colors(256), asp = 1, legend = FALSE)
      graphics::points(xpos[i], ypos[i], col = "red", pch = 19)
    }
    
    pbapply::setTimerProgressBar(pbloop, i)
  }

  time = seq(0, length.out = length(xpos), by = 1/fps)

  if (length(breaks) > 0) {
    warning("Tracking was not possible for ", length(breaks), " frames: you can proceed with this tracked path but you might consider using a higher frame rate or increasing the tracking 'box' size to improve the result.")
    utils::flush.console()
  }

  # Write file
  if(!is.null(output.filename)){
    # Check output filename is sane. It should use the correct extension.
    # Non-standard extensions will not be removed (in case '.' is in the filename) but '.csv' will be appended.
    if(tools::file_ext(output.filename) != "csv") output.filename = paste(output.filename, "csv", sep = ".")
    utils::write.csv(cbind(t = time, x = xpos, y = ypos), file = output.filename, row.names = FALSE)
  }
  
  return(list(position = cbind(x = xpos, y = ypos), timestamp = cbind(t = time), dim.area = c(bg.dim[2], bg.dim[1]), fps = fps, breaks = breaks))

}
