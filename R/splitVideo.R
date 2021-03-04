#' Compress and split a video into jpeg files
#'
#' \code{splitVideo} uses FFmpeg via a system call to compress a video file and
#' split it into separate jpeg files in a new directory.
#' @param filepath a character string specifying the location of the video file.
#' @param fps an integer: the number of frames per second of video to keep; if
#'   \code{fps} is the same as the frame rate of the video, all frames will be
#'   saved.
#' @param xpix an integer: the width of saved jpegs in pixels; the default of -1
#'   will use the native width of the video without rescaling.
#' @param ypix an integer: the height of saved jpegs in pixels; the default is
#'   -1, which will preserve the aspect ratio.
#' @details The tracking functions within the \code{pathtrackr} package do not
#'   require high resolution imagery. It is is recommended that videos are
#'   compressed using the \code{xpix} and \code{ypix} variables to improve
#'   processing time. If FFmpeg is not available, video frames can be extracted
#'   using other video editing software.
#' @return Returns a new directory (in the same directory as the video file)
#'   containing each video frame as a jpeg file.
#' @note \code{splitVideo} requires FFmpeg to be installed on your machine.
#'   FFmpeg is a cross-platform, open-source video editing tool. It can be
#'   downloaded from \url{https://ffmpeg.org}. For Mac OS X, easier installation
#'   options exist; such as via Homebrew (\url{https://brew.sh/}) with
#'   \code{brew install ffmpeg}. For Linux, it is even simpler as it should
#'   already be in your package repository (e.g. \code{sudo apt install ffmpeg}
#'   for Ubuntu).
#' @export
splitVideo = function(filepath, fps, xpix = -1, ypix = -1) {

  if (!file.exists(filepath)) {
    stop("No video file was found... check that the file path to your video is correct.")
  }

  if (file.exists(file.path(unlist(strsplit(filepath, "\\."))[1]))) {
    unlink(file.path(unlist(strsplit(filepath, "\\."))[1]), recursive = TRUE)
  }

  if (xpix > 0) {
    if (file.exists(gsub("\\.", "_COMPRESSED.", filepath))) {
      unlink(gsub("\\.", "_COMPRESSED.", filepath))
    }
    system(paste0("ffmpeg -loglevel panic -y -i \"", filepath, "\" -vf scale=", xpix, ":", ypix, " \"", gsub("\\.", "_COMPRESSED.", filepath), "\""))
    dir.create(file.path(unlist(strsplit(filepath, "\\."))[1]), showWarnings = FALSE)
    system(paste0("ffmpeg -loglevel panic -y -i \"", gsub("\\.", "_COMPRESSED.", filepath), "\" -q:v 2 -vf fps=", fps, " -b:v 2000 -bt 20M \"", unlist(strsplit(filepath, "\\."))[1], "/frame%06d.jpg\""))
  } else {
    dir.create(file.path(unlist(strsplit(filepath, "\\."))[1]), showWarnings = FALSE)
    system(paste0("ffmpeg -loglevel panic -y -i \"", filepath, "\" -q:v 2 -vf fps=", fps, " -b:v 2000 -bt 20M \"", unlist(strsplit(filepath, "\\."))[1], "/frame%06d.jpg\""))
  }
}
