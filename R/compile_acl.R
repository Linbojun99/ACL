#' Compile and Load the ACL TMB Model (Cross-Platform)
#'
#' Internal helper function that handles TMB compilation and dynamic library
#' loading across Windows, macOS, and Linux.
#'
#' @return A list with \code{cpp_path} and \code{dll_path}.
#' @keywords internal
compile_and_load_acl <- function() {

  # --- Locate the ACL.cpp bundled with the package ---------------------------
  acl_cpp_path <- system.file("extdata", "ACL.cpp", package = "ACL")
  if (acl_cpp_path == "") {
    stop("ACL.cpp not found in the package. Please reinstall the ACL package.")
  }

  acl_cpp_dir <- dirname(acl_cpp_path)

  # --- Platform-specific shared library extension ----------------------------
  #  Windows  -> .dll
  #  macOS    -> .so
  #  Linux    -> .so
  dll_ext  <- .Platform$dynlib.ext                       # ".so" or ".dll"
  acl_dll_path <- file.path(acl_cpp_dir, paste0("ACL", dll_ext))

  # --- Compile only if the .so/.dll is missing or older than the .cpp --------
  need_compile <- TRUE
  if (file.exists(acl_dll_path) && file.exists(acl_cpp_path)) {
    if (file.mtime(acl_dll_path) >= file.mtime(acl_cpp_path)) {
      need_compile <- FALSE
    }
  }

  if (need_compile) {
    cat("Compiling ACL.cpp for", Sys.info()[["sysname"]], "...\n")

    # Platform-specific log redirection
    if (.Platform$OS.type == "windows") {
      log_redirect <- ""                     # Windows: no redirect (TMB handles it)
    } else {
      log_redirect <- "&> /tmp/acl_compile.log"   # macOS / Linux
    }

    compile(file = acl_cpp_path, log_redirect)

    if (!file.exists(acl_dll_path)) {
      stop(
        "Compilation succeeded but shared library not found at:\n  ",
        acl_dll_path,
        "\nCheck /tmp/acl_compile.log for details."
      )
    }
    cat("Compilation successful:", acl_dll_path, "\n")
  }

  # --- Load the shared library ------------------------------------------------
  if (!is.loaded("ACL")) {
    dyn.load(acl_dll_path)
  }

  invisible(list(cpp_path = acl_cpp_path, dll_path = acl_dll_path))
}


#' Unload the ACL Dynamic Library
#'
#' @param dll_path Path to the shared library file.
#' @keywords internal
unload_acl <- function(dll_path) {
  if (is.loaded("ACL")) {
    dyn.unload(dll_path)
  }
}
