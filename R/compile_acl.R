#' Compile and Load the ACL TMB Model (Cross-Platform, OpenMP-aware)
#'
#' Internal helper function that handles TMB compilation and dynamic library
#' loading across Windows, macOS, and Linux. Supports OpenMP compilation
#' for parallel gradient computation via \code{TMB::openmp()}.
#'
#' @param openmp Logical. Compile with OpenMP support for parallel gradient
#'   computation. Default is TRUE. If OpenMP is not available on the system,
#'   falls back to single-threaded compilation automatically.
#' @return A list with \code{cpp_path}, \code{dll_path}, and \code{openmp} (logical, whether OpenMP was used).
#' @keywords internal
compile_and_load_acl <- function(openmp = TRUE) {

  # --- Locate the ACL.cpp bundled with the package ---------------------------
  acl_cpp_path <- system.file("extdata", "ACL.cpp", package = "ACL")
  if (acl_cpp_path == "") {
    stop("ACL.cpp not found in the package. Please reinstall the ACL package.")
  }

  acl_cpp_dir <- dirname(acl_cpp_path)

  # --- Platform-specific shared library extension ----------------------------
  dll_ext  <- .Platform$dynlib.ext                       # ".so" or ".dll"
  acl_dll_path <- file.path(acl_cpp_dir, paste0("ACL", dll_ext))

  # --- Compile only if the .so/.dll is missing or older than the .cpp --------
  need_compile <- TRUE
  if (file.exists(acl_dll_path) && file.exists(acl_cpp_path)) {
    if (file.mtime(acl_dll_path) >= file.mtime(acl_cpp_path)) {
      need_compile <- FALSE
    }
  }

  openmp_used <- FALSE

  if (need_compile) {
    sysname <- Sys.info()[["sysname"]]
    cat("Compiling ACL.cpp for", sysname, "...\n")

    # Platform-specific log redirection
    if (.Platform$OS.type == "windows") {
      log_redirect <- ""
    } else {
      log_redirect <- "&> /tmp/acl_compile.log"
    }

    # --- Attempt OpenMP compilation ---
    if (openmp) {
      omp_flags <- ""
      if (sysname == "Linux") {
        omp_flags <- "-fopenmp"
      } else if (sysname == "Darwin") {
        # macOS: requires libomp (brew install libomp)
        omp_flags <- "-Xpreprocessor -fopenmp"
      } else {
        # Windows with Rtools
        omp_flags <- "-fopenmp"
      }

      compile_ok <- tryCatch({
        compile(file = acl_cpp_path, flags = omp_flags, log_redirect)
        TRUE
      }, error = function(e) FALSE)

      if (compile_ok && file.exists(acl_dll_path)) {
        cat("Compilation successful (with OpenMP):", acl_dll_path, "\n")
        openmp_used <- TRUE
      } else {
        # OpenMP failed, fallback to standard compilation
        cat("OpenMP compilation failed, retrying without OpenMP...\n")
        if (sysname == "Darwin") {
          cat("  Tip: install libomp for OpenMP on Mac: brew install libomp\n")
        }
        # Remove partial output
        if (file.exists(acl_dll_path)) file.remove(acl_dll_path)

        compile(file = acl_cpp_path, log_redirect)

        if (!file.exists(acl_dll_path)) {
          stop("Compilation failed. Check /tmp/acl_compile.log for details.")
        }
        cat("Compilation successful (without OpenMP):", acl_dll_path, "\n")
      }
    } else {
      # Standard compilation without OpenMP
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
  } else {
    # DLL already up to date — check if it was compiled with OpenMP
    # (We can't know for sure, but TMB::openmp() will silently work or not)
    openmp_used <- openmp
  }

  # --- Load the shared library ------------------------------------------------
  if (!is.loaded("ACL")) {
    dyn.load(acl_dll_path)
  }

  invisible(list(cpp_path = acl_cpp_path, dll_path = acl_dll_path, openmp = openmp_used))
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
