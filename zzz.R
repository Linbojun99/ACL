.onLoad <- function(libname, pkgname) {
  cpp_file_path <- system.file("src", "ACL.cpp", package = pkgname)
  TMB::compile(file = cpp_file_path, "&>/tmp/logfile.log")
  message("C++ file compiled.")
}
