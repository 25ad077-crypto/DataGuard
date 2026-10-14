# DataGuard package requirements

packages <- c("shiny", "ggplot2")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

cat("DataGuard packages are ready!\n")
