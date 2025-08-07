# how to install r package from github
#
# install.packages("devtools")
# devtools::install_github("username/repo")
#
#
# To install an R package from GitHub, you use the devtools or remotes package. These tools can install packages directly from a GitHub repository.
#
# ✅ 1. Install devtools (if not already installed)
# r
# Copy
# Edit
install.packages("devtools")
# Or alternatively:
#
#   r
# Copy
# Edit
# install.packages("remotes")  # lighter than devtools
# ✅ 2. Install the GitHub package
# Use the format:
#
devtools::install_github("hlhizonebdoh-maker/epith", force=TRUE)
# Example:
# devtools::install_github("tidyverse/ggplot2")
# This installs the latest development version of ggplot2 from GitHub.
#
# 🔁 If your package is under an organization:
# devtools::install_github("orgname/packagename")
# Example:
# devtools::install_github("hlhizonebdoh-maker/GreetR")
# ⚠️ Common Issues
# 404 error: Repository is private, does not exist, or you're not logged in
#
# Empty repository: No code uploaded yet
#
# Missing dependencies: Run install.packages() for missing packages, or set dependencies = TRUE:
#
devtools::install_github("hlhizonebdoh-maker/epith", dependencies = TRUE)
devtools::install_github("hlhizonebdoh-maker/epith", force = TRUE)
# ✅ 3. If installing your own package
# Make sure your GitHub repo has:
#
# A valid DESCRIPTION file
#
# R code inside the R/ folder
#
# Optional: NAMESPACE, man/, and README.md
#
# You can also install directly from a local folder (during development):
# devtools::install("path/to/package")
