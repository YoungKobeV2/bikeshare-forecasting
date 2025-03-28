set -e

# install language server and httpgd packages
sudo R --vanilla <<END
install.packages(c('languageserver','httpgd'),dependencies=TRUE,repos='https://cloud.r-project.org')
q()
END