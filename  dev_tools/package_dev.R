library(devtools)

# Development workflow
setwd('~/Dropbox/projetos/pacote/')
#setwd('~/pacote/')
load_all('epinemo')
document('epinemo')
install('epinemo')
library(epinemo)
setwd('~/pacotes/epinemo')

# News preview
show_news('epinemo')

# Checking
check()
check_doc()
run_examples()
build_win()

# Update version in:
#   DESCRIPTION
#   capm-package
#   README.md
#   NEWS
#   Home web page
#   Web documentation in all languages

#####################
## release('epinemo') ##
#####################