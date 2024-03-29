library(devtools)

# Development workflow
#define o diretorio de trabalho

# setwd('~/Dropbox/projetos/pacote/')
setwd('C:/Users/ze/Dropbox/projetos/pacote/')
#setwd('~/pacote/')

#comandos para preparar o pacote
load_all('epinemo')
document('epinemo')
install('epinemo')
library(epinemo)
#setwd('~/pacotes/epinemo')

# News preview
show_news('epinemo')

# Checking
check()
check_doc()
run_examples()
build_win()

# Update version in:
#   DESCRIPTION
#   epinemo-package
#   README.md
#   NEWS
#   Home web page

#####################
## release('epinemo') ##
#####################