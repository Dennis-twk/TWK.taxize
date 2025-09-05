library(devtools)
document()

# use_package("taxize")
# use_agpl3_license()
use_r("lineage_ncbi")
use_r("lineage_gbif")
use_r("lineage_superfamily_ncbi")

install()
library(TWK.taxize)


