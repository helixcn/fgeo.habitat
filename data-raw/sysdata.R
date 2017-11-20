# Export object from geoR that wasn't exported but is needed
.geoR.cov.models <- geoR:::.geoR.cov.models
use_data(.geoR.cov.models, internal = TRUE)
