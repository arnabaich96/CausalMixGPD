library(devtools)
load_all('.')
Sys.setenv(LEGACY_FAST='FALSE')
data('nc_pos200_k3')
y_mixed <- nc_pos200_k3
print(typeof(y_mixed))
print(str(y_mixed))
