#
# Download medthreats data
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3

download.file("http://neptune.nceas.ucsb.edu/medthreats/data.zip", "data.zip")

# uncompress
unzip("data.zip")
unlink("data.zip")
unlink("__MACOSX", recursive=TRUE)

# rename
file.rename("data", "medthreats")

# remove habitat which we do not care about here
unlink("medthreats/habitat", recursive=TRUE)

# remove cached info about .tifs, which we will not use
aux <- list.files("medthreats", pattern="((tfw)|(xml)|(ovr))$", full.names=TRUE, recursive=TRUE)
unlink(aux)
