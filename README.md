# gisutil

Example implementation of Geo Spatial Index by Scala, using Z-order curve and scan skip technique with sub-space.

Exacutions (from Python) are described in `sample-usage.ipynb` or `docs/plot-access-data.ipynb` file.

## Insert and Split

Sub-spaces are splitted as follows while insertion of data:

![sub-space split in insert](https://raw.githubusercontent.com/signdoubt/gisutil/images/insert.gif)

## Range scan

Sub-spaces are scanned as follows while range scan:

![sub-space scan in range scan](https://raw.githubusercontent.com/signdoubt/gisutil/images/rangescan.gif)

## k-Nearest Neighbors scan

Sub-spaces are scanned as follows while k-Nearest Neighbors scan (6 nearest neighbors):

![sub-space scan in nearest neighbors](https://raw.githubusercontent.com/signdoubt/gisutil/images/nearestneighborscan.gif)
