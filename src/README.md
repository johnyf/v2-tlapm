# A short description of src files

* commons - data strcutures shared between all layers. Like location,
  etc.
* errors
* fpfile
* method
* params
* revision
* sysconf
* tlapm_args
* toolbox
* types
* version
* config
* expr
* ext
* loc - locations, like properties, it should be removed. locations are
  now defined in commons.
* obligations - obligations data structures. This layer holds the data
strcuture which form the boligations used to compute the input for
backend theorem provers.
* property - hold mutable data. This file was improted from V1 for
  compilation old code but should be removed asap.
* sany - sany data structures. This layer holds the data structure as
  imported from sany. It is different from TLA input data as it flatten
and combine the specs, add level information, etc.
* timing
* tlapm - the main file. it lunch the java sany program, reads the
  generated xml and create a DS in the sany layer, apply
trransformations between the other layers until reaching the obligations
later and schedule the work of the backend provers on these obligations.
In addition it checks fp, etc.
* toolbox_msg
* util

