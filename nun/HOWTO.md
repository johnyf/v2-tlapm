# How to use Nunchaku with TLAPM

## Compiling TLAPMv2

`make clean`

`make test`

## Find a countermodel

From the `v2-tlapm-nunchaku` directory :

### Edit TLA

Use the Toolbox (TLAv1) to edit `tests.tla` (in `nun/tla`)

### Contever TLA to XML

`sh tla2xml.sh -o -I ./library/ nun/tla/tests.tla > nun/xml/tests.xml`

### Output TLA obligation

`./tlapm.byte xml2obligations nun/xml/tests.xml nun/obligations.txt`

### From obligations to nunchaku files

`./tlapm.byte xml2nun nun/xml/tests.xml nun/nun`

### Call nunchaku 

#### on one file

`nunchaku nun/nun/1.nun`

#### on several files

`for f in nun/nun/*.nun; do echo "Processing file $f :" ; nunchaku $f; done`

## Automatized process

Just run `./tlapm.byte auto tests`