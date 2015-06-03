# OPAM configuration
. /builds/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

echo $USER
echo $PATH

#create xml file instead of stdout test results
export TLAPM_TEST_OUTPUT=xml

oasis setup
./configure --enable-tests
make
make test
