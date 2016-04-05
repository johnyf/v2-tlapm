echo "--- TLA -> XML ---"
/usr/bin/java -jar lib/sany.jar -o -I ./library/ nun/tests.tla > nun/tests.xml
echo "--- Compiling TLA with tests ---"
make test
echo "--- Calling TLA ---"
./tlapm.byte nun/tests.xml
echo "--- Opening file ---"
open nun/complex.txt
open nun/simple.txt
