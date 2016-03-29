/usr/bin/java -jar lib/sany.jar -o -I ./library/ nun/tests.tla > nun/tests.xml
make test
./tlapm.byte nun/tests.xml
open nun/temp_obl.txt