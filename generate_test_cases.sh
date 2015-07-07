#/bin/bash
# runs the sany to xml converter on each file in test/resources/tla/*.tla and writes the
# output to test/resources/xml/*.xml
for I in test/resources/tla/*tla; 
do 
  ./tla2xml.sh -o -I library -I test/resources/tla $I > `echo $I | sed s/tla/xml/g`;
done
