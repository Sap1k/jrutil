#!/bin/sh

cd "$(dirname $0)"

echo Generating CzPttXml...
xscgen \
	--namespace=CzPttXml \
	--nullable \
	--collectionType=System.Array \
	--collectionSettersMode=Public \
	--netCore \
	-o CzPttXml \
	../jrutil/src/CzPtt/czptt.xsd

