
MAKESUBDIRS=netsys netstring

all:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done 

opt:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i opt || exit 1; done 

all.opt: opt


depend::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done 

clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 
