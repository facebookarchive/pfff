
##############################################################################
# FLAGS
##############################################################################

DEPLOY_PORT=9081
OPAFLAGS=--parser js-like

##############################################################################
# Top rules
##############################################################################

all: pfff_logger.exe

pfff_logger.exe: pfff_logger.opa
	opa $(OPAFLAGS) $<

run: pfff_logger.exe
	./pfff_logger.exe

clean:
	rm -rf _build _tracks *.exe *.log


##############################################################################
# Install
##############################################################################
deploy:
	make kill
	nohup opa-cloud --host localhost,3 ./pfff_logger.exe --port $(DEPLOY_PORT)

#old:nohup ./pfff_logger.exe -p $(DEPLOY_PORT) &

kill:
	pkill pfff_logger.exe || echo no process killed

##############################################################################
# Test
##############################################################################

JSON_TEST='{ unixname: "pad", extra_args: "" }'

test_add:
	curl http://localhost:8080/view/counter
	curl http://localhost:8080/_rest_/t/ -d $(JSON_TEST)
	curl http://localhost:8080/view/counter

test_view:
	curl http://localhost:8080/view/t

view:
	curl http://localhost:$(DEPLOY_PORT)/view/codemap
