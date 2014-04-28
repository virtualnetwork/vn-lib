.PHONY: clean
.PHONY: all

ubuntu: all

ravenscar: clean
	echo "pragma Profile(Ravenscar);" > project_pragmas
	gnatmake -d -p -P application_information.gpr

restricted: clean
	echo "pragma Profile(Restricted);" > project_pragmas
	gnatmake -d -p -P application_information.gpr

all: clean
	echo "" > project_pragmas
	gnatmake -d -p -P application_information.gpr

clean:
	-gnat clean -P application_information.gpr

clean-all:
	gnat clean -r -P application_information.gpr
