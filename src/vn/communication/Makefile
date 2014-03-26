.PHONY: clean
.PHONY: all

ubuntu: all

ravenscar: clean
	echo "pragma Profile(Ravenscar);" > project_pragmas
	gnatmake -d -p -P communication.gpr

restricted: clean
	echo "pragma Profile(Restricted);" > project_pragmas
	gnatmake -d -p -P communication.gpr

all: clean
	echo "" > project_pragmas
	gnatmake -d -p -P communication.gpr

clean:
	gnat clean -P communication.gpr

clean-all:
	gnat clean -r -P communication.gpr
