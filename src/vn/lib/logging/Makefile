.PHONY: clean
.PHONY: all

ubuntu: all

ravenscar: clean
	echo "pragma Profile(Ravenscar);" > project_pragmas
	gnatmake -d -p -P logging.gpr

restricted: clean
	echo "pragma Profile(Restricted);" > project_pragmas
	gnatmake -d -p -P logging.gpr

all: clean
	echo "" > project_pragmas
	gnatmake -d -p -P logging.gpr

clean:
	-gnat clean -P logging.gpr

clean-all:
	gnat clean -r -P logging.gpr
