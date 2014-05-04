.PHONY: clean
.PHONY: all

ubuntu: all

ravenscar: clean
	echo "pragma Profile(Ravenscar);" > project_pragmas
	gnatmake -d -p -P logger.gpr

restricted: clean
	echo "pragma Profile(Restricted);" > project_pragmas
	gnatmake -d -p -P logger.gpr

all: clean
	echo "" > project_pragmas
	gnatmake -d -p -P logger.gpr

clean:
	-gnat clean -P logger.gpr

clean-all:
	gnat clean -r -P logger.gpr
