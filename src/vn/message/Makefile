.PHONY: clean
.PHONY: all

ubuntu: all

ravenscar: clean
	echo "pragma Profile(Ravenscar);" > project_pragmas
	gnatmake -d -p -P message.gpr

restricted: clean
	echo "pragma Profile(Restricted);" > project_pragmas
	gnatmake -d -p -P message.gpr

all: clean
	echo "" > project_pragmas
	gnatmake -d -p -P message.gpr

clean:
	gnat clean -P message.gpr

clean-all:
	gnat clean -r -P message.gpr
