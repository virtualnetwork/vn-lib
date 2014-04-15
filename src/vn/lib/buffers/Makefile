.PHONY: clean
.PHONY: all

ubuntu: all

ravenscar: clean
	echo "pragma Profile(Ravenscar);" > project_pragmas
	gnatmake -d -p -P buffers.gpr

restricted: clean
	echo "pragma Profile(Restricted);" > project_pragmas
	gnatmake -d -p -P buffers.gpr

all: clean
	echo "" > project_pragmas
	gnatmake -d -p -P buffers.gpr

clean:
	gnat clean -P buffers.gpr

clean-all:
	gnat clean -r -P buffers.gpr
