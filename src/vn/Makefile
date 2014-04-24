.PHONY: clean
.PHONY: all

ubuntu: all

ravenscar: clean
	echo "pragma Profile(Ravenscar);" > project_pragmas
	gnatmake -d -p -P vn.gpr

restricted: clean
	echo "pragma Profile(Restricted);" > project_pragmas
	gnatmake -d -p -P vn.gpr

all: clean
	echo "" > project_pragmas
	gnatmake -d -p -P vn.gpr

clean:
	-gnat clean -P vn.gpr

clean-all:
	gnat clean -r -P vn.gpr
