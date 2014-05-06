.PHONY: clean
.PHONY: all

ubuntu: all

ravenscar: clean
	echo "pragma Profile(Ravenscar);" > project_pragmas
	gnatmake -d -p -P basic_vn_application.gpr

restricted: clean
	echo "pragma Profile(Restricted);" > project_pragmas
	gnatmake -d -p -P basic_vn_application.gpr

all: clean
	echo "" > project_pragmas
	gnatmake -d -p -P basic_vn_application.gpr

clean:
	-gnat clean -P basic_vn_application.gpr

clean-all:
	gnat clean -r -P basic_vn_application.gpr
