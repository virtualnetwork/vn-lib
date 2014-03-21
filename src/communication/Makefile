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
	rm -rf obj/*.o
	rm -rf obj/*.ali
	rm -rf obj/main
