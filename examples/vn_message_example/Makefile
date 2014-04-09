.PHONY: clean
.PHONY: all

ubuntu: all

ravenscar: clean
	echo "pragma Profile(Ravenscar);" > project_pragmas
	gnatmake -d -p -P vn_message_example.gpr

restricted: clean
	echo "pragma Profile(Restricted);" > project_pragmas
	gnatmake -d -p -P vn_message_example.gpr

all: clean
	echo "" > project_pragmas
	gnatmake -d -p -P vn_message_example.gpr

clean:
	gnat clean -P vn_message_example.gpr

clean-all:
	gnat clean -r -P vn_message_example.gpr
