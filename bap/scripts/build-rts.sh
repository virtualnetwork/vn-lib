################################################################################
# Filename         # build-rts.sh
# Purpose          # Build a Ravenscar Runtime System for Ada
# Description      # Enable Ada Ravenscar Runtime for Bare Metal systems
# Copyright        # Fredrik Bruhn, Magnus Norgren (c) 2014
# Contact          # f@adv.bruhnspace.com, fredrik.bruhn@mdh.se
#		   # magnus@adv.bruhnspace.com
#
#   Inspired by Luke A. Guest, David Rees, The Ada Microkernel (TAMP).
################################################################################

#!/bin/sh
clear
source ./errors.inc

##########################################################################
# This defines the boards we currently support. This will also define where
# the RTS is built.
#
# Current list of supported boards.
##########################################################################
BOARDS="sf2-starter-kit-010 sf2-starter-kit-050 sf2-dev-kit sf2-unibap-e2000 lm3s811"

function list_boards()
{
    echo "  Boardname is one of:"
	echo ""
    for b in $BOARDS
    do
	  echo "  --> $b"
    done
}


##########################################################################
# Match board RTS with the proper marchitecture.
# ARCHITECTURE options:
#   * arm-legacy    // ARM arm2v, arm4v, arm7tdmi with FIQ controller
#   * cortex-m      // ARM Cortex-M family (M0, M1, M3, M4...)
#   * or1k          // OpenRISC 1000 Architecture, OR1200, MOR1k etc.
#   * sparcv8       // SPARC v8, LEON2, LEON3 etc....
#
##########################################################################
BOARD=$1
ARCHITECTURE=empty

if [[ $BOARD == lm3s811 ]]
then
    ARCHITECTURE="cortex-m"
    GCC_TARGET="arm-none-eabi-gcc"
fi

if [[ $BOARD == sf2-starter-kit-050 ]]
then
    ARCHITECTURE="cortex-m"
    GCC_TARGET="arm-none-eabi-gcc"
fi

if [[ $BOARD == sf2-starter-kit-010 ]]
then
    ARCHITECTURE="cortex-m"
    GCC_TARGET="arm-none-eabi-gcc"
fi

if [[ $BOARD == sf2-dev-kit ]]
then
    ARCHITECTURE="cortex-m"
    GCC_TARGET="arm-none-eabi-gcc"
fi

if [[ $BOARD == sf2-unibap-e2000 ]]
then
    ARCHITECTURE="cortex-m"
    GCC_TARGET="arm-none-eabi-gcc"
fi

if [[ $ARCHITECTURE == empty ]]
then
	echo " --------------------------------------------"
	echo " | Bruhnspace Advanced Projects AB (BAP)    |"
	echo " | Build Realtime System for Ada bare metal |"
	echo " --------------------------------------------"
	echo ""
    	echo "  Script Usage:"
    	echo "  ./build-rts.sh <boardname>"
    	echo "  or"
    	echo "  ./build-rts.sh <boardname> rebuild"
    	echo "  or"
    	echo "  ./build-rts.sh clean"
    	echo ""
    	list_boards
	echo ""
	echo "Re-run script in accordance with the instruction above."
	echo ""
	exit 2
fi
##########################################################################

#if [ ! -f ./config.inc ]; then
	source ./config.inc
#fi

export PATH=$INSTALL_DIR/bin:$PATH

GCC_VERSION=`$GCC_TARGET --version | grep $GCC_TARGET`

RTS=$TOP/../rts

echo ""
echo "  Creating RTS with GCC cross-compiler:"
echo "  $GCC_VERSION"
echo "  for selected '$1' board configuration"
echo "  for selected '$ARCHITECTURE' board architecture"
echo ""
echo "  With root build RTS directory: $RTS"
echo ""

##########################################################################

# $1 = board name
function check_board_name()
{
    if [[ ! $BOARDS =~ $1 ]]
    then
	echo "  ERROR: Incorrect board name selected"
	echo ""

	list_boards

	exit 2
    fi
}

function create_dirs()
{
    cd $RTS
    echo ""
    echo "  >> Creating RTS directory for board <$BOARD>..."

    if [ ! -d  obj ]
    then
        echo "Creating $RTS/obj dir..."
        mkdir -p obj
    fi

    if [ ! -d  boards ]
    then
        echo "Creating $RTS/boards dir..."
        mkdir -p boards
    fi

    # Don't create directories for all boards, only the selected one.
    #for b in $BOARDS
    #do
	if [ ! -d boards/$BOARD ]
	then
 	    echo "Creating $RTS/boards/$BOARD/adainclude dir..."
	    echo "Creating $RTS/boards/$BOARD/adalib dir..."
	    mkdir -p boards/$BOARD/adainclude
	    mkdir -p boards/$BOARD/adalib
	fi
    #done
}

#
# Ravenscar LIBGNAT Common sources
#

LIBGNAT="system.ads ada.ads a-except.ads "
LIBGNAT=$LIBGNAT"gnat.ads g-souinf.ads g-io.ads interfac.ads i-c.ads s-assert.ads s-stoele.ads s-maccod.ads s-unstyp.ads s-fatflt.ads "
LIBGNAT=$LIBGNAT"s-fatlfl.ads s-fatllf.ads s-fatsfl.ads memory_compare.ads memory_copy.ads memory_set.ads s-secsta.ads a-tags.ads s-sssita.ads"

# Sort the list.
LIBGNAT=$(echo "$LIBGNAT" | tr " " "\n" | sort | tr "\n" " ")
echo "********************************"
echo "Ravenscar LIBGNAT Common sources"
echo "|-"
echo " $LIBGNAT"
#
# Ravenscar LIBGNAT Bare Board sources
#
# s-bb*.ads s-ba*.ads

Listing=(`ls -1 ../rts/src/common/s-bb*.ads | xargs -n1 basename`)
LIBGNAT=$LIBGNAT${Listing[@]}
echo ""
echo "************************************"
echo "Ravenscar LIBGNAT bare board sources"
echo "|-"
echo " ${Listing[@]}"
echo ""
echo " |- Board specific sources"
Listing=(`ls -1 ../rts/src/boards/$1/*.ads | xargs -n1 basename`)
LIBGNAT_BOARD=${Listing[@]}
echo "$LIBGNAT_BOARD"
echo ""

# Sort the final list
LIBGNAT=$(echo "$LIBGNAT" | tr " " "\n" | sort | tr "\n" " ")

#
# Ravenscar LIBGNARL
#
echo "************************************"
echo "Ravenscar LIBGNARL files"
echo "|-"
Exclude=($LIBGNAT)
Listing=(`ls -1 ../rts/src/common/*.ads | xargs -n1 basename`)

LIBGNARL=( $(comm -23 <( printf "%s\n" "${Listing[@]}" ) <( printf "%s\n" "${Exclude[@]}") ) )
LIBGNARL=${LIBGNARL[@]}
echo $LIBGNARL
echo ""

#
# Ravenscar .adb files
echo "*************************************"
echo "Ravenscar .adb files"
echo "|-"
ADB_Listing=(`ls -1 ../rts/src/common/*.adb | xargs -n1 basename`)
ADB2_Listing=(`ls -1 ../rts/src/boards/$1/*.adb | xargs -n1 basename`)
C_Listing=(`ls -1 ../rts/src/boards/$1/*.c | xargs -n1 basename`)

ADB_Listing=${ADB_Listing[@]}
ADB2_Listing=${ADB2_Listing[@]}
C_Listing=${C_Listing[@]}

echo "$ADB_Listing"
echo "|-"
echo "$ADB2_Listing"
echo ""

#
# Ravenscar architectural files
echo "*************************************"
echo "Ravenscar architectural files"
echo "|-"
ARCH_Listing=(`ls -1 ../rts/src/arch/$ARCHITECTURE/*.* | xargs -n1 basename`)
ARCH_Listing=${ARCH_Listing[@]}
echo "$ARCH_Listing"

# function copy_rts_files()
# {
#     if [ ! -f $RTS/.rts_copied ]
#     then
# 	FILES="$SPECS $BODIES"

# 	for f in $FILES
# 	do
# 	    echo "  >> Copying $GCC_DIR/gcc/ada/$f to $RTS/src/common..."

# 	    cp $GCC_DIR/gcc/ada/$f $RTS/src/common

# 	    check_error_exit
# 	done

# 	check_error $RTS/.rts_copied
#     fi
# }

# $1 = board name

function create_symlinks()
{
    cd $RTS/boards/$BOARD/adainclude

    if [ ! -f .symlinks ]
    then
	FILES=$LIBGNAT
	echo ""
	echo "---- Linking RAVENSCAR LIBGNAT files from common ----"
	echo ""
	for f in $FILES
	do
	    echo "$f"
#	    echo "  >> Linking $f to $RTS/src/common/$f..."
	    ln -s $RTS/src/common/$f $f
	    check_error_exit
	done

	FILES=$LIBGNAT_BOARD
	echo ""
	echo "---- Linking RAVENSCAR LIBGNAT files from selected <$1> board ----"
	echo ""
	for f in $FILES
	do
	    echo "$f"
#	    echo "  >> Linking $f to $RTS/src/boards/$1/$f..."
	    ln -s $RTS/src/boards/$1/$f $f
	    check_error_exit
	done

	FILES=$LIBGNARL
	echo ""
	echo "---- Linking LIBGNARL from common ----"
	echo ""
	for f in $FILES
	do
	    echo "$f"
#	    echo "  >> Linking $f to $RTS/src/common/$f..."
	    ln -s $RTS/src/common/$f $f
	    check_error_exit
	done

	echo ""
	echo "---- Linking adb and startup routine files from common and board <$1>----"
	echo ""

	FILES=$ADB_Listing
	for f in $FILES
	do
#	    echo "  >> Linking $f to $RTS/src/common/$f..."
	    ln -s $RTS/src/common/$f $f
	    check_error_exit
	done

	FILES=$ADB2_Listing
	for f in $FILES
	do
	    echo "$f"
#	    echo "  >> Linking $f to $RTS/src/boards/$1/$f..."
	    ln -s $RTS/src/boards/$1/$f $f
	    check_error_exit
	done

    FILES=$C_Listing
	for f in $FILES
	do
	    echo "$f"
#	    echo "  >> Linking $f to $RTS/src/boards/$1/$f..."
	    ln -s $RTS/src/boards/$1/$f $f
	    check_error_exit
	done

	echo ""
	echo "---- Linking architectural files from arch <$ARCHITECTURE>----"
	echo ""
        FILES=$ARCH_Listing
	for f in $FILES
	do
            echo "$f"
#	    echo "  >> Linking $f to $RTS/src/arch/$ARCHITECTURE/$f..."
	    ln -s $RTS/src/arch/$ARCHITECTURE/$f $f
	    check_error_exit
	done

    # Generate empty libnat.a and libgnarl.a
    # arm-none-eabi-ar rc $RTS/boards/$1/adalib/libgnat.a
    # arm-none-eabi-ar rc $RTS/boards/$1/adalib/libgnarl.a

    fi
}

# $1 = board name

function build_rts()
{
    cd $RTS

    GNATMAKE=""
    case $1 in
	"pc")
	    GNATMAKE="gnatmake"
	    ;;
	"sf2-starter-kit-050")
	    GNATMAKE="arm-none-eabi-gnatmake"
	    ;;
	"lm3s811")
	    GNATMAKE="arm-none-eabi-gnatmake"
	    ;;
	"sf2-starter-kit-010")
	    GNATMAKE="arm-none-eabi-gnatmake"
	    ;;
	"sf2-dev-kit")
	    GNATMAKE="arm-none-eabi-gnatmake"
	    ;;
	"sf2-unibap-e2000")
	    GNATMAKE="arm-none-eabi-gnatmake"
	    ;;
    esac

    FLAGS="-gnatf -gnatv"

#    $GNATMAKE --RTS=$RTS/boards/$1 -XBoard=$1 -Pgnat.gpr
    BOARD=$1 make
}


function clean_objs()
{
    cd $RTS
    rm boards/$1/adalib/*
}

if [ $1 = "clean" ]
then
    cd $RTS
    rm -rf obj/*
    rm -rf boards/*
else
    if [ $2 = "rebuild" ]
    then
        cd $RTS
        BOARD=$1 make clean
    fi

    check_board_name $1
    create_dirs
    create_symlinks $1
    echo ""
    echo "****************************************"
    echo "Run ../rts/makefile and build rts."
    echo "Using : " `which arm-none-eabi-gnatmake`
    echo "|-"
#    build_rts $1
#    clean_objs
fi

cd $TOP
