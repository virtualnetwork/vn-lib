# TODO: Use this until gnatmake can build a lib.
#BOARD	=	pc

DEBUG		=	-g
#DEBUG		=	-O2

ifeq ($(BOARD),lm3s811)
TARGET		=	arm-none-eabi-
TARGETFLAGS	=	-mcpu=cortex-m3 -mthumb
endif

ifeq ($(BOARD),sf2-starter-kit-050)
TARGET		=	arm-none-eabi-
TARGETFLAGS	=	-mcpu=cortex-m3 -mthumb
endif

ifeq ($(BOARD),sf2-starter-kit-010)
TARGET		=	arm-none-eabi-
TARGETFLAGS	=	-mcpu=cortex-m3 -mthumb
endif

ifeq ($(BOARD),sf2-dev-kit)
TARGET		=	arm-none-eabi-
TARGETFLAGS	=	-mcpu=cortex-m3 -mthumb
endif

ifeq ($(BOARD),sf2-unibap-e2000)
TARGET		=	arm-none-eabi-
TARGETFLAGS	=	-mcpu=cortex-m3 -mthumb
endif


# Add flags to force the compiler to conform to the standard, see
# S.12.6 of the GNAT Reference Manual.
CONFORMANCE	=	-gnato -gnatE -fstack-check

ADA_FLAGS		=	-c -gnat2005 $(TARGETFLAGS) $(DEBUG) -gnatg -gnata \
			$(CONFORMANCE) \
			-gnatyy \
			-ffunction-sections -fdata-sections \
			-nostdinc -nostdlib --RTS=./boards/$(BOARD)
#			-I./boards/$(BOARD)

SRCDIR=./boards/$(BOARD)/adainclude

LIBGNAT		=	./boards/$(BOARD)/adalib/libgnat.a

# Source files for Ravenscar LIBGNAT

# --  Common sources  --
LIBGNAT_ADA_SRC = \
system.ads   \
ada.ads      \
a-except.ads \
gnat.ads     \
g-souinf.ads \
g-io.ads     \
interfac.ads \
i-c.ads      \
s-assert.ads \
s-stoele.ads \
s-maccod.ads \
s-unstyp.ads \
s-fatflt.ads \
s-fatlfl.ads \
s-fatllf.ads \
s-fatsfl.ads \
memory_compare.ads \
memory_copy.ads \
memory_set.ads \
s-secsta.ads \
a-tags.ads   \
s-sssita.ads

# -- Board specfic

# --  Bare Board sources  --
MUPP = $(wildcard $(SRCDIR)/s-bb*.ads)
MUPP := $(MUPP:$(SRCDIR)/%=%)
MUPP2 = $(wildcard $(SRCDIR)/s-t*.ads)
MUPP2 := $(MUPP2:$(SRCDIR)/%=%)

LIBGNAT_ADA_SRC += $(MUPP) #$(wildcard s-bb*.ads)  $(wildcard s-ba*.ads) 
LIBGNAT_ADA_SRC += $(MUPP2) #$(wildcard s-bb*.ads)  $(wildcard s-ba*.ads) 

LIBGNAT_OBJS = $(addprefix boards/$(BOARD)/adalib/, $(LIBGNAT_ADA_SRC:.ads=.o))
LIBGNAT_ALIS = $(addprefix boards/$(BOARD)/adalib/, $(LIBGNAT_ADA_SRC:.ads=.ali)) 

# Source files for Ravenscar LIBGNARL
# All other except GNAT ones

APA = $(wildcard $(SRCDIR)/*.ads)
APA := $(APA:$(SRCDIR)/%=%)

LIBGNARL_SRC = $(filter-out $(LIBGNAT_ADA_SRC), $(APA))

LIBGNARL = ./boards/$(BOARD)/adalib/libgnarl.a
LIBGNARL_OBJS = $(addprefix boards/$(BOARD)/adalib/, $(LIBGNARL_SRC:.ads=.o))
LIBGNARL_ALIS = $(addprefix boards/$(BOARD)/adalib/, $(LIBGNARL_SRC:.ads=.ali))

#ADA_SRC = $(LIBGNAT_ADA_SRC) $(LIBGNARL_SRC)

# Utils
CC	    = arm-none-eabi-gcc
BIN	    = arm-none-eabi-objcopy
SIZE	    = arm-none-eabi-size
AS          = arm-none-eabi-as
AR	    = arm-none-eabi-ar
RANLIB      = arm-none-eabi-ranlib
MKDIR       = mkdir
RM	    = rm -f

vpath %.ads $(SRCDIR)
vpath %.adb $(SRCDIR)

# ------------------------------------------------------------------------

all: $(SCRDIR)/gnat.adc $(LIBGNAT) $(LIBGNARL)

$(SCRDIR)/gnat.adc:
	echo "pragma Profile (Ravenscar);" > $(SRCDIR)/gnat.adc 
#	echo $(LIBGNAT_ADA_SRC)
#	echo "----->$(LIBGNARL_SRC)"

$(LIBGNAT): $(LIBGNAT_OBJS) $(LIBGNAT_ALIS)
	$(AR) -rcs $@ $(LIBGNAT_OBJS)
	$(RANLIB) $@
	$(RM) $(LIBGNAT_OBJS)

$(LIBGNARL): $(LIBGNARL_OBJS) $(LIBGNARL_ALIS)
	$(AR) -rcs $@ $(LIBGNARL_OBJS)
	$(RANLIB) $@ 
	$(RM) $(LIBGNARL_OBJS)

#s-traceb.ali ./boards/$(BOARD)/adalib/s-traceb.o: s-traceb.adb s-traceb.ads
#	$(CC) -c $(ADA_FLAGS) -fno-optimize-sibling-calls -O0 -o $@ $<

#s-tasdeb.ali ./boards/$(BOARD)/adalib/s-tasdeb.o: s-tasdeb.adb s-tasdeb.ads
#	$(CC) -c $(ADA_FLAGS) -g -O0 -o $@ $<

#a-except.ali ./boards/$(BOARD)/adalib/a-except.o: a-except.adb a-except.ads
#	$(CC) -c $(ADA_FLAGS) -g -fno-inline -O1 -o $@ $<

%.ali ./boards/$(BOARD)/adalib/%.o : %.adb %.ads
	$(CC) -c $(ADA_FLAGS) -O2 -o $@ $< 

%.ali ./boards/$(BOARD)/adalib/%.o : %.ads
	$(CC) -c $(ADA_FLAGS) -O2 -o $@ $< 

clean_o:
	$(RM) $(LIBGNAT_OBJS)
	$(RM) $(LIBGNARL_OBJS)

clean: clean_o
	$(RM) ./boards/$(BOARD)/adainclude/*
	$(RM) $(LIBGNAT_ALIS)
	$(RM) $(LIBGNARL_ALIS)
	$(RM) $(LIBGNAT)
	$(RM) $(LIBGNARL) 

.PHONY: printvars

printvars:
	@$(foreach V,$(sort $(.VARIABLES)), \
           $(if $(filter-out environment% default automatic, \
           $(origin $V)),$(warning $V=$($V) ($(value $V))))) 

