########################################
# Binary name
NAME		?=	protocol_routing_test_main

########################################
# Ada project file
ADAPRJ		?=	protocol_routing_test_sf2.gpr

########################################
# Path to support folder
SPTPATH		=	$(shell pwd)/../../../../bap-arm-ravenscar-repository-rev280/support
# Path to Microsemi drivers
DRVPATH		=	$(SPTPATH)/microsemi-drivers
# Path to Ada RTS
RTS_DIR		=	$(shell pwd)/../../../../bap-arm-ravenscar-repository-rev280/rts/boards/sf2-starter-kit-010

########################################
# Link command file, set defaults
LDFILE		?=	$(SPTPATH)/link-dbg-in-microsemi-sf2-esram.ld
# Programming target address (in SF2, make sure to use correct link file)
PRGADR		?=	0x20000000

PRGTTY		?=	/dev/ttyUSB0

########################################
# Driver sources
CSRC		+=	$(DRVPATH)/drivers/mss_uart/mss_uart.c
CSRC		+=	$(DRVPATH)/drivers/mss_can/mss_can.c
CSRC		+=	../../src/firmware/c_drivers/can_driver/main.c
#CSRC		+=	$(DRVPATH)/drivers/mss_usb/mss_usb_device.c
#CSRC		+=	$(DRVPATH)/drivers/mss_ethernet_mac/mss_ethernet_mac.c
CSRC		+=	$(SPTPATH)/bap-drivers/uart/uart.c
# Driver paths
INCPATH		+=	$(SPTPATH)/bap-drivers/

########################################
# Startup & support routines
ASRC		+=	$(DRVPATH)/CMSIS/startup_gcc/startup_m2sxxx.s
CSRC		+=	$(DRVPATH)/CMSIS/startup_gcc/newlib_stubs.c
CSRC		+=	$(DRVPATH)/CMSIS/system_m2sxxx.c
CSRC		+=	$(RTS_DIR)/adainclude/context_switch-bb.c


# Include paths
# (Create include paths from source paths, add manually when not feasible)
INCPATH		+=	$(dir $(CSRC))
INCPATH		+=	$(DRVPATH)/armgcc-extra-includes
INCPATH		+=	$(DRVPATH)/drivers_config/sys_config

CC		=	arm-none-eabi-gcc
OBJCPY		= 	arm-none-eabi-objcopy
AR		= 	arm-none-eabi-ar
CFLAGS		=	-c -Wall -O2 -mcpu=cortex-m3 -mthumb
ADAMAKE		=	arm-none-eabi-gnatmake
ADAFLAGS	= 	--RTS=$(RTS_DIR) -P$(ADAPRJ) -XLDFILE=$(LDFILE)

# Add "-I" to all includes
INCLUDES	=	$(addprefix -I, $(INCPATH))

# Make sure objects (.o) will be placed in obj/
OBJECTS		+=	$(patsubst %.c, obj/%.o, $(notdir $(CSRC)))
OBJECTS		+=	$(patsubst %.s, obj/%.o, $(notdir $(ASRC)))

# A little cheating to get proper paths to sources
VPATH 		= 	$(dir $(ASRC))
VPATH 		+= 	$(dir $(CSRC))

########################################
# Must explicitly define target address
EXIT 		= 	0
PRGPAR 		=	-p $(PRGTTY)
ifeq "$(TARGET_ADR)" "ESRAM"
	LDFILE	=	$(SPTPATH)/link-dbg-in-microsemi-sf2-esram.ld
	PRGADR	= 	0x20000000
# If adr == 0xa0000000 => DDR mirrored to 0x0
else ifeq "$(TARGET_ADR)" "EXTRAM"
	LDFILE	=	$(SPTPATH)/link-microsemi-sf2-extram.ld
	PRGADR	= 	0xA0000000
# Id adr == 0x60000000 => ENVM (FlashRam) mirrored to 0x0
else ifeq "$(TARGET_ADR)" "ENVM"
	LDFILE	=	$(SPTPATH)/link-microsemi-sf2-envm.ld
	PRGADR	= 	0x60000000
	PRGPAR 	+= 	-w 128
else
	EXIT = 1
endif

.PHONY:	all exitck clean

ifeq "$(EXIT)" "0"
all: $(OBJECTS)
	@echo
	@echo
	@echo "*** Creating library file obj/lib$(NAME)_c.a"
	@echo
	$(AR) rcs obj/lib$(NAME)_c.a $^
	@echo
	@echo
	@echo "*** Running gnatmake ***"
	@echo
	$(ADAMAKE) $(ADAFLAGS)
	@echo
	@echo
	@echo "*** Converting $(NAME) to binary $(NAME).bin"
	@echo
	$(OBJCPY) -O binary obj/$(NAME) $(NAME).bin
else
all:
	$(error Please specify TARGET_ADR=<adr> where adr is ESRAM, EXTRAM or ENVM)
endif

obj/%.o: %.c
	@echo " Compiling $< "
	@$(CC) $(CFLAGS) $(INCLUDES) $< -o $@

obj/%.o: %.s
	@echo " Compiling $< "
	@$(CC) $(CFLAGS) $(INCLUDES) $< -o $@

upload: all
	@echo
	@echo
	@echo "*** Programing target device"
	@echo "*** Uploading to $(TARGET_ADR)"
	@echo
	$(SPTPATH)/serial_programmer -f $(NAME).bin -a $(PRGADR) $(PRGPAR)

clean:
	@echo
	@echo
	@echo "*** Clean project"
	@echo	
	@-rm obj/*.o obj/*.a obj/*.ali $(NAME) $(NAME).bin


