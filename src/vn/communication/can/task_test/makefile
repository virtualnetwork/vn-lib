RTS_DIR		=	$(shell pwd)/../../../../../../../bap-arm-ravenscar-repository-rev271/rts/boards/lm3s811
#RTS_DIR		=	$(shell pwd)
#../../../../../../../rts/boards/lm3s811
#../../../../../../../bap-arm-ravenscar-repository-rev271/rts/boards/lm3s811

.PHONY:	all

all:
	@# Build crt0 (startup script for Texas Instrument Stellaris lm3s811) for QEMU
	arm-none-eabi-gcc -c -Wall -O2 -mcpu=cortex-m3 -mthumb $(RTS_DIR)/adainclude/startup-lm3s811.c -o obj/startup-lm3s811.o
	arm-none-eabi-gcc -c -Wall -O2 -mcpu=cortex-m3 -mthumb $(RTS_DIR)/adainclude/context_switch-bb.c -o obj/context_switch-bb-arm.o
	@# Run GNAT
	arm-none-eabi-gnatmake --RTS=$(RTS_DIR) -Ptask_test_qemu.gpr
	@# Convert to binary (not needed with QEMU)
	arm-none-eabi-objcopy -O binary task_test_main task_test_main.bin

qemu: all
	qemu-system-arm -M lm3s811evb -kernel task_test_main.bin -serial stdio

clean:
	-rm obj/*.o task_test_main task_test_main.bin
