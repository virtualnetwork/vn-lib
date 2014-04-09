all:
	gcc microsemi-drivers/drivers/mss_can/mss_can.c -c -Imicrosemi-drivers/armgcc-extra-includes -o obj/mss_can.o
	gcc microsemi-drivers/main.c -c -Imicrosemi-drivers/armgcc-extra-includes -o obj/main.o
	gnatmake -Pcan_driver.gpr #-largs obj/main.o obj/mss_can.o
	#gnatmake -c src/can_driver_main.adb -o obj/can_driver_main.o -D obj/
	#gnatbind obj/can_driver_main.ali
	#gnatlink obj/can_driver_main.ali obj/main.o obj/mss_can.o
clean:
	rm obj/main -f
	#rm obj/mss_can.o -f
	#rm obj/main.o -f
	rm obj/can_driver_main -f
	rm obj/*.o
	rm obj/*.ali
	rm can_driver_main -f
