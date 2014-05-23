all: 
	gcc -c ../../src/firmware/UartWrapper/src/uartlib.c -o ../../src/firmware/UartWrapper/obj/uartlib.o
	gnatmake -c -P  ../../src/firmware/UartWrapper/uartwrapper.gpr
	gnatmake -c -P  automated_test_server.gpr
	gnatbind obj/automated_test_server_main.ali -I../../src/firmware/UartWrapper/obj/
	gnatlink obj/automated_test_server_main.ali ../../src/firmware/UartWrapper/obj/uartlib.o
	
