
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Automated_Test_Server_Main is a test for the VN protocol.

with Interfaces.C;
with Ada.Real_Time;
use Ada.Real_Time;

with Ada.Text_IO;

with Buffers;
with UartWrapper;

procedure Automated_Test_Server_Main is
   uartHandler0 : UartWrapper.pCUartHandler := UartWrapper.pxCreate("/dev/ttyUSB0", UartWrapper.B115200,
                                                                    Interfaces.C.int(0), 200, Interfaces.C.int(0));

   package charBuffers is new Buffers(Character);

   buffer : charBuffers.Buffer(200);

   str : String(1..1000);
   bytesRead : Integer;

   now : Ada.Real_Time.Time;

   isCommand : Boolean := false;

   package Read is
   begin
      for i in str'Range loop
         if str(i) = '*' then
            isCommand := true;
         elsif str(i) = '^' then
            isCommand := false;
         else
            if isCommand then
               charBuffers.Insert(str(i), buffer);
            end if;
         end if;
      end loop;
   end Read;

begin

   loop

      uartHandler0.UartRead(str, bytesRead);
--        Ada.Text_IO.Put_Line(str(1..bytesRead));

      Read;



      now := Ada.Real_Time.Clock;
      delay until now + Ada.Real_Time.Milliseconds(100);
   end loop;

end Automated_Test_Server_Main;
