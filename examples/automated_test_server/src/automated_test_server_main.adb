
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Automated_Test_Server_Main is a test for the VN protocol.

with Interfaces.C;
with Ada.Real_Time;
use Ada.Real_Time;

with Ada.Text_IO;

with UartWrapper;

procedure Automated_Test_Server_Main is
  uartHandler0 : UartWrapper.pCUartHandler := UartWrapper.pxCreate("/dev/ttyUSB0", UartWrapper.B115200,
                                                                   Interfaces.C.int(0), 200, Interfaces.C.int(0));

   str : String(1..1000);
   bytesRead : Integer;

   now : Ada.Real_Time.Time;
begin

   loop

      uartHandler0.UartRead(str, bytesRead);
      Ada.Text_IO.Put_Line(str(1..bytesRead));

      now := Ada.Real_Time.Clock;
      delay until now + Ada.Real_Time.Milliseconds(100);
   end loop;

end Automated_Test_Server_Main;
