-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Protocol_Routing_Test is a test for the Protocol_Routing package.

--with GNAT.IO;
package body Protocol_Routing_Test is

   procedure Init is
   begin
      myInterface.Add_Interface(CANInterface'Access);
    --  GNAT.IO.Put_Line("Protocol_Routing_Test Initiated");
   end Init;

begin
   Init;
end Protocol_Routing_Test;
