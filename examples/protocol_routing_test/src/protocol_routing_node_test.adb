-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Protocol_Routing_Test is a test for the Protocol_Routing package.

with GNAT.IO;

with VN.Message.Factory;

package body Protocol_Routing_Node_Test is

   procedure Init is
   begin

      PO_Wrapper_1_2.Init;
      PO_Wrapper_1_3.Init;
      PO_Wrapper_2_1.Init;
      PO_Wrapper_3_1.Init;

      first_PO_Router.Add_Interface(PO_Wrapper_1_2'Access);
      first_PO_Router.Add_Interface(PO_Wrapper_1_3'Access);

      second_PO_Router.Add_Interface(PO_Wrapper_2_1'Access);
      third_PO_Router.Add_Interface(PO_Wrapper_3_1'Access);

      myInterface.Add_Interface(CANInterface'Access);
      myInterface.Add_Interface(first_PO_Router'Access);

      GNAT.IO.Put_Line("Protocol_Routing_Test Initiated");
   end Init;

begin
   Init;
end Protocol_Routing_Node_Test;
