-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Automated_Test is a test for the VN protocol.

with GNAT.IO;

with VN;
use VN;

with VN.Message;
use VN.Message;


with VN.Message.Local_Ack;
with VN.Message.Local_Hello;
with VN.Message.Assign_Address;
with VN.Message.Request_Address_Block;

with VN.Message.Factory;

package body Automated_Test is

   procedure Init is
   begin

--        PO_Wrapper_1_2.Init;
--        PO_Wrapper_1_3.Init;
--        PO_Wrapper_2_1.Init;
--        PO_Wrapper_3_1.Init;
--
--        first_PO_Router.Add_Interface(PO_Wrapper_1_2'Access);
--        first_PO_Router.Add_Interface(PO_Wrapper_1_3'Access);
--
--        second_PO_Router.Add_Interface(PO_Wrapper_2_1'Access);
--        third_PO_Router.Add_Interface(PO_Wrapper_3_1'Access);

      myInterface.Add_Interface(CANInterface'Access);
--        myInterface.Add_Interface(first_PO_Router'Access);

      GNAT.IO.Put_Line("Automated_Test Initiated");
   end Init;

begin
   Init;
end Automated_Test;
