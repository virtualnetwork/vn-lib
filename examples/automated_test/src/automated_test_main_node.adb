
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Automated_Test is a test for the VN protocol.


pragma Profile (Ravenscar);

with GNAT.IO;

with VN;
use VN;

with Ada.Real_Time;
use Ada.Real_Time;

with VN.Message;
use VN.Message;

with VN.Message.Local_Hello;
with VN.Message.Assign_Address;
with VN.Message.Request_Address_Block;
with VN.Message.Factory;


with Automated_Test_Node;

--  with System.BB.Interrupts; -- Remove when compiling for PC, keep when compiling for SmartFusion2

procedure Automated_Test_Main_Node is
   msg	     : VN.Message.VN_Message_Basic;
   msgAssign : VN.Message.Assign_Address.VN_Message_Assign_Address;
--     msgReq    : VN.Message.Request_Address_Block.VN_Message_Request_Address_Block;

   sendStatus    : VN.Send_Status;
   recStatus : VN.Receive_Status;

   msgLocalHello : VN.Message.Local_Hello.VN_Message_Local_Hello;
 --  msgReqAddrBlock   : VN.Message.Request_Address_Block.VN_Message_Request_Address_Block;

   now : Ada.Real_Time.Time;

   use VN; -- for if Status = VN.OK then

   hasLogicalAddress : boolean := false;
   myAddress : VN.VN_Logical_Address;

begin

   GNAT.IO.Put_Line("Automated_Test_Node started, CUUID(1)=" & Automated_Test_Node.C1(1)'Img);

   loop
      now := Ada.Real_Time.Clock;
      delay until now + Ada.Real_Time.Milliseconds(500);
      GNAT.IO.Put_Line("<Main function hearbeat>");

      Automated_Test_Node.myInterface.Receive(msg, recStatus);

      while recStatus = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or recStatus = VN.MSG_RECEIVED_MORE_AVAILABLE loop

         if msg.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR then
            VN.Message.Assign_Address.To_Assign_Address(msg, msgAssign);

            if msgAssign.CUUID = Automated_Test_Node.C1 then
               VN.Text_IO.Put_Line("*WasAssinged," & msgAssign.Assigned_Address'Img & "," & Automated_Test_Node.C1(1)'Img & "^");
               hasLogicalAddress := true;
               myAddress := msgAssign.Assigned_Address;
            end if;
         end if;

         Automated_Test_Node.myInterface.Receive(msg, recStatus);
      end loop;

   end loop;
end Automated_Test_Main_Node;
