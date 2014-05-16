
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Protocol_Routing_Test_Main is a test for the Protocol_Routing package.

pragma Profile (Ravenscar);


with Ada.Real_Time;
use Ada.Real_Time;

with GNAT.IO;
use GNAT.IO;

with VN.Message;
use VN.Message;

with VN.Message.Local_Hello;
with VN.Message.Assign_Address;
with VN.Message.Request_Address_Block;
with VN.Message.Factory;
with VN.Message.Distribute_Route;

with Protocol_Routing_Test;


procedure Protocol_Routing_Test_Main is

   type Address_Element is
      record
         address 	: VN.VN_Logical_Address;
         CUUID 		: VN.VN_CUUID;
         compType	: VN.Message.VN_Component_Type;
        -- exists 	: boolean := false;
      end record;
   type Address_Table is array(1..20) of Address_Element;

   myTable : Address_Table;
   numAddresses : Integer := 0;

   msg : VN.Message.VN_Message_Basic;
   msgAssign : VN.Message.Assign_Address.VN_Message_Assign_Address;
--     msgReq    : VN.Message.Request_Address_Block.VN_Message_Request_Address_Block;
   msgRoute  : VN.Message.Distribute_Route.VN_Message_Distribute_Route;

   sendStatus : VN.Send_Status;
   recStatus  : VN.Receive_Status;

   msgLocalHello : VN.Message.Local_Hello.VN_Message_Local_Hello;
   msgReqAddrBlock   : VN.Message.Request_Address_Block.VN_Message_Request_Address_Block;

   now : Ada.Real_Time.Time;
   myAddress : VN.VN_Logical_Address;

   use VN; -- for if Status = VN.OK then
begin

   GNAT.IO.New_Line(2);
   GNAT.IO.Put_Line("Hello world! Protocol_Routing_Test started! CUUID(1)= " & Protocol_Routing_Test.C1(1)'Img);

   VN.Message.Assign_Address.To_Assign_Address
     (VN.Message.Factory.Create(VN.Message.Type_Assign_Address), msgAssign);

   VN.Message.Request_Address_Block.To_Request_Address_Block
     (VN.Message.Factory.Create(VN.Message.Type_Request_Address_Block), msgReqAddrBlock);

   VN.Message.Distribute_Route.To_Distribute_Route
     (VN.Message.Factory.Create(VN.Message.Type_Distribute_Route), msgRoute);


   if Protocol_Routing_Test.Temp = 0 then
      myAddress := 100 + VN.VN_Logical_Address(Protocol_Routing_Test.C1(1));
   end if;

   GNAT.IO.Put_Line("Main function entering infinte loop.");
   loop
      now := Ada.Real_Time.Clock;
      delay until now + Ada.Real_Time.Milliseconds(500);
--        GNAT.IO.Put_Line("<Main function hearbeat>");

      Protocol_Routing_Test.myInterface.Receive(msg, recStatus);

      while recStatus = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or recStatus = VN.MSG_RECEIVED_MORE_AVAILABLE loop
      --   GNAT.IO.Put_Line("Main function (SM_L) received VN message, Opcode=" & msg.Header.Opcode'Img);

         if msg.Header.Opcode = VN.Message.OPCODE_LOCAL_HELLO then
            VN.Message.Local_Hello.To_Local_Hello(msg, msgLocalHello);
            VN.Text_IO.Put("Application received LocalHello, CUUID(1)= " & msgLocalHello.CUUID(1)'img & " component type = ");

            if msgLocalHello.Component_Type = VN.Message.CAS then
               VN.Text_IO.Put_Line("CAS");
            elsif msgLocalHello.Component_Type = VN.Message.SM_L then
               VN.Text_IO.Put_Line("SM_L");
            elsif  msgLocalHello.Component_Type = VN.Message.LS then
               VN.Text_IO.Put_Line("LS");
            elsif msgLocalHello.Component_Type = VN.Message.SM_Gateway then
               VN.Text_IO.Put_Line("SM_Gateway");
            elsif msgLocalHello.Component_Type = VN.Message.SM_x then
               VN.Text_IO.Put_Line("SM_Gateway");
            elsif msgLocalHello.Component_Type = VN.Message.Other then
               VN.Text_IO.Put_Line("Other");
            end if;

            if Protocol_Routing_Test.Temp = 0 then
--                 msg := VN.Message.Factory.Create(VN.Message.Type_Assign_Address);
--                 VN.Message.Assign_Address.To_Assign_Address(msg, msgAssign);

               msgAssign.Header.Destination := VN.LOGICAL_ADDRES_UNKNOWN;
               msgAssign.Header.Source := myAddress;
               msgAssign.CUUID := msgLocalHello.CUUID;
               msgAssign.Assigned_Address := 100 + VN.VN_Logical_Address(msgLocalHello.CUUID(1));

               VN.Message.Assign_Address.To_Basic(msgAssign, msg);
               Protocol_Routing_Test.myInterface.Send(msg, sendStatus);

               VN.Text_IO.Put_Line("SM_L assinged Logical address " & msgAssign.Assigned_Address'Img &
                                     " to CUUD(1)= " & msgAssign.CUUID(1)'Img);

               -- store the assigned address:
               numAddresses := numAddresses + 1;
               myTable(numAddresses).CUUID    := msgAssign.CUUID;
               myTable(numAddresses).compType := msgLocalHello.Component_Type;
               myTable(numAddresses).address  := msgAssign.Assigned_Address;
            end if;


         elsif msg.Header.Opcode = VN.Message.OPCODE_REQUEST_ADDR_BLOCK then

            VN.Message.Request_Address_Block.To_Request_Address_Block(msg, msgReqAddrBlock);

            VN.Text_IO.Put_Line("Request_Address_Block received, CUUID(1)= " & msgReqAddrBlock.CUUID(1)'img &
                          " Sender= " & msgReqAddrBlock.Header.Source'Img & " Sent to " & msgReqAddrBlock.Header.Destination'Img);
            VN.Text_IO.New_Line;


            if Protocol_Routing_Test.Temp = 0 then

               msgRoute.Header.Source := myAddress;
               msgRoute.Header.Destination := msg.Header.Source;

               if numAddresses > 0 then
                  for i in 1 .. numAddresses loop

                     msgRoute.CUUID := myTable(i).CUUID;
                     msgRoute.Component_Type    := myTable(i).compType;
                     msgRoute.Component_Address := myTable(i).address;

                     VN.Message.Distribute_Route.To_Basic(msgRoute, msg);
                     Protocol_Routing_Test.myInterface.Send(msg, sendStatus);

                     VN.Text_IO.Put_Line("Sending Route regarding log addr " & msgRoute.Component_Address'Img &
                                           " to " & msgRoute.Header.Destination'Img);
                  end loop;
               end if;
            end if;

         elsif msg.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR then
            VN.Text_IO.Put_Line("Assign address message received from logical address " & msg.Header.Source'Img);

            VN.Message.Assign_Address.To_Assign_Address(msg, msgAssign);
            myAddress := msgAssign.Assigned_Address;

            --Respond with a Request_Address_Block message, just for testing:
            msg := VN.Message.Factory.Create(VN.Message.Type_Request_Address_Block);
            VN.Message.Request_Address_Block.To_Request_Address_Block(msg, msgReqAddrBlock);

            msgReqAddrBlock.Header.Destination := msgAssign.Header.Source;
            msgReqAddrBlock.Header.Source := msgAssign.Assigned_Address;
            msgReqAddrBlock.CUUID := Protocol_Routing_Test.C1;

            VN.Message.Request_Address_Block.To_Basic(msgReqAddrBlock, msg);
            Protocol_Routing_Test.myInterface.Send(msg, sendStatus);

            VN.Text_IO.Put_Line("Responds (just for testing) with Request_Address_Block message");
         end if;

         Protocol_Routing_Test.myInterface.Receive(msg, recStatus);
      end loop;

   end loop;
end Protocol_Routing_Test_Main;
