-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Protocol_Routing_Test is a test for the Protocol_Routing package.

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

package body Protocol_Routing_Test is

   procedure Init is
   begin

      first_PO_Wrapper.Init;
      second_PO_Wrapper.Init;

      first_PO_Router.Add_Interface(first_PO_Wrapper'Access);
      second_PO_Router.Add_Interface(second_PO_Wrapper'Access);

      myInterface.Add_Interface(CANInterface'Access); -- ToDo: Commented out for testing

      myInterface.Add_Interface(first_PO_Router'Access);

      GNAT.IO.Put_Line("Protocol_Routing_Test Initiated");
   end Init;


   task body Second_Task_Type is
      use Ada.Real_Time;

      myPeriod : Ada.Real_Time.Time_Span;
      Next_Period : Ada.Real_Time.Time;

      msgLocalHello : VN.Message.Local_Hello.VN_Message_Local_Hello;
      msgLocalAck : VN.Message.Local_Ack.VN_Message_Local_Ack;

      msgAssignAddr     : VN.Message.Assign_Address.VN_Message_Assign_Address;
      msgReqAddrBlock   : VN.Message.Request_Address_Block.VN_Message_Request_Address_Block;

      msg : VN.Message.VN_Message_Basic;
      receiveStatus : VN.Receive_Status;
      sendStatus : VN.Send_Status;

   begin

      myPeriod := thePeriod.all;

      Next_Period := Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds(5000);
      delay until Next_Period;
      VN.Text_IO.Put_Line("Second_Task started");

      Next_Period := Ada.Real_Time.Clock;
      loop
         Next_Period := Next_Period + myPeriod;
         delay until Next_Period;

         myAccess.Receive(msg, receiveStatus);

         if receiveStatus = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or receiveStatus = VN.MSG_RECEIVED_MORE_AVAILABLE then

            if msg.Header.Opcode = VN.Message.OPCODE_LOCAL_HELLO then
               VN.Message.Local_Hello.To_Local_Hello(msg, msgLocalHello);
               VN.Text_IO.Put("LocalHello, type= " & " CUUID(1)= " & msgLocalHello.CUUID(1)'img &
                                     " component type = ");
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

               -- Simulate SM-CAN master assigning addresses or address blocks:

               msg := VN.Message.Factory.Create(VN.Message.Type_Assign_Address);
               VN.Message.Assign_Address.To_Assign_Address(msg, msgAssignAddr);

               msgAssignAddr.Header.Destination := VN.LOGICAL_ADDRES_UNKNOWN;
               msgAssignAddr.Header.Source := 10;
               msgAssignAddr.CUUID := msgLocalHello.CUUID;
               msgAssignAddr.Assigned_Address := 20;

               VN.Message.Assign_Address.To_Basic(msgAssignAddr, msg);
               myAccess.Send(msg, sendStatus);

               VN.Text_IO.Put_Line("Assinged Logical address");

            end if;

            if msg.Header.Opcode = VN.Message.OPCODE_LOCAL_ACK then
               VN.Message.Local_Ack.To_Local_Ack(msg, msgLocalAck);
               VN.Text_IO.Put_Line("LocalAck, type= " & " status = " & msgLocalAck.Status'img);
            end if;

            if msg.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR then
               VN.Message.Assign_Address.To_Assign_Address(msg, msgAssignAddr);

               if msgAssignAddr.CUUID = myCUUID.all then
                  VN.Text_IO.Put_Line("Second_Task was assigned logical address= " & msgAssignAddr.Assigned_Address'Img &
                                        " by SM with logical address = " & msgAssignAddr.Header.Source'img);

                  --Respond with a Request_Address_Block message, just for testing:
                  msg := VN.Message.Factory.Create(VN.Message.Type_Request_Address_Block);
                  VN.Message.Request_Address_Block.To_Request_Address_Block(msg, msgReqAddrBlock);
                  msgReqAddrBlock.Header.Destination := msgAssignAddr.Header.Source;
                  msgReqAddrBlock.Header.Source := msgAssignAddr.Assigned_Address;
                  msgReqAddrBlock.CUUID := myCUUID.all;

                  VN.Message.Request_Address_Block.To_Basic(msgReqAddrBlock, msg);

                  myAccess.Send(msg, sendStatus);

                  VN.Text_IO.Put_Line("Second_Task responds (just for testing) with Request_Address_Block message");
               end if;
            end if;

            VN.Text_IO.New_Line;
         end if;
      end loop;
   end Second_Task_Type;

begin
   Init;
end Protocol_Routing_Test;
