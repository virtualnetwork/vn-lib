
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Protocol_Routing_Second_Task implements a task that tests the Protocol_Routing package.

with VN.Message;
with VN.Message.Assign_Address;
with VN.Message.Factory;
with VN.Message.Local_Ack;
with VN.Message.Local_Hello;
with VN.Message.Request_Address_Block;

package body Protocol_Routing_Second_Task is

   task body Second_Task_Type is
      use Ada.Real_Time;
      use VN;
      use VN.Message;

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

      VN.Text_IO.Put_Line("Task " & myCUUID(1)'Img & " started");

      Next_Period := Ada.Real_Time.Clock;
      loop
         Next_Period := Next_Period + myPeriod;
         delay until Next_Period;

         myAccess.Receive(msg, receiveStatus);

         if receiveStatus = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or receiveStatus = VN.MSG_RECEIVED_MORE_AVAILABLE then

            if msg.Header.Opcode = VN.Message.OPCODE_LOCAL_ACK then
               VN.Message.Local_Ack.To_Local_Ack(msg, msgLocalAck);
               VN.Text_IO.Put_Line("Task " & myCUUID(1)'Img & ": LocalAck, type= " & " status = " & msgLocalAck.Status'img);
            end if;

            if msg.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR then
               VN.Message.Assign_Address.To_Assign_Address(msg, msgAssignAddr);

               if msgAssignAddr.CUUID = myCUUID.all then
                  VN.Text_IO.Put_Line("Task " & myCUUID(1)'Img & " was assigned logical address= " & msgAssignAddr.Assigned_Address'Img &
                                        " by SM with logical address = " & msgAssignAddr.Header.Source'img);

                  --Respond with a Request_Address_Block message, just for testing:
                  msg := VN.Message.Factory.Create(VN.Message.Type_Request_Address_Block);
                  VN.Message.Request_Address_Block.To_Request_Address_Block(msg, msgReqAddrBlock);
                  msgReqAddrBlock.Header.Destination := msgAssignAddr.Header.Source;
                  msgReqAddrBlock.Header.Source := msgAssignAddr.Assigned_Address;
                  msgReqAddrBlock.CUUID := myCUUID.all;

                  VN.Message.Request_Address_Block.To_Basic(msgReqAddrBlock, msg);

                  myAccess.Send(msg, sendStatus);

                  VN.Text_IO.Put_Line("Task " & myCUUID(1)'Img & "  responds (just for testing) with Request_Address_Block message");
               end if;
            end if;

            VN.Text_IO.New_Line;
         end if;
      end loop;
   end Second_Task_Type;


end Protocol_Routing_Second_Task;
