------------------------------------------------------------------------------
--  This file is part of VN-Lib.
--
--  VN-Lib is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  VN-Lib is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with VN-Lib.  If not, see <http://www.gnu.org/licenses/>.
--
--  Copyright 2014, Nils Brynedal Ignell (nils.brynedal@gmail.com)
------------------------------------------------------------------------------

-- Summary:
-- Protocol_Routing_Second_Task implements a task that tests the Protocol_Routing package.

with VN.Message;
with VN.Message.Assign_Address;
with VN.Message.Factory;
with VN.Message.Local_Ack;
with VN.Message.Local_Hello;
with VN.Message.Request_Address_Block;
with VN.Message.Distribute_Route;

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
      msgRoute  : VN.Message.Distribute_Route.VN_Message_Distribute_Route;

      msg : VN.Message.VN_Message_Basic;
      receiveStatus : VN.Receive_Status;
      sendStatus : VN.Send_Status;

      myAddress : VN.VN_Logical_Address;

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


            elsif msg.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR then
               VN.Message.Assign_Address.To_Assign_Address(msg, msgAssignAddr);

               if msgAssignAddr.CUUID = myCUUID.all then
                  VN.Text_IO.Put_Line("Task " & myCUUID(1)'Img & " was assigned logical address= " & msgAssignAddr.Assigned_Address'Img &
                                        " by SM with logical address = " & msgAssignAddr.Header.Source'img);

                  myAddress := msgAssignAddr.Assigned_Address;

                  --Respond with a Request_Address_Block message, just for testing:
                  msg := VN.Message.Factory.Create(VN.Message.Type_Request_Address_Block);
                  VN.Message.Request_Address_Block.To_Request_Address_Block(msg, msgReqAddrBlock);
                  msgReqAddrBlock.Header.Destination := msgAssignAddr.Header.Source;
                  msgReqAddrBlock.Header.Source := msgAssignAddr.Assigned_Address;
                  msgReqAddrBlock.CUUID := myCUUID.all;

                  VN.Message.Request_Address_Block.To_Basic(msgReqAddrBlock, msg);

                  VN.Text_IO.Put_Line("Task " & myCUUID(1)'Img & "  responds (just for testing) with Request_Address_Block message");
                  myAccess.Send(msg, sendStatus);
               end if;

               VN.Text_IO.New_Line;

            elsif msg.Header.Opcode = VN.Message.OPCODE_REQUEST_ADDR_BLOCK then

               VN.Message.Request_Address_Block.To_Request_Address_Block(msg, msgReqAddrBlock);

               VN.Text_IO.Put_Line("Task " & myCUUID(1)'Img & " received Request_Address_Block message, CUUID(1)= " &
                                     msgReqAddrBlock.CUUID(1)'img &
                                     " Sender= " & msgReqAddrBlock.Header.Source'Img & " sent to " &
                                     msgReqAddrBlock.Header.Destination'Img);

               VN.Text_IO.New_Line;

            elsif msg.Header.Opcode = VN.Message.OPCODE_DISTRIBUTE_ROUTE then
               VN.Message.Distribute_Route.To_Distribute_Route(msg, msgRoute);

               VN.Text_IO.Put_Line("Task " & myCUUID(1)'Img & " received Distribute_Route message, CUUID(1)= " & msgRoute.CUUID(1)'img &
                                     " logical address = " & msgRoute.Component_Address'Img &
                                     " Sender= " & msgRoute.Header.Source'Img & " sent to " & msgRoute.Header.Destination'Img);

               --Respond with a Request_Address_Block message, just for testing:
               msg := VN.Message.Factory.Create(VN.Message.Type_Request_Address_Block);
               VN.Message.Request_Address_Block.To_Request_Address_Block(msg, msgReqAddrBlock);

               msgReqAddrBlock.Header.Destination := msgRoute.Component_Address;
               msgReqAddrBlock.Header.Source := myAddress;
               msgReqAddrBlock.CUUID := myCUUID.all;

               VN.Message.Request_Address_Block.To_Basic(msgReqAddrBlock, msg);

               VN.Text_IO.Put_Line("Responds (just for testing) with Request_Address_Block message");
               myAccess.Send(msg, sendStatus);

            end if;
         end if;
      end loop;
   end Second_Task_Type;
end Protocol_Routing_Second_Task;
