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

-- Summary: SM_Duty is an object that holds send and receive buffers
-- for VN messages that are to be sent, or have been received, over the CAN network.
-- SM_Duty holds instances of the classes that implement the state machines
-- of the VN-CAN protcol.
-- Please also note that this functionality is regarding a Subnet Manager for CAN (SM-CAN),
-- not an ordinary node.

pragma Profile (Ravenscar);

with VN.Message;
use VN.Message;

with VN.Message.Factory;
with VN.Message.Local_Hello;
with VN.Message.Local_Ack;
with VN.Message.Distribute_Route;
with VN.Message.Assign_Address;
with VN.Message.Assign_Address_Block;

with VN.Communication.CAN.Logic.Message_Utils;

package body VN.Communication.CAN.Logic.SM is

   procedure Update(this : in out SM_Duty; msgsBuffer : in out CAN_Message_Buffers.Buffer; ret : out CAN_Message_Buffers.Buffer) is

      use VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation; -- needed for =-sign for SM_CAN_Mode

      bWillSend : boolean;
      msgIn, msgOut : CAN_Message_Logical;

      logAddress : VN.VN_Logical_Address;
      CANAddress : VN.Communication.CAN.CAN_Address_Sender;

   begin
      if not this.isInitialized then
         Init(this);
      end if;

      if CAN_Message_Buffers.Extent(msgsBuffer) = 0 then
         for i in this.DutyArray'Range loop
            this.DutyArray(i).Update(msgIn, false, msgOut, bWillSend);

            if bWillSend then
               CAN_Message_Buffers.Insert(msgOut, ret);
            end if;
         end loop;

      else
         while CAN_Message_Buffers.Extent(msgsBuffer) /= 0 loop

            CAN_Message_Buffers.Remove(msgIn, msgsBuffer);

            if msgIn.isNormal and then msgIn.msgType = VN.Communication.CAN.Logic.ADDRESS_ANSWER then
               -- Receive routing information from AddressAnswer message

               VN.Communication.CAN.Logic.Message_Utils.AddressAnswerFromMessage(msgIn, CANAddress, logAddress);
               
               CAN_Routing.Insert(this.myTable, logAddress, CANAddress, true);

               VN.Communication.CAN.Logic.DebugOutput(this.myUCID'Img & ": AddresAnswer: Logical address " & logAddress'Img & 
                                                        " exists on CAN address " & CANAddress'Img, 1);
               
            else
               for i in this.DutyArray'Range loop

                  this.DutyArray(i).Update(msgIn, true, msgOut, bWillSend);

                  if bWillSend then
--                       if msgOut.isNormal and then msgOut.msgType = VN.Communication.CAN.Logic.TRANSMISSION then
--                          VN.Communication.CAN.Logic.DebugOutput("TRANSMISSION msg sent from " &
--                                                                   msgOut.Sender'Img & " to " & msgOut.Receiver'Img, 4);
--                       end if;
                     CAN_Message_Buffers.Insert(msgOut, ret);
                  end if;
               end loop;
            end if;
         end loop;
      end if;

      if not this.hasRole then
         if this.masterNegotiation.CurrentMode = VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation.MASTER then

            this.hasCANAddress := true;
            this.hasRole := true;

            this.assigner.Activate(this.myUCID);
            this.sender.Activate(0);
            this.receiver.Activate(0);
            this.logAddrHandler.Activate(0);
            this.componentTypeResponder.Activate(this.myCUUID, 0, true);
            this.componentTypeHandler.Activate(this.myCUUID, 0, this.sender'Unchecked_Access);

            --Change CAN message filters, SM_CAN_MasterNegotioation longer wishes to receceive
            -- normal CAN messages, only RequestCANAddress messages:
            this.theFilter.Change_Filter(this.negotioationFilterID, VN.Communication.CAN.CAN_message_ID(2 ** 28),
                                         VN.Communication.CAN.CAN_message_ID(2 ** 28));

            --Create filter to filter out messages addressed to the SM_CAN master's CAN address (0):
            this.theFilter.Create_Transmission_Filter(this.transmissionFilterID, 0);

         elsif this.masterNegotiation.CurrentMode = VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation.SLAVE then
            this.addressReceiver.Activate;
            this.hasRole := true;
         end if;
      end if;

      if not this.hasCANAddress then
         declare
            isAssigned : boolean;
            address    : VN.Communication.CAN.CAN_Address_Sender;
         begin
            this.addressReceiver.Address(address, isAssigned);
            if isAssigned then
               this.hasCANAddress := true;

               this.sender.Activate(address);
               this.receiver.Activate(address);
               this.logAddrHandler.Activate(address);
               this.componentTypeResponder.Activate(this.myCUUID, address, true);
               this.componentTypeHandler.Activate(this.myCUUID, address, this.sender'Unchecked_Access);

               --Change CAN message filters, SM_CAN_MasterNegotioation does no longer wishe
               -- to receceive any CAN messages:
               this.theFilter.Remove_Filter(this.negotioationFilterID);

               --Create filter to filter out messages addressed to the assigned CAN address:
               this.theFilter.Create_Transmission_Filter(this.transmissionFilterID,
                                                         VN.Communication.CAN.Convert(address));

               --FOR TESTING:
               --              if not this.hasSent then
               --                 VN.Communication.CAN.Logic.DebugOutput("Starting to send VN message", 3);
               --                 this.hasSent := true;
               --
               --
               --                 declare
               --                    tempStr : String := "Hello world its working, sent from UCID " & this.myUCID'Img & "                        ";
               --                    d : VN.Communication.CAN.Logic.DataArray := VN.Communication.CAN.Logic.DataArray(tempStr(VN.Communication.CAN.Logic.DataArray'Range));
               --                    msg : VN.Communication.CAN.Logic.VN_Message_Internal := (d, 50, 0, address);
               --                 begin
               --                    this.sender.SendVNMessage(msg);
               --                 end;
               --              end if;
            end if;
         end;
      end if;

      --For testing:
--          declare
--             msg : VN.Communication.CAN.Logic.VN_Message_Internal;
--             msgWasReceived : boolean;
--          begin
--             this.receiver.ReceiveVNMessage(msg, msgWasReceived);
--             if msgWasReceived then
--                VN.Communication.CAN.Logic.DebugOutput("Received VN message:", 4);
--                VN.Communication.CAN.Logic.DebugOutput(String(msg.Data), 4);
--             end if;
--          end;
   end Update;


   procedure Send(this : in out SM_Duty; msg : VN.Message.VN_Message_Basic;
                  result : out VN.Send_Status) is
      internal : VN.Communication.CAN.Logic.VN_Message_Internal;
      receiver : VN.Communication.CAN.CAN_Address_Sender;
      found : boolean;

      msgAssignAddr 	 : VN.Message.Assign_Address.VN_Message_Assign_Address;
      msgAssignAddrBlock : VN.Message.Assign_Address_Block.VN_Message_Assign_Address_Block;

      isDirect : aliased Boolean;
   begin
      if not this.isInitialized then
         Init(this);
      end if;

      --The SPA protocol says that messages addressed to logical address 0 shall be thrown away
      if msg.Header.Destination = 0 then
         result := OK;
         return;
      end if;

      -- ASSIGN_ADDR and ASSIGN_ADDR_BLOCK are routed on their receiver's
      -- CUUID since the receiver does not have a logical address yet
      if msg.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR then
         VN.Message.Assign_Address.To_Assign_Address(msg, msgAssignAddr);
         CUUID_CAN_Routing.Search(this.myCUUIDTable, msgAssignAddr.CUUID, receiver, found);
         
--           VN.Text_IO.Put_Line("CAN routing: OPCODE_ASSIGN_ADDR, CUUID(1)= " & msgAssignAddr.CUUID(1)'Img & 
--                               ", address found = " & found'Img);

         -- Since we assign an logical address, we know that this logical address exists on this subnet
         -- (We know that the receiver exists on that subnet because of the CUUID routing)
         CAN_Routing.Insert(this.myTable, msgAssignAddr.Assigned_Address, receiver); --new

      elsif msg.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR_BLOCK  and 
        msg.Header.Destination = VN.LOGICAL_ADDRES_UNKNOWN then --new

         VN.Message.Assign_Address_Block.To_Assign_Address_Block(msg, msgAssignAddrBlock);
         CUUID_CAN_Routing.Search(this.myCUUIDTable, msgAssignAddrBlock.CUUID, receiver, found);

         -- Since we assign an logical address, we know that this logical address exists on this subnet
         -- (We know that the receiver exists on that subnet because of the CUUID routing)
         CAN_Routing.Insert(this.myTable, msgAssignAddrBlock.Assigned_Base_Address, receiver); --new
      else

         CAN_Routing.Search(this.myTable, msg.Header.Destination, receiver, found, isDirect'Access);

         if isDirect then
            VN.Communication.CAN.Logic.DebugOutput("CAN routing: Sending VN message via direct routing. Destination " & msg.Header.Destination'Img &
                                                     " CAN address = " & receiver'Img, 1);
         end if;
      end if;
      
--        if msg.Header.Opcode = 16#73# then
--           VN.Text_IO.Put_Line("Sent REQUEST_LS_PROBE from " & msg.Header.Source'img &
--                                 " sent to " & msg.Header.Destination'Img &
--                                 " rerouted to CAN address " & receiver'Img);
--  
--        elsif msg.Header.Opcode = 16#78# then
--           VN.Text_IO.Put_Line("Sent PROBE_REQUEST from " & msg.Header.Source'img &
--                                 " sent to " & msg.Header.Destination'Img &
--                                 " rerouted to CAN address " & receiver'Img);
--        end if;

      if found then
         internal.Receiver := VN.Communication.CAN.Convert(receiver);
         internal.Data := msg;

         internal.NumBytes := Interfaces.Unsigned_16(Integer(msg.Header.Payload_Length) +
                                                       VN.Message.HEADER_SIZE +
                                                         VN.Message.CHECKSUM_SIZE);

         this.sender.SendVNMessage(internal, result);

         if result /= VN.OK then
            VN.Text_IO.Put_Line("CAN routing: Send-result /= VN.OK ");
         end if;

         this.logAddrHandler.Sent_From_Address(msg.Header.Source); 
      else
         result := ERROR_NO_ADDRESS_RECEIVED;
         VN.Communication.CAN.Logic.DebugOutput("VN.Communication.CAN.Logic.SM.Send, Status := ERROR_NO_ADDRESS_RECEIVED;", 0);
      end if;
   end Send;

   procedure Receive(this : in out SM_Duty; msg : out VN.Message.VN_Message_Basic; 
                     status : out VN.Receive_Status) is

      procedure Local_Ack_Response(internalMsg : VN.Communication.CAN.Logic.VN_Message_Internal) is
         msgLocalAck   	: VN.Message.Local_Ack.VN_Message_Local_Ack;
         msgBasic      	: VN.Message.VN_Message_Basic  := VN.Message.Factory.Create(VN.Message.Type_Local_Ack);
         ackMessage    	: VN.Communication.CAN.Logic.VN_Message_Internal;
         result        	: VN.Send_Status;
      begin

         VN.Message.Local_Ack.To_Local_Ack(msgBasic, msgLocalAck);
         msgLocalAck.Status := VN.Message.ACK_OK;

         msgLocalAck.Header.Destination := VN.LOGICAL_ADDRES_UNKNOWN;
         msgLocalAck.Header.Source 	:= VN.LOGICAL_ADDRES_UNKNOWN;

         VN.Message.Local_Ack.To_Basic(msgLocalAck, ackMessage.Data);
         ackMessage.NumBytes := Interfaces.Unsigned_16(Integer(msgLocalAck.Header.Payload_Length) +
                                                         VN.Message.CHECKSUM_SIZE +
                                                         VN.Message.HEADER_SIZE);

         ackMessage.Receiver := VN.Communication.CAN.Convert(internalMsg.Sender);

         this.sender.SendVNMessage(ackMessage, result);
      end Local_Ack_Response;


      procedure Handle_Distribute_Route(internalMsg : VN.Communication.CAN.Logic.VN_Message_Internal) is
         msgDistribute : VN.Message.Distribute_Route.VN_Message_Distribute_Route;
      begin
         VN.Message.Distribute_Route.To_Distribute_Route(internalMsg.Data, msgDistribute);
         CAN_Routing.Insert(this.myTable, msgDistribute.Component_Address, internalMsg.Sender);
         VN.Communication.CAN.Logic.DebugOutput("CAN address " & internalMsg.Receiver'Img &
                                                  " received Distribute Route message regarding logical address " &
                                                  msgDistribute.Component_Address'Img &
                                                  " from CAN address " &
                                                  internalMsg.Sender'Img, 2);
      end Handle_Distribute_Route;

      internal 	    : VN.Communication.CAN.Logic.VN_Message_Internal;
      msgLocalHello : VN.Message.Local_Hello.VN_Message_Local_Hello;
   begin

      if not this.isInitialized then
         Init(this);
      end if;

      this.receiver.ReceiveVNMessage(internal, status);

      if status = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or --TODO, this will need to be updated if more options for VN.Receive_Status are added
        status = VN.MSG_RECEIVED_MORE_AVAILABLE then

         msg := internal.Data;

--           if msg.Header.Opcode = 16#73# then
--              VN.Text_IO.Put_Line("Received REQUEST_LS_PROBE from " & msg.Header.Source'img &
--                                    " sent to " & msg.Header.Destination'Img &
--                                    " from CAN address " & internal.Sender'Img);
--           elsif msg.Header.Opcode = 16#78# then
--              VN.Text_IO.Put_Line("Received PROBE_REQUEST from " & msg.Header.Source'img &
--                                    " sent to " & msg.Header.Destination'Img &
--                                    " from CAN address " & internal.Sender'Img); 
--           end if;

         -- ToDo: Add more special cases
         -- Some special cases:
         if msg.Header.Opcode = VN.Message.OPCODE_LOCAL_HELLO then

            VN.Message.Local_Hello.To_Local_Hello(msg, msgLocalHello);
            CUUID_CAN_Routing.Insert(this.myCUUIDTable, msgLocalHello.CUUID, internal.Sender);

            Local_Ack_Response(internal); -- Respond with a LocalAck

            VN.Communication.CAN.Logic.DebugOutput("CAN address " & internal.Receiver'Img &
                                                     " received LocalHello from CAN address " &
                                                     internal.Sender'Img & " CUUID(1)= " &
                                                     msgLocalHello.CUUID(1)'Img &
                                                     " responded with LocalAck", 1);

         elsif msg.Header.Opcode = VN.Message.OPCODE_LOCAL_ACK then
            -- ToDo: We should remember that our Local_Hello was acknowledged

            VN.Communication.CAN.Logic.DebugOutput("CAN address " & internal.Receiver'Img &
                                                     " received LocalAck from CAN address " &
                                                     internal.Sender'Img, 2);

         else -- messages that are received from units that have a logical address:

            --Store information about the sender of the message:
            CAN_Routing.Insert(this.myTable, internal.Data.Header.Source, internal.Sender);

            this.logAddrHandler.Received_From_Address(msg.Header.Source);

            if msg.Header.Opcode = VN.Message.OPCODE_DISTRIBUTE_ROUTE then
               Handle_Distribute_Route(internal);
            end if;
         end if;
      end if;
   end Receive;

   procedure GetCANAddress(this : in out SM_Duty; address : out CAN_Address_Sender;
                           isAssigned : out boolean) is
      use VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation;
   begin
      if not this.isInitialized then
         Init(this);
      end if;

      if this.masterNegotiation.CurrentMode = VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation.MASTER then
         address := 0;
         isAssigned := true;
      else
         this.addressReceiver.Address(address, isAssigned);
      end if;
   end GetCANAddress;

   procedure Init(this : in out SM_Duty) is
      testCUUID : VN.VN_CUUID := (others => 42); --ToDo: For testing only!!!!
   begin
      VN.Communication.CAN.Logic.DebugOutput("SM_Duty initialized", 4);

      this.isInitialized := true;

      this.DutyArray(this.DutyArray'First) := this.masterNegotiation'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 1) := this.addressReceiver'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 2) := this.assigner'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 3) := this.sender'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 4) := this.receiver'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 5) := this.componentTypeResponder'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 6) := this.componentTypeHandler'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 7) := this.logAddrHandler'Unchecked_Access;

      -- Set CAN filters:
      this.theFilter.Create_Filter(this.negotioationFilterID, 0, 0); --will listen to all CAN messages, for now

      -- receiving messages sent to CAN address 255 (broadcast)
      this.theFilter.Create_Transmission_Filter(this.broadcastFilterID, 255);

      -- receiving messages sent to CAN address 254 (selective broadcast)
      this.theFilter.Create_Transmission_Filter(this.selectiveBroadcastFilterID, 254);

      --ToDo: For testing only!!!!
--        CAN_Routing.Insert(this.myTable, 1337, 42);
--        CUUID_CAN_Routing.Insert(this.myCUUIDTable, testCUUID, 42);
   end Init;

end VN.Communication.CAN.Logic.SM;
