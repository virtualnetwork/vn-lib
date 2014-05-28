-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary: Node_Duty is an object that holds send and receive buffers
-- for VN messages that are to be sent, or have been received, over the CAN network.
-- Node_Duty holds instances of the classes that implement the state machines
-- of the VN-CAN protcol.
-- Please also note that this functionality is regarding 
-- an ordinary node not a Subnet Manager for CAN (SM-CAN),

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

package body VN.Communication.CAN.Logic.Node is

   procedure Update(this : in out Node_Duty; msgsBuffer : in out CAN_Message_Buffers.Buffer; ret : out CAN_Message_Buffers.Buffer) is

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
                     CAN_Message_Buffers.Insert(msgOut, ret);
                  end if;
               end loop;
            end if;
         end loop;
      end if;

      if not this.hasRole then
         this.addressReceiver.Activate;
         this.hasRole := true;
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
               this.componentTypeResponder.Activate(this.myCUUID, address, false);
               this.componentTypeHandler.Activate(this.myCUUID, address, this.sender'Unchecked_Access);


               --Create filter to filter out messages addressed to the assigned CAN address:
               this.theFilter.Create_Transmission_Filter(this.transmissionFilterID,
                                                         VN.Communication.CAN.Convert(address));
            end if;
         end;
      end if;
   end Update;


   procedure Send(this : in out Node_Duty; msg : VN.Message.VN_Message_Basic;
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

      elsif msg.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR_BLOCK then

         VN.Message.Assign_Address_Block.To_Assign_Address_Block(msg, msgAssignAddrBlock);
         CUUID_CAN_Routing.Search(this.myCUUIDTable, msgAssignAddrBlock.CUUID, receiver, found);
      else

         CAN_Routing.Search(this.myTable, msg.Header.Destination, receiver, found, isDirect'Access);

         if isDirect then
            VN.Communication.CAN.Logic.DebugOutput("CAN routing: Sending VN message via direct routing. Destination " & msg.Header.Destination'Img &
                                                     " CAN address = " & receiver'Img, 1);
         end if;
      end if;

      if found then
         internal.Receiver := VN.Communication.CAN.Convert(receiver);
         internal.Data := msg;

         internal.NumBytes := Interfaces.Unsigned_16(Integer(msg.Header.Payload_Length) +
                                                       VN.Message.HEADER_SIZE +
                                                         VN.Message.CHECKSUM_SIZE);
         this.sender.SendVNMessage(internal, result);

         this.logAddrHandler.Sent_From_Address(msg.Header.Source);

         result := OK;
      else
         result := ERROR_NO_ADDRESS_RECEIVED;
         VN.Communication.CAN.Logic.DebugOutput("VN.Communication.CAN.Logic.Node.Send, Status := ERROR_NO_ADDRESS_RECEIVED;", 5);
      end if;
   end Send;

   procedure Receive(this : in out Node_Duty; msg : out VN.Message.VN_Message_Basic; 
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

         -- ToDo: Add more special cases
         -- Some special cases:
         if msg.Header.Opcode = VN.Message.OPCODE_LOCAL_HELLO then

            VN.Message.Local_Hello.To_Local_Hello(msg, msgLocalHello);
            CUUID_CAN_Routing.Insert(this.myCUUIDTable, msgLocalHello.CUUID, internal.Sender);

            Local_Ack_Response(internal); -- Respond with a LocalAck

            VN.Communication.CAN.Logic.DebugOutput("CAN address " & internal.Receiver'Img &
                                                     " received LocalHello from CAN address " &
                                                     internal.Sender'Img &
                                                     " responded with LocalAck", 2);

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

   procedure GetCANAddress(this : in out Node_Duty; address : out CAN_Address_Sender;
                           isAssigned : out boolean) is
   begin
      if not this.isInitialized then
         Init(this);
      end if;

      this.addressReceiver.Address(address, isAssigned);
   end GetCANAddress;

   procedure Init(this : in out Node_Duty) is
      testCUUID : VN.VN_CUUID := (others => 42); --ToDo: For testing only!!!!
   begin
      VN.Communication.CAN.Logic.DebugOutput("Node_Duty initialized", 4);

      this.isInitialized := true;

      this.DutyArray(this.DutyArray'First) := this.addressReceiver'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 1) := this.sender'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 2) := this.receiver'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 3) := this.componentTypeResponder'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 4) := this.componentTypeHandler'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 5) := this.logAddrHandler'Unchecked_Access;

      -- receiving messages sent to CAN address 255 (broadcast)
      this.theFilter.Create_Transmission_Filter(this.broadcastFilterID, 255);
      
      --ToDo: For testing only!!!!
--        CAN_Routing.Insert(this.myTable, 1337, 42);
--        CUUID_CAN_Routing.Insert(this.myCUUIDTable, testCUUID, 42);
   end Init;

end VN.Communication.CAN.Logic.Node;
