-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
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

with Ada.Text_IO; --ToDo, for testing only


package body VN.Communication.CAN.Logic.SM is

   procedure Binary_IO(value : VN.Communication.CAN.CAN_message_ID) is  --ToDo, for testing only
      package CAN_MSG_ID_IO is new Ada.Text_IO.Integer_IO(Integer);
      temp : integer := Integer(value);
   begin
      CAN_MSG_ID_IO.Put(Item  => temp,
                        Width => 20,
                        Base  => 2);
      VN.Text_IO.New_Line(2);
   end Binary_IO;

   procedure Update(this : in out SM_Duty; msgsBuffer : in out CAN_Message_Buffers.Buffer; ret : out CAN_Message_Buffers.Buffer) is

      use VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation; -- needed for =-sign for SM_CAN_Mode

      bWillSend : boolean;
      msgIn, msgOut : CAN_Message_Logical;

   begin
      if not this.isInitialized then
         Init(this);
      end if;

      if CAN_Message_Buffers.Extent(msgsBuffer) = 0 then
         for i in this.DutyArray'Range loop
            this.DutyArray(i).Update(msgIn, false, msgOut, bWillSend);

            if bWillSend then
--                 if msgOut.isNormal and then msgOut.msgType = VN.Communication.CAN.Logic.TRANSMISSION then
--                    VN.Communication.CAN.Logic.DebugOutput("TRANSMISSION msg sent from " &
--                                                             msgOut.Sender'Img & " to " & msgOut.Receiver'Img, 4);
--                 end if;

               CAN_Message_Buffers.Insert(msgOut, ret);
            end if;
         end loop;

      else
         while CAN_Message_Buffers.Extent(msgsBuffer) /= 0 loop

            CAN_Message_Buffers.Remove(msgIn, msgsBuffer);

            for i in this.DutyArray'Range loop

               this.DutyArray(i).Update(msgIn, true, msgOut, bWillSend);

               if bWillSend then
                  if msgOut.isNormal and then msgOut.msgType = VN.Communication.CAN.Logic.TRANSMISSION then
                     VN.Communication.CAN.Logic.DebugOutput("TRANSMISSION msg sent from " &
                                                              msgOut.Sender'Img & " to " & msgOut.Receiver'Img, 4);
                  end if;
                  CAN_Message_Buffers.Insert(msgOut, ret);
               end if;
            end loop;
         end loop;
      end if;

      if not this.hasRole then
         if this.masterNegotiation.CurrentMode = VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation.MASTER then

            this.hasCANAddress := true;
            this.hasRole := true;

            this.assigner.Activate(this.myUCID);
            this.sender.Activate(0);
            this.receiver.Activate(0);
            this.cuuidResponder.Activate(this.myCUUID, 0, true);
            this.cuuidHandler.Activate(this.myCUUID, 0, this.sender'Unchecked_Access);

            --Change CAN message filters, SM_CAN_MasterNegotioation longer wishes to receceive
            -- normal CAN messages, only RequestCANAddress messages:
            this.theFilter.Change_Filter(this.negotioationFilterID, VN.Communication.CAN.CAN_message_ID(2 ** 28), VN.Communication.CAN.CAN_message_ID(2 ** 28));
            VN.Communication.CAN.Logic.DebugOutput("Change_Filter negotioationFilterID, template=", 5);
            Binary_IO(VN.Communication.CAN.CAN_message_ID(2 ** 28));

            VN.Communication.CAN.Logic.DebugOutput("Change_Filter negotioationFilterID, mask=", 5);
            Binary_IO(VN.Communication.CAN.CAN_message_ID(2 ** 28));

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
               this.cuuidResponder.Activate(this.myCUUID, address, true);
               this.cuuidHandler.Activate(this.myCUUID, address, this.sender'Unchecked_Access);

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

   procedure Discover(this : in out SM_Duty; discoveredUnits : out Unit_Buffers.Buffer) is
      isSet  : boolean;
      aCUUID : VN.VN_CUUID;
      aUnit  : Unit;
      isSM_CAN  : boolean;
   begin
      if not this.isInitialized then
         Init(this);
      end if;

      Unit_Buffers.Clear(discoveredUnits);

      for i in VN.Communication.CAN.CAN_Address_Sender'range loop
         this.cuuidHandler.ReadEntry(i, aCUUID, isSM_CAN, isSet);

         if isSet then
            aUnit.unitCANAddress := i;
            aUnit.unitCUUID 	 := aCUUID;
            aUnit.isSM_CAN := isSM_CAN;
            Unit_Buffers.Insert(aUnit, discoveredUnits);
         end if;
      end loop;
   end Discover;

   procedure Send(this : in out SM_Duty; msg : VN.Message.VN_Message_Basic; --VN.Communication.CAN.Logic.VN_Message_Internal;
                  result : out VN.Send_Status) is
      internal : VN.Communication.CAN.Logic.VN_Message_Internal;
      receiver : VN.Communication.CAN.CAN_Address_Sender;
      found : boolean;

      msgAssignAddr 	 : VN.Message.Assign_Address.VN_Message_Assign_Address;
      msgAssignAddrBlock : VN.Message.Assign_Address_Block.VN_Message_Assign_Address_Block;
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
         CUUID_CAN_Routing.Search(msgAssignAddr.CUUID, receiver, found);

      elsif msg.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR_BLOCK then

         VN.Message.Assign_Address_Block.To_Assign_Address_Block(msg, msgAssignAddrBlock);
         CUUID_CAN_Routing.Search(msgAssignAddrBlock.CUUID, receiver, found);
      else

         CAN_Routing.Search(this.myTable, msg.Header.Destination, receiver, found);
      end if;

      if found then
         internal.Receiver := VN.Communication.CAN.Convert(receiver);
         internal.Data := msg;

         -- ToDo: test if this is right:
         internal.NumBytes := Interfaces.Unsigned_16(Integer(msg.Header.Payload_Length) +
                                                       VN.Message.HEADER_SIZE +
                                                         VN.Message.CHECKSUM_SIZE);
         this.sender.SendVNMessage(internal, result);
         result := OK;
      else
         result := ERROR_NO_ADDRESS_RECEIVED;
         VN.Communication.CAN.Logic.DebugOutput("VN.Communication.CAN.Logic.SM.Send, Status := ERROR_NO_ADDRESS_RECEIVED;", 5);
      end if;
   end Send;

   procedure Receive(this : in out SM_Duty; msg : out VN.Message.VN_Message_Basic; --VN.Communication.CAN.Logic.VN_Message_Internal;
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

      --stop 	    : boolean := false;
   begin

      if not this.isInitialized then
         Init(this);
      end if;

      -- while not stop loop
      this.receiver.ReceiveVNMessage(internal, status);

      if status = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or --TODO, this will need to be updated if more options for VN.Receive_Status are added
        status = VN.MSG_RECEIVED_MORE_AVAILABLE then

         msg := internal.Data;

         -- ToDo: Add more special cases
         -- Some special cases:
         if msg.Header.Opcode = VN.Message.OPCODE_LOCAL_HELLO then

            VN.Message.Local_Hello.To_Local_Hello(msg, msgLocalHello);
            CUUID_CAN_Routing.Insert(msgLocalHello.CUUID, internal.Sender);

            Local_Ack_Response(internal); -- Respond with a LocalAck
            --   stop := false;

            VN.Communication.CAN.Logic.DebugOutput("CAN address " & internal.Receiver'Img &
                                                     " received LocalHello from CAN address " &
                                                     internal.Sender'Img &
                                                     " responded with LocalAck", 2);

         elsif msg.Header.Opcode = VN.Message.OPCODE_LOCAL_ACK then
            -- ToDo: We should remember that our Local_Hello was acknowledged
            --stop := false;

            VN.Communication.CAN.Logic.DebugOutput("CAN address " & internal.Receiver'Img &
                                                     " received LocalAck from CAN address " &
                                                     internal.Sender'Img, 2);

         else -- messages that are received from units that have a logical address:

            --Store information about the sender of the message:
            CAN_Routing.Insert(this.myTable, internal.Data.Header.Source, internal.Sender);

            if msg.Header.Opcode = VN.Message.OPCODE_DISTRIBUTE_ROUTE then
               Handle_Distribute_Route(internal);
            end if;
            --     stop := true;
         end if;
         --   else
         --    stop := true;
      end if;
      --end loop;
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

      template, mask    : VN.Communication.CAN.CAN_message_ID;
      POWER28 		: constant Interfaces.Unsigned_32 := 2 ** 28;
   begin
      VN.Communication.CAN.Logic.DebugOutput("SM_Duty initialized", 4);

      this.isInitialized := true;

      this.DutyArray(this.DutyArray'First) := this.masterNegotiation'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 1) := this.addressReceiver'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 2) := this.assigner'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 3) := this.sender'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 4) := this.receiver'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 5) := this.cuuidResponder'Unchecked_Access;
      this.DutyArray(this.DutyArray'First + 6) := this.cuuidHandler'Unchecked_Access;

      -- Set CAN filters:
      this.theFilter.Create_Filter(this.negotioationFilterID, 0, 0); --will listen to all CAN messages, for now

      template := VN.Communication.CAN.CAN_message_ID(Interfaces.Shift_Left(Interfaces.Unsigned_32(255),
                                                      VN.Communication.CAN.OFFSET_CAN_RECEIVER));

      mask := VN.Communication.CAN.CAN_message_ID(Interfaces.Shift_Left(Interfaces.Unsigned_32(CAN_Address_Receiver'Last),
                                                  VN.Communication.CAN.OFFSET_CAN_RECEIVER) + POWER28);

      this.theFilter.Create_Filter(this.broadcastFilterID, template, mask); -- receiving messages sent to CAN address 255 (broadcast)
      VN.Communication.CAN.Logic.DebugOutput("Create_Filter broadcastFilterID, template=", 5);
      Binary_IO(template);

      VN.Communication.CAN.Logic.DebugOutput("Create_Filter broadcastFilterID, mask=", 5);
      Binary_IO(mask);

      --ToDo: For testing only!!!!
      CAN_Routing.Insert(this.myTable, 1337, 42);
      CUUID_CAN_Routing.Insert(testCUUID, 42);
   end Init;

end VN.Communication.CAN.Logic.SM;


