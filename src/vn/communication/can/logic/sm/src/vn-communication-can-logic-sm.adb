-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary: SM_Duty is an object that holds send and receive buffers
-- for VN messages that are to be sent, or have been received, over the CAN network.
-- SM_Duty holds instances of the classes that implement the state machines
-- of the VN-CAN protcol.
-- Please also note that this functionality is regarding a Subnet Manager for CAN (SM-CAN),
-- not an ordinary node.

--ToDo: Right now no routing information is retreived from DistributeRoute messages!!
--ToDo: Send LocalHello messages when detecting a new SM-CAN
--ToDo: Send LocalAck messages when receiving a LocalHello message

pragma Profile (Ravenscar);

with VN.Message;
use VN.Message;

with VN.Message.Factory;
with VN.Message.Local_Hello;
with VN.Message.Local_Ack;
with VN.Message.Distribute_Route;

package body VN.Communication.CAN.Logic.SM is

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

            if bWillSend and then msgOut.isNormal and then msgOut.msgType = VN.Communication.CAN.Logic.TRANSMISSION and then
              msgOut.Receiver = VN.Communication.CAN.CAN_Address_Receiver(1) then

               VN.Communication.CAN.Logic.DebugOutput("SM sent transmission msg", 3);
            end if;

            if bWillSend then
               CAN_Message_Buffers.Insert(msgOut, ret);
            end if;
         end loop;

      else
         while CAN_Message_Buffers.Extent(msgsBuffer) /= 0 loop

            CAN_Message_Buffers.Remove(msgIn, msgsBuffer);

            for i in this.DutyArray'Range loop

--                 if i = this.DutyArray'First + 1 and then
--                   msgIn.isNormal and then msgIn.msgType = VN.Communication.CAN.Logic.TRANSMISSION and then
--                   msgIn.Receiver = VN.Communication.CAN.CAN_Address_Receiver(1) then
--
--                   VN.Communication.CAN.Logic.DebugOutput("SM recieved transmission msg", 3);
--                 end if;

               this.DutyArray(i).Update(msgIn, true, msgOut, bWillSend);

               if bWillSend and then msgOut.isNormal and then msgOut.msgType = VN.Communication.CAN.Logic.TRANSMISSION and then
                 msgOut.Receiver = VN.Communication.CAN.CAN_Address_Receiver(1) then

                  VN.Communication.CAN.Logic.DebugOutput("SM sent transmission msg", 3);
               end if;

               if bWillSend then
                  CAN_Message_Buffers.Insert(msgOut, ret);
               end if;
            end loop;
         end loop;
      end if;

      if this.masterNegotiation.CurrentMode = VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation.MASTER then
         this.assigner.Activate(this.myUCID);
         this.sender.Activate(0);
         this.receiver.Activate(0);
         this.cuuidResponder.Activate(this.myCUUID, 0, true);
         this.cuuidHandler.Activate(this.myCUUID, 0, this.sender'Unchecked_Access);
         --this.logicalAddressHandler.Activate(0, true);

      elsif this.masterNegotiation.CurrentMode = VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation.SLAVE then
         this.addressReceiver.Activate;
      end if;

      declare
         isAssigned : boolean;
         address    : VN.Communication.CAN.CAN_Address_Sender;
      begin
         this.addressReceiver.Address(address, isAssigned);
         if isAssigned then
            this.sender.Activate(address);
            this.receiver.Activate(address);
            this.cuuidResponder.Activate(this.myCUUID, address, true);
            this.cuuidHandler.Activate(this.myCUUID, address, this.sender'Unchecked_Access);
           -- this.logicalAddressHandler.Activate(address, false); --isSM_CAN is set to false here in some cases of testing

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
   begin
      if not this.isInitialized then
         Init(this);
      end if;

      --The SPA protocol says that messages addressed to logical address 0 shall be thrown away
      if msg.Header.Destination = 0 then
         result := OK;
         return;
      end if;

      CAN_Routing.Search(this.myTable, msg.Header.Destination, receiver, found);

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

            VN.Communication.CAN.Logic.DebugOutput("TEST: borde ha svarat, LocalHello",0);
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

   --THIS IS JUST TESTING FUNCTIONALLITY FOR NODES, NOT SM-CANs
--     procedure GetLogicalAddress(this : in out SM_Duty; LogicalAddress : out VN.VN_Logical_Address;
--                                 isAssigned : out boolean) is
--     begin
--        this.logicalAddressHandler.GetAddress(LogicalAddress, isAssigned);
--     end GetLogicalAddress;
--
--
--     procedure SetMyAddress(this : in out SM_Duty; LogicalAddress : VN.VN_Logical_Address) is
--     begin
--        this.logicalAddressHandler.SetMyAddress(LogicalAddress);
--     end SetMyAddress;
--
--     procedure Assign(this : in out SM_Duty; CANAddress : CAN_Address_Sender;
--                      LogicalAddress : VN.VN_Logical_Address) is
--     begin
--        this.logicalAddressHandler.Assign(CANAddress, LogicalAddress);
--     end Assign;
--
--     procedure AddressQuestion(this : in out SM_Duty; LogicalAddress : VN.VN_Logical_Address;
--                               CANAddress : out CAN_Address_Sender; wasFound : out boolean) is
--     begin
--        this.logicalAddressHandler.AddressQuestion(LogicalAddress, CANAddress, wasFound);
--     end AddressQuestion;


   procedure Init(this : in out SM_Duty) is
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

      --ToDo: For testing only!!!!
      CAN_Routing.Insert(this.myTable, 1337, 42);
   end Init;

end VN.Communication.CAN.Logic.SM;


