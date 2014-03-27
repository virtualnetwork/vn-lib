-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary: SM_Duty is an object that holds send and receive buffers
-- for VN messages that are to be sent, or have been received, over the CAN network.
-- SM_Duty holds instances of the classes that implement the state machines
-- of the VN-CAN protcol.

pragma Profile (Ravenscar);

package body VN.Communication.CAN.Logic.SM is

   procedure Update(this : in out SM_Duty; msgsBuffer : in out CAN_Message_Buffers.Buffer; ret : out CAN_Message_Buffers.Buffer) is

      use VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation; -- needed for =-sign for SM_CAN_Mode

--        theCursor : CAN_Message_Buffers.Cursor := msgsBuffer.First;
      bWillSend : boolean;
      msgIn, msgOut : CAN_Message_Logical;

   begin

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
            for i in this.DutyArray'Range loop

               this.DutyArray(i).Update(msgIn, true, msgOut, bWillSend);

               if bWillSend then
                  CAN_Message_Buffers.Insert(msgOut, ret);
               end if;
            end loop;
            --CAN_Message_Buffers.Next(theCursor);
         end loop;
      end if;

      if this.masterNegotiation.CurrentMode = VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation.MASTER then
         this.assigner.Activate(this.myUCID);
         this.sender.Activate(0);
         this.receiver.Activate(0);
         this.cuuidResponder.Activate(this.myCUUID, 0, true);
         this.cuuidHandler.Activate(this.myCUUID, 0);
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
            this.cuuidHandler.Activate(this.myCUUID, address);
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
      aCUUID : VN.Communication.CAN.Logic.CUUID;
      aUnit  : Unit;
      isSM_CAN  : boolean;
   begin

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

   procedure Send(this : in out SM_Duty; msg : VN.Communication.CAN.Logic.VN_Message_Internal;
                  result : out VN.Communication.CAN.Logic.Transmission_Result) is
   begin
      this.sender.SendVNMessage(msg, result);
   end Send;

   procedure Receive(this : in out SM_Duty; msg : out VN.Communication.CAN.Logic.VN_Message_Internal;
                     hasReceived : out boolean) is
   begin
      this.receiver.ReceiveVNMessage(msg, hasReceived);
   end Receive;

   procedure GetCANAddress(this : in out SM_Duty; address : out CAN_Address_Sender;
                     isAssigned : out boolean) is
      use VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation;
   begin
      if this.masterNegotiation.CurrentMode = VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation.MASTER then
         address := 0;
         isAssigned := true;
      else
         this.addressReceiver.Address(address, isAssigned);
      end if;
   end GetCANAddress;

   --THIS IS JUST TESTING FUNCTIONALLITY FOR NODES, NOT SM-CANs
--     procedure GetLogicalAddress(this : in out SM_Duty; LogicalAddress : out VN.Communication.CAN.Logic.Logical_Address;
--                                 isAssigned : out boolean) is
--     begin
--        this.logicalAddressHandler.GetAddress(LogicalAddress, isAssigned);
--     end GetLogicalAddress;
--
--
--     procedure SetMyAddress(this : in out SM_Duty; LogicalAddress : VN.Communication.CAN.Logic.Logical_Address) is
--     begin
--        this.logicalAddressHandler.SetMyAddress(LogicalAddress);
--     end SetMyAddress;
--
--     procedure Assign(this : in out SM_Duty; CANAddress : CAN_Address_Sender;
--                      LogicalAddress : VN.Communication.CAN.Logic.Logical_Address) is
--     begin
--        this.logicalAddressHandler.Assign(CANAddress, LogicalAddress);
--     end Assign;
--
--     procedure AddressQuestion(this : in out SM_Duty; LogicalAddress : VN.Communication.CAN.Logic.Logical_Address;
--                               CANAddress : out CAN_Address_Sender; wasFound : out boolean) is
--     begin
--        this.logicalAddressHandler.AddressQuestion(LogicalAddress, CANAddress, wasFound);
--     end AddressQuestion;


   procedure Initialize (this : in out SM_Duty) is
   begin
      VN.Communication.CAN.Logic.DebugOutput("SM_Duty initialized", 4);

      this.DutyArray := (VN.Communication.CAN.Logic.Duty_Ptr(this.masterNegotiation),
                         VN.Communication.CAN.Logic.Duty_Ptr(this.addressReceiver),
                         VN.Communication.CAN.Logic.Duty_Ptr(this.assigner),
                         VN.Communication.CAN.Logic.Duty_Ptr(this.sender),
                         VN.Communication.CAN.Logic.Duty_Ptr(this.receiver),
                         VN.Communication.CAN.Logic.Duty_Ptr(this.cuuidResponder),
                         VN.Communication.CAN.Logic.Duty_Ptr(this.cuuidHandler));
   end Initialize;

end VN.Communication.CAN.Logic.SM;


