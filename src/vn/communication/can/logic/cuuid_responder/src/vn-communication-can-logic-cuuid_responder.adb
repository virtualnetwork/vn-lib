-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- CUUID_Responder responds to the RequestCUUID message. Shall be used by all units (nodes or SM-CANs).
-- CUUID_Responder shall be activated once one has been assigned a CAN address.

with VN.Communication.CAN.Logic.Message_Utils;

package body VN.Communication.CAN.Logic.CUUID_Responder is

   overriding procedure Update(this : in out CUUID_Responder; msgIn : VN.Communication.CAN.Logic.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.Logic.CAN_Message_Logical; bWillSend : out boolean) is
   begin
      case this.currentState is
         when Unactivated =>
            bWillSend := false;

         when Activated =>
            if bMsgReceived and then msgIn.isNormal and then msgIn.msgType = VN.Communication.CAN.Logic.REQUEST_CUUID then
               VN.Communication.CAN.Logic.Message_Utils.CUUIDHalfToMessage(msgOut, this.myCANAddress, this.myCUUID, true);
               bWillSend := true;
               this.currentState := SendSecondCUUIDHalf;
               VN.Communication.CAN.Logic.DebugOutput("Sent first CUUID half from CAN address " & this.myCANAddress'img, 5);

            else
               bWillSend := false;
            end if;

         when SendSecondCUUIDHalf =>
            -- If a new REQUEST_CUUID message is received before both the messages have been sent, respond with the first one first, then the second.
            -- (just to make sure that whoever who sent the REQUEST_CUUID message gets both messages)
            if bMsgReceived and then msgIn.isNormal and then msgIn.msgType = VN.Communication.CAN.Logic.REQUEST_CUUID then
               VN.Communication.CAN.Logic.Message_Utils.CUUIDHalfToMessage(msgOut, this.myCANAddress, this.myCUUID, true);
               bWillSend := true;
               this.currentState := SendSecondCUUIDHalf;
               VN.Communication.CAN.Logic.DebugOutput("Resent first CUUID half from CAN address " & this.myCANAddress'img, 5);

            else
               VN.Communication.CAN.Logic.Message_Utils.CUUIDHalfToMessage(msgOut, this.myCANAddress, this.myCUUID, false);
               bWillSend := true;
               this.currentState := SendType;
               VN.Communication.CAN.Logic.DebugOutput("Sent second CUUID half from CAN address " & this.myCANAddress'img, 5);
            end if;

         when SendType =>

            VN.Communication.CAN.Logic.Message_Utils.ComponentTypeToMessage(msgOut, this.myCANAddress, 0, this.isSM_CAN); --prio???
            bWillSend := true;
            this.currentState := Activated;
            VN.Communication.CAN.Logic.DebugOutput("Sent type (node or SM-CAN) from CAN address " & this.myCANAddress'img, 5);
      end case;
   end Update;

   procedure Activate(this : in out CUUID_Responder; theCUUID : VN.Communication.CAN.Logic.CUUID; CANAddress : VN.Communication.CAN.Logic.CAN_Address_Sender; isSM_CAN : boolean) is
   begin
      if this.currentState = Unactivated then
         VN.Communication.CAN.Logic.DebugOutput("CUUID_Responder at address " & CANAddress'Img & " activated", 5);
         this.myCANAddress := CANAddress;
         this.currentState := Activated;
         this.myCUUID 	   := theCUUID;
         this.isSM_CAN     := isSM_CAN;
      end if;
   end Activate;
end VN.Communication.CAN.Logic.CUUID_Responder;
