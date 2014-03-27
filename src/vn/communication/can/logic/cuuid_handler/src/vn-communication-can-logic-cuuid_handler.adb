-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- CUUID_Handler handles the discovery process on the CAN network.
-- CUUID_Handler sends out the RequestCUUID message to which other units will respond.
-- The function ReadEntry can then be used to retrieve information about
-- the units of the CAN network.


with VN.Communication.CAN.Logic.Message_Utils;

package body VN.Communication.CAN.Logic.CUUID_Handler is

   overriding procedure Update(this : in out CUUID_Handler; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean) is
      use Ada.Real_Time;
      wasSM_CAN : Boolean;
   begin
      case this.currentState is
         when Unactivated =>
            bWillSend := false;

         when Activated =>
            if bMsgReceived then
               if msgIn.isNormal and msgIn.msgType = VN.Communication.CAN.Logic.FIRST_CUUID_HALF then
                  if not this.units(msgIn.Sender).isFirstCUUIDHalfSet then
                     VN.Communication.CAN.Logic.Message_Utils.CUUIDHalfFromMessage(msgIn, this.units(msgIn.Sender).unitCUUID, true);
                     this.units(msgIn.Sender).isFirstCUUIDHalfSet := true;
                     VN.Communication.CAN.Logic.DebugOutput("Recieved first CUUID half from address " & msgIn.Sender'Img, 5);
                  end if;

               elsif msgIn.isNormal and msgIn.msgType = VN.Communication.CAN.Logic.SECOND_CUUID_HALF then
                  if not this.units(msgIn.Sender).isSecondCUUIDHalfSet then
                     VN.Communication.CAN.Logic.Message_Utils.CUUIDHalfFromMessage(msgIn, this.units(msgIn.Sender).unitCUUID, false);
                     this.units(msgIn.Sender).isSecondCUUIDHalfSet := true;
                     VN.Communication.CAN.Logic.DebugOutput("Recieved second CUUID half from address " & msgIn.Sender'Img, 5);
                  end if;

               elsif msgIn.isNormal and msgIn.msgType = VN.Communication.CAN.Logic.COMPONENT_TYPE then
                  if not this.units(msgIn.Sender).isComponentTypeSet then
                     VN.Communication.CAN.Logic.Message_Utils.ComponentTypeFromMessage(msgIn, wasSM_CAN);
                     this.units(msgIn.Sender).isSM_CAN := wasSM_CAN;
                     this.units(msgIn.Sender).isComponentTypeSet := true;
                     VN.Communication.CAN.Logic.DebugOutput("Recieved ComponentType from address " & msgIn.Sender'Img, 5);
                  end if;
               end if;
            end if;
            bWillSend := false;

            if not this.hasRequested and Ada.Real_Time.Clock - this.timer >= DELAY_TIME then
               this.hasRequested := true;
               VN.Communication.CAN.Logic.DebugOutput("CUUID request sent from address " & this.myCANAddress'Img, 5);
               VN.Communication.CAN.Logic.Message_Utils.RequestCUUIDToMessage(msgOut, this.myCANAddress, 0);
               bWillSend := true;
            end if;
      end case;
   end Update;

   procedure Activate(this : in out CUUID_Handler; theCUUID : VN.VN_CUUID; CANAddress : VN.Communication.CAN.CAN_Address_Sender) is
   begin

      if this.currentState = Unactivated then
         this.timer := Ada.Real_Time.Clock;
         VN.Communication.CAN.Logic.DebugOutput("CUUID_Handler at address " & CANAddress'Img & " activated", 5);

         this.currentState := Activated;
         this.myCANAddress := CANAddress;
         this.units(this.myCANAddress).unitCUUID := theCUUID;
         this.units(this.myCANAddress).isFirstCUUIDHalfSet  := true;
         this.units(this.myCANAddress).isSecondCUUIDHalfSet := true;
      end if;
   end Activate;

   procedure ReadEntry(this : in out CUUID_Handler; index : VN.Communication.CAN.CAN_Address_Sender;
                       unitCUUID : out VN.VN_CUUID; isSM_CAN : out boolean; isSet : out Boolean) is
   begin
      isSet 	:= this.units(index).isFirstCUUIDHalfSet and this.units(index).isSecondCUUIDHalfSet and this.units(index).isComponentTypeSet;

      if isSet then
         unitCUUID := this.units(index).unitCUUID;
         isSM_CAN  := this.units(index).isSM_CAN;
      end if;
   end ReadEntry;
end VN.Communication.CAN.Logic.CUUID_Handler;

