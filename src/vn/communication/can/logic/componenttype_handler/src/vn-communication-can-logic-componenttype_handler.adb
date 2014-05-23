-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- ComponentType_Handler handles the component discovery process on the CAN network.
-- ComponentType_Handler sends out the DiscoveryRequest message to which other units will respond.

-- ComponentType_Handler takes a procedure pointer HelloProc as input when initiated.
-- This procedure is called each time new unit is discovered.

with VN.Communication.CAN.Logic.Message_Utils;

with VN.Message.Factory;
with VN.Message.Local_Hello;

package body VN.Communication.CAN.Logic.ComponentType_Handler is

   overriding procedure Update(this : in out ComponentType_Handler; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean) is
      use Ada.Real_Time;
      wasSM_CAN : Boolean;
   begin
      case this.currentState is
         when Unactivated =>
            bWillSend := false;

         when Activated =>
            if bMsgReceived then

               if msgIn.isNormal and then msgIn.msgType = VN.Communication.CAN.Logic.COMPONENT_TYPE then
                  if not this.units(msgIn.Sender).isComponentTypeSet then
                     VN.Communication.CAN.Logic.Message_Utils.ComponentTypeFromMessage(msgIn, wasSM_CAN);
                     this.units(msgIn.Sender).isSM_CAN := wasSM_CAN;
                     this.units(msgIn.Sender).isComponentTypeSet := true;
                     VN.Communication.CAN.Logic.DebugOutput("Recieved ComponentType from address " & msgIn.Sender'Img, 5);

                     this.HelloProc(this.mySender, msgIn.Sender, wasSM_CAN); -- Send LocalHello etc.
                  end if;
               end if;
            end if;
            bWillSend := false;

            if not this.hasRequested and Ada.Real_Time.Clock - this.timer >= DELAY_TIME then
               -- this.hasRequested := true;
               this.timer := Ada.Real_Time.Clock; --send DiscoveryRequests periodically, not just once

               VN.Communication.CAN.Logic.DebugOutput("Discovery request sent from address " & this.myCANAddress'Img, 5);
               VN.Communication.CAN.Logic.Message_Utils.DiscoveryRequestToMessage(msgOut, this.myCANAddress, 0);
               bWillSend := true;
            end if;
      end case;
   end Update;

   procedure Activate(this : in out ComponentType_Handler;
                      theCUUID : VN.VN_CUUID;
                      CANAddress : VN.Communication.CAN.CAN_Address_Sender;
                      sender : VN.Communication.CAN.Logic.Sender.Sender_Duty_ptr) is
   begin

      if this.currentState = Unactivated then
         this.timer := Ada.Real_Time.Clock;
         VN.Communication.CAN.Logic.DebugOutput("ComponentType_Handler at address " & CANAddress'Img & " activated", 5);

         this.mySender     := sender;
         this.myCUUID      := theCUUID;
         this.currentState := Activated;
         this.myCANAddress := CANAddress;
         this.units(this.myCANAddress).isSM_CAN := true;
         this.units(this.myCANAddress).isComponentTypeSet  := true;
      end if;
   end Activate;

   procedure Init(this     : in out ComponentType_Handler;
                  sender   : VN.Communication.CAN.Logic.Sender.Sender_Duty_ptr;
                  theCUUID : VN.VN_CUUID) is
   begin
      this.myCUUID := theCUUID;
      this.mySender := sender;
   end Init;

   procedure HelloProc(this 	  : in out ComponentType_Handler;
                       sender     : in VN.Communication.CAN.Logic.Sender.Sender_Duty_ptr;
                       CANAddress : VN.Communication.CAN.CAN_Address_Sender;
                       isSM_CAN   : Boolean) is

      msgBasic : VN.Message.VN_Message_Basic :=
        VN.Message.Factory.Create(VN.Message.Type_Local_Hello);

      msgLocalHello : VN.Message.Local_Hello.VN_Message_Local_Hello;
      msg : VN.Communication.CAN.Logic.VN_Message_Internal;
      result : VN.Send_Status;
   begin

      VN.Communication.CAN.Logic.DebugOutput("SM_Duty discovered a unit, CANAddress= " &
                                               CANAddress'Img & " isSM_CAN = " &
                                               isSM_CAN'img, 4);

      -- Send LocalHello message:
      if isSM_CAN then
         VN.Message.Local_Hello.To_Local_Hello(msgBasic, msgLocalHello);

         msgLocalHello.Header.Source 		:= VN.LOGICAL_ADDRES_UNKNOWN;
         msgLocalHello.Header.Destination 	:= VN.LOGICAL_ADDRES_UNKNOWN;
         msgLocalHello.CUUID := this.myCUUID;
         msgLocalHello.Component_Type := VN.Message.SM_x;

         VN.Message.Local_Hello.To_Basic(msgLocalHello, msg.Data);

         VN.Communication.CAN.Logic.DebugOutput("CAN address " & this.myCANAddress'Img &
                                                  " sent LocalHello to CAN address " &
                                                  CANAddress'img, 2);

         msg.Receiver := VN.Communication.CAN.Convert(CANAddress);
         msg.NumBytes := Interfaces.Unsigned_16(Integer(msgLocalHello.Header.Payload_Length) +
                                                  VN.Message.HEADER_SIZE + VN.Message.CHECKSUM_SIZE);

         sender.SendVNMessage(msg, result);

         --ToDo: If result is not equal to OK we have a problem
      end if;
   end HelloProc;
end VN.Communication.CAN.Logic.ComponentType_Handler;

