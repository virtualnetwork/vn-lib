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
-- ComponentType_Responder responds to the DiscoveryRequest message. Shall be used by all units (nodes or SM-CANs).
-- ComponentType_Responder shall be activated once one has been assigned a CAN address.

with VN.Communication.CAN.Logic.Message_Utils;

package body VN.Communication.CAN.Logic.ComponentType_Responder is

   overriding procedure Update(this : in out ComponentType_Responder; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean) is
   begin
      case this.currentState is
         when Unactivated =>
            bWillSend := false;

         when Activated =>
            if bMsgReceived and then msgIn.isNormal and then msgIn.Receiver = this.myCANAddress and then
              msgIn.msgType = VN.Communication.CAN.Logic.DISCOVERY_REQUEST then


               VN.Communication.CAN.Logic.Message_Utils.ComponentTypeToMessage(msgOut, this.myCANAddress, 0, this.isSM_CAN); --ToDo: prio???
               bWillSend := true;

               msgOut.Receiver := VN.Communication.CAN.Convert(msgIn.Sender); -- set the recever address to the sender of the request

               VN.Communication.CAN.Logic.DebugOutput("Sent component type (node or SM-CAN) from CAN address " & this.myCANAddress'img, 5);

            elsif bMsgReceived and then msgIn.isNormal and then msgIn.Receiver = VN.Communication.CAN.CAN_Address_Receiver(254) and then
              msgIn.msgType = VN.Communication.CAN.Logic.DISCOVERY_REQUEST then

               VN.Communication.CAN.Logic.Message_Utils.ComponentTypeToMessage(msgOut, this.myCANAddress, 0, this.isSM_CAN); --ToDo: prio???
               bWillSend := true;
               msgOut.Receiver := 255; -- set the recever address to the broadcast address

               VN.Communication.CAN.Logic.DebugOutput("Sent component type (node or SM-CAN) from CAN address " & this.myCANAddress'img, 5);
            else
               bWillSend := false;
            end if;
      end case;
   end Update;

   procedure Activate(this : in out ComponentType_Responder; theCUUID : VN.VN_CUUID; CANAddress : VN.Communication.CAN.CAN_Address_Sender; isSM_CAN : boolean) is
   begin
      if this.currentState = Unactivated then
         VN.Communication.CAN.Logic.DebugOutput("CUUID_Responder at address " & CANAddress'Img & " activated", 5);
         this.myCANAddress := CANAddress;
         this.currentState := Activated;
         this.myCUUID 	   := theCUUID;
         this.isSM_CAN     := isSM_CAN;
      end if;
   end Activate;
end VN.Communication.CAN.Logic.ComponentType_Responder;
