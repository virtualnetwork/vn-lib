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

pragma Profile (Ravenscar);


with VN.Communication.CAN.CAN_Filtering;
with VN.Communication.CAN.Logic;

package VN.Communication.CAN.Logic.ComponentType_Responder is

   type ComponentType_Responder is
     new VN.Communication.CAN.Logic.Duty with private;

   type ComponentType_Responder_ptr is access all ComponentType_Responder'Class;

   overriding procedure Update(this : in out ComponentType_Responder; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean);

   procedure Activate(this : in out ComponentType_Responder; theCUUID : VN.VN_CUUID;
                      CANAddress : VN.Communication.CAN.CAN_Address_Sender; isSM_CAN : boolean);

private

   type ComponentType_Responder_State is (Unactivated, Activated);

   type ComponentType_Responder is
     new VN.Communication.CAN.Logic.Duty with
      record
         currentState 	: ComponentType_Responder_State := Unactivated;
         myCUUID 	: VN.VN_CUUID;
         myCANAddress   : VN.Communication.CAN.CAN_Address_Sender;
         isSM_CAN	: boolean;
      end record;
end VN.Communication.CAN.Logic.ComponentType_Responder;
