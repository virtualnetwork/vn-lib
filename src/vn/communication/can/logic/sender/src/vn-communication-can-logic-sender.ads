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
-- Sender_Duty is a VN.Communication.CAN.Logic interface for sending VN-messages. Sender_Duty
-- owns several Sender unit duties.
-- Before it can be used, Sender_Duty will need to be activated. This cannot
-- be done until one has been assigned a CAN address.
-- Sender_Duty has a send buffer, if no Sender units are available when a VN
-- message is to be sent it will be written to this buffer.
-- The message will then be sent when a Sender unit becomes available.

pragma Profile (Ravenscar);

with VN.Communication.CAN.Logic;
with VN.Communication.CAN.Logic.Sender_Unit;
use VN.Communication.CAN.Logic.Sender_Unit;

with Interfaces;

with Buffers;

package VN.Communication.CAN.Logic.Sender is


   type Sender_Duty is
     new VN.Communication.CAN.Logic.Duty with private;

   type Sender_Duty_ptr is access all Sender_Duty'Class;

   overriding procedure Update(this : in out Sender_Duty; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean);

   procedure SendVNMessage(this : in out Sender_Duty; msg : VN_Message_Internal;
                           result : out VN.Send_Status);

   procedure Activate(this : in out Sender_Duty; address : VN.Communication.CAN.CAN_Address_Sender);

private

   package Send_Buffer_pack is new Buffers(VN.Communication.CAN.Logic.VN_Message_Internal);
   use Send_Buffer_pack;

   SIZE : constant Integer := 20;      --ToDO: Put this in a config file of some sort
   NUM_UNITS : constant integer := 4;  --ToDO: Put this in a config file of some sort

   type UnitArray is array(1..NUM_UNITS) of aliased VN.Communication.CAN.Logic.Sender_Unit.Sender_Unit_Duty;

   type Sender_State is (Unactivated, Activated);

   type Sender_Duty is
     new VN.Communication.CAN.Logic.Duty with
      record
         currentState 	: Sender_State := Unactivated;
         myCANAddress 	: VN.Communication.CAN.CAN_Address_Sender;
         sendBuffer   	: Send_Buffer_pack.Buffer(SIZE);
         units		: UnitArray;
         iterator	: Integer := UnitArray'First;
      end record;

   procedure GetFreeUnit(this : in out Sender_Duty; ret : out Sender_Unit_Duty_ptr);

end VN.Communication.CAN.Logic.Sender;
