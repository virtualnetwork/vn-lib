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
-- Implements the state machine for receiving VN messages. Receiver_Duty will
-- use an instane of Receiver_Unit_Duty to send a VN message.
-- Before it can be used, Receiver_Duty will need to be activated. This cannot
-- be done until one has been assigned a CAN address.
-- Receiver_Duty has a receive buffer. Each Receiver_Unit_Duty has a pointer
-- (access variable) to this received. When a Receiver_Unit_Duty has received
-- a VN message, it will write it to this buffer.


with VN.Communication.CAN.Logic;
with VN.Communication.CAN.Logic.Receiver_Unit;
use VN.Communication.CAN.Logic.Receiver_Unit;

use VN.Communication.CAN.Logic.Receiver_Unit.Receive_Buffer_pack;
use VN.Communication.CAN.Logic.Receiver_Unit.Pending_Senders_pack;

with Interfaces;
with Buffers;

package VN.Communication.CAN.Logic.Receiver is

   type Receiver_Duty is
     new VN.Communication.CAN.Logic.Duty with private;

   type Receiver_Duty_ptr is access all Receiver_Duty'Class;

   overriding procedure Update(this : in out Receiver_Duty; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean);

   procedure ReceiveVNMessage(this : in out Receiver_Duty; msg : out VN.Communication.CAN.Logic.VN_Message_Internal;
                              status : out VN.Receive_Status);

   procedure Activate(this : in out Receiver_Duty; address : VN.Communication.CAN.CAN_Address_Sender);

private


   NUM_UNITS : constant integer := 4;  --ToDO: Put this in a config file of some sort

   type UnitArray is array(1..NUM_UNITS) of aliased VN.Communication.CAN.Logic.Receiver_Unit.Receiver_Unit_Duty;

   type Receiver_State is (Unactivated, Activated);
   type Receiver_Duty is
     new VN.Communication.CAN.Logic.Duty with
      record
         currentState 	 : Receiver_State := Unactivated;
         myCANAddress 	 : VN.Communication.CAN.CAN_Address_Sender;
         receiveBuffer   : aliased Receive_Buffer_pack.Buffer(SIZE);
         units		 : UnitArray;
         pendingSenders  : aliased Pending_Senders_pack.Buffer(VN.Communication.CAN.Logic.Receiver_Unit.SIZE);
      end record;

  procedure GetFreeUnit(this : in out Receiver_Duty; ret : out Receiver_Unit_Duty_ptr);

end VN.Communication.CAN.Logic.Receiver;
