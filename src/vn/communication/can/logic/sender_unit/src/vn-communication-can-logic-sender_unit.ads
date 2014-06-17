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
-- Implements the state machine for sending a single VN message. Sender_Duty will
-- use an instane of Sender_Unit_Duty to send a VN message.
-- Before it can be used, Sender_Unit_Duty will need to be activated. This cannot
-- be done until one has been assigned a CAN address.

with VN.Communication.CAN.Logic;
with Interfaces;
with Ada.Real_Time;

package VN.Communication.CAN.Logic.Sender_Unit is

   type Sender_Unit_Duty is
     new VN.Communication.CAN.Logic.Duty with private;

   type Sender_Unit_Duty_ptr is access all Sender_Unit_Duty'Class;

   overriding procedure Update(this : in out Sender_Unit_Duty; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean);

   -- shall be called after a CAN address has been obtained
   procedure Activate(this : in out Sender_Unit_Duty; address : VN.Communication.CAN.CAN_Address_Sender);

   procedure Send(this : in out Sender_Unit_Duty;
                  message : VN.Communication.CAN.Logic.VN_Message_Internal);

   function isActive(this : in Sender_Unit_Duty) return boolean;

   function Receiver(this : in Sender_Unit_Duty) return VN.Communication.CAN.CAN_Address_Receiver;

private

   WAIT_TIME : Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(2000);

   type Sender_Unit_State is (Idle, Initiated, Started, Transmitting, BlockFull);
   type Sender_Unit_Duty is
     new VN.Communication.CAN.Logic.Duty with
      record
         currentState 	: Sender_Unit_State := Idle;
         myCANAddress 	: VN.Communication.CAN.CAN_Address_Sender;
         Receiver 	: VN.Communication.CAN.CAN_Address_Receiver;
         ToSend  	: VN.Message.VN_Message_Byte_Array;
         numBytesToSend	: Interfaces.Unsigned_16;
         useFlowControl : Boolean;
         blockSize  	: Interfaces.Unsigned_16;
         blockCount	: Interfaces.Unsigned_16; -- counts the number of Transmission message sent in this block
         sequenceNumber : Interfaces.Unsigned_16; -- will equal 0 when the first Transmission message is sent, is incremented after the message is sent
         time 		: Ada.Real_Time.Time;
      end record;

   function NumMessagesToSend(messageLength : Interfaces.Unsigned_16) return Interfaces.Unsigned_16;

end VN.Communication.CAN.Logic.Sender_Unit;

