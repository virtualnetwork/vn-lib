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
-- Implements the state machine for receiving a single VN message. Receiver_Duty will
-- use an instane of Receiver_Unit_Duty to receive a VN message.
-- Before it can be used, Receiver_Unit_Duty will need to be activated. This cannot
-- be done until one has been assigned a CAN address.

pragma Profile (Ravenscar);

with VN.Communication.CAN.Logic;
with Interfaces;
with Buffers;

package VN.Communication.CAN.Logic.Receiver_Unit is

   type Receiver_Unit_Duty is
     new VN.Communication.CAN.Logic.Duty with private;

   type Receiver_Unit_Duty_ptr is access all Receiver_Unit_Duty'Class;

   type Pending_Sender is
      record
         sender 	: VN.Communication.CAN.CAN_Address_Sender;
         numBytes	: Interfaces.Unsigned_16;
      end record;

   SIZE : constant Integer := 20;  --ToDO: Put this in a config file of some sort

   package Pending_Senders_pack is new Buffers(Pending_Sender);
   use Pending_Senders_pack;
   type Pending_Senders_ptr is access all Pending_Senders_pack.Buffer(SIZE);

   package Receive_Buffer_pack is new Buffers(VN.Communication.CAN.Logic.VN_Message_Internal);
   use Receive_Buffer_pack;
   type Receive_Buffer_ptr is access all Receive_Buffer_pack.Buffer(SIZE);

   overriding procedure Update(this : in out Receiver_Unit_Duty; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean);


   -- shall be called after a CAN address has been obtained
   procedure Activate(this : in out Receiver_Unit_Duty; address : VN.Communication.CAN.CAN_Address_Sender;
                      receiveBufferPtr : Receive_Buffer_ptr; pendingSendersPtr : Pending_Senders_ptr);


   procedure Assign(this : in out Receiver_Unit_Duty; sender : VN.Communication.CAN.CAN_Address_Sender;
                    numBytes	: Interfaces.Unsigned_16);

   function isActive(this : in Receiver_Unit_Duty) return boolean;

   function Sender(this : in Receiver_Unit_Duty) return VN.Communication.CAN.CAN_Address_Sender;

private

   DEFAULT_BLOCK_SIZE : Interfaces.Unsigned_16 := 2;  --ToDO: Put this in a config file of some sort

   type Receiver_Unit_State is (Idle, Started, Transmitting);

   type Receiver_Unit_Duty is
     new VN.Communication.CAN.Logic.Duty with
      record
         currentState 	: Receiver_Unit_State := Idle;
         myCANAddress 	: VN.Communication.CAN.CAN_Address_Sender;

         sender 	: VN.Communication.CAN.CAN_Address_Sender;
         receivedData	: VN.Message.VN_Message_Byte_Array;
         numBytes	: Interfaces.Unsigned_16;

         useFlowControl : Boolean;
         blockSize  	: Interfaces.Unsigned_16;
         blockCount	: Interfaces.Unsigned_16; -- counts the number of Transmission message received in this block
         sequenceNumber : Interfaces.Unsigned_16; -- will equal 0 when the first Transmission message is received, is incremented after the message is received

         receiveBuffer 	: Receive_Buffer_ptr 	:= null;
         pendingSenders : Pending_Senders_ptr 	:= null;
      end record;

end VN.Communication.CAN.Logic.Receiver_Unit;
