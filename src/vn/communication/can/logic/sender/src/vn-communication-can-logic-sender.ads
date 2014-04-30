-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
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


   type Sender_Duty is new VN.Communication.CAN.Logic.Duty with private;
   type Sender_Duty_ptr is access all Sender_Duty'Class;

   overriding procedure Update(this : in out Sender_Duty; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean);

   procedure SendVNMessage(this : in out Sender_Duty; msg : VN_Message_Internal;
                           result : out VN.Send_Status);

   procedure Activate(this : in out Sender_Duty; address : VN.Communication.CAN.CAN_Address_Sender);

private

--     package Send_Buffer_pack is new Limited_Buffers(VN.Communication.CAN.Logic.VN_Message_Internal,
--                                                     VN.Communication.CAN.Logic.Assignment);

   package Send_Buffer_pack is new Buffers(VN.Communication.CAN.Logic.VN_Message_Internal);
   use Send_Buffer_pack;

   SIZE : constant Integer := 20;      --ToDO: Put this in a config file of some sort
   NUM_UNITS : constant integer := 4;  --ToDO: Put this in a config file of some sort

   type UnitArray is array(1..NUM_UNITS) of aliased VN.Communication.CAN.Logic.Sender_Unit.Sender_Unit_Duty;

   type Sender_State is (Unactivated, Activated);

   type Sender_Duty is new VN.Communication.CAN.Logic.Duty with
      record
         currentState 	: Sender_State := Unactivated;
         myCANAddress 	: VN.Communication.CAN.CAN_Address_Sender;
         sendBuffer   	: Send_Buffer_pack.Buffer(SIZE);
         units		: UnitArray;
         iterator	: Integer := UnitArray'First;
      end record;

   procedure GetFreeUnit(this : in out Sender_Duty; ret : out Sender_Unit_Duty_ptr);

end VN.Communication.CAN.Logic.Sender;
