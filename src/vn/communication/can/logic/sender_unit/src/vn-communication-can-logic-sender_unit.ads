-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Implements the state machine for sending a single VN message. Sender_Duty will
-- use an instane of Sender_Unit_Duty to send a VN message.
-- Before it can be used, Sender_Unit_Duty will need to be activated. This cannot
-- be done until one has been assigned a CAN address.

-- ToDo: Fragment must be implemented further

with VN.Communication.CAN.Logic;
with Interfaces;
with Ada.Real_Time;

package VN.Communication.CAN.Logic.Sender_Unit is

   type Sender_Unit_Duty is new VN.Communication.CAN.Logic.Duty with private;
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
   type Sender_Unit_Duty is new VN.Communication.CAN.Logic.Duty with
      record
         currentState 	: Sender_Unit_State := Idle;
         myCANAddress 	: VN.Communication.CAN.CAN_Address_Sender;
         ToSend  	: VN.Communication.CAN.Logic.VN_Message_Internal;
         useFlowControl : Boolean;
         blockSize  	: Interfaces.Unsigned_16;
         blockCount	: Interfaces.Unsigned_16; -- counts the number of Transmission message sent in this block
         sequenceNumber : Interfaces.Unsigned_16; -- will equal 0 when the first Transmission message is sent, is incremented after the message is sent
         time 		: Ada.Real_Time.Time;
      end record;

   function NumMessagesToSend(messageLength : Interfaces.Unsigned_16) return Interfaces.Unsigned_16;

   procedure Fragment(VNMessage : VN.Communication.CAN.Logic.VN_Message_Internal;
                      seqNumber : in out Interfaces.Unsigned_16;
                      CANMessage : in out VN.Communication.CAN.CAN_Message_Logical; isLastMessage : out boolean);

end VN.Communication.CAN.Logic.Sender_Unit;

