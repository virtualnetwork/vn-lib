-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- VN.Communication.CAN.Logic is a package that implements the logic 
-- of the VN-CAN protocol itself. 
-- This file includes:
-- Definition of lowlevel datatypes, constants and operations as well as
-- definition of abstract base class Duty. Duty is a base class for
-- each of the state machines that constitute the implementation of
-- the VN-CAN protocol.


with Interfaces;
use Interfaces;
with VN.Message;

package VN.Communication.CAN.Logic is

   GIVE_DEBUG_OUTPUT : constant integer := 5;

   OFFSET_CAN_PRIORITY 	: constant Natural := 22;
   OFFSET_CAN_TYPE	: constant Natural := 15;
   OFFSET_CAN_RECEIVER 	: constant Natural := 7;
   OFFSET_CAN_SENDER 	: constant Natural := 0;

   procedure DebugOutput(str : String; level : Integer; newLine : boolean := true);


   type CAN_Message_Physical is
      record
         ID     : CAN_message_ID;
         Length : DLC_Type;
         Data   : Byte8;
      end record;


   ASSIGN_CAN_ADDRESS 	: CAN_Message_Type := 0;
   CAN_MASTER_ASSIGNED 	: CAN_Message_Type := 1;
   ADDRESS_QUESTION 	: CAN_Message_Type := 2;
   ADDRESS_ANSWER 	: CAN_Message_Type := 3;
   PING 		: CAN_Message_Type := 4;
   PONG 		: CAN_Message_Type := 5;
   START_TRANSMISSION 	: CAN_Message_Type := 6;
   FLOW_CONTROL 	: CAN_Message_Type := 7;
   TRANSMISSION 	: CAN_Message_Type := 8;
   REQUEST_CUUID	: CAN_Message_Type := 9;
   FIRST_CUUID_HALF 	: CAN_Message_Type := 10;
   SECOND_CUUID_HALF 	: CAN_Message_Type := 11;
   COMPONENT_TYPE	: CAN_Message_Type := 12;
   ASSIGN_LOGICAL_ADDR	: CAN_Message_Type := 13;

--     type DataArray is array(1..10) of Interfaces.Unsigned_8;
--     type DataArray is new String(1..50);

   type VN_Message_Internal is
      record
--           Data 		: DataArray;
         Data		: VN.Message.VN_Message_Basic;
         NumBytes	: Interfaces.Unsigned_16;
         Receiver 	: CAN_Address_Receiver;
         Sender		: CAN_Address_Sender;
      end record;

  -- procedure Assignment (destination : out VN_Message_Internal; source : in VN_Message_Internal);

   type Duty is abstract tagged limited private;

   procedure Update(me : in out Duty; msg : CAN_Message_Logical; bMsgReceived : boolean;
                    msgOut : out CAN_Message_Logical; bWillSend : out boolean) is abstract;

   type Duty_Ptr is access all Duty'Class;

private
     type Duty is abstract tagged limited
      record
         null;
      end record;
end VN.Communication.CAN.Logic;
