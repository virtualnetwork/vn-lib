-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary: SM_Duty is an object that holds send and receive buffers
-- for VN messages that are to be sent, or have been received, over the CAN network.
-- SM_Duty holds instances of the classes that implement the state machines
-- of the VN-CAN protcol.


with VN.Communication.CAN.Logic;
with VN.Communication.CAN.Logic.CAN_Address_Assignment;
with VN.Communication.CAN.Logic.CAN_Address_Reception;
with VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation;
with VN.Communication.CAN.Logic.Sender;
with VN.Communication.CAN.Logic.Receiver;
with VN.Communication.CAN.Logic.CUUID_Responder;
with VN.Communication.CAN.Logic.CUUID_Handler;

with Buffers;

package VN.Communication.CAN.Logic.SM is

   package CAN_Message_Buffers is new Buffers(VN.Communication.CAN.CAN_Message_Logical);
   use CAN_Message_Buffers;

   type Unit is
      record
         unitCANAddress : VN.Communication.CAN.CAN_Address_Sender;
         unitCUUID 	: VN.VN_CUUID;
         isSM_CAN	: Boolean;
      end record;

   package Unit_Buffers is new Buffers(Unit);
   use Unit_Buffers;

   type SM_Duty(theUCID : access VN.Communication.CAN.UCID; theCUUID : access VN.VN_CUUID) is private;

   type SM_Duty_ptr is access all SM_Duty;

   procedure Update(this : in out SM_Duty; msgsBuffer : in out CAN_Message_Buffers.Buffer;
                    ret : out CAN_Message_Buffers.Buffer);

   procedure Discover(this : in out SM_Duty; discoveredUnits : out Unit_Buffers.Buffer);

   procedure Send(this : in out SM_Duty; msg : VN.Communication.CAN.Logic.VN_Message_Internal;
                                result : out VN.Send_Status);

   procedure Receive(this : in out SM_Duty; msg : out VN.Communication.CAN.Logic.VN_Message_Internal;
                     hasReceived : out boolean);

   procedure GetCANAddress(this : in out SM_Duty; address : out CAN_Address_Sender; isAssigned : out boolean);


   procedure Init(this : in out SM_Duty); -- has to be called before any other procedure

   --THIS IS JUST TESTING FUNCTIONALLITY FOR NODES, NOT SM-CANs
--     procedure GetLogicalAddress(this : in out SM_Duty; LogicalAddress : out VN.VN_Logical_Address;
--                                 isAssigned : out boolean);
--
--     procedure SetMyAddress(this : in out SM_Duty; LogicalAddress : VN.VN_Logical_Address);
--
--     procedure Assign(this : in out SM_Duty; CANAddress : CAN_Address_Sender;
--                      LogicalAddress : VN.VN_Logical_Address);
--
--     procedure AddressQuestion(this : in out SM_Duty; LogicalAddress : VN.VN_Logical_Address;
--                               CANAddress : out CAN_Address_Sender; wasFound : out boolean);


private

   NUM_DUTIES : constant integer := 7;
   type ArrayOfDuties is array(1..NUM_DUTIES) of VN.Communication.CAN.Logic.Duty_Ptr;

   type SM_Duty(theUCID : access VN.Communication.CAN.UCID; theCUUID : access VN.VN_CUUID) is
      record

         myUCID  : VN.Communication.CAN.UCID  := theUCID.all;
         myCUUID : VN.VN_CUUID := theCUUID.all;

         masterNegotiation : VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation.SM_CAN_MN_Duty_ptr :=
           new VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation.SM_CAN_MN_Duty(theUCID);

         addressReceiver : VN.Communication.CAN.Logic.CAN_Address_Reception.CAN_Assignment_Node_ptr :=
           new VN.Communication.CAN.Logic.CAN_Address_Reception.CAN_Assignment_Node(theUCID);

         assigner : VN.Communication.CAN.Logic.CAN_Address_Assignment.CAN_Assignment_Master_ptr :=
           new VN.Communication.CAN.Logic.CAN_Address_Assignment.CAN_Assignment_Master;

         sender : VN.Communication.CAN.Logic.Sender.Sender_Duty_ptr :=
           new VN.Communication.CAN.Logic.Sender.Sender_Duty;

         receiver : VN.Communication.CAN.Logic.Receiver.Receiver_Duty_ptr :=
           new VN.Communication.CAN.Logic.Receiver.Receiver_Duty;

         cuuidResponder : VN.Communication.CAN.Logic.CUUID_Responder.CUUID_Responder_ptr :=
           new VN.Communication.CAN.Logic.CUUID_Responder.CUUID_Responder;

         cuuidHandler : VN.Communication.CAN.Logic.CUUID_Handler.CUUID_Handler_ptr :=
           new VN.Communication.CAN.Logic.CUUID_Handler.CUUID_Handler;

         DutyArray : ArrayOfDuties;

         hasSent : boolean := true; --for testing
      end record;

end VN.Communication.CAN.Logic.SM;

