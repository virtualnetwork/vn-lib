-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary: SM_Duty is an object that holds send and receive buffers
-- for VN messages that are to be sent, or have been received, over the CAN network.
-- SM_Duty holds instances of the classes that implement the state machines
-- of the VN-CAN protcol.
-- Please also note that this functionality is regarding a Subnet Manager for CAN (SM-CAN),
-- not an ordinary node.

with VN.Message;
with VN.Communication.CAN.CAN_Filtering;

with VN.Communication.CAN.Logic;
with VN.Communication.CAN.Logic.CAN_Address_Assignment;
with VN.Communication.CAN.Logic.CAN_Address_Reception;
with VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation;
with VN.Communication.CAN.Logic.Sender;
with VN.Communication.CAN.Logic.Receiver;
with VN.Communication.CAN.Logic.CUUID_Responder;
with VN.Communication.CAN.Logic.CUUID_Handler;

with VN.Communication.Routing_Table;
with VN.Communication.CUUID_Routing;

with Buffers;

package VN.Communication.CAN.Logic.SM is

   -- The routing table will map logical addresses to CAN_Address_Sender, not CAN_Address_Receiver
   -- The reason for this is that no VN-message will be sent to an address not referring to a
   -- single unit (as opposed to broadcast addresses) on the CAN network,
   -- i.e. to an address outside the CAN_Address_Sender range.
   package CAN_Routing is new VN.Communication.Routing_Table(VN.Communication.CAN.CAN_Address_Sender);
   use CAN_Routing;

   -- This routing table will map CUUIDs to CAN-addresses (CAN_Address_Sender, see above)
   package CUUID_CAN_Routing is new VN.Communication.CUUID_Routing(VN.Communication.CAN.CAN_Address_Sender);
   use CUUID_CAN_Routing;

   use VN.Communication.CAN.CAN_Message_Buffers;

   type Unit is
      record
         unitCANAddress : VN.Communication.CAN.CAN_Address_Sender;
         unitCUUID 	: VN.VN_CUUID;
         isSM_CAN	: Boolean;
      end record;

   package Unit_Buffers is new Buffers(Unit);
   use Unit_Buffers;


   type SM_Duty(theUCID   : access VN.Communication.CAN.UCID;
                theCUUID  : access VN.VN_CUUID;
                theFilter : VN.Communication.CAN.CAN_Filtering.CAN_Filter_Access) is limited private;

   type SM_Duty_ptr is access all SM_Duty;

   procedure Update(this : in out SM_Duty; msgsBuffer : in out CAN_Message_Buffers.Buffer;
                    ret : out CAN_Message_Buffers.Buffer);

   --This function is most likely obsolete:
   procedure Discover(this : in out SM_Duty; discoveredUnits : out Unit_Buffers.Buffer);

   procedure Send(this : in out SM_Duty; msg : VN.Message.VN_Message_Basic;
                                result : out VN.Send_Status);

   procedure Receive(this : in out SM_Duty; msg : out VN.Message.VN_Message_Basic;
                     status : out VN.Receive_Status);

   --This function is most likely obsolete:
   procedure GetCANAddress(this : in out SM_Duty; address : out CAN_Address_Sender; isAssigned : out boolean);

private

   procedure Init(this : in out SM_Duty);

   --ToDo: These constants should be put in a config file of some sort
   CAN_ROUTING_TABLE_SIZE : constant VN.VN_Logical_Address := 500;
   NUM_DUTIES : constant integer := 7;

   type ArrayOfDuties is array(1..NUM_DUTIES) of VN.Communication.CAN.Logic.Duty_Ptr;

   type SM_Duty(theUCID : access VN.Communication.CAN.UCID; theCUUID : access VN.VN_CUUID) is limited
      record

         isInitialized : Boolean := false;

         myUCID  : VN.Communication.CAN.UCID  := theUCID.all;
         myCUUID : VN.VN_CUUID := theCUUID.all;
         myTable : CAN_Routing.Table_Type(CAN_ROUTING_TABLE_SIZE);

         masterNegotiation : aliased VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation.SM_CAN_MN_Duty(theUCID, theFilter);

         addressReceiver : aliased VN.Communication.CAN.Logic.CAN_Address_Reception.CAN_Assignment_Node(theUCID, theFilter);

         assigner : aliased VN.Communication.CAN.Logic.CAN_Address_Assignment.CAN_Assignment_Master(theFilter);

         sender : aliased VN.Communication.CAN.Logic.Sender.Sender_Duty(theFilter);

         receiver : aliased VN.Communication.CAN.Logic.Receiver.Receiver_Duty(theFilter);
         ----theFilter : VN.Communication.CAN.CAN_Filtering.CAN_Filter_Access
         cuuidResponder : aliased VN.Communication.CAN.Logic.CUUID_Responder.CUUID_Responder(theFilter);

         cuuidHandler : aliased VN.Communication.CAN.Logic.CUUID_Handler.CUUID_Handler(theFilter);

         DutyArray : ArrayOfDuties;

         hasSent : boolean := false; --for testing only, set false to do the test
      end record;

end VN.Communication.CAN.Logic.SM;

