-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary: SM_Duty is an object that holds send and receive buffers
-- for VN messages that are to be sent, or have been received, over the CAN network.
-- SM_Duty holds instances of the classes that implement the state machines
-- of the VN-CAN protcol.
-- Please also note that this functionality is regarding a Subnet Manager for CAN (SM-CAN),
-- not an ordinary node.

--ToDo: Right now no routing information is retreived from DistributeRoute messages!!
--ToDo: Send LocalHello messages when detecting a new SM-CAN
--ToDo: Send LocalAck messages when receiving a LocalHello message

with VN.Communication.CAN.Logic;
with VN.Communication.CAN.Logic.CAN_Address_Assignment;
with VN.Communication.CAN.Logic.CAN_Address_Reception;
with VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation;
with VN.Communication.CAN.Logic.Sender;
with VN.Communication.CAN.Logic.Receiver;
with VN.Communication.CAN.Logic.CUUID_Responder;
with VN.Communication.CAN.Logic.CUUID_Handler;

with VN.Communication.Routing_Table;

with Buffers;

package VN.Communication.CAN.Logic.SM is

   -- The routing table will map logical addresses to CAN_Address_Sender, not CAN_Address_Receiver
   -- The reason for this is that no VN-message will be sent to an address not referring to a
   -- single unit (as opposed to broadcast addresses) on the CAN network,
   -- i.e. to an address outside the CAN_Address_Sender range.
   package CAN_Routing is new VN.Communication.Routing_Table(VN.Communication.CAN.CAN_Address_Sender);
   use CAN_Routing;

   use VN.Communication.CAN.CAN_Message_Buffers;

   type Unit is
      record
         unitCANAddress : VN.Communication.CAN.CAN_Address_Sender;
         unitCUUID 	: VN.VN_CUUID;
         isSM_CAN	: Boolean;
      end record;

   package Unit_Buffers is new Buffers(Unit);
   use Unit_Buffers;


   type SM_Duty(theUCID : access VN.Communication.CAN.UCID; theCUUID : access VN.VN_CUUID) is limited private;

   type SM_Duty_ptr is access all SM_Duty;

   procedure Update(this : in out SM_Duty; msgsBuffer : in out CAN_Message_Buffers.Buffer;
                    ret : out CAN_Message_Buffers.Buffer);

   --This function should be private sooner or later?
   procedure Discover(this : in out SM_Duty; discoveredUnits : out Unit_Buffers.Buffer);

   procedure Send(this : in out SM_Duty; msg : VN.Message.VN_Message_Basic; --VN.Communication.CAN.Logic.VN_Message_Internal;
                                result : out VN.Send_Status);

   procedure Receive(this : in out SM_Duty; msg : out VN.Message.VN_Message_Basic; --VN.Communication.CAN.Logic.VN_Message_Internal;
                     status : out VN.Receive_Status);

   --This function is most likely obsolete:
   procedure GetCANAddress(this : in out SM_Duty; address : out CAN_Address_Sender; isAssigned : out boolean);

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

   procedure Init(this : in out SM_Duty);

   --ToDo: These constants should be put in a config file of some sort
   CAN_ROUTING_TABLE_SIZE : constant VN.VN_Logical_Address := 500;
   NUM_DUTIES : constant integer := 7;

   procedure HelloProc(CANAddress : VN.Communication.CAN.CAN_Address_Sender;
                       isSM_CAN : Boolean);

   type ArrayOfDuties is array(1..NUM_DUTIES) of VN.Communication.CAN.Logic.Duty_Ptr;

   type SM_Duty(theUCID : access VN.Communication.CAN.UCID; theCUUID : access VN.VN_CUUID) is limited
      record

         isInitialized : Boolean := false;

         myUCID  : VN.Communication.CAN.UCID  := theUCID.all;
         myCUUID : VN.VN_CUUID := theCUUID.all;
         myTable : CAN_Routing.Table_Type(CAN_ROUTING_TABLE_SIZE);


           masterNegotiation : aliased VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation.SM_CAN_MN_Duty(theUCID);
--           masterNegotiation : VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation.SM_CAN_MN_Duty_ptr :=
--             new VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation.SM_CAN_MN_Duty(theUCID);

         addressReceiver : aliased VN.Communication.CAN.Logic.CAN_Address_Reception.CAN_Assignment_Node(theUCID);
--           addressReceiver : VN.Communication.CAN.Logic.CAN_Address_Reception.CAN_Assignment_Node_ptr :=
--             new VN.Communication.CAN.Logic.CAN_Address_Reception.CAN_Assignment_Node(theUCID);

         assigner : aliased VN.Communication.CAN.Logic.CAN_Address_Assignment.CAN_Assignment_Master;
--           assigner : VN.Communication.CAN.Logic.CAN_Address_Assignment.CAN_Assignment_Master_ptr :=
--             new VN.Communication.CAN.Logic.CAN_Address_Assignment.CAN_Assignment_Master;

         sender : aliased VN.Communication.CAN.Logic.Sender.Sender_Duty;
--           sender : VN.Communication.CAN.Logic.Sender.Sender_Duty_ptr :=
--             new VN.Communication.CAN.Logic.Sender.Sender_Duty;

         receiver : aliased VN.Communication.CAN.Logic.Receiver.Receiver_Duty;
--           receiver : VN.Communication.CAN.Logic.Receiver.Receiver_Duty_ptr :=
--             new VN.Communication.CAN.Logic.Receiver.Receiver_Duty;

         cuuidResponder : aliased VN.Communication.CAN.Logic.CUUID_Responder.CUUID_Responder;
--           cuuidResponder : VN.Communication.CAN.Logic.CUUID_Responder.CUUID_Responder_ptr :=
--             new VN.Communication.CAN.Logic.CUUID_Responder.CUUID_Responder;

         cuuidHandler : aliased VN.Communication.CAN.Logic.CUUID_Handler.CUUID_Handler(HelloProc'Access);
--           cuuidHandler : VN.Communication.CAN.Logic.CUUID_Handler.CUUID_Handler_ptr :=
--             new VN.Communication.CAN.Logic.CUUID_Handler.CUUID_Handler(HelloProc'Access);

         DutyArray : ArrayOfDuties;

         hasSent : boolean := true; --for testing only
      end record;

end VN.Communication.CAN.Logic.SM;

