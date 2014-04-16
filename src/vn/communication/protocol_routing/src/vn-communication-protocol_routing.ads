
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary: 
-- Protocol_Routing is a middle layer between the subnets and the 
-- application layer. The Protocol_Routing package will decide on 
-- which subnet to send a VN message sent by the application layer
-- and whether a VN message received via a subnet shall be 
-- delivered to the application layer or sent to another
-- subnet, and if so, which one.

-- ToDo: Protocol_Routing_Type might not need to be protected?
-- ToDo: Only CAN subnet added so far.
-- ToDo: Some special cases not handled.

with VN;
with VN.Communication;
with VN.Communication.CAN;
with VN.Communication.CAN.CAN_Interface;
with VN.Communication.Routing_Table;
with VN.Communication.CUUID_Routing;

package VN.Communication.Protocol_Routing is

   type Protocol_Address_Type is (CAN_Subnet, Application_Layer);
   
   package Protocol_Router is new VN.Communication.Routing_Table(Protocol_Address_Type);
   use Protocol_Router;

   package CUUID_Protocol_Routing  is new VN.Communication.CUUID_Routing(Protocol_Address_Type);
   use CUUID_Protocol_Routing;

   --ToDo: These constants should be put in a config file of some sort
   PROTOCOL_ROUTING_TABLE_SIZE : constant VN.VN_Logical_Address := 500;

   protected type Protocol_Routing_Type(theCANInterface : VN.Communication.CAN.CAN_Interface.CAN_Interface_Access) is 
        new VN.Communication.Com with
      
      overriding procedure Send(Message: in VN.Message.VN_Message_Basic;
                                Status: out VN.Send_Status);

      overriding procedure Receive(Message : out VN.Message.VN_Message_Basic;
                                   Status: out VN.Receive_Status);

   private

      procedure Init; -- ToDo: For testing only!!!

      myCANInterface 	 : VN.Communication.CAN.CAN_Interface.CAN_Interface_Access := theCANInterface;
      myTable 		 : Protocol_Router.Table_Type(PROTOCOL_ROUTING_TABLE_SIZE);
      nextProtocolInTurn : Protocol_Address_Type;

      Initiated : Boolean := false; -- ToDo: for testing only!!!
   end Protocol_Routing_Type;

private


end VN.Communication.Protocol_Routing;
