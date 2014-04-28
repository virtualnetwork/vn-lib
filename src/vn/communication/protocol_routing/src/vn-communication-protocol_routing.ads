
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

with VN.Communication.Routing_Table;
with VN.Communication.CUUID_Routing;

package VN.Communication.Protocol_Routing is

   type Protocol_Routing_Type is 
     new VN.Communication.Com with private;
      
   procedure Send(this : in out Protocol_Routing_Type; 
                             Message: in VN.Message.VN_Message_Basic;
                             Status: out VN.Send_Status);

   procedure Receive(this : in out Protocol_Routing_Type; 
                                Message : out VN.Message.VN_Message_Basic;
                                Status: out VN.Receive_Status);

private
   
   procedure Init(this : in out Protocol_Routing_Type); -- ToDo: For testing only!!!

   --ToDo: These constants should be put in a config file of some sort
   PROTOCOL_ROUTING_TABLE_SIZE : constant VN.VN_Logical_Address := 500;
   MAX_NUMBER_OF_SUBNETS : constant Integer := 10;
   
   subtype Protocol_Address_Type is Integer range 0 .. MAX_NUMBER_OF_SUBNETS; --the value 0 means Application Layer
   type Interface_Array is array(1..MAX_NUMBER_OF_SUBNETS) of VN.Communication.Com_Access;
   
   package Protocol_Router is new VN.Communication.Routing_Table(Protocol_Address_Type);
   use Protocol_Router;

   package CUUID_Protocol_Routing  is new VN.Communication.CUUID_Routing(Protocol_Address_Type);
   use CUUID_Protocol_Routing;

   type Protocol_Routing_Type is 
     new VN.Communication.Com with
      record
         myCANInterfaces    : Interface_Array;
         myTable 	    : Protocol_Router.Table_Type(PROTOCOL_ROUTING_TABLE_SIZE);
         nextProtocolInTurn : Protocol_Address_Type := Interface_Array'First;

         Initiated : Boolean := false; -- ToDo: for testing only!!!
      end record;

end VN.Communication.Protocol_Routing;
