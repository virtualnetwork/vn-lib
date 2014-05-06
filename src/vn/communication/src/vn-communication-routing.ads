with VN.Message;
with VN.Communication.Temp_Routing_Table;
with VN.Communication.Temp_CUUID_Routing;

package VN.Communication.Routing is

   -- This Router routes traffic between multiple Com Objects
   type Router is new Com with Private;

   overriding
   procedure Send(This: in out Router;
                  Message: in VN.Message.VN_Message_Basic;
                  Status: out VN.Send_Status);

   overriding
   procedure Receive(This: in out Router;
                     Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Receive_Status);

   procedure Add_Com(This : in out Router;
               Com_Ptr: VN.Communication.Com_Access);

private
   PROTOCOL_ROUTING_TABLE_SIZE : constant VN.VN_Logical_Address := 500;
   MAX_NUMBER_OF_SUBNETS : constant Integer := 10;

   subtype Protocol_Address_Type is Integer range 0 .. MAX_NUMBER_OF_SUBNETS; --the value 0 means Application Layer
   type Com_Access_Array is array(1..MAX_NUMBER_OF_SUBNETS) of VN.Communication.Com_Access;

   package Protocol_Router is new VN.Communication.Temp_Routing_Table(Protocol_Address_Type);
   use Protocol_Router;

   package CUUID_Protocol_Routing is new VN.Communication.Temp_CUUID_Routing(Protocol_Address_Type);
   use CUUID_Protocol_Routing;

   type Router is new Com with
      record
        Number_Of_Routes      : Natural := 0;
        Array_Of_Routes       : Com_Access_Array;
        myTable               : Protocol_Router.Table_Type(PROTOCOL_ROUTING_TABLE_SIZE);
        nextProtocolInTurn    : Protocol_Address_Type := Com_Access_Array'First;
      end record;

end VN.Communication.Routing;
