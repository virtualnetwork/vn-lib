with VN.Message;
--  with Ada.Text_IO;
with VN.Communication.PO;
with VN.Communication.PO_Wrapper;
with VN.Communication.Temp_Routing_Table;
with VN.Communication.Temp_CUUID_Routing;

package VN.Communication.PO_Routing is

   --package Int_IO is new Ada.Text_IO.Integer_IO (Integer);

   -- This PO_Router routes traffic between multiple PO_Wrappers
   type PO_Router is new Com with Private;

   overriding
   procedure Send(This: in out PO_Router;
                  Message: in VN.Message.VN_Message_Basic;
                  Status: out VN.Send_Status);

   overriding
   procedure Receive(This: in out PO_Router;
                     Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Receive_Status);

   procedure Add_PO_Wrapper(This : in out PO_Router;
               PO_Wrapper_Access: VN.Communication.PO_Wrapper.PO_Wrapper_Access);

private
   PROTOCOL_ROUTING_TABLE_SIZE : constant VN.VN_Logical_Address := 500;
   MAX_NUMBER_OF_SUBNETS : constant Integer := 10;

   subtype Protocol_Address_Type is Integer range 0 .. MAX_NUMBER_OF_SUBNETS; --the value 0 means Application Layer
   type PO_Wrapper_Access_Array is array(1..MAX_NUMBER_OF_SUBNETS) of VN.Communication.PO_Wrapper.PO_Wrapper_Access;

   package Protocol_Router is new VN.Communication.Temp_Routing_Table(Protocol_Address_Type);
   use Protocol_Router;

   package CUUID_Protocol_Routing is new VN.Communication.Temp_CUUID_Routing(Protocol_Address_Type);
   use CUUID_Protocol_Routing;

   type PO_Router is new Com with
      record
        Number_Of_PO_Wrappers : Natural := 0;
        PO_Wrapper_Array      : PO_Wrapper_Access_Array;
        myTable               : Protocol_Router.Table_Type(PROTOCOL_ROUTING_TABLE_SIZE);
        nextProtocolInTurn    : Protocol_Address_Type := PO_Wrapper_Access_Array'First;
        Value: Integer := 0;
      end record;

end VN.Communication.PO_Routing;
