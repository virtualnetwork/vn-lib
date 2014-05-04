with VN.Message;
with VN.Message.Factory;
with VN.Message.Local_Hello;
with VN.Communication.PO_Wrapper;

package body VN.Communication.PO_Routing is

   -- PO Router Send procedure
   procedure Send(This: in out PO_Router;
                  Message: in VN.Message.VN_Message_Basic;
                  Status: out VN.Send_Status) is
   begin
      null;
   end Send;

   -- PO Router Receive procedure
   procedure Receive( This: in out PO_Router;
                     Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Receive_Status) is
   begin
      null;
   end Receive;

   -- PO Router Add procedure
   procedure Add_PO_Wrapper(This : in out PO_Router;
               PO_Wrapper_Access: VN.Communication.PO_Wrapper.PO_Wrapper_Access)
   is
      use VN.Communication.PO_Wrapper;
   begin
      if This.Number_Of_PO_Wrappers >= MAX_NUMBER_OF_SUBNETS then
         return;
      end if;

      for i in PO_Wrapper_Access_Array'First .. PO_Wrapper_Access_Array'First + This.Number_Of_PO_Wrappers - 1 loop
         if This.PO_Wrapper_Array(i) = PO_Wrapper_Access then
            return;
         end if;
      end loop;

      This.PO_Wrapper_Array(PO_Wrapper_Access_Array'First + This.Number_Of_PO_Wrappers) := PO_Wrapper_Access;
      This.Number_Of_PO_Wrappers := This.Number_Of_PO_Wrappers + 1;
   end Add_PO_Wrapper;

end VN.Communication.PO_Routing;
