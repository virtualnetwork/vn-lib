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
   begin
      null;
   end Add_PO_Wrapper;

end VN.Communication.PO_Routing;
