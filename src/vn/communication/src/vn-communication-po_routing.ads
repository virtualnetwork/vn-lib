with VN.Message;
with Ada.Text_IO;
with VN.Communication.PO;
with VN.Communication.PO_Wrapper;

package VN.Communication.PO_Routing is

   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);

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

   type PO_Router is new Com with
      record
         Value: Integer := 0;
      end record;

end VN.Communication.PO_Routing;
