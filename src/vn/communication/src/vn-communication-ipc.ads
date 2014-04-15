with VN.Message;
with Ada.Text_IO;
with VN.Communication.PO;

package VN.Communication.IPC is

   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);

   -- TODO: Define a Protected object type here that is used for this wrapper.
   -- VN_Message buffer that is.
   type IPC_Wrapper(VN_PO_Access: VN.Communication.PO.VN_PO_Access;
                    Is_SM_L: Boolean)
                     is new Com with Private;

   procedure Send(This: in out IPC_Wrapper;
                  Message: in VN.Message.VN_Message_Basic;
                  Status: out VN.Send_Status);

   procedure Receive(This: in out IPC_Wrapper;
                     Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Receive_Status);
private

   -- TODO: Modify code so the buffer is of variable length.
   type VN_Buffer is array (1 .. 10) of VN.Message.VN_Message_Basic;

   type IPC_Wrapper(VN_PO_Access: VN.Communication.PO.VN_PO_Access;
                    Is_SM_L: Boolean)
                     is new Com with
      record
         PO_Access: VN.Communication.PO.VN_PO_Access := VN_PO_Access;
         Is_From_SM_L: Boolean := Is_SM_L;
         Value: Integer := 0;
      end record;

end VN.Communication.IPC;
