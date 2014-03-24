with VN_Message;
with Ada.Text_IO;
with Communication.PO;

package Communication.IPC is

   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);

   -- TODO: Define a Protected object type here that is used for this wrapper.
   -- VN_Message buffer that is.
   type IPC_Wrapper(VN_PO_Access: Communication.PO.VN_PO_Access;
                    Is_SM_L: Boolean)
                     is new Com with Private;

   procedure Send(This: in out IPC_Wrapper;
                  Message: in VN_Message.VN_Message_Basic;
                  Status: out VN_Message.Send_Status);

   procedure Receive(This: in out IPC_Wrapper;
                     Message: out VN_Message.VN_Message_Basic;
                     Status: out VN_Message.Receive_Status);
private

   -- TODO: Modify code so the buffer is of variable length.
   type VN_Buffer is array (1 .. 10) of VN_Message.VN_Message_Basic;

   type IPC_Wrapper(VN_PO_Access: Communication.PO.VN_PO_Access;
                    Is_SM_L: Boolean)
                     is new Com with
      record
         PO_Access: Communication.PO.VN_PO_Access := VN_PO_Access;
         Is_From_SM_L: Boolean := Is_SM_L;
         Value: Integer := 0;
      end record;

end Communication.IPC;
