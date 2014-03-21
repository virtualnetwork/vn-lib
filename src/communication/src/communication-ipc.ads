with VN_Message;
with Ada.Text_IO;

package Communication.IPC is

   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);

   -- TODO: Define a Protected object type here that is used for this wrapper.
   -- VN_Message buffer that is.
   type Com_IPC is new Com with private;

   procedure Send(This: in out Com_IPC;
                  Message: in VN_Message.VN_Message_Basic;
                  Status: out VN_Message.Send_Status);

   procedure Receive( This: in out Com_IPC;
                     Message: out VN_Message.VN_Message_Basic;
                     Status: out VN_Message.Receive_Status);
private

   -- TODO: Modify code so the buffer is of variable length.
   type VN_Buffer is array (1 .. 10) of VN_Message.VN_Message_Basic;

   type Com_IPC is new Com with
      record
         Value: Integer := 0;
      end record;

   protected type VN_Message_Queue is
      procedure Read(Message: out VN_Message.VN_Message_Basic;
                     Status: out VN_Message.Receive_Status);
      procedure Write(Message: in VN_Message.VN_Message_Basic;
                      Status: out VN_Message.Send_Status);
      function Get_Length return Integer;
   private
      Length: Integer := 0;
      VN_Msg_Buffer : VN_Buffer;
   end VN_Message_Queue;

   Message_Queue: VN_Message_Queue;

end Communication.IPC;
