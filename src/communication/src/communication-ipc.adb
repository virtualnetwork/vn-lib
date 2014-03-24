package body Communication.IPC is

   -- IPC Wrapper Send procedure
   procedure Send(This: in out IPC_Wrapper;
                  Message: in VN_Message.VN_Message_Basic;
                  Status: out VN_Message.Send_Status) is
   begin
      if This.Is_From_SM_L then
         This.PO_Access.Send_To_Other(Message, Status);
      else
         This.PO_Access.Send_To_SM_L(Message, Status);
      end if;
   end Send;

   -- IPC Wrapper Receive procedure
   procedure Receive( This: in out IPC_Wrapper;
                     Message: out VN_Message.VN_Message_Basic;
                     Status: out VN_Message.Receive_Status) is
   begin
      if This.Is_From_SM_L then
         This.PO_Access.Receive_From_Other(Message, Status);
      else
         This.PO_Access.Receive_From_SM_L(Message, Status);
      end if;
   end Receive;

end Communication.IPC;
