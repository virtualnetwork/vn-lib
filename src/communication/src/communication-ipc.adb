package body Communication.IPC is

   -- Com Send procedure
   procedure Send(This: in out Com_IPC;
                  Message: in VN_Message.VN_Message_Basic;
                  Status: out VN_Message.Send_Status) is
   begin
      Message_Queue.Write(Message, Status);
   end Send;

   -- Com Receive procedure
   procedure Receive( This: in out Com_IPC;
                     Message: out VN_Message.VN_Message_Basic;
                     Status: out VN_Message.Receive_Status) is
   begin
      Message_Queue.Read(Message, Status);
   end Receive;

   --- Protected buffer for all messages
   protected body VN_Message_Queue is
      procedure Read(Message: out VN_Message.VN_Message_Basic;
                     Status: out VN_Message.Receive_Status) is
      begin
         if Length > 0 then
            Message := VN_Msg_Buffer(Length);
            Length := Length - 1;
            Status := VN_Message.OK;
         end if;
            Status := VN_Message.ERROR_UNKNOWN;
      end Read;

      procedure Write(Message: in VN_Message.VN_Message_Basic;
                      Status: out VN_Message.Send_Status) is
      begin
         if Length < 10 then
            Length := Length + 1;
            VN_Msg_Buffer(Length) := Message;
            Status := VN_Message.OK;
         end if;
            Status := VN_Message.ERROR_UNKNOWN;
      end Write;

      function Get_Length return Integer is
      begin
         return Length;
      end Get_Length;
   end VN_Message_Queue;

end Communication.IPC;
