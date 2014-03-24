with VN_Message;

package body Communication.PO is

   protected body VN_PO is

      procedure Receive_From_SM_L(Message: out VN_Message.VN_Message_Basic;
                     Status: out VN_Message.Receive_Status) is
      begin
         null; -- TODO: Implement
      end Receive_From_SM_L;

      procedure Send_To_SM_L(Message: in VN_Message.VN_Message_Basic;
                      Status: out VN_Message.Send_Status) is
      begin
         null; -- TODO: Implement
      end Send_To_SM_L;

      procedure Receive_From_Other(Message: out VN_Message.VN_Message_Basic;
                     Status: out VN_Message.Receive_Status) is
      begin
         null; -- TODO: Implement
      end Receive_From_Other;

      procedure Send_To_Other(Message: in VN_Message.VN_Message_Basic;
                              Status: out VN_Message.Send_Status) is
      begin
         null; -- TODO: Implement
      end Send_To_Other;

      -- Get buffer length
      function Get_Buffer_Length_From_SM_L return Integer is
      begin
         return Buffer_Length_From_SM_L;
      end Get_Buffer_Length_From_SM_L;

      -- Get buffer length
      function Get_Buffer_Length_To_SM_L return Integer is
      begin
         return Buffer_Length_To_SM_L;
      end Get_Buffer_Length_To_SM_L;

   end VN_PO;

end Communication.PO;
