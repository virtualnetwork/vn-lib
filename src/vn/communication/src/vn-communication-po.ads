with VN.Message;

package VN.Communication.PO is

   protected type VN_PO is

      procedure Receive_from_SM_L(Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Message.Receive_Status);
      procedure Send_to_SM_L(Message: in VN.Message.VN_Message_Basic;
                      Status: out VN.Message.Send_Status);

      procedure Receive_from_Other(Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Message.Receive_Status);
      procedure Send_to_Other(Message: in VN.Message.VN_Message_Basic;
                      Status: out VN.Message.Send_Status);

      function Get_Buffer_Length_To_SM_L return Integer;
      function Get_Buffer_Length_From_SM_L return Integer;

   private
      Buffer_Length_To_SM_L: Integer := 0;
      Buffer_Length_From_SM_L: Integer := 0;
--
--      VN_Msg_Buffer : VN_Buffer;
   end VN_PO;

   type VN_PO_Access is access all VN_PO;

end VN.Communication.PO;