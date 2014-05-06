with VN.Message;

package VN.Communication.PO is

   protected type VN_PO is

      procedure Receive_from_SM_L(Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Receive_Status);
      procedure Send_to_SM_L(Message: in VN.Message.VN_Message_Basic;
                      Status: out VN.Send_Status);

      procedure Receive_from_Other(Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Receive_Status);
      procedure Send_to_Other(Message: in VN.Message.VN_Message_Basic;
                      Status: out VN.Send_Status);

      function Get_Buffer_Length_To_SM_L return Integer;
      function Get_Buffer_Length_To_Other return Integer;

   private
      Buffer_Length_To_SM_L: Integer := 0;
      Buffer_Length_To_Other: Integer := 0;

      Buffer_To_SM_L: VN_Message_Buffer.Buffer(10);
      Buffer_To_Other: VN_Message_Buffer.Buffer(10);
   end VN_PO;

   type VN_PO_Access is access all VN_PO;

end VN.Communication.PO;
