package body VN.Message.Assign_Address_Block is

   procedure To_Basic(Assign_Address_Block_VN_Msg: in VN_Message_Assign_Address_Block;
                      Basic_VN_Msg: out VN_Message_Basic) is
      tempMsg : VN_Message_Assign_Address_Block := Assign_Address_Block_VN_Msg;
      for tempMsg'Address use Basic_VN_Msg'Address;
   begin
      null;
   end To_Basic;

   procedure To_Assign_Address_Block(
                           Basic_VN_Msg: in VN_Message_Basic;
                           Assign_Address_Block_VN_Msg: out VN_Message_Assign_Address_Block) is
      tempMsg : VN_Message_Basic := Basic_VN_Msg;
      for tempMsg'Address use Assign_Address_Block_VN_Msg'Address;
   begin
      null;
   end To_Assign_Address_Block;

end VN.Message.Assign_Address_Block;
