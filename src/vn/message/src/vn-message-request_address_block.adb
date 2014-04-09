package body VN.Message.Request_Address_Block is

   procedure To_Basic(Request_Address_Block_VN_Msg: in VN_Message_Request_Address_Block;
                      Basic_VN_Msg: out VN_Message_Basic) is
      tempMsg : VN_Message_Request_Address_Block := Request_Address_Block_VN_Msg;
      for tempMsg'Address use Basic_VN_Msg'Address;
   begin
      null;
   end To_Basic;

   procedure To_Request_Address_Block(
                           Basic_VN_Msg: in VN_Message_Basic;
                           Request_Address_Block_VN_Msg: out VN_Message_Request_Address_Block) is
      tempMsg : VN_Message_Basic := Basic_VN_Msg;
      for tempMsg'Address use Request_Address_Block_VN_Msg'Address;
   begin
      null;
   end To_Request_Address_Block;

end VN.Message.Request_Address_Block;
