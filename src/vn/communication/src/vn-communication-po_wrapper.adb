with VN.Message;
with VN.Message.Factory;
with VN.Message.Local_Hello;
with VN.Message.Request_Address_Block;

package body VN.Communication.PO_Wrapper is

   -- PO Wrapper Send procedure
   procedure Send(This: in out VN_PO_Wrapper;
                  Message: in VN.Message.VN_Message_Basic;
                  Status: out VN.Send_Status) is
   begin
      if This.Is_From_SM_L then
         This.PO_Access.Send_To_Other(Message, Status);
      else
         This.PO_Access.Send_To_SM_L(Message, Status);
      end if;
   end Send;

   -- PO Wrapper Receive procedure
   procedure Receive(This: in out VN_PO_Wrapper;
                     Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Receive_Status) is
      use VN.Message;
      Basic_Msg         : VN.Message.VN_Message_Basic;
      Local_Hello_Msg   : VN.Message.Local_Hello.VN_Message_Local_Hello;
   begin
      if This.Is_From_SM_L then
         This.PO_Access.Receive_From_Other(Message, Status);

         if Message.Header.Opcode = VN.Message.OPCODE_LOCAL_HELLO and
               (Status = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or
                Status = VN.MSG_RECEIVED_MORE_AVAILABLE) then

            -- Reply with LOCAL_ACK
            This.Send_Local_Ack;

            VN.Message.Local_Hello.To_Local_Hello(Message, Local_Hello_Msg);

            -- Reply with Request_Address_Block if Local_Hello from CAS
            if Local_Hello_Msg.Component_Type = VN.Message.CAS then
               This.Send_Request_Address_Block;
            end if;
         end if;
      else
         This.PO_Access.Receive_From_SM_L(Message, Status);
      end if;
   end Receive;

   -- If not a Subnet Manager this application should send a LocalHello
   -- message.
   procedure Init(This: in out VN_PO_Wrapper) is
      Basic_Msg         : VN.Message.VN_Message_Basic;
      Local_Hello_Msg   : VN.Message.Local_Hello.VN_Message_Local_Hello;
      Status            : VN.Send_Status;
   begin
      Basic_Msg := VN.Message.Factory.Create(VN.Message.Type_Local_Hello);
      VN.Message.Local_Hello.To_Local_Hello(Basic_Msg, Local_Hello_Msg);
      Local_Hello_Msg.CUUID := This.CUUID;
      Local_Hello_Msg.Header.Source := VN.LOGICAL_ADDRES_UNKNOWN;
      Local_Hello_Msg.Header.Destination := VN.LOGICAL_ADDRES_UNKNOWN;
      Local_Hello_Msg.Component_Type := This.This_Component_Type;
      VN.Message.Local_Hello.To_Basic(Local_Hello_Msg, Basic_Msg);

      if This.Is_From_SM_L then
         -- Only applications send the Local_Hello message
         null;
      else
         This.PO_Access.Send_To_SM_L(Basic_Msg, Status);
      end if;
   end Init;

   -- If Subnet Manager send a LocalAck in response to LocalHello
   -- message.
   procedure Send_Local_Ack(This: in out VN_PO_Wrapper) is
      Basic_Msg         : VN.Message.VN_Message_Basic;
      Status            : VN.Send_Status;
   begin
      Basic_Msg := VN.Message.Factory.Create(VN.Message.Type_Local_Ack);
      Basic_Msg.Header.Source := VN.LOGICAL_ADDRES_UNKNOWN;
      Basic_Msg.Header.Destination := VN.LOGICAL_ADDRES_UNKNOWN;

      This.PO_Access.Send_To_Other(Basic_Msg, Status);

   end Send_Local_Ack;

   -- If Subnet Manager and Local_Hello is from CAS
   -- send a Request_Address_Block in response to LocalHello
   -- message.
   procedure Send_Request_Address_Block(This: in out VN_PO_Wrapper) is
      Basic_Msg         : VN.Message.VN_Message_Basic;
      Request_Address_Block_Msg : VN.Message.Request_Address_Block.VN_Message_Request_Address_Block;
      Status            : VN.Send_Status;
   begin
      Basic_Msg := VN.Message.Factory.Create(VN.Message.Type_Request_Address_Block);
      VN.Message.Request_Address_Block.To_Request_Address_Block(Basic_Msg, Request_Address_Block_Msg);
      Request_Address_Block_Msg.CUUID := This.CUUID;
      VN.Message.Request_Address_Block.To_Basic(Request_Address_Block_Msg, Basic_Msg);
      Basic_Msg.Header.Source := VN.LOGICAL_ADDRES_UNKNOWN;
      Basic_Msg.Header.Destination := VN.LOGICAL_ADDRES_UNKNOWN;

      This.PO_Access.Send_To_Other(Basic_Msg, Status);

   end Send_Request_Address_Block;

end VN.Communication.PO_Wrapper;
