with VN;
with VN.Message;
with VN.Message.Factory;
with VN.Message.Local_Hello;

package body VN.Communication.PO_Wrapper is

   -- PO Wrapper Send procedure
   procedure Send(This: in out VN_PO_Wrapper;
                  Message: in VN.Message.VN_Message_Basic;
                  Status: out VN.Send_Status) is
   begin
      if This.Is_From_SM_L then
         VN.Text_IO.Put_Line("PO_Wrapper sends message to Other");
         This.PO_Access.Send_To_Other(Message, Status);
      else
         VN.Text_IO.Put_Line("PO_Wrapper sends message to SM-L");
         This.PO_Access.Send_To_SM_L(Message, Status);
      end if;
   end Send;

   -- PO Wrapper Receive procedure
   procedure Receive( This: in out VN_PO_Wrapper;
                     Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Receive_Status) is
   begin
   --   VN.Text_IO.Put_Line("PO_Wrapper receive subprogram runs");
      if This.Is_From_SM_L then
         This.PO_Access.Receive_From_Other(Message, Status);
       --  VN.Text_IO.Put_Line("PO_Wrapper reads message from Other");
      else
         This.PO_Access.Receive_From_SM_L(Message, Status);
      --   VN.Text_IO.Put_Line("PO_Wrapper reads message from SM-L");
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
      Local_Hello_Msg.Component_Type := This.This_Component_Type;
      VN.Message.Local_Hello.To_Basic(Local_Hello_Msg, Basic_Msg);

      if This.Is_From_SM_L then
         -- Only applications send the Local_Hello message
         null;
      else
         This.PO_Access.Send_To_SM_L(Basic_Msg, Status);
         VN.Text_IO.Put_Line("PO_Wrapper sends LocalHello message to SM-L");
      end if;
   end Init;

end VN.Communication.PO_Wrapper;
