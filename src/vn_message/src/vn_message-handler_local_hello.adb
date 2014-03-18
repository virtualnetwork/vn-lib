with VN_Message.Local_Hello;
with VN_Message.Local_Ack;
with Ada.Text_IO;

-- package VN_CUUID_Print is new Text_IO.Modular_IO(VN_Message.VN_CUUID);

package body VN_Message.Handler_Local_Hello is

   -- Parse
   procedure Parse(Message: in VN_Message.Local_Hello.VN_Message_Local_Hello) is
      -- Message_Local_Ack : VN_Message.Local_Ack.VN_Message_Local_Ack;
      CUUID : VN_CUUID;
   begin
      CUUID := Message.Get_CUUID;

      if CUUID = 16#FFFF_FFFF_DEAD_BEAF# then
         Ada.Text_IO.Put_Line("");
         Ada.Text_IO.Put_Line("CUUID ok, send Local_Ack with good status");
         Ada.Text_IO.Put_Line("");

      else
         Ada.Text_IO.Put_Line("");
         Ada.Text_IO.Put_Line("CUUID not ok, send Local_Ack with bad status!");
         Ada.Text_IO.Put_Line("");
      end if;

   end Parse;

end VN_Message.Handler_Local_Hello;
