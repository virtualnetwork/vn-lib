with VN.Message;

package body Logging.Empty_Logger is

   procedure Log(This: in out Empty_Logger_Type;
                 Message: out VN.Message.VN_Message_Basic) is
   begin
      null;
   end Log;

end Logging.Empty_Logger;
