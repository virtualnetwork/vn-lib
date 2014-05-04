with VN.Message;

package body Logger.Empty_Logger is

   procedure Log(This: in out Empty_Logger_Type;
                 Message: out VN.Message.VN_Message_Basic) is
   begin
      null;
   end Log;

end Logger.Empty_Logger;
