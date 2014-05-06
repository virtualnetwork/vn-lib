with VN.Message;

package Logging.Empty_Logger is

   type Empty_Logger_Type is new Message_Logger with null record;

   procedure Log(This: in out Empty_Logger_Type;
                 Message: out VN.Message.VN_Message_Basic);

end Logging.Empty_Logger;
