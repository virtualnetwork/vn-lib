with VN.Message;

package Logging is

   type Message_Logger is limited interface;
   type Message_Logger_Access is access all Message_Logger'Class;

   procedure Log(This: in out Message_Logger;
                 Message: out VN.Message.VN_Message_Basic) is abstract;

end Logging;
