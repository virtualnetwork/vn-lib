with VN.Message;

package VN.Communication is

   -- TODO: Modify code so the buffer is of variable length.
--   type VN_Message_Buffer is range 1 .. 10;
   -- type VN_Message_Buffer is array (1 .. 10) of VN_Message.VN_Message_Basic;
   -- TODO: Fix this VN_Buffer so it's a buffer of access variables to
   -- VN_Message'Class instead.

   type Com is limited interface; --changed "protected" to "limited"

   type Com_Access is access all Com'Class;

   procedure Send(This: in out Com;
                  Message: in VN.Message.VN_Message_Basic;
                  Status: out VN.Send_Status)
                        is abstract;

   procedure Receive(This: in out Com;
                     Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Receive_Status)
                        is abstract;
end VN.Communication;
