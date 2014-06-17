with VN.Message;
with Buffers;

package VN.Communication is

   package VN_Message_Buffer is
         new Buffers(VN.Message.VN_Message_Basic);

   -- The Com interface is inherited by all subnet protocols and by the Protocol routing layer.
   type Com is limited interface;

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
