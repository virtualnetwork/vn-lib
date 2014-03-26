with Ada.Finalization;
with VN.Message;

package VN.Communication is

   -- TODO: Modify code so the buffer is of variable length.
   type VN_Message_Buffer is range 1 .. 10;
   -- type VN_Message_Buffer is array (1 .. 10) of VN_Message.VN_Message_Basic;
   -- TODO: Fix this VN_Buffer so it's a buffer of access variables to
   -- VN_Message'Class instead.

   type Com is abstract tagged private;

   procedure Send(This: in out Com;
                  Message: in VN.Message.VN_Message_Basic;
                  Status: out VN.Message.Send_Status)
                        is abstract;

   procedure Receive( This: in out Com;
                     Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Message.Receive_Status)
                        is abstract;
private

   type Com is abstract new Ada.Finalization.Controlled with
      record
         Buffer: VN_Message_Buffer := 1;
      end record;

end VN.Communication;
