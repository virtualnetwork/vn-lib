with Ada.Finalization;
with VN_Message;

package Communication is

   type VN_Message_Buffer is range 1 .. 10;

   type Com is abstract tagged private;

   procedure Send(This: in out Com;
                  Message: in VN_Message.VN_Message_Basic)
                        is abstract;

   procedure Receive( This: in out Com;
                     Message: out VN_Message.VN_Message_Basic;
                     Is_Received: out Boolean)
                        is abstract;
private

   type Com is abstract new Ada.Finalization.Controlled with
      record
         Buffer: VN_Message_Buffer := 1;
      end record;

end Communication;
