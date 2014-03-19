with Ada.Finalization;
with VN_Message;

package Communication is

   type Type_Buffer is range 1 .. 10;
   type Com is abstract tagged private;

   procedure Send(This: in out Com;
                  Message: in VN_Message.VN_Message_Basic)
                        is abstract;

   function Receive( This: in out Com;
                     Message: out VN_Message.VN_Message_Basic;
                     Is_Received: out Boolean)
                        is abstract;
private

   type Com is new Ada.Finalization.Controlled with
      record
         Buffer: Type_Buffer := 1;
      end record;

end Communication;
