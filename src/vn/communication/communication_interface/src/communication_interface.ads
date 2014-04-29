with VN.Communications.Com;

package Communication_Interface is

  type Com_Interface_Type is abstract tagged limited private; -- with new VN.Communications.Com;

  type Com_Interface_Access is access all Communication_Interface_Type'Class;

   procedure Send(This : in out Com_Interface_Type;
                  Message : in VN.Message.VN_Message_Basic;
                  Status : out VN.Send_Status) is abstract;

   procedure Receive(This : in out Com_Interface_Type;
                     Message : out VN.Message.VN_Message_Basic;
                     Status : out VN.Receive_Status) is abstract;

private
     type Com_Interface_Type is new VN.Communications.Com with abstract tagged limited null record;

end Communication_Interface;
