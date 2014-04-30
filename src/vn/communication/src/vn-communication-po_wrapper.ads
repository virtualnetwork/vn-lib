with VN.Message;
with Ada.Text_IO;
with VN.Communication.PO;

package VN.Communication.PO_Wrapper is

   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);

   -- This PO_Wrapper makes it possible to wrap two different Protected
   -- Objects in one "wrapper". The two different Protected Objects are
   -- buffers for in and outgoing traffic respectively.
   type VN_PO_Wrapper(VN_PO_Access: VN.Communication.PO.VN_PO_Access;
                      CUUID_Access: access VN.VN_CUUID;
                      Component_Type: VN.Message.VN_Component_Type;
                    Is_SM_L: Boolean)
                     is new Com with Private;

   procedure Send(This: in out VN_PO_Wrapper;
                  Message: in VN.Message.VN_Message_Basic;
                  Status: out VN.Send_Status);

   procedure Receive(This: in out VN_PO_Wrapper;
                     Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Receive_Status);

   procedure Init(This: in out VN_PO_Wrapper);

private

   type VN_Buffer is array (1 .. 10) of VN.Message.VN_Message_Basic;

   type VN_PO_Wrapper(VN_PO_Access: VN.Communication.PO.VN_PO_Access;
                      CUUID_Access: access VN.VN_CUUID;
                      Component_Type: VN.Message.VN_Component_Type;
                    Is_SM_L: Boolean)
                     is new Com with
      record
         PO_Access: VN.Communication.PO.VN_PO_Access := VN_PO_Access;
         Is_From_SM_L: Boolean := Is_SM_L;
         CUUID:  VN.VN_CUUID := CUUID_Access.all;
         This_Component_Type:  VN.Message.VN_Component_Type := Component_Type;
         Value: Integer := 0;
      end record;

end VN.Communication.PO_Wrapper;
