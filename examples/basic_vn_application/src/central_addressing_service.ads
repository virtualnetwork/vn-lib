with VN.Message;
with System;
with Buffers;
with Global_Settings;
with VN.Application_Information;
with VN.Message.Factory;
with VN.Message.Local_Hello;
with VN.Message.Assign_Address_Block;
with VN.Message.Request_Address_Block;
with Interfaces;

package Central_Addressing_Service is

   task type CAS(Pri : System.Priority;
                     Cycle_Time : Positive;
                     Task_ID : Positive;
                     Increment_By : Positive) is
      pragma Priority(Pri);
   end CAS;

   private

      package Unsigned_8_Buffer is
         new Buffers(Interfaces.Unsigned_8);

      CAS_Info: VN.Application_Information.VN_Application_Information;

      Basic_Msg: VN.Message.VN_Message_Basic;
      Local_Hello_Msg: VN.Message.Local_Hello.VN_Message_Local_Hello;
      Request_Address_Block_Msg: VN.Message.Request_Address_Block.VN_Message_Request_Address_Block;
      Assign_Address_Block_Msg: VN.Message.Assign_Address_Block.VN_Message_Assign_Address_Block;

      Recv_Status: VN.Receive_Status;
      Send_Status: VN.Send_Status;

      Temp_Uint8: Interfaces.Unsigned_8;
      Assigned_Address_Block : VN.VN_Logical_Address := 65535;

      -- TODO: Change this buffer to some kind of data store.
      Assign_Address_Block_Buffer: Unsigned_8_Buffer.Buffer(10);

end Central_Addressing_Service;
