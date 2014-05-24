with VN.Message;
with System;
with Ada.Text_IO;
with Buffers;
with Global_Settings;
with VN.Application_Information;
with VN.Message.Factory;
with VN.Message.Local_Hello;
with VN.Message.Assign_Address;
with Interfaces;

package Subnet_Manager_Local is

   task type SM_L(Pri : System.Priority;
                     Cycle_Time : Positive;
                     Task_ID : Positive;
                     Increment_By : Positive) is
      pragma Priority(Pri);
   end SM_L;

   private

      package Natural_Buffer is
         new Buffers(Natural);

      package Unsigned_8_Buffer is
         new Buffers(Interfaces.Unsigned_8);

      SM_L_Info: VN.Application_Information.VN_Application_Information;

      Basic_Msg: VN.Message.VN_Message_Basic;
      Local_Hello_Msg: VN.Message.Local_Hello.VN_Message_Local_Hello;
      Assign_Address_Msg: VN.Message.Assign_Address.VN_Message_Assign_Address;
      Request_Address_Block_Msg: VN.Message.Request_Address_Block.VN_Message_Request_Address_Block;

      Recv_Status: VN.Receive_Status;
      Send_Status: VN.Send_Status;

      Version: VN.Message.VN_Version;

      Temp_Uint8: Interfaces.Unsigned_8;

      Received_Address_Block : VN.VN_Logical_Address := 0;
      Assigned_Address : VN.VN_Logical_Address := 10;
      Assign_Address_Buffer: Unsigned_8_Buffer.Buffer(10);

      function Get_Address_To_Assign(CUUID_Uint8: in Interfaces.Unsigned_8)
         return VN.VN_Logical_Address;

      function Has_Received_Address_Block return Boolean;

end Subnet_Manager_Local;
