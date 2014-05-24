with Ada.Real_Time;
with Ada.Text_IO;
with Global_Settings;
with VN.Application_Information;
with VN.Message.Factory;
with VN.Message.Local_Hello;

package body Central_Addressing_Service is

   task body CAS is
      use Ada.Real_Time;
      use VN;
      use VN.Message.Local_Hello;

      i: Integer := 1;

      App_Info: VN.Application_Information.VN_Application_Information;

      Basic_Msg: VN.Message.VN_Message_Basic;
      Local_Hello_Msg: VN.Message.Local_Hello.VN_Message_Local_Hello;

      Recv_Status: VN.Receive_Status;
      Send_Status: VN.Send_Status;

      Next_Period : Ada.Real_Time.Time;
      Period : constant Ada.Real_Time.Time_Span :=
                           Ada.Real_Time.Microseconds(Cycle_Time);
   begin
      App_Info.Logical_Address := 1;
      App_Info.Component_Type := VN.Message.Other;

      Global_Settings.Start_Time.Get(Next_Period);
      Ada.Text_IO.Put_Line("CAS  STAT: Starts.");

      ----------------------------
      loop
         delay until Next_Period;

         ----------------------------
         -- Receive loop
         ----------------------------
         Global_Settings.Com_CAS.Receive(Basic_Msg, Recv_Status);

         if Recv_Status = VN.NO_MSG_RECEIVED then
            Ada.Text_IO.Put_Line("CAS  RECV: Empty.");
         elsif Recv_Status = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or
            Recv_Status = VN.MSG_RECEIVED_MORE_AVAILABLE    then

            Ada.Text_IO.Put("CAS  RECV: ");
            Global_Settings.Logger.Log(Basic_Msg);

         --   if Basic_Msg.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR then
         --      To_Assign_Address(Basic_Msg, Assign_Address_Msg);
         --      App_Info.Logical_Address := Assign_Address_Msg.Assigned_Address;
         --   end if;

         end if;

         ----------------------------
         -- Send loop
         ----------------------------
         --if App_Info.Has_Logical_Address then
         --   null;
         --elsif not App_Info.Has_Logical_Address then
         --   null;
         --end if


         Next_Period := Next_Period + Period;
         i := i + 1;
         exit when i = 6;
      end loop;
      ----------------------------

      Ada.Text_IO.Put_Line("CAS  STAT: Stop. Logical Address: " &
                                 App_Info.Logical_Address'Img);

   end CAS;

   CAS1: CAS(20, 5000000, 10, 3);

end Central_Addressing_Service;
