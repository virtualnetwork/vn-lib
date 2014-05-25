with Ada.Real_Time;
with Global_Settings;
with VN.Application_Information;
with VN.Message.Factory;
with VN.Message.Local_Hello;
with VN.Message.Assign_Address;

package body Application is

   task body VN_Application is
      use Ada.Real_Time;
      use VN;
      use VN.Message;
      use VN.Message.Local_Hello;
      use VN.Message.Assign_Address;

      i: Integer := 1;

      App_Info: VN.Application_Information.VN_Application_Information;

      Basic_Msg: VN.Message.VN_Message_Basic;
      Local_Hello_Msg: VN.Message.Local_Hello.VN_Message_Local_Hello;
      Assign_Address_Msg: VN.Message.Assign_Address.VN_Message_Assign_Address;

      Recv_Status: VN.Receive_Status;
      Send_Status: VN.Send_Status;

      Next_Period : Ada.Real_Time.Time;
      Period : constant Ada.Real_Time.Time_Span :=
                           Ada.Real_Time.Microseconds(Cycle_Time);
   begin
      App_Info.Component_Type := VN.Message.Other;
      App_Info.Logical_Address := VN.LOGICAL_ADDRES_UNKNOWN;

      Global_Settings.Start_Time.Get(Next_Period);
      VN.Text_IO.Put_Line("APPL STAT: Starts.");

      ----------------------------
      loop
         delay until Next_Period;

         ----------------------------
         -- Receive loop
         ----------------------------
         Global_Settings.Com_Application.Receive(Basic_Msg, Recv_Status);

         if Recv_Status = VN.NO_MSG_RECEIVED then
            VN.Text_IO.Put_Line("APPL RECV: Empty.");
         elsif Recv_Status = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or
            Recv_Status = VN.MSG_RECEIVED_MORE_AVAILABLE    then

            VN.Text_IO.Put("APPL RECV: ");
            Global_Settings.Logger.Log(Basic_Msg);

            if Basic_Msg.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR then
               To_Assign_Address(Basic_Msg, Assign_Address_Msg);
               App_Info.Logical_Address := Assign_Address_Msg.Assigned_Address;
            end if;

         end if;

         ----------------------------
         -- Send loop
         ----------------------------
         --if App_Info.Has_Logical_Address then
         --   null;
         --elsif not App_Info.Has_Logical_Address then
         --   null;
         --end if;


         Next_Period := Next_Period + Period;
         i := i + 1;
         exit when i = 6;
      end loop;
      ----------------------------

      VN.Text_IO.Put_Line("APPL STAT: Stop. Logical Address: " &
                                 App_Info.Logical_Address'Img);

   end VN_Application;

   App: VN_Application(30, Global_Settings.Cycle_Time_Applications, 30, 3);

end Application;
