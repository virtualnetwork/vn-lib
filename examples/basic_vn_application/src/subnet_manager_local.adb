with Ada.Real_Time;
with Ada.Text_IO;
with Buffers;
with Global_Settings;
with VN.Application_Information;
with VN.Message.Factory;
with VN.Message.Local_Hello;
with VN.Message.Assign_Address;
with Interfaces;

package body Subnet_Manager_Local is

   task body SM_L is
      use Ada.Real_Time;
      use VN;
      use VN.Message;
      use VN.Message.Local_Hello;
      use VN.Message.Assign_Address;
      Counter_For_Testing: Integer := 1;

      Next_Period : Ada.Real_Time.Time;
      Period : constant Ada.Real_Time.Time_Span :=
                           Ada.Real_Time.Microseconds(Cycle_Time);

   begin
      SM_L_Info.Component_Type := VN.Message.SM_L;
      SM_L_Info.Logical_Address := 2;

      Global_Settings.Start_Time.Get(Next_Period);
      Ada.Text_IO.Put_Line("SM-L STAT: Starts.");

      ----------------------------
      loop
         delay until Next_Period;

         ----------------------------
         -- Receive loop
         ----------------------------
         Global_Settings.Com_SM_L.Receive(Basic_Msg, Recv_Status);

         --Ada.Text_IO.Put_Line("SM-L RECV Status: " &
         --                       VN.Receive_Status'Image(Recv_Status));

         if Recv_Status = VN.NO_MSG_RECEIVED then
            Ada.Text_IO.Put_Line("SM-L RECV: Empty.");

         elsif Recv_Status = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or
            Recv_Status = VN.MSG_RECEIVED_MORE_AVAILABLE    then

            -- Print debug text.
            Ada.Text_IO.Put("SM-L RECV: ");
            Global_Settings.Logger.Log(Basic_Msg);

            -- Process incoming message.
            if Basic_Msg.Header.Opcode = VN.Message.OPCODE_LOCAL_HELLO then
               To_Local_Hello(Basic_Msg, Local_Hello_Msg);
               if (Local_Hello_Msg.Component_Type = VN.Message.Other or
                  Local_Hello_Msg.Component_Type = VN.Message.LS) then
                     Unsigned_8_Buffer.Insert(Local_Hello_Msg.CUUID(1), Assign_Address_Buffer);
               end if;
            end if;

         end if;

         ----------------------------
         -- Send loop
         ----------------------------

         -- Assign Address
        if not Unsigned_8_Buffer.Empty(Assign_Address_Buffer) and
            Has_Received_Address_Block then
           Unsigned_8_Buffer.Remove(Temp_Uint8, Assign_Address_Buffer);

           Basic_Msg := VN.Message.Factory.Create(VN.Message.Type_Assign_Address);
           Basic_Msg.Header.Destination := 0;
           To_Assign_Address(Basic_Msg, Assign_Address_Msg);
           Assign_Address_Msg.CUUID := (others => Temp_Uint8);
           Assign_Address_Msg.Assigned_Address := Assigned_Address;
           To_Basic(Assign_Address_Msg, Basic_Msg);

           Assigned_Address := Assigned_Address + 1;

           Ada.Text_IO.Put("SM-L SEND: ");
           Global_Settings.Logger.Log(Basic_Msg);
           Global_Settings.Com_SM_L.Send(Basic_Msg, Send_Status);

        end if;

         Next_Period := Next_Period + Period;
         Counter_For_Testing := Counter_For_Testing + 1;
         exit when Counter_For_Testing = 30;
      end loop;
      ----------------------------

      Ada.Text_IO.Put_Line("SM-L STAT: Stop.");

   end SM_L;

   -- Start one instance of the SM-L
   SM_L1: SM_L(20, 1000000, 80, 3);

   ----------------------------
   -- Helper functions below
   ----------------------------
   function Get_Address_To_Assign(CUUID_Uint8: in Interfaces.Unsigned_8)
         return VN.VN_Logical_Address is
      use VN;
      Assigned_Address : VN.VN_Logical_Address := 10;
   begin
      return Assigned_Address + 1;
   end Get_Address_To_Assign;

   function Has_Received_Address_Block return Boolean is
      use VN;
   begin
      if Received_Address_Block /= 0 then
         return true;
      else
         return false;
      end if;
   end Has_Received_Address_Block;

end Subnet_Manager_Local;
