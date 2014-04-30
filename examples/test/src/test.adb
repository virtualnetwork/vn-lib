with VN;

with VN.Message;
with VN.Message.Local_Hello;
with VN.Message.Factory;


procedure Test is

   msg1, msg2 : VN.Message.VN_Message_Basic;
   msg3 : VN.Message.Local_Hello.VN_Message_Local_Hello;

   arr : VN.Message.VN_Message_Byte_Array;

begin

   msg1 := VN.Message.Factory.Create(VN.Message.Type_Local_Hello);
   msg1.Header.Message_Type := VN.Message.Type_Distribute_Route;
   msg1.Header.Version := 1;
   msg1.Header.Priority := 2;
   msg1.Header.Payload_Length := 3;
   msg1.Header.Destination := 4;
   msg1.Header.Source := 5;
   msg1.Header.Flags := 6;
   msg1.Header.Opcode := 7;
   msg1.Header.Ext_Header := 8;
   msg1.Checksum := 257;

   VN.Message.Local_Hello.To_Local_Hello(msg1, msg3);

   msg3.CUUID := (others => 3);
   msg3.Component_Type := VN.Message.SM_x;

   VN.Message.Local_Hello.To_Basic(msg3, msg1);

   VN.Message.Serialize(msg1, arr);

   arr(arr'First + 33) := 1;

  -- arr(arr'Last) := 0;

   VN.Message.DeSerialize(msg2, arr);

   VN.Message.Local_Hello.To_Local_Hello(msg2, msg3);

   VN.Text_IO.Put_Line("Message_Type= " & msg3.Header.Message_Type'Img);
   VN.Text_IO.Put_Line("Version= " & msg3.Header.Version'Img & ", arr'Size " & arr'Size'Img);
   VN.Text_IO.Put_Line("Priority= " & msg3.Header.Priority'Img);
   VN.Text_IO.Put_Line("Payload_Length= " & msg3.Header.Payload_Length'Img);
   VN.Text_IO.Put_Line("Destination= " & msg3.Header.Destination'Img);
   VN.Text_IO.Put_Line("Source= " & msg3.Header.Source'Img);
   VN.Text_IO.Put_Line("Flags= " & msg3.Header.Flags'Img);
   VN.Text_IO.Put_Line("Opcode= " & msg3.Header.Opcode'Img);
   VN.Text_IO.Put_Line("Ext_Header= " & msg3.Header.Ext_Header'Img);
   VN.Text_IO.Put_Line("Checksum= " & msg3.Checksum'Img);
   VN.Text_IO.Put_Line("CUUID(1)= " & msg3.CUUID(1)'Img);
   VN.Text_IO.Put_Line("Component_Type= " & msg3.Component_Type'Img);

end Test;
