with VN;

with VN.Message;
with VN.Message.Factory;


procedure Test is

   msg1, msg2 : VN.Message.VN_Message_Basic;
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

   VN.Message.Serialize(msg1, arr);

   arr(arr'First + 1) := 0;

--   arr(arr'Last) := 0;

   VN.Message.DeSerialize(msg2, arr);


   VN.Text_IO.Put_Line("Message_Type= " & msg2.Header.Message_Type'Img);
   VN.Text_IO.Put_Line("Version= " & msg2.Header.Version'Img & ", arr'Size " & arr'Size'Img);
   VN.Text_IO.Put_Line("Priority= " & msg2.Header.Priority'Img);
   VN.Text_IO.Put_Line("Payload_Length= " & msg2.Header.Payload_Length'Img);
   VN.Text_IO.Put_Line("Destination= " & msg2.Header.Destination'Img);
   VN.Text_IO.Put_Line("Source= " & msg2.Header.Source'Img);
   VN.Text_IO.Put_Line("Flags= " & msg2.Header.Flags'Img);
   VN.Text_IO.Put_Line("Opcode= " & msg2.Header.Opcode'Img);
   VN.Text_IO.Put_Line("Ext_Header= " & msg2.Header.Ext_Header'Img);
   VN.Text_IO.Put_Line("Checksum= " & msg2.Checksum'Img);


end Test;
