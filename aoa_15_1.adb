with Ada.Text_IO;
with PragmARC.Conversions.Unbounded_Strings;
with PragmARC.Line_Fields;

procedure AOA_15_1 is
   use PragmARC.Conversions.Unbounded_Strings;

   function Sum_Hash (Line : in String) return Natural;
   -- Divides Line into substrings separated by commas, runs the HASH algorithm on each substring, and returns the sum of the
   -- hashes

   function Sum_Hash (Line : in String) return Natural is
      function Hash (Line : in String) return Natural;
      -- Returns the HASH algorithm result for Line

      function Hash (Line : in String) return Natural is
         type Hash_Value is mod 256;

         Result : Hash_Value := 0;
      begin -- Hash
         All_Chars : for C of Line loop
            Result := 17 * (Result + Character'Pos (C) );
         end loop All_Chars;

         return Integer (Result);
      end Hash;

      Field : constant PragmARC.Line_Fields.Line_Field_Info := PragmARC.Line_Fields.Parsed (Line, ',');

      Sum : Natural := 0;
   begin -- Sum_Hash
      All_Fields : for I in 1 .. Field.Field.Last_Index loop
         Sum := Sum + Hash (+Field.Field.Element (I) );
      end loop All_Fields;

      return Sum;
   end Sum_Hash;

   Test_Input  : constant String := "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";
   Test_Result : constant        := 1320;

   Input : Ada.Text_IO.File_Type;
   Sum   : Natural;
begin -- AOA_15_1
   Sum := Sum_Hash (Test_Input);
   Ada.Text_IO.Put_Line (Item => Sum'Image);

   if Sum = Test_Result then
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_15");
      Sum := Sum_Hash (Ada.Text_IO.Get_Line (Input) );
      Ada.Text_IO.Close (File => Input);
      Ada.Text_IO.Put_Line (Item => Sum'Image);
   end if;
end AOA_15_1;
