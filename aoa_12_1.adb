with Ada.Strings.Fixed;
with Ada.Text_IO;
with PragmARC.Conversions.Unbounded_Strings;
with PragmARC.Images;
with PragmARC.Line_Fields;

procedure AOA_12_1 is
   use PragmARC.Conversions.Unbounded_Strings;

   function Count_Of (Text : in String; Char : in Character) return Natural;
   -- Returns the number of Char in Text

   function Sum_Over (List : in PragmARC.Line_Fields.Field_Lists.Vector) return Natural;
   -- Converts the strings in list to integers and sums them

   type Bitmap is mod 2 ** 32;

   function Image is new PragmARC.Images.Modular_Image (Number => Bitmap);

   function One_Count (Value : in Bitmap) return Natural;
   -- Returns the number of 1 bits in value

   function Valid (Bit : in Bitmap; Text : in String; Num_Q : in Natural; Count : in PragmARC.Line_Fields.Field_Lists.Vector)
   return Boolean;
   -- After replacing Num_Q occurrences of '?' in Text with the corresponding Characters for Bit according to
   --   0 => '.'
   --   1 => '#'
   -- Returns True if that is a valid condition record for the run lengths in Count

   function Count_Of (Text : in String; Char : in Character) return Natural is
      Result : Natural := 0;
   begin -- Count_Of
      Sum : for C of Text loop
         if C = Char then
            Result := Result + 1;
         end if;
      end loop Sum;

      return Result;
   end Count_Of;

   function Sum_Over (List : in PragmARC.Line_Fields.Field_Lists.Vector) return Natural is
      Result : Natural := 0;
   begin -- Sum_Over
      Sum : for I in 1 .. List.Last_Index loop
         Result := Result + Integer'Value (+List.Element (I) );
      end loop Sum;

      return Result;
   end Sum_Over;

   function One_Count (Value : in Bitmap) return Natural is
      Text : constant String := Image (Value, Base => 2);

      Result : Natural := 0;
   begin -- One_Count
      All_Bits : for C of Text loop
         if C = '1' then
            Result := Result + 1;
         end if;
      end loop All_Bits;

      return Result;
   end One_Count;

   function Valid (Bit : in Bitmap; Text : in String; Num_Q : in Natural; Count : in PragmARC.Line_Fields.Field_Lists.Vector)
   return Boolean is
      function Replaced return String;
      -- Returns Text with '?' replaced according to the bits in Bit

      function Replaced return String is
         Result : String   := Text;
         Pos    : Natural := Result'First;
      begin -- Replaced
         All_Powers : for P in reverse 0 .. Num_Q - 1 loop
            Pos := Ada.Strings.Fixed.Index (Result, "?", From => Pos);

            exit All_Powers when Pos = 0;

            Result (Pos) := (if (Bit and 2 ** P) = 0 then '.' else '#');
            Pos := Pos + 1;
         end loop All_Powers;

         return Result;
      end Replaced;

      Full : constant String := Replaced;

      Start : Positive := Full'First;
      Stop  : Natural;
   begin -- Valid
      All_Runs : for I in 1 .. Count.Last_Index loop
         Start := Ada.Strings.Fixed.Index (Full, "#", From => Start);
         Stop := Ada.Strings.Fixed.Index (Full, ".", From => Start + 1);

         if Stop = 0 then
            Stop := Full'Last + 1;
         end if;

         if Stop - Start /= Integer'Value (+Count.Element (I) ) then
            return False;
         end if;

         Start := Stop + 1;
      end loop All_Runs;

      return True;
   end Valid;

   Input : Ada.Text_IO.File_Type;
   Sum   : Natural := 0;
begin -- AOA_12_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_12");

   All_Lines : loop
      exit All_Lines when Ada.Text_IO.End_Of_File (Input);

      One_Line : declare
         Line  : constant String                               := Ada.Text_IO.Get_Line (Input);
         Text  : constant PragmARC.Line_Fields.Line_Field_Info := PragmARC.Line_Fields.Parsed (Line);
         Count : constant PragmARC.Line_Fields.Line_Field_Info := PragmARC.Line_Fields.Parsed (+Text.Field.Element (2), ',');
         Field : constant String                               := +Text.Field.Element (1);
         Num_Q : constant Natural                              := Count_Of (Field, '?');
         Need  : constant Natural                              := Sum_Over (Count.Field) - Count_Of (Field, '#');
      begin -- One_Line
         All_Possible : for I in Bitmap range 0 .. 2 ** Num_Q - 1 loop
            if One_Count (I) = Need and then Valid (I, Field, Num_Q, Count.Field) then
               Sum := Sum + 1;
            end if;
         end loop All_Possible;
      end One_Line;
   end loop All_Lines;

   Ada.Text_IO.Close (File => Input);
   Ada.Text_IO.Put_Line (Item => Sum'Image);
end AOA_12_1;
