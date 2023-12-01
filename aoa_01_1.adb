with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

procedure AOA_01_1 is
   Digit : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (Span => (Low => '0', High => '9') );
   Input : Ada.Text_IO.File_Type;
   Sum   : Natural := 0;
begin -- AOA_01_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_01");

   All_Lines : loop
      exit All_Lines when Ada.Text_IO.End_Of_File (Input);

      One_Line : declare
         Line  : constant String  := Ada.Text_IO.Get_Line (Input);
         First : constant Natural := Ada.Strings.Fixed.Index (Line, Digit);
         Last  : constant Natural := Ada.Strings.Fixed.Index (Line, Digit, Going => Ada.Strings.Backward);
      begin -- One_Line
         if First = 0 or Last = 0 then
            Ada.Text_IO.Put_Line (Item => "Line without 2 digits: " & Line);
         else
            Sum := Sum + Integer'Value (Line (First) & Line (Last) );
         end if;
      end One_Line;
   end loop All_Lines;

   Ada.Text_IO.Close (File => Input);
   Ada.Text_IO.Put_Line (Item => Sum'Image);
end AOA_01_1;
