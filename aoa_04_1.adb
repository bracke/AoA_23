with Ada.Strings.Fixed;
with Ada.Text_IO;
with PragmARC.Conversions.Unbounded_Strings;
with PragmARC.Line_Fields;

procedure AOA_04_1 is
   use PragmARC.Conversions.Unbounded_Strings;

   Input : Ada.Text_IO.File_Type;
   Score : Natural;
   Sum   : Natural := 0;
begin -- AOA_04_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_04");

   All_Lines : loop
      exit All_Lines when Ada.Text_IO.End_Of_File (Input);

      One_Line : declare
         Line   : constant String                               := Ada.Text_IO.Get_Line (Input);
         Colon  : constant Natural                              := Ada.Strings.Fixed.Index (Line, ":");
         Bar    : constant Natural                              := Ada.Strings.Fixed.Index (Line, "|");
         Drawn  : constant PragmARC.Line_Fields.Line_Field_Info := PragmARC.Line_Fields.Parsed (Line (Colon + 2 .. Bar - 2) );
         Choice : constant PragmARC.Line_Fields.Line_Field_Info := PragmARC.Line_Fields.Parsed (Line (Bar + 2 .. Line'Last) );
      begin -- One_Line
         Score := 0;

         All_Drawn : for I in 1 .. Drawn.Field.Last_Index loop
            Check : for J in 1 .. Choice.Field.Last_Index loop
               if Integer'Value (+Drawn.Field.Element (I) ) = Integer'Value (+Choice.Field.Element (J) ) then
                  Score := (if Score = 0 then 1 else 2 * Score);

                  exit Check;
               end if;
            end loop Check;
         end loop All_Drawn;

         Sum := Sum + Score;
      end One_Line;
   end loop All_Lines;

   Ada.Text_IO.Close (File => Input);
   Ada.Text_IO.Put_Line (Item => Sum'Image);
end AOA_04_1;
