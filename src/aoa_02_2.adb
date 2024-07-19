with Ada.Strings.Fixed;
with Ada.Text_IO;
with PragmARC.Conversions.Unbounded_Strings;
with PragmARC.Line_Fields;

procedure AOA_02_2 is
   use PragmARC.Conversions.Unbounded_Strings;

   type Color_ID is (Red, Green, Blue);

   type Color_Count is array (Color_ID) of Natural;

   Input : Ada.Text_IO.File_Type;
   Sum   : Natural := 0;
begin -- AOA_02_2
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_02");

   All_Lines : loop
      exit All_Lines when Ada.Text_IO.End_Of_File (Input);

      One_Line : declare
         Line  : constant String                               := Ada.Text_IO.Get_Line (Input);
         Blank : constant Natural                              := Ada.Strings.Fixed.Index (Line, " ");
         Colon : constant Natural                              := Ada.Strings.Fixed.Index (Line, ":", From => Blank + 1);
         ID    : constant Positive                             := Integer'Value (Line (Blank + 1 .. Colon - 1) );
         Game  : constant PragmARC.Line_Fields.Line_Field_Info :=
            PragmARC.Line_Fields.Parsed (Line (Colon + 1 .. Line'Last), ';');

         Max : Color_Count := (others => 0);
      begin -- One_Line
         All_Games : for G in 1 .. Game.Field.Last_Index loop
            One_Game : declare
               Grab  : constant PragmARC.Line_Fields.Line_Field_Info := PragmARC.Line_Fields.Parsed (+Game.Field.Element (G), ',');
            begin -- One_Game
               All_Colors : for I in 1 .. Grab.Field.Last_Index loop
                  One_Color : declare
                     Info  : constant String   := +Grab.Field.Element (I);
                     Space : constant Natural  := Ada.Strings.Fixed.Index (Info, " ", From => 2);
                     Count : constant Positive := Integer'Value (Info (2 .. Space - 1) );
                     Color : constant Color_ID := Color_ID'Value (Info (Space + 1 .. Info'Last) );
                  begin -- One_Color
                     Max (Color) := Integer'Max (Max (Color), Count);
                  end One_Color;
               end loop All_Colors;
            end One_Game;
         end loop All_Games;

         Sum := Sum + Max (Red) * Max (Green) * Max (Blue);
      end One_Line;
   end loop All_Lines;

   Ada.Text_IO.Close (File => Input);
   Ada.Text_IO.Put_Line (Item => Sum'Image);
end AOA_02_2;
