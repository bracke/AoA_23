with Ada.Containers.Vectors;
with Ada.Text_IO;
with PragmARC.Conversions.Unbounded_Strings;
with PragmARC.Line_Fields;

procedure AOA_09_1 is
   use PragmARC.Conversions.Unbounded_Strings;

   package Sequences is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Integer);

   type Difference_Set is array (1 .. 19) of Sequences.Vector; -- 1 => input, 2 .. 19 => differences

   Input : Ada.Text_IO.File_Type;
   Diff  : Difference_Set;
   Level : Positive;
   Sum   : Natural := 0;
begin -- AOA_09_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_09");

   All_Lines : loop
      exit All_Lines when Ada.Text_IO.End_Of_File (Input);

      One_Line : declare
         Line  : constant String                               := Ada.Text_IO.Get_Line (Input);
         Field : constant PragmARC.Line_Fields.Line_Field_Info := PragmARC.Line_Fields.Parsed (Line);
      begin -- One_Line
         Diff := (others => Sequences.Empty_Vector);

         Convert : for I in 1 .. Field.Field.Last_Index loop
            Diff (1).Append (New_Item => Integer'Value (+Field.Field.Element (I) ) );
         end loop Convert;

         All_Levels : for L in 2 .. Diff'Last loop
            All_Diffs : for I in 1 .. Diff (L - 1).Last_Index - 1 loop
               Diff (L).Append (New_Item => Diff (L - 1).Element (I + 1) - Diff (L - 1).Element (I) );
            end loop All_Diffs;

            if (for all I in 2 .. Diff (L).Last_Index => Diff (L).Element (1) = Diff (L).Element (I) ) then
               Level := L;

               exit All_Levels;
            end if;

            if L = Diff'Last then -- 18 levels of differences not enough
               raise Program_Error with "need more than 10 levels for " & Line;
            end if;
         end loop All_Levels;

         Extend : for L in reverse 1 .. Level - 1 loop
            Diff (L).Append
               (New_Item => Diff (L + 1).Element (Diff (L + 1).Last_Index) + Diff (L).Element (Diff (L).Last_Index) );
         end loop Extend;

         Sum := Sum + Diff (1).Element (Diff (1).Last_Index);
      end One_Line;
   end loop All_Lines;

   Ada.Text_IO.Close (File => Input);
   Ada.Text_IO.Put_Line (Item => Sum'Image);
end AOA_09_1;
