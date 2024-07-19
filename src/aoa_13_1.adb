with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

procedure AOA_13_1 is
   package Pattern_Line_List is new Ada.Containers.Indefinite_Vectors (Index_Type => Positive, Element_Type => String);

   procedure Check_Horiz (Pattern : in Pattern_Line_List.Vector; Found : out Boolean; Top : out Natural);
   -- Check Pattern for horizontal reflection
   -- Found is true if reflection is found; False otherwise
   -- If not Found, Top is zero; otherwise, Top is the row number just above the reflection line

   procedure Check_Vert (Pattern : in Pattern_Line_List.Vector; Found : out Boolean; Left : out Natural);
   -- Check Pattern for vertical reflection
   -- Found is true if reflection is found; False otherwise
   -- If not Found, Left is zero; otherwise, Left is the column number just left of the reflection line

   procedure Check_Horiz (Pattern : in Pattern_Line_List.Vector; Found : out Boolean; Top : out Natural) is
      Height : Positive;
   begin -- Check_Horiz
      Top := 0;

      All_Rows : for R in 1 .. Pattern.Last_Index - 1 loop
         Height := Integer'Min (R, Pattern.Last_Index - R);
         Found := True;

         All_Pairs : for I in 1 .. Height loop
            Found := Found and Pattern.Element (R - I + 1) = Pattern.Element (R + I);

            exit All_Pairs when not Found;
         end loop All_Pairs;

         if Found then
            Top := R;

            return;
         end if;
      end loop All_Rows;
   end Check_Horiz;

   procedure Check_Vert (Pattern : in Pattern_Line_List.Vector; Found : out Boolean; Left : out Natural) is
      subtype C_String is String (1 .. Pattern.Last_Index);

      function Column_String (Column : in Positive) return C_String;
      -- Combines the Characters at column Column of all rows of Pattern into a string

      function Column_String (Column : in Positive) return C_String is
         Result : C_String;
      begin -- Column_String
         All_Rows : for R in C_String'Range loop
            Result (R) := Pattern.Element (R) (Column);
         end loop All_Rows;

         return Result;
      end Column_String;

      Width : Positive;
   begin -- Check_Vert
      Left := 0;

      All_Columns : for C in 1 .. Pattern.Element (1)'Last - 1 loop
         Width := Integer'Min (C, Pattern.Element (1)'Last - C);
         Found := True;

         All_Pairs : for I in 1 .. Width loop
            Found := Found and Column_String (C - I + 1) = Column_String (C + I);

            exit All_Pairs when not Found;
         end loop All_Pairs;

         if Found then
            Left := C;

            return;
         end if;
      end loop All_Columns;
   end Check_Vert;

   Input   : Ada.Text_IO.File_Type;
   Pattern : Pattern_Line_List.Vector;
   Pat_Num : Natural := 0;
   Found   : Boolean;
   Pos     : Natural;
   Sum     : Natural := 0;
begin -- AOA_13_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_13");

   All_Patterns : loop
      exit All_Patterns when Ada.Text_IO.End_Of_File (Input);

      Pattern.Clear;
      Pat_Num := Pat_Num + 1;

      All_Lines : loop
         One_Line : declare
            Line : constant String := Ada.Text_IO.Get_Line (Input);
         begin -- One_Line
            exit All_Lines when Line = "";

            Pattern.Append (New_Item => Line);
         end One_Line;
      end loop All_Lines;

      Check_Horiz (Pattern => Pattern, Found => Found, Top => Pos);

      if Found then
         Sum := Sum + 100 * Pos;
      else
         Check_Vert (Pattern => Pattern, Found => Found, Left => Pos);

         if Found then
            Sum := Sum + Pos;
         end if;
      end if;
   end loop All_Patterns;

   Ada.Text_IO.Close (File => Input);
   Ada.Text_IO.Put_Line (Item => Sum'Image);
end AOA_13_1;
