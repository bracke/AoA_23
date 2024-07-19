with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

procedure AOA_14_1 is
   package Platform_Line_List is new Ada.Containers.Indefinite_Vectors (Index_Type => Positive, Element_Type => String);

   Empty : constant Character := '.';
   Round : constant Character := 'O';
   Block : constant Character := '#';

   procedure Roll_North (Platform : in out Platform_Line_List.Vector);
   -- Rolls all the Round rocks on Platform as far north as possible

   procedure Roll_North (Platform : in out Platform_Line_List.Vector) is
      Dest : Natural;
   begin -- Roll_North
      All_Rows : for R in 2 .. Platform.Last_Index loop
         One_Row : declare
            Row : String := Platform.Element (R);
         begin -- One_Row
            All_Columns : for C in Row'Range loop
               if Row (C) = Round then
                  Find_Dest : for D in reverse 1 .. R - 1 loop
                     One_Dest : declare
                        Line : constant String := Platform.Element (D);
                     begin -- One_Dest
                        if Line (C) /= Empty then
                           Dest := D + 1;

                           exit Find_Dest;
                        elsif D = 1 then
                           Dest := 1;
                        else
                           null;
                        end if;
                     end One_Dest;
                  end loop Find_Dest;

                  if Dest < R then
                     Row (C) := Empty;

                     New_Home : declare
                        Line : String := Platform.Element (Dest);
                     begin -- New_Home
                        Line (C) := Round;
                        Platform.Replace_Element (Index => Dest, New_Item => Line);
                     end New_Home;
                  end if;
               end if;
            end loop All_Columns;

            Platform.Replace_Element (Index => R, New_Item => Row);
         end One_Row;
      end loop All_Rows;
   end Roll_North;

   Input    : Ada.Text_IO.File_Type;
   Platform : Platform_Line_List.Vector;
   Load     : Positive;
   Sum      : Natural := 0;
begin -- AOA_14_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_14");

   All_Lines : loop
      exit All_Lines when Ada.Text_IO.End_Of_File (Input);

      Platform.Append (New_Item => Ada.Text_IO.Get_Line (Input) );
   end loop All_Lines;

   Ada.Text_IO.Close (File => Input);
   Roll_North (Platform => Platform);

   Row_Load : for R in 1 .. Platform.Last_Index loop
      Load := Platform.Last_Index - R + 1;

      Get_Row : declare
         Row : constant String := Platform.Element (R);
      begin -- Get_Row
         Column_Load : for C in Row'Range loop
            if Row (C) = Round then
               Sum := Sum + Load;
            end if;
         end loop Column_Load;
      end Get_Row;
   end loop Row_Load;

   Ada.Text_IO.Put_Line (Item => Sum'Image);
end AOA_14_1;
