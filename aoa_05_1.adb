with Ada.Containers.Vectors;
with Ada.Text_IO;
with PragmARC.Conversions.Unbounded_Strings;
with PragmARC.Line_Fields;

procedure AOA_05_1 is
   use PragmARC.Conversions.Unbounded_Strings;

   type Thing_ID is range 0 .. 2 ** 63 - 1;

   type Range_Info is record
      Low  : Thing_ID;
      High : Thing_ID;
   end record;

   type Mapping_Info is record
      Source : Range_Info;
      Dest   : Thing_ID;
   end record;

   package Map_Sets  is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Mapping_Info);
   package Map_Lists is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Map_Sets.Vector, "=" => Map_Sets."=");

   Input    : Ada.Text_IO.File_Type;
   Initial  : PragmARC.Line_Fields.Line_Field_Info;
   Set_List : Map_Sets.Vector;
   Map      : Map_Lists.Vector;
   Source   : Thing_ID;
   Found    : Boolean;
   Dest     : Thing_ID;
   Min      : Thing_ID := Thing_ID'Last;
begin -- AOA_05_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_05");
   Initial := PragmARC.Line_Fields.Parsed (Ada.Text_IO.Get_Line (Input) ); -- Seed IDs start at 2
   Ada.Text_IO.Skip_Line (File => Input);

   Handle_EOF : begin
      All_Maps : loop
         Ada.Text_IO.Put_Line (Item => Ada.Text_IO.Get_Line (Input) ); -- Skip header
         Set_List.Clear;

         All_Sets : loop
            One_Set : declare
               Line : constant String := Ada.Text_IO.Get_Line (Input);

               Set   : Mapping_Info;
               Value : PragmARC.Line_Fields.Line_Field_Info;
               Src   : Thing_ID;
               Len   : Thing_ID;
            begin -- One_Set
               exit All_Sets when Line = "";

               Value := PragmARC.Line_Fields.Parsed (Line);
               Src := Thing_ID'Value (+Value.Field.Element (2) );
               Len := Thing_ID'Value (+Value.Field.Element (3) ) - 1;
               Set := (Source => (Low => Src, High => Src + Len),
                       Dest   => Thing_ID'Value (+Value.Field.Element (1) ) );
               Set_List.Append (New_Item => Set);
            end One_Set;
         end loop All_Sets;

         Map.Append (New_Item => Set_List);
      end loop All_Maps;
   exception -- Handle_EOF
   when Ada.Text_IO.End_Error =>
      null;
   end Handle_EOF;

   Ada.Text_IO.Close (File => Input);

   All_Seeds : for I in 2 .. Initial.Field.Last_Index loop
      Source := Thing_ID'Value (+Initial.Field.Element (I) );

      Check_Maps : for M in 1 .. Map.Last_Index loop
         Found := False;

         Check_Sets : for S in 1 .. Map.Element (M).Last_Index loop
            Check_Set : declare
               Set : Mapping_Info renames Map.Element (M).Element (S);
            begin -- Check_Set
               if Source in Set.Source.Low .. Set.Source.High then
                  Found := True;
                  Dest := Set.Dest + (Source - Set.Source.Low);

                  exit Check_Sets;
               end if;
            end Check_Set;
         end loop Check_Sets;

         if not Found then
            Dest := Source;
         end if;

         Source := Dest;
      end loop Check_Maps;

      Min := Thing_ID'Min (Min, Dest);
   end loop All_Seeds;

   Ada.Text_IO.Put_Line (Item => Min'Image);
end AOA_05_1;
