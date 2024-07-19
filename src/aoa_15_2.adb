with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with PragmARC.Conversions.Unbounded_Strings;
with PragmARC.Line_Fields;

procedure AOA_15_2 is
   use Ada.Strings.Unbounded;
   use PragmARC.Conversions.Unbounded_Strings;

   function Sum_Powers (Line : in String) return Natural;
   -- Divides Line into substrings separated by commas, uses them to fill a map of lenses, and returns the sum of the powers of
   -- the mapped lenses

   function Sum_Powers (Line : in String) return Natural is
      type Hash_Value is mod 256;

      subtype Focal_Length is Integer range 1 .. 9;

      type Lens_Info is record
         ID     : Unbounded_String;
         Length : Focal_Length;
      end record;

      function "=" (Left : in Lens_Info; Right : in Lens_Info) return Boolean is
         (Left.ID = Right.ID);

      package Lens_Lists is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Lens_Info);
      type Lens_Map is array (Hash_Value) of Lens_Lists.Vector;

      function Hash (Line : in String) return Hash_Value;
      -- Returns the HASH algorithm result for Line

      function Hash (Line : in String) return Hash_Value is
         Result : Hash_Value := 0;
      begin -- Hash
         All_Chars : for C of Line loop
            Result := 17 * (Result + Character'Pos (C) );
         end loop All_Chars;

         return Result;
      end Hash;

      Field : constant PragmARC.Line_Fields.Line_Field_Info := PragmARC.Line_Fields.Parsed (Line, ',');

      Map : Lens_Map;
      Sum : Natural := 0;
   begin -- Sum_Powers
      All_Fields : for I in 1 .. Field.Field.Last_Index loop
         One_Field : declare
            Line : constant String := +Field.Field.Element (I);

            Sep  : Positive;
            Box  : Hash_Value;
            Lens : Lens_Info;
            Idx  : Natural;
         begin -- One_Field
            Sep := (if Line (Line'Last) = '-' then Line'Last else Line'Last - 1);
            Box := Hash (Line (Line'First .. Sep - 1) );
            Lens := (ID => +Line (Line'First .. Sep - 1),
                     Length => (if Line (Sep) = '=' then Integer'Value (Line (Sep + 1 .. Sep + 1) ) else 1) );
            Idx := Map (Box).Find_Index (Lens);

            if Line (Sep) = '=' then
               if Idx = 0 then
                  Map (Box).Append (New_Item => Lens);
               else
                  Map (Box).Replace_Element (Index => Idx, New_Item => Lens);
               end if;
            elsif Idx > 0 then
               Map (Box).Delete (Index => Idx);
            else
               null;
            end if;
         end One_Field;
      end loop All_Fields;

      All_Boxes : for B in Map'Range loop
         All_Lenses : for I in 1 .. Map (B).Last_Index loop
            Sum := Sum + (Integer (B) + 1) * I * Map (B).Element (I).Length;
         end loop All_Lenses;
      end loop All_Boxes;

      return Sum;
   end Sum_Powers;

   Test_Input  : constant String := "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";
   Test_Result : constant        := 145;

   Input : Ada.Text_IO.File_Type;
   Sum   : Natural;
begin -- AOA_15_2
   Sum := Sum_Powers (Test_Input);
   Ada.Text_IO.Put_Line (Item => Sum'Image);

   if Sum = Test_Result then
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_15");
      Sum := Sum_Powers (Ada.Text_IO.Get_Line (Input) );
      Ada.Text_IO.Close (File => Input);
      Ada.Text_IO.Put_Line (Item => Sum'Image);
   end if;
end AOA_15_2;
