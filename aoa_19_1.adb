with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with PragmARC.Conversions.Unbounded_Strings;
with PragmARC.Line_Fields;

procedure AOA_19_1 is
   use Ada.Strings.Unbounded;
   use PragmARC.Conversions.Unbounded_Strings;

   type Category_ID is (Cool, Musical, Aerodynamic, Shiny); -- 'x', 'm', 'a', 's'

   function To_Category (Char : in Character) return Category_ID is
      (case Char is
       when 'x' => Cool,
       when 'm' => Musical,
       when 'a' => Aerodynamic,
       when 's' => Shiny,
       when others => raise Program_Error with "Invalid category character " & Char);

   type Rule_Info (Test : Boolean := False) is record
      Next_Rule : Unbounded_String;

      case Test is
      when False =>
         null;
      when True =>
         Category : Category_ID;
         Operator : Character;
         Value    : Natural;
      end case;
   end record;

   package Rule_Lists is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Rule_Info);
   package Workflows is new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                                   Element_Type    => Rule_Lists.Vector,
                                                                   Hash            => Ada.Strings.Hash,
                                                                   Equivalent_Keys => "=",
                                                                   "="             => Rule_Lists."=");

   type Part_Map is array (Category_ID) of Natural;

   Input : Ada.Text_IO.File_Type;
   Map   : Workflows.Map;
   Sum   : Natural := 0;
begin -- AOA_19_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_19");

   Read_Workflows : loop
      One_Flow : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);

         Brace : Natural;
         List  : PragmARC.Line_Fields.Line_Field_Info;
         Colon : Natural;
         Rule  : Rule_Lists.Vector;
      begin -- One_Flow
         exit Read_Workflows when Line = "";

         Brace := Ada.Strings.Fixed.Index (Line, "{");
         List := PragmARC.Line_Fields.Parsed (Line (Brace + 1 .. Line'Last - 1), ',');

         All_Rules : for I in 1 .. List.Field.Last_Index loop
            One_Rule : declare
               Field : constant String := +List.Field.Element (I);
            begin -- One_Rule
               Colon := Ada.Strings.Fixed.Index (Field, ":");

               if Colon = 0 then
                  Rule.Append (New_Item => (Test => False, Next_Rule => +Field) );
               else
                  Rule.Append (New_Item => (Test      => True,
                                            Next_Rule => +Field (Colon + 1 .. Field'Last),
                                            Category => To_Category (Field (1) ),
                                            Operator => Field (2),
                                            Value    => Integer'Value (Field (3 .. Colon - 1) ) ) );
               end if;
            end One_Rule;
         end loop All_Rules;

         Map.Insert (Key => Line (1 .. Brace - 1), New_Item => Rule);
      end One_Flow;
   end loop Read_Workflows;

   All_Parts : loop
      exit All_Parts when Ada.Text_IO.End_Of_File (Input);

      One_Part : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);
         Field : constant PragmARC.Line_Fields.Line_Field_Info := PragmARC.Line_Fields.Parsed (Line (2 .. Line'Last - 1), ',');

         Part : Part_Map;
         Next : Unbounded_String := +"in";
         Flow : Rule_Lists.Vector;
         Rule : Rule_Info;
         Test : Boolean := False;
      begin -- One_Part
         All_Categories : for I in 1 .. Field.Field.Last_Index loop
            One_Category : declare
               Assoc : constant String := +Field.Field.Element (I);
            begin -- One_Category
               Part (To_Category (Assoc (1) ) ) := Integer'Value (Assoc (3 .. Assoc'Last) );
            end One_Category;
         end loop All_Categories;

         All_Flows : loop
            Flow := Map.Element (+Next);

            Flow_Rules : for I in 1 .. Flow.Last_Index loop
               Rule := Flow.Element (I);

               if Rule.Test then
                  if Rule.Operator = '<' then
                     Test := Part (Rule.Category) < Rule.Value;
                  else
                     Test := Part (Rule.Category) > Rule.Value;
                  end if;
               end if;

               if Test or not Rule.Test then
                  Next := Rule.Next_Rule;

                  exit All_Flows when Next = "R";

                  if Next = "A" then
                     Sum_Values : for Category in Part'Range loop
                        Sum := Sum + Part (Category);
                     end loop Sum_Values;

                     exit All_Flows;
                  end if;

                  exit Flow_Rules;
               end if;

               if I = Flow.Last_Index then
                  raise Program_Error with "Invalid flow with no matching rule";
               end if;
            end loop Flow_Rules;
         end loop All_Flows;
      end One_Part;
   end loop All_Parts;

   Ada.Text_IO.Close (File => Input);
   Ada.Text_IO.Put_Line (Item => Sum'Image);
end AOA_19_1;
