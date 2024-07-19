with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure AOA_07_1 is
   type Card_ID is (Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace);

   function To_Card (Letter : in Character) return Card_ID with
      Pre => Letter in 'A' | 'K' | 'Q' | 'J' | 'T' | '2' .. '9';
   -- Converts letter to a Card_ID

   function To_Card (Letter : in Character) return Card_ID is
      (case Letter is
       when 'A' => Ace,
       when 'K' => King,
       when 'Q' => Queen,
       when 'J' => Jack,
       when 'T' => Ten,
       when '2' .. '9' => Card_ID'Val (Character'Pos (Letter) - Character'Pos ('2') ),
       when others => raise Program_Error with "To_Card: invalid letter " & Letter);

   type Card_List is array (1 .. 5) of Card_ID;

   type Hand_Kind_ID is (Nothing, Pair, Two_Pair, Three_Same, Full_House, Four_Same, Five_Same);

   function To_Kind (Hand : in Card_List) return Hand_Kind_ID;
   -- Determines what kind of hand Hand is

   function To_Kind (Hand : in Card_List) return Hand_Kind_ID is
      type Count_List is array (Hand'Range) of Positive;

      Count : Count_List := (others => 1);
      Pos   : Positive;
   begin -- To_Kind
      Count_Em : for I in 1 .. Hand'Last - 1 loop
         Check : for J in I + 1 .. Hand'Last loop
            Count (I) := Count (I) + (if Hand (I) = Hand (J) then 1 else 0);
         end loop Check;
      end loop Count_Em;

      if (for Some C of Count => C = 5) then
         return Five_Same;
      end if;

      if (for Some C of Count => C = 4) then
         return Four_Same;
      end if;

      if (for Some C of Count => C = 3) then
         Find_3 : for I in Count'Range loop
            if Count (I) = 3 then
               Pos := I;

               exit Find_3;
            end if;
         end loop Find_3;

         Find_Other_2 : for I in 1 .. Hand'Last loop         -- Every Three_Same will have a card with a count of 2 because of how
            if Count (I) = 2 and Hand (Pos) /= Hand (I) then -- the counts are done (the 2nd card with the same ID)
               return Full_House;                            -- Only if there's a count of 2 with a different ID is it Full_House
            end if;
         end loop Find_Other_2;

         return Three_Same;
      end if;

      if (for Some C of Count => C = 2) then
         Find_2 : for I in Count'Range loop
            if Count (I) = 2 then
               Pos := I;

               exit Find_2;
            end if;
         end loop Find_2;

         Check_Another : for I in Pos + 1 .. Hand'Last loop
            if Count (I) = 2 then
               return Two_Pair;
            end if;
         end loop Check_Another;

         return Pair;
      end if;

      return Nothing;
   end To_Kind;

   type Hand_Info is record
      Hand : Card_List;
      Bid  : Positive;
      Kind : Hand_Kind_ID;
   end record;

   package Hand_Lists is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Hand_Info);

   function Less (Left : in Hand_Info; Right : in Hand_Info) return Boolean;
   -- Main comparison on Kind; if Kinds are equal, secondary comparison on first unequal card

   package Sorting is new Hand_Lists.Generic_Sorting ("<" => Less);

   function Less (Left : in Hand_Info; Right : in Hand_Info) return Boolean is
      -- Empty
   begin -- Less
      if Left.Kind /= Right.Kind then
         return Left.Kind < Right.Kind;
      end if;

      Find_Unequal : for I in Left.Hand'Range loop
         if Left.Hand (I) /= Right.Hand (I) then
            return Left.Hand (I) < Right.Hand (I);
         end if;
      end loop Find_Unequal;

      raise Program_Error with "Less: identical hands";
   end Less;

   Input : Ada.Text_IO.File_Type;
   Hand  : Hand_Lists.Vector;
   Sum   : Natural := 0;
begin -- AOA_07_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_07");

   All_Lines : loop
      exit All_Lines when Ada.Text_IO.End_Of_File (Input);

      One_Line : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);

         Info : Hand_Info;
      begin -- One_Line
         Convert_Hand : for I in Info.Hand'Range loop
            Info.Hand (I) := To_Card (Line (I) );
         end loop Convert_Hand;

         Info.Bid := Integer'Value (Line (7 .. Line'Last) );
         Info.Kind := To_Kind (Info.Hand);

         Hand.Append (New_Item => Info);
      end One_Line;
   end loop All_Lines;

   Ada.Text_IO.Close (File => Input);
   Sorting.Sort (Container => Hand);

   Calculate : for I in 1 .. Hand.Last_Index loop
      Sum := Sum + I * Hand.Element (I).Bid;
   end loop Calculate;

   Ada.Text_IO.Put_Line (Item => Sum'Image);
end AOA_07_1;
