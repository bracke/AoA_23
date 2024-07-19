with Ada.Text_IO;

procedure AOA_06_1 is
   type Record_Info is record
      Time : Positive;
      Dist : Positive;
   end record;

   type Record_ID is range 1 .. 4;

   type Record_List is array (Record_ID) of Record_Info;

   Best : constant Record_List := (1 => (Time => 46, Dist =>  358),
                                   2 => (Time => 68, Dist => 1054),
                                   3 => (Time => 98, Dist => 1807),
                                   4 => (Time => 66, Dist => 1080) );

   Count : Natural;
   Prod  : Natural := 1;
begin -- AOA_06_1
   All_Records : for Race of Best loop
      Count := 0;

      All_Speeds : for Speed in 1 .. Race.Time - 1 loop
         if Speed * (Race.Time - Speed) > Race.Dist then
            Count := Count + 1;
         end if;
      end loop All_Speeds;

      Prod := Prod * Count;
   end loop All_Records;

   Ada.Text_IO.Put_Line (Item => Prod'Image);
end AOA_06_1;
