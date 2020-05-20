pragma SPARK_Mode (On);

with PasswordDatabase;
with MyCommandLine;
with MyString;
with MyStringTokeniser;
with PIN;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
procedure Main is
   DB : PasswordDatabase.Database;
   U1 : PasswordDatabase.URL := PasswordDatabase.From_String("google.com");
   P1 : PasswordDatabase.Password := PasswordDatabase.From_String("test_password");
   PIN1  : PIN.PIN := PIN.From_String("1234");
   PIN2  : PIN.PIN := PIN.From_String("1234");
   package Lines is new MyString(Max_MyString_Length => 2048);
   S  : Lines.MyString;
begin

   Put(MyCommandLine.Command_Name); Put_Line(" is running!");
   Put("I was invoked with "); Put(MyCommandLine.Argument_Count,0); Put_Line(" arguments.");
   for Arg in 1..MyCommandLine.Argument_Count loop
      Put("Argument "); Put(Arg,0); Put(": """);
      Put(MyCommandLine.Argument(Arg)); Put_Line("""");
   end loop;

   PasswordDatabase.Init(DB);
   Put_Line("Adding an entry to the database");
   PasswordDatabase.Put(DB,U1,P1);

   Put_Line("Reading the entry:");
   Put_Line(PasswordDatabase.To_String(PasswordDatabase.Get(DB,U1)));

   Put_Line("Removing the entry");
   PasswordDatabase.Remove(DB,U1);
   If PasswordDatabase.Has_Password_For(DB,U1) then
      Put_Line("Entry still present! It is: ");
      Put_Line(PasswordDatabase.To_String(PasswordDatabase.Get(DB,U1)));
   else
      Put_Line("Entry successfully removed");
   end if;

   Put_Line("Reading a line of input. Enter some text (at most 3 tokens): ");
   Lines.Get_Line(S);

   Put_Line("Splitting the text into at most 4 tokens");
   declare
      T : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
      NumTokens : Natural;
   begin
      MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
      Put("You entered "); Put(NumTokens); Put_Line(" tokens.");
      if NumTokens > 3 then
         Put_Line("Too many tokens!");
      else
         for I in 1..NumTokens loop
            declare
               TokStr : String := Lines.To_String(Lines.Substring(S,T(I).Start,T(I).Start+T(I).Length-1));
            begin
               Put("Token "); Put(I); Put(" is: """);
               Put(TokStr); Put_Line("""");
            end;
         end loop;
      end if;
   end;

   If PIN."="(PIN1,PIN2) then
      Put_Line("The two PINs are equal, as expected.");
   end if;
end Main;
