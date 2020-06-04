with Ada.Characters.Latin_1;

package MyStringTokeniser with SPARK_Mode is

   type TokenExtent is record
      Start : Positive;
      Length : Natural;
   end record;

   type TokenArray is array(Positive range <>) of TokenExtent;

   function Is_Whitespace(Ch : Character) return Boolean is
     (Ch = ' ' or Ch = Ada.Characters.Latin_1.LF or
        Ch = Ada.Characters.Latin_1.HT);

   -- Pre:
   --   Checks length of the string being passed
   --   If it is larger than 0, then first element index is smaller than the
   --   last element index
   --   First index of Tokens is less than or equal to the last index of Tokens
   --
   -- Post:
   --   The count of tokens is less or equal to the length of Tokens
   --   For all tokens:
   --     - the start index of the token is greater than or equal to the start
   --       index of the string
   --     - the length of the token is greater than 0
   --     - if these hold then the index of the last character of the token is
   --       less than the last index of S
   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) with
     Pre => (if S'Length > 0 then S'First <= S'Last) and Tokens'First <= Tokens'Last,
     Post => Count <= Tokens'Length and
     (for all Index in Tokens'First..Tokens'First+(Count-1) =>
          (Tokens(Index).Start >= S'First and
          Tokens(Index).Length > 0) and then
            Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start);


end MyStringTokeniser;
