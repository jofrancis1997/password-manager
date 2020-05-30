package body PasswordManager with Spark_Mode is
   function Is_PIN(S : in String) return Boolean is
   begin
      if S'Length /= 4 then
         return false;
      end if;
      for I in S'Range loop
         declare
            Ch : Character := S(I);
         begin
            if Ch < '0' or Ch > '9' then
               return false;
            end if;
            pragma Loop_Invariant (Ch >= '0' and Ch <= '9');
         end;
      end loop;
      return true;
   end Is_PIN;
end PasswordManager;
