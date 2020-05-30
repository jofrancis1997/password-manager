package PasswordManager with Spark_Mode is
   function Is_PIN(S : in String) return Boolean with
      Post => (if Is_PIN'Result then
         S'Length = 4 and
         (for all I in S'Range => S(I) >= '0' and S(I) <= '9'));
end PasswordManager;
