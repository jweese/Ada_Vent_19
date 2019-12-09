package body IC2 is
   protected body Port is
      entry Put(I: in Integer) when Status /= Full is
      begin
         if Status = Empty then
            Value := I;
            Status := Full;
         end if;
      end Put;

      entry Get(X: out Maybe_Integer) when Status /= Empty is
      begin
         case Status is
            when Full =>
               X := (Present => True, Value => Value);
               Status := Empty;
            when others => X := (Present => False);
         end case;
      end Get;

      entry Close when Status /= Full is
      begin
         Status := Closed;
      end Close;
   end Port;
end IC2;
