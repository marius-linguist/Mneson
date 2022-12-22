package AI302.Containers.Prime_Numbers is

   pragma Pure;

   type Primes_Type is array (Positive range <>) of Hash_Type;

   --NOTE:
   --I got these numbers from the SGI STL sources.  I don't understand
   --how A.S. came up with these values.  Is he trying to stay way from
   --powers of 2 maybe?
   --END NOTE.

   Primes : constant Primes_Type :=
     (5, 7, 11, 13, 17, 19, 23, 31, --for debugging
      53,         97,         193,       389,       769,
      1543,       3079,       6151,      12289,     24593,
      49157,      98317,      196613,    393241,    786433,
      1572869,    3145739,    6291469,   12582917,  25165843,
      50331653,   100663319,  201326611, 402653189, 805306457,
      1610612741, 3221225473, 4294967291);

   function To_Prime (Length : Size_Type) return Hash_Type;

end AI302.Containers.Prime_Numbers;
