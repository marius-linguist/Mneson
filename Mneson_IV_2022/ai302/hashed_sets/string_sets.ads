with Indefinite_Hashed_Sets;
pragma Elaborate_All (Indefinite_Hashed_Sets);

with AI302.Strings.Hash;

package String_Sets is
   new Indefinite_Hashed_Sets (String, AI302.Strings.Hash);

pragma Preelaborate (String_Sets);
