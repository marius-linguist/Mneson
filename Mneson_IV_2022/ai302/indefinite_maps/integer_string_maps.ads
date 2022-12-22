with Indefinite_Maps;
pragma Elaborate_All (Indefinite_Maps);

with Hash_Integer;

package Integer_String_Maps is
   new Indefinite_Maps (Integer, String, Hash_Integer);

pragma Preelaborate (Integer_String_Maps);



