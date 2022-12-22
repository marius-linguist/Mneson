with Indefinite_Vectors;
pragma Elaborate_All (Indefinite_Vectors);

package String_Vectors is
   new Indefinite_Vectors (Positive, String);

pragma Preelaborate (String_Vectors);


