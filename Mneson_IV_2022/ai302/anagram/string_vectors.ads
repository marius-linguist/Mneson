with AI302.Containers.Indefinite_Vectors;
pragma Elaborate_All (AI302.Containers.Indefinite_Vectors);

package String_Vectors is
   new AI302.Containers.Indefinite_Vectors (Positive, String);

pragma Preelaborate (String_Vectors);

