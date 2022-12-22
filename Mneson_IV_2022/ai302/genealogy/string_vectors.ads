with AI302.Containers.Indefinite_Vectors;
pragma Elaborate_All (AI302.Containers.Indefinite_Vectors);

with AI302.Strings.Case_Insensitive;  use AI302.Strings;

package String_Vectors is
  new AI302.Containers.Indefinite_Vectors
    (Positive,
     String,
     Case_Insensitive."=");

pragma Preelaborate (String_Vectors);

