with AI302.Containers.String_Hashed_Indefinite_Maps;
pragma Elaborate_All (AI302.Containers.String_Hashed_Indefinite_Maps);

with AI302.Strings.Case_Insensitive;
pragma Elaborate_All (AI302.Strings.Case_Insensitive);  --need?

package Data_Maps is
   new AI302.Containers.String_Hashed_Indefinite_Maps
     (String,
      AI302.Strings.Case_Insensitive.Hash,
      AI302.Strings.Case_Insensitive."=",
      AI302.Strings.Case_Insensitive."=");

pragma Preelaborate (Data_Maps);



