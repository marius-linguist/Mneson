with AI302.Containers.String_Hashed_Maps;
pragma Elaborate_All (AI302.Containers.String_Hashed_Maps);

with AI302.Strings.Case_Insensitive;  use AI302.Strings;

package Wordcount_Maps is
   new AI302.Containers.String_Hashed_Maps
     (Element_Type => Natural,
      Hash         => Case_Insensitive.Hash,
      Is_Equal_Key => Case_Insensitive."=");

pragma Preelaborate (Wordcount_Maps);


