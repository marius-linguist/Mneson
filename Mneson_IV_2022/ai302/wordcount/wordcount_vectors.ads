with AI302.Containers.Vectors;
pragma Elaborate_All (AI302.Containers.Vectors);

with Wordcount_Maps;  use Wordcount_Maps;

package Wordcount_Vectors is
   new AI302.Containers.Vectors
    (Positive,
     Cursor_Type);

pragma Preelaborate (Wordcount_Vectors);



